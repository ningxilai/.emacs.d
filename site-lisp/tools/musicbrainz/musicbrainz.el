;;; musicbrainz.el --- MusicBrainz API client with structured resources -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: https://github.com/larsmagne/musicbrainz.el, Mimo V2 Flash Free/MiniMax M2.5 Free
;; Keywords: music, api
;; Package-Requires: ((emacs "27.1") (pdd "0.2.3") (eieio "1.4"))

;;; Commentary:

;; MusicBrainz API client using pdd.el for async HTTP requests.
;; This version adds structured resources (using EIEIO), caching, and better rate limiting
;; inspired by the Common Lisp cl-musicbrainz implementation.
;;
;; Usage:
;;   (musicbrainz-search-artist "artist name")
;;   (musicbrainz-search-release "album name")
;;   (musicbrainz-find-year "artist name" "album name")
;;   (musicbrainz-find-genre "artist name" "album name")
;;   (musicbrainz-find-tracklist "artist name" "album name")
;;   (musicbrainz-find-release-tracklist "artist name" "album name")
;;   M-x musicbrainz

;;; Code:

(require 'pdd)
(require 'json)
(require 'cl-lib)
(require 'subr-x)
(require 'eieio)

(defgroup musicbrainz nil
  "MusicBrainz API client for Emacs."
  :group 'music
  :prefix "musicbrainz-")

(defcustom musicbrainz-api-base "https://musicbrainz.org/ws/2"
  "Base URL for MusicBrainz API."
  :type 'string
  :group 'musicbrainz)
(defcustom musicbrainz-user-agent 
  (format "musicbrainz.el/%s" (or (ignore-errors (require 'musicbrainz)) "0.1.0"))
  "User agent string for MusicBrainz API requests.
Using anonymous format as per MusicBrainz API rate limiting policy."
  :type 'string
  :group 'musicbrainz)

(defcustom musicbrainz-rate-limit-requests 15
  "Number of allowed requests per period."
  :type 'integer
  :group 'musicbrainz)

(defcustom musicbrainz-rate-limit-period 18
  "Period in seconds for rate limiting."
  :type 'integer
  :group 'musicbrainz)

(defcustom musicbrainz-max-retries 3
  "Maximum number of retries for rate-limited requests."
  :type 'integer
  :group 'musicbrainz)

(defcustom musicbrainz-retry-delay 2.0
  "Delay in seconds before retrying a rate-limited request."
  :type 'float
  :group 'musicbrainz)

(defcustom musicbrainz-cache-size 100
  "Maximum number of cached responses."
  :type 'integer
  :group 'musicbrainz)

(defvar musicbrainz--last-request-time nil
  "Timestamp of the last API request.")

(defvar musicbrainz--cache (make-hash-table :test 'equal)
  "Cache for API responses.")

(defvar musicbrainz--cache-keys nil
  "List of cache keys for LRU eviction.")

(defvar musicbrainz--request-queue nil
  "Queue of request timestamps for rate limiting.")

;;; Rate limiting using token bucket algorithm (similar to rate-limit-threshold)

(defun musicbrainz--rate-limit ()
  "Apply smart rate limiting using token bucket algorithm.
This implements a sliding window rate limiter similar to rate-limit-threshold."
  (let* ((now (float-time))
         (period (* 1000 musicbrainz-rate-limit-period)) ; period in milliseconds
         (t0 (- now (/ period 1000.0))) ; start of sliding window
         (queue musicbrainz--request-queue))
    ;; Remove old requests from queue
    (while (and queue (< (car queue) t0))
      (setq queue (cdr queue)))
    (setq musicbrainz--request-queue queue)
    
    ;; Check if we've reached the rate limit
    (when (>= (length queue) musicbrainz-rate-limit-requests)
      ;; Calculate delay needed
      (let* ((oldest-request (car queue))
             (delay (- (+ oldest-request (/ period 1000.0)) now)))
        (when (> delay 0)
          (message "Rate limit reached, waiting %.1f seconds..." delay)
          (sleep-for delay))))
    
    ;; Add current request to queue
    (setq musicbrainz--request-queue (append musicbrainz--request-queue (list now)))))

;;; Caching

(defun musicbrainz--cache-get (key)
  "Get cached response for KEY."
  (gethash key musicbrainz--cache))

(defun musicbrainz--cache-put (key value)
  "Put VALUE in cache with KEY."
  (when (>= (length musicbrainz--cache-keys) musicbrainz-cache-size)
    (let ((oldest (pop musicbrainz--cache-keys)))
      (remhash oldest musicbrainz--cache)))
  (puthash key value musicbrainz--cache)
  (push key musicbrainz--cache-keys))

(defun musicbrainz--make-cache-key (url parameters)
  "Create cache key from URL and PARAMETERS."
  (concat url "::" (prin1-to-string parameters)))

;;; HTTP requests using pdd

(defun musicbrainz--request-with-retry (url params retries)
  "Make request to URL with PARAMS, retrying up to RETRIES times on HTTP 503."
  (let ((response (pdd url
                      :headers `((User-Agent . ,musicbrainz-user-agent)
                                 (Accept . "application/json"))
                      :params params
                      :as 'json
                      :response 'status)))
    (cond
     ((= (car response) 503)
      (if (> retries 0)
          (progn
            (message "Rate limited (HTTP 503), retrying in %s seconds... (retries left: %s)" 
                     musicbrainz-retry-delay (1- retries))
            (sleep-for musicbrainz-retry-delay)
            (musicbrainz--request-with-retry url params (1- retries)))
        (error "MusicBrainz API rate limit exceeded after %s retries" musicbrainz-max-retries)))
     ((>= (car response) 400)
      (error "MusicBrainz API error %s: %s" (car response) (cdr response)))
     (t (cdr response)))))

(defun musicbrainz--request (endpoint &optional query limit offset callback)
  "Make request to ENDPOINT with QUERY.
If CALLBACK provided, performs async request. Returns parsed JSON response."
  (musicbrainz--rate-limit)
  (let* ((url (format "%s/%s" musicbrainz-api-base endpoint))
         ;; /genre/all doesn't support limit/offset according to docs
         (params (append (when query (list (cons "query" query)))
                         (list (cons "fmt" "json"))
                         (unless (string= endpoint "genre/all")
                           (append (when limit (list (cons "limit" (number-to-string limit))))
                                   (when offset (list (cons "offset" (number-to-string offset))))))))
         (cache-key (musicbrainz--make-cache-key url params)))
    (if callback
        (if-let* ((cached (musicbrainz--cache-get cache-key)))
            (funcall callback cached)
          (pdd url
            :headers `((User-Agent . ,musicbrainz-user-agent)
                       (Accept . "application/json"))
            :params params
            :as 'json
            :done (lambda (body)
                    (musicbrainz--cache-put cache-key body)
                    (funcall callback body))))
      (if-let* ((cached (musicbrainz--cache-get cache-key)))
          cached
        (let ((response (musicbrainz--request-with-retry url params musicbrainz-max-retries)))
          (musicbrainz--cache-put cache-key response)
          response)))))

(defun musicbrainz--lookup (endpoint mbid &optional inc callback)
  "Make lookup request to ENDPOINT with MBID.
If CALLBACK provided, performs async request. Returns parsed JSON response."
  (musicbrainz--rate-limit)
  (let* ((url (format "%s/%s/%s" musicbrainz-api-base endpoint mbid))
         (params (append (when inc (list (cons "inc" inc)))
                         (list (cons "fmt" "json"))))
         (cache-key (musicbrainz--make-cache-key url params)))
    (if callback
        (if-let* ((cached (musicbrainz--cache-get cache-key)))
            (funcall callback cached)
          (pdd url
            :headers `((User-Agent . ,musicbrainz-user-agent)
                       (Accept . "application/json"))
            :params params
            :as 'json
            :done (lambda (body)
                    (musicbrainz--cache-put cache-key body)
                    (funcall callback body))))
      (if-let* ((cached (musicbrainz--cache-get cache-key)))
          cached
        (let ((response (pdd url
                          :headers `((User-Agent . ,musicbrainz-user-agent)
                                     (Accept . "application/json"))
                          :params params
                          :as 'json)))
          (musicbrainz--cache-put cache-key response)
          response)))))

;;; Structured resources (inspired by cl-musicbrainz, using EIEIO)

(defclass musicbrainz-artist ()
  ((id :initarg :id :type string :documentation "MusicBrainz ID")
   (name :initarg :name :type string :documentation "Artist name")
   (type :initarg :type :type (or null string) :documentation "Artist type")
   (country :initarg :country :type (or null string) :documentation "Country code")
   (disambiguation :initarg :disambiguation :type (or null string) :documentation "Disambiguation")
   (begin-date :initarg :begin-date :type (or null string) :documentation "Begin date")
   (end-date :initarg :end-date :type (or null string) :documentation "End date"))
  "MusicBrainz artist resource.")

(defclass musicbrainz-release ()
  ((id :initarg :id :type string :documentation "MusicBrainz ID")
   (title :initarg :title :type string :documentation "Release title")
   (artist :initarg :artist :type string :documentation "Artist name")
   (artist-credit :initarg :artist-credit :type (or null vector list) :documentation "Artist credit list")
   (tracks :initarg :tracks :type (or null vector list) :documentation "Tracklist")
   (date :initarg :date :type (or null string) :documentation "Release date")
   (country :initarg :country :type (or null string) :documentation "Country code")
   (status :initarg :status :type (or null string) :documentation "Release status")
   (format :initarg :format :type (or null string) :documentation "Release format")
   (release-group-id :initarg :release-group-id :type (or null string) :documentation "Release group ID"))
  "MusicBrainz release resource.")

(defclass musicbrainz-release-group ()
  ((id :initarg :id :type string :documentation "MusicBrainz ID")
   (title :initarg :title :type string :documentation "Release group title")
   (type :initarg :type :type (or null string) :documentation "Release group type")
   (primary-type :initarg :primary-type :type (or null string) :documentation "Primary type")
   (first-release-date :initarg :first-release-date :type (or null string) :documentation "First release date")
   (artist-credit :initarg :artist-credit :type (or null vector list) :documentation "Artist credit"))
  "MusicBrainz release group resource.")

(defclass musicbrainz-recording ()
  ((id :initarg :id :type string :documentation "MusicBrainz ID")
   (title :initarg :title :type string :documentation "Recording title")
   (artist-credit :initarg :artist-credit :type (or null vector list) :documentation "Artist credit")
   (length :initarg :length :type (or null number) :documentation "Recording length")
   (first-release-date :initarg :first-release-date :type (or null string) :documentation "First release date"))
  "MusicBrainz recording resource.")

;;; New entities (libmusicbrainz compatible)

(defclass musicbrainz-label ()
  ((id :initarg :id :type string :documentation "MusicBrainz ID")
   (name :initarg :name :type string :documentation "Label name")
   (type :initarg :type :type (or null string) :documentation "Label type")
   (country :initarg :country :type (or null string) :documentation "Country code")
   (disambiguation :initarg :disambiguation :type (or null string) :documentation "Disambiguation")
   (begin-date :initarg :begin-date :type (or null string) :documentation "Begin date")
   (end-date :initarg :end-date :type (or null string) :documentation "End date"))
  "MusicBrainz label resource.")

(defclass musicbrainz-work ()
  ((id :initarg :id :type string :documentation "MusicBrainz ID")
   (title :initarg :title :type string :documentation "Work title")
   (type :initarg :type :type (or null string) :documentation "Work type")
   (language :initarg :language :type (or null string) :documentation "Language code")
   (disambiguation :initarg :disambiguation :type (or null string) :documentation "Disambiguation"))
  "MusicBrainz work resource.")

(defclass musicbrainz-area ()
  ((id :initarg :id :type string :documentation "MusicBrainz ID")
   (name :initarg :name :type string :documentation "Area name")
   (type :initarg :type :type (or null string) :documentation "Area type")
   (country-code :initarg :country-code :type (or null string) :documentation "ISO country code")
   (disambiguation :initarg :disambiguation :type (or null string) :documentation "Disambiguation"))
  "MusicBrainz area resource.")

(defclass musicbrainz-event ()
  ((id :initarg :id :type string :documentation "MusicBrainz ID")
   (name :initarg :name :type string :documentation "Event name")
   (type :initarg :type :type (or null string) :documentation "Event type")
   (time :initarg :time :type (or null string) :documentation "Event time")
   (setlist :initarg :setlist :type (or null string) :documentation "Setlist")
   (disambiguation :initarg :disambiguation :type (or null string) :documentation "Disambiguation"))
  "MusicBrainz event resource.")

(defclass musicbrainz-instrument ()
  ((id :initarg :id :type string :documentation "MusicBrainz ID")
   (name :initarg :name :type string :documentation "Instrument name")
   (type :initarg :type :type (or null string) :documentation "Instrument type")
   (disambiguation :initarg :disambiguation :type (or null string) :documentation "Disambiguation"))
  "MusicBrainz instrument resource.")

(defclass musicbrainz-place ()
  ((id :initarg :id :type string :documentation "MusicBrainz ID")
   (name :initarg :name :type string :documentation "Place name")
   (type :initarg :type :type (or null string) :documentation "Place type")
   (address :initarg :address :type (or null string) :documentation "Address")
   (coordinates :initarg :coordinates :type (or null string) :documentation "Coordinates")
   (disambiguation :initarg :disambiguation :type (or null string) :documentation "Disambiguation"))
  "MusicBrainz place resource.")

(defclass musicbrainz-series ()
  ((id :initarg :id :type string :documentation "MusicBrainz ID")
   (name :initarg :name :type string :documentation "Series name")
   (type :initarg :type :type (or null string) :documentation "Series type")
   (disambiguation :initarg :disambiguation :type (or null string) :documentation "Disambiguation"))
  "MusicBrainz series resource.")

(defclass musicbrainz-url ()
  ((id :initarg :id :type string :documentation "MusicBrainz ID")
   (resource :initarg :resource :type string :documentation "URL resource"))
  "MusicBrainz URL resource.")

(defclass musicbrainz-genre ()
  ((id :initarg :id :type string :documentation "MusicBrainz ID")
   (name :initarg :name :type string :documentation "Genre name")
   (disambiguation :initarg :disambiguation :type (or null string) :documentation "Disambiguation"))
  "MusicBrainz genre resource.")

;;; Resource parsing

(defun musicbrainz--parse-artist (artist-json)
  "Parse ARTIST-JSON into musicbrainz-artist object."
  (let ((life-span (alist-get 'life-span artist-json)))
    (make-instance 'musicbrainz-artist
     :id (alist-get 'id artist-json)
     :name (alist-get 'name artist-json)
     :type (alist-get 'type artist-json)
     :country (alist-get 'country artist-json)
     :disambiguation (alist-get 'disambiguation artist-json)
     :begin-date (alist-get 'begin life-span)
     :end-date (alist-get 'end life-span))))

(defun musicbrainz--parse-release (release-json)
  "Parse RELEASE-JSON into musicbrainz-release object."
  (let* ((artist-credit-list (alist-get 'artist-credit release-json))
         (artist-name (if (consp artist-credit-list)
                          (alist-get 'name (car (alist-get 'artist (car artist-credit-list))))
                        "Unknown"))
         (formats (alist-get 'formats release-json))
         (format-info (when formats (car formats)))
         (release-group (alist-get 'release-group release-json))
         ;; Parse tracklist - handle both vector and list for media
         (media (alist-get 'media release-json))
         (tracks (when media
                   (cl-loop for medium across media
                            append (append (alist-get 'tracks medium) nil)))))
    (make-instance 'musicbrainz-release
     :id (alist-get 'id release-json)
     :title (alist-get 'title release-json)
     :artist artist-name
     :artist-credit artist-credit-list
     :tracks tracks
     :date (alist-get 'date release-json)
     :country (alist-get 'country release-json)
     :status (alist-get 'status release-json)
     :format (when format-info (alist-get 'name format-info))
     :release-group-id (when release-group (alist-get 'id release-group)))))

(defun musicbrainz--parse-release-group (release-group-json)
  "Parse RELEASE-GROUP-JSON into musicbrainz-release-group object."
  (make-instance 'musicbrainz-release-group
   :id (alist-get 'id release-group-json)
   :title (alist-get 'title release-group-json)
   :type (alist-get 'type release-group-json)
   :primary-type (alist-get 'primary-type release-group-json)
   :first-release-date (alist-get 'first-release-date release-group-json)
   :artist-credit (alist-get 'artist-credit release-group-json)))

(defun musicbrainz--parse-recording (recording-json)
  "Parse RECORDING-JSON into musicbrainz-recording object."
  (make-instance 'musicbrainz-recording
   :id (alist-get 'id recording-json)
   :title (alist-get 'title recording-json)
   :artist-credit (alist-get 'artist-credit recording-json)
   :length (alist-get 'length recording-json)
   :first-release-date (alist-get 'first-release-date recording-json)))

(defun musicbrainz--parse-label (label-json)
  "Parse LABEL-JSON into musicbrainz-label object."
  (let ((life-span (alist-get 'life-span label-json)))
    (make-instance 'musicbrainz-label
     :id (alist-get 'id label-json)
     :name (alist-get 'name label-json)
     :type (alist-get 'type label-json)
     :country (alist-get 'country label-json)
     :disambiguation (alist-get 'disambiguation label-json)
     :begin-date (alist-get 'begin life-span)
     :end-date (alist-get 'end life-span))))

(defun musicbrainz--parse-work (work-json)
  "Parse WORK-JSON into musicbrainz-work object."
  (make-instance 'musicbrainz-work
   :id (alist-get 'id work-json)
   :title (alist-get 'title work-json)
   :type (alist-get 'type work-json)
   :language (alist-get 'language work-json)
   :disambiguation (alist-get 'disambiguation work-json)))

(defun musicbrainz--parse-area (area-json)
  "Parse AREA-JSON into musicbrainz-area object."
  (make-instance 'musicbrainz-area
   :id (alist-get 'id area-json)
   :name (alist-get 'name area-json)
   :type (alist-get 'type area-json)
   :country-code (alist-get 'country-code area-json)
   :disambiguation (alist-get 'disambiguation area-json)))

(defun musicbrainz--parse-event (event-json)
  "Parse EVENT-JSON into musicbrainz-event object."
  (make-instance 'musicbrainz-event
   :id (alist-get 'id event-json)
   :name (alist-get 'name event-json)
   :type (alist-get 'type event-json)
   :time (alist-get 'time event-json)
   :setlist (alist-get 'setlist event-json)
   :disambiguation (alist-get 'disambiguation event-json)))

(defun musicbrainz--parse-instrument (instrument-json)
  "Parse INSTRUMENT-JSON into musicbrainz-instrument object."
  (make-instance 'musicbrainz-instrument
   :id (alist-get 'id instrument-json)
   :name (alist-get 'name instrument-json)
   :type (alist-get 'type instrument-json)
   :disambiguation (alist-get 'disambiguation instrument-json)))

(defun musicbrainz--parse-place (place-json)
  "Parse PLACE-JSON into musicbrainz-place object."
  (make-instance 'musicbrainz-place
   :id (alist-get 'id place-json)
   :name (alist-get 'name place-json)
   :type (alist-get 'type place-json)
   :address (alist-get 'address place-json)
   :coordinates (alist-get 'coordinates place-json)
   :disambiguation (alist-get 'disambiguation place-json)))

(defun musicbrainz--parse-series (series-json)
  "Parse SERIES-JSON into musicbrainz-series object."
  (make-instance 'musicbrainz-series
   :id (alist-get 'id series-json)
   :name (alist-get 'name series-json)
   :type (alist-get 'type series-json)
   :disambiguation (alist-get 'disambiguation series-json)))

(defun musicbrainz--parse-url (url-json)
  "Parse URL-JSON into musicbrainz-url object."
  (make-instance 'musicbrainz-url
   :id (alist-get 'id url-json)
   :resource (alist-get 'resource url-json)))

(defun musicbrainz--parse-genre (genre-json)
  "Parse GENRE-JSON into musicbrainz-genre object."
  (make-instance 'musicbrainz-genre
   :id (alist-get 'id genre-json)
   :name (alist-get 'name genre-json)
   :disambiguation (alist-get 'disambiguation genre-json)))

;;; Public API - Search functions

(defun musicbrainz-search-artist (query &optional limit offset callback)
  "Search for artists matching QUERY.
If CALLBACK provided, performs async request."
  (if callback
      (musicbrainz--request "artist" query limit offset
        (lambda (json)
          (when-let* ((artists (alist-get 'artists json)))
            (funcall callback (mapcar #'musicbrainz--parse-artist artists)))))
    (when-let* ((json (musicbrainz--request "artist" query limit offset))
                (artists (alist-get 'artists json)))
      (mapcar #'musicbrainz--parse-artist artists))))

(defun musicbrainz-search-release (query &optional limit offset callback)
  "Search for releases matching QUERY.
If CALLBACK provided, performs async request."
  (if callback
      (musicbrainz--request "release" query limit offset
        (lambda (json)
          (when-let* ((releases (alist-get 'releases json)))
            (funcall callback (mapcar #'musicbrainz--parse-release releases)))))
    (when-let* ((json (musicbrainz--request "release" query limit offset))
                (releases (alist-get 'releases json)))
      (mapcar #'musicbrainz--parse-release releases))))

;;; Public API - Lookup functions

(defun musicbrainz-lookup-artist (mbid &optional inc callback)
  "Look up artist by MBID.
If CALLBACK provided, performs async request."
  (if callback
      (musicbrainz--lookup "artist" mbid inc
        (lambda (json)
          (funcall callback (musicbrainz--parse-artist json))))
    (when-let* ((json (musicbrainz--lookup "artist" mbid inc)))
      (musicbrainz--parse-artist json))))

(defun musicbrainz-lookup-release (mbid &optional inc callback)
  "Look up release by MBID.
If CALLBACK provided, performs async request."
  (if callback
      (musicbrainz--lookup "release" mbid inc
        (lambda (json)
          (funcall callback (musicbrainz--parse-release json))))
    (when-let* ((json (musicbrainz--lookup "release" mbid inc)))
      (musicbrainz--parse-release json))))

(defun musicbrainz-lookup-release-group (mbid &optional inc callback)
  "Look up release group by MBID.
If CALLBACK provided, performs async request."
  (if callback
      (musicbrainz--lookup "release-group" mbid inc
        (lambda (json)
          (funcall callback (musicbrainz--parse-release-group json))))
    (when-let* ((json (musicbrainz--lookup "release-group" mbid inc)))
      (musicbrainz--parse-release-group json))))

;;; New entity search/lookup functions

;; Label
(defun musicbrainz-search-label (query &optional limit offset callback)
  "Search for labels matching QUERY.
If CALLBACK provided, performs async request."
  (if callback
      (musicbrainz--request "label" query limit offset
        (lambda (json)
          (when-let* ((labels (alist-get 'labels json)))
            (funcall callback (mapcar #'musicbrainz--parse-label labels)))))
    (when-let* ((json (musicbrainz--request "label" query limit offset))
                (labels (alist-get 'labels json)))
      (mapcar #'musicbrainz--parse-label labels))))

(defun musicbrainz-lookup-label (mbid &optional inc callback)
  "Look up label by MBID.
If CALLBACK provided, performs async request."
  (if callback
      (musicbrainz--lookup "label" mbid inc
        (lambda (json)
          (funcall callback (musicbrainz--parse-label json))))
    (when-let* ((json (musicbrainz--lookup "label" mbid inc)))
      (musicbrainz--parse-label json))))

;; Work
(defun musicbrainz-search-work (query &optional limit offset callback)
  "Search for works matching QUERY.
If CALLBACK provided, performs async request."
  (if callback
      (musicbrainz--request "work" query limit offset
        (lambda (json)
          (when-let* ((works (alist-get 'works json)))
            (funcall callback (mapcar #'musicbrainz--parse-work works)))))
    (when-let* ((json (musicbrainz--request "work" query limit offset))
                (works (alist-get 'works json)))
      (mapcar #'musicbrainz--parse-work works))))

(defun musicbrainz-lookup-work (mbid &optional inc callback)
  "Look up work by MBID.
If CALLBACK provided, performs async request."
  (if callback
      (musicbrainz--lookup "work" mbid inc
        (lambda (json)
          (funcall callback (musicbrainz--parse-work json))))
    (when-let* ((json (musicbrainz--lookup "work" mbid inc)))
      (musicbrainz--parse-work json))))

;; Area
(defun musicbrainz-search-area (query &optional limit offset callback)
  "Search for areas matching QUERY.
If CALLBACK provided, performs async request."
  (if callback
      (musicbrainz--request "area" query limit offset
        (lambda (json)
          (when-let* ((areas (alist-get 'areas json)))
            (funcall callback (mapcar #'musicbrainz--parse-area areas)))))
    (when-let* ((json (musicbrainz--request "area" query limit offset))
                (areas (alist-get 'areas json)))
      (mapcar #'musicbrainz--parse-area areas))))

(defun musicbrainz-lookup-area (mbid &optional inc callback)
  "Look up area by MBID.
If CALLBACK provided, performs async request."
  (if callback
      (musicbrainz--lookup "area" mbid inc
        (lambda (json)
          (funcall callback (musicbrainz--parse-area json))))
    (when-let* ((json (musicbrainz--lookup "area" mbid inc)))
      (musicbrainz--parse-area json))))

;; Event
(defun musicbrainz-search-event (query &optional limit offset callback)
  "Search for events matching QUERY.
If CALLBACK provided, performs async request."
  (if callback
      (musicbrainz--request "event" query limit offset
        (lambda (json)
          (when-let* ((events (alist-get 'events json)))
            (funcall callback (mapcar #'musicbrainz--parse-event events)))))
    (when-let* ((json (musicbrainz--request "event" query limit offset))
                (events (alist-get 'events json)))
      (mapcar #'musicbrainz--parse-event events))))

(defun musicbrainz-lookup-event (mbid &optional inc callback)
  "Look up event by MBID.
If CALLBACK provided, performs async request."
  (if callback
      (musicbrainz--lookup "event" mbid inc
        (lambda (json)
          (funcall callback (musicbrainz--parse-event json))))
    (when-let* ((json (musicbrainz--lookup "event" mbid inc)))
      (musicbrainz--parse-event json))))

;; Instrument
(defun musicbrainz-search-instrument (query &optional limit offset callback)
  "Search for instruments matching QUERY.
If CALLBACK provided, performs async request."
  (if callback
      (musicbrainz--request "instrument" query limit offset
        (lambda (json)
          (when-let* ((instruments (alist-get 'instruments json)))
            (funcall callback (mapcar #'musicbrainz--parse-instrument instruments)))))
    (when-let* ((json (musicbrainz--request "instrument" query limit offset))
                (instruments (alist-get 'instruments json)))
      (mapcar #'musicbrainz--parse-instrument instruments))))

(defun musicbrainz-lookup-instrument (mbid &optional inc callback)
  "Look up instrument by MBID.
If CALLBACK provided, performs async request."
  (if callback
      (musicbrainz--lookup "instrument" mbid inc
        (lambda (json)
          (funcall callback (musicbrainz--parse-instrument json))))
    (when-let* ((json (musicbrainz--lookup "instrument" mbid inc)))
      (musicbrainz--parse-instrument json))))

;; Place
(defun musicbrainz-search-place (query &optional limit offset callback)
  "Search for places matching QUERY.
If CALLBACK provided, performs async request."
  (if callback
      (musicbrainz--request "place" query limit offset
        (lambda (json)
          (when-let* ((places (alist-get 'places json)))
            (funcall callback (mapcar #'musicbrainz--parse-place places)))))
    (when-let* ((json (musicbrainz--request "place" query limit offset))
                (places (alist-get 'places json)))
      (mapcar #'musicbrainz--parse-place places))))

(defun musicbrainz-lookup-place (mbid &optional inc callback)
  "Look up place by MBID.
If CALLBACK provided, performs async request."
  (if callback
      (musicbrainz--lookup "place" mbid inc
        (lambda (json)
          (funcall callback (musicbrainz--parse-place json))))
    (when-let* ((json (musicbrainz--lookup "place" mbid inc)))
      (musicbrainz--parse-place json))))

;; Series
(defun musicbrainz-search-series (query &optional limit offset callback)
  "Search for series matching QUERY.
If CALLBACK provided, performs async request."
  (if callback
      (musicbrainz--request "series" query limit offset
        (lambda (json)
          (when-let* ((series (alist-get 'series json)))
            (funcall callback (mapcar #'musicbrainz--parse-series series)))))
    (when-let* ((json (musicbrainz--request "series" query limit offset))
                (series (alist-get 'series json)))
      (mapcar #'musicbrainz--parse-series series))))

(defun musicbrainz-lookup-series (mbid &optional inc callback)
  "Look up series by MBID.
If CALLBACK provided, performs async request."
  (if callback
      (musicbrainz--lookup "series" mbid inc
        (lambda (json)
          (funcall callback (musicbrainz--parse-series json))))
    (when-let* ((json (musicbrainz--lookup "series" mbid inc)))
      (musicbrainz--parse-series json))))

;; URL
(defun musicbrainz-search-url (query &optional limit offset callback)
  "Search for URLs matching QUERY.
If CALLBACK provided, performs async request."
  (if callback
      (musicbrainz--request "url" query limit offset
        (lambda (json)
          (when-let* ((urls (alist-get 'urls json)))
            (funcall callback (mapcar #'musicbrainz--parse-url urls)))))
    (when-let* ((json (musicbrainz--request "url" query limit offset))
                (urls (alist-get 'urls json)))
      (mapcar #'musicbrainz--parse-url urls))))

(defun musicbrainz-lookup-url (mbid &optional inc callback)
  "Look up URL by MBID.
If CALLBACK provided, performs async request."
  (if callback
      (musicbrainz--lookup "url" mbid inc
        (lambda (json)
          (funcall callback (musicbrainz--parse-url json))))
    (when-let* ((json (musicbrainz--lookup "url" mbid inc)))
      (musicbrainz--parse-url json))))

;; Recording
(defun musicbrainz-search-recording (query &optional limit offset callback)
  "Search for recordings matching QUERY.
If CALLBACK provided, performs async request."
  (if callback
      (musicbrainz--request "recording" query limit offset
        (lambda (json)
          (when-let* ((recordings (alist-get 'recordings json)))
            (funcall callback (mapcar #'musicbrainz--parse-recording recordings)))))
    (when-let* ((json (musicbrainz--request "recording" query limit offset))
                (recordings (alist-get 'recordings json)))
      (mapcar #'musicbrainz--parse-recording recordings))))

(defun musicbrainz-lookup-recording (mbid &optional inc callback)
  "Look up recording by MBID.
If CALLBACK provided, performs async request."
  (if callback
      (musicbrainz--lookup "recording" mbid inc
        (lambda (json)
          (funcall callback (musicbrainz--parse-recording json))))
    (when-let* ((json (musicbrainz--lookup "recording" mbid inc)))
      (musicbrainz--parse-recording json))))

;; Release Group
(defun musicbrainz-search-release-group (query &optional limit offset callback)
  "Search for release groups matching QUERY.
If CALLBACK provided, performs async request."
  (if callback
      (musicbrainz--request "release-group" query limit offset
        (lambda (json)
          (when-let* ((release-groups (alist-get 'release-groups json)))
            (funcall callback (mapcar #'musicbrainz--parse-release-group release-groups)))))
    (when-let* ((json (musicbrainz--request "release-group" query limit offset))
                (release-groups (alist-get 'release-groups json)))
      (mapcar #'musicbrainz--parse-release-group release-groups))))

;; Genre (no search, only lookup and /genre/all)
(defun musicbrainz-lookup-genre (mbid &optional inc callback)
  "Look up genre by MBID.
If CALLBACK provided, performs async request."
  (if callback
      (musicbrainz--lookup "genre" mbid inc
        (lambda (json)
          (funcall callback (musicbrainz--parse-genre json))))
    (when-let* ((json (musicbrainz--lookup "genre" mbid inc)))
      (musicbrainz--parse-genre json))))

(defun musicbrainz-browse-genres (&optional limit offset callback)
  "Browse all genres from MusicBrainz.
If CALLBACK provided, performs async request."
  (if callback
      (musicbrainz--request "genre/all" nil limit offset
        (lambda (json)
          (when-let* ((genres (alist-get 'genres json)))
            (funcall callback (mapcar #'musicbrainz--parse-genre genres)))))
    (when-let* ((json (musicbrainz--request "genre/all" nil limit offset))
                (genres (alist-get 'genres json)))
      (mapcar #'musicbrainz--parse-genre genres))))

(defun musicbrainz-browse-releases (artist-mbid &optional limit offset callback)
  "Browse releases for ARTIST-MBID.
If CALLBACK provided, performs async request."
  (if callback
      (musicbrainz--request "release" nil limit offset
        (lambda (json)
          (when-let* ((releases (alist-get 'releases json)))
            (funcall callback (mapcar #'musicbrainz--parse-release releases)))))
    (when-let* ((url (format "%s/release?artist=%s" musicbrainz-api-base artist-mbid))
                (params (append (list (cons "fmt" "json"))
                                (when limit (list (cons "limit" (number-to-string limit))))
                                (when offset (list (cons "offset" (number-to-string offset))))))
                (response (pdd url
                              :headers `((User-Agent . ,musicbrainz-user-agent)
                                         (Accept . "application/json"))
                              :params params
                              :as 'json))
                (releases (alist-get 'releases response)))
      (mapcar #'musicbrainz--parse-release releases))))

;;; Utility functions

(defun musicbrainz--extract-year (date)
  "Extract year from DATE string (YYYY-MM-DD or YYYY or YYYY-MM)."
  (when (and date (string-match "\\([0-9]\\{4\\}\\)" date))
    (string-to-number (match-string 1 date))))

(defun musicbrainz-format-artist (artist index)
  "Format ARTIST for display with INDEX."
  (format "[%d] %s%s%s"
          index
          (oref artist name)
          (if-let* ((type (oref artist type)))
              (format " (%s)" type) "")
          (if-let* ((country (oref artist country)))
              (format " [%s]" country) "")))

(defun musicbrainz-format-release (release index)
  "Format RELEASE for display with INDEX."
  (format "[%d] %s - %s (%s%s)"
          index
          (oref release artist)
          (oref release title)
          (oref release date)
          (if-let* ((country (oref release country)))
              (format ", %s" country) "")))

(defun musicbrainz-format-label (label index)
  "Format LABEL for display with INDEX."
  (format "[%d] %s%s%s"
          index
          (oref label name)
          (if-let* ((type (oref label type)))
              (format " (%s)" type) "")
          (if-let* ((country (oref label country)))
              (format " [%s]" country) "")))

(defun musicbrainz-format-work (work index)
  "Format WORK for display with INDEX."
  (format "[%d] %s%s"
          index
          (oref work title)
          (if-let* ((type (oref work type)))
              (format " (%s)" type) "")))

(defun musicbrainz-format-area (area index)
  "Format AREA for display with INDEX."
  (format "[%d] %s%s"
          index
          (oref area name)
          (if-let* ((type (oref area type)))
              (format " (%s)" type) "")))

(defun musicbrainz-format-event (event index)
  "Format EVENT for display with INDEX."
  (format "[%d] %s%s"
          index
          (oref event name)
          (if-let* ((type (oref event type)))
              (format " (%s)" type) "")))

(defun musicbrainz-format-instrument (instrument index)
  "Format INSTRUMENT for display with INDEX."
  (format "[%d] %s%s"
          index
          (oref instrument name)
          (if-let* ((type (oref instrument type)))
              (format " (%s)" type) "")))

(defun musicbrainz-format-place (place index)
  "Format PLACE for display with INDEX."
  (format "[%d] %s%s"
          index
          (oref place name)
          (if-let* ((type (oref place type)))
              (format " (%s)" type) "")))

(defun musicbrainz-format-series (series index)
  "Format SERIES for display with INDEX."
  (format "[%d] %s%s"
          index
          (oref series name)
          (if-let* ((type (oref series type)))
              (format " (%s)" type) "")))

(defun musicbrainz-format-url (url index)
  "Format URL for display with INDEX."
  (format "[%d] %s -> %s"
          index
          (oref url resource)
          (oref url id)))

;;; Interactive functions

;;;###autoload
(defun musicbrainz-find-year (artist title &optional limit callback)
  "Find the earliest release year for ARTIST and TITLE.
If CALLBACK provided, performs async request."
  (interactive "sArtist: \nsTitle: ")
  (let ((query (format "artist:\"%s\" AND release:\"%s\"" artist title)))
    (if callback
        (musicbrainz--request "release" query limit offset
          (lambda (json)
            (when-let* ((releases (alist-get 'releases json)))
              (let ((years (cl-loop for release across releases
                                   for date = (alist-get 'date release)
                                   for year = (musicbrainz--extract-year date)
                                   when year
                                   collect year)))
                (funcall callback (when years (apply #'min years)))))))
      (let ((result (when-let* ((json (musicbrainz--request "release" query limit offset))
                                (releases (alist-get 'releases json)))
                      (let ((years (cl-loop for release across releases
                                           for date = (alist-get 'date release)
                                           for year = (musicbrainz--extract-year date)
                                           when year
                                           collect year)))
                        (when years (apply #'min years))))))
        (when (called-interactively-p 'any)
          (if result
              (message "Year for %s - %s: %s" artist title result)
            (message "No year found for %s - %s" artist title)))
        result))))

;;;###autoload
(defun musicbrainz-find-genre (artist title &optional limit callback)
  "Find genres for ARTIST and TITLE.
If CALLBACK provided, performs async request."
  (interactive "sArtist: \nsTitle: ")
  (let ((query (format "artist:\"%s\" AND release:\"%s\"" artist title)))
    (if callback
        (musicbrainz--request "release" query limit offset
          (lambda (json)
            (when-let* ((releases (alist-get 'releases json))
                        (parsed-releases (cl-map 'list #'musicbrainz--parse-release releases))
                        (release-group-id (oref (car parsed-releases) release-group-id)))
              (musicbrainz--lookup "release-group" release-group-id "genres"
                (lambda (rg-json)
                  (let ((genres (alist-get 'genres rg-json)))
                    (funcall callback (mapcar (lambda (g) (alist-get 'name g)) genres))))))))
      (let ((result (when-let* ((json (musicbrainz--request "release" query limit offset))
                                (releases (alist-get 'releases json))
                                (parsed-releases (cl-map 'list #'musicbrainz--parse-release releases))
                                (release-group-id (oref (car parsed-releases) release-group-id)))
                      (when-let* ((rg-json (musicbrainz--lookup "release-group" release-group-id "genres"))
                                  (genres (alist-get 'genres rg-json)))
                        (mapcar (lambda (g) (alist-get 'name g)) genres)))))
        (when (called-interactively-p 'any)
          (if result
              (message "Genres for %s - %s: %s" artist title result)
            (message "No genres found for %s - %s" artist title)))
        result))))

(defun musicbrainz--parse-tracklist (release-data)
  "Parse tracklist from RELEASE-DATA.
Returns a list of track titles, or a list of (artist . title) cons cells
if there are multiple artists."
  (let ((tracks (cl-loop for medium across (alist-get 'media release-data)
                         append (cl-loop for track across (alist-get 'tracks medium)
                                         for recording = (alist-get 'recording track)
                                         for title = (alist-get 'title recording)
                                         for artist-credit = (alist-get 'artist-credit recording)
                                         collect (cons artist-credit title)))))
    (if (= (length (cl-delete-duplicates (mapcar #'car tracks) :test #'equal)) 1)
        ;; If there's just one band, then return only track names.
        (mapcar #'cdr tracks)
      ;; If there's more, return Band / Track.
      (cl-loop for (artist-credit . title) in tracks
               collect (format "%s / %s"
                              (musicbrainz--format-artist-credit artist-credit)
                              title)))))

(defun musicbrainz--format-artist-credit (artist-credit)
  "Format ARTIST-CREDIT into a string."
  (if (or (consp artist-credit) (vectorp artist-credit))
      (mapconcat (lambda (credit)
                   (alist-get 'name (alist-get 'artist credit)))
                 artist-credit
                 ", ")
    "Unknown"))

;;;###autoload
(defun musicbrainz-find-tracklist (artist title &optional limit callback)
  "Find tracklist for ARTIST and TITLE.
If CALLBACK provided, performs async request."
  (interactive "sArtist: \nsTitle: ")
  (let ((query (format "artist:\"%s\" AND release:\"%s\"" artist title)))
    (if callback
        (musicbrainz--request "release" query limit offset
          (lambda (json)
            (when-let* ((releases (alist-get 'releases json))
                        (parsed-releases (cl-map 'list #'musicbrainz--parse-release releases))
                        (release-id (musicbrainz-release-id (car parsed-releases))))
              (musicbrainz--lookup "release" release-id "recordings"
                 (lambda (release-json)
                   (funcall callback (musicbrainz--parse-tracklist release-json)))))))
       (let ((result (when-let* ((json (musicbrainz--request "release" query limit offset))
                                 (releases (alist-get 'releases json))
                                 (parsed-releases (cl-map 'list #'musicbrainz--parse-release releases))
                                 (release-id (oref (car parsed-releases) id)))
                       (when-let* ((release-json (musicbrainz--lookup "release" release-id "recordings")))
                         (musicbrainz--parse-tracklist release-json)))))
        (when (called-interactively-p 'any)
          (if result
              (with-current-buffer (get-buffer-create "*MusicBrainz Tracklist*")
                (erase-buffer)
                (insert (format "Tracklist for %s - %s:\n\n" artist title))
                (dolist (track result)
                  (insert track "\n"))
                (display-buffer (current-buffer)))
            (message "No tracklist found for %s - %s" artist title)))
        result))))

;;;###autoload
(defun musicbrainz-find-release-tracklist (artist title &optional limit callback)
  "Find tracklist for ARTIST and TITLE, allowing selection of specific release.
If CALLBACK provided, performs async request."
  (interactive "sArtist: \nsTitle: ")
  (let ((query (format "artist:\"%s\" AND release:\"%s\"" artist title)))
    (if callback
        (musicbrainz--request "release" query limit offset
          (lambda (json)
            (when-let* ((releases (alist-get 'releases json))
                        (parsed-releases (cl-map 'list #'musicbrainz--parse-release releases)))
              (if (= (length parsed-releases) 1)
                  (let ((release-id (oref (car parsed-releases) id)))
                    (musicbrainz--lookup "release" release-id "recordings"
                      (lambda (release-json)
                        (funcall callback (musicbrainz--parse-tracklist release-json)))))
                (let* ((entries (cl-loop for release in parsed-releases
                                         collect (cons
                                                  (format "%s %s %s (%s)"
                                                          (oref release title)
                                                          (oref release format)
                                                          (oref release date)
                                                          (oref release id))
                                                  (oref release id))))
                       (choice (completing-read "Choose release: " (mapcar #'car entries))))
                  (when choice
                    (let ((release-id (cdr (assoc choice entries))))
                      (musicbrainz--lookup "release" release-id "recordings"
                        (lambda (release-json)
                          (funcall callback (musicbrainz--parse-tracklist release-json)))))))))))
      (let ((result (when-let* ((json (musicbrainz--request "release" query limit offset))
                                 (releases (alist-get 'releases json))
                                 (parsed-releases (cl-map 'list #'musicbrainz--parse-release releases)))
                       (if (= (length parsed-releases) 1)
                           (let ((release-id (oref (car parsed-releases) id)))
                             (when-let* ((release-json (musicbrainz--lookup "release" release-id "recordings")))
                               (musicbrainz--parse-tracklist release-json)))
                         (let* ((entries (cl-loop for release in parsed-releases
                                                  collect (cons
                                                           (format "%s %s %s (%s)"
                                                                   (oref release title)
                                                                   (oref release format)
                                                                   (oref release date)
                                                                   (oref release id))
                                                           (oref release id))))
                                (choice (completing-read "Choose release: " (mapcar #'car entries))))
                           (when choice
                             (let ((release-id (cdr (assoc choice entries))))
                               (when-let* ((release-json (musicbrainz--lookup "release" release-id "recordings")))
                                 (musicbrainz--parse-tracklist release-json)))))))))
        (when (called-interactively-p 'any)
          (if result
              (with-current-buffer (get-buffer-create "*MusicBrainz Tracklist*")
                (erase-buffer)
                (insert (format "Tracklist for %s - %s:\n\n" artist title))
                (dolist (track result)
                  (insert track "\n"))
                (display-buffer (current-buffer)))
            (message "No tracklist found for %s - %s" artist title)))
        result))))

(defun musicbrainz-browse (id type)
  "Browse MusicBrainz entity ID of TYPE in eww."
  (eww (format "https://musicbrainz.org/%s/%s" type id)))

(defun musicbrainz-interactive-search (type)
  "Interactive search for TYPE."
  (let* ((type-info (assoc type '(("artist" . "Artist: ")
                                  ("release" . "Album: ")
                                  ("label" . "Label: ")
                                  ("work" . "Work: ")
                                  ("area" . "Area: ")
                                  ("event" . "Event: ")
                                  ("instrument" . "Instrument: ")
                                  ("place" . "Place: ")
                                  ("series" . "Series: ")
                                  ("url" . "URL: "))))
         (prompt (or (cdr type-info) "Query: "))
         (query (read-string prompt))
         (buf (get-buffer-create (format "*MusicBrainz %s*" (capitalize type))))
         results)
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert "Searching...\n"))
    (pop-to-buffer buf)
    (let ((search-func (cond
                        ((equal type "artist") #'musicbrainz-search-artist)
                        ((equal type "release") #'musicbrainz-search-release)
                        ((equal type "label") #'musicbrainz-search-label)
                        ((equal type "work") #'musicbrainz-search-work)
                        ((equal type "area") #'musicbrainz-search-area)
                        ((equal type "event") #'musicbrainz-search-event)
                        ((equal type "instrument") #'musicbrainz-search-instrument)
                        ((equal type "place") #'musicbrainz-search-place)
                        ((equal type "series") #'musicbrainz-search-series)
                        ((equal type "url") #'musicbrainz-search-url))))
      (funcall search-func query 25
               (lambda (res)
                 (setq results res)
                 (with-current-buffer buf
                   (setq buffer-read-only nil)
                   (erase-buffer)
                   (if (null results)
                       (insert "No results found\n")
                     (insert (format "Found %d results:\n\n" (length results)))
                     (cl-loop for r in results
                              for i from 1
                              do (insert (funcall (cond
                                                   ((equal type "artist") #'musicbrainz-format-artist)
                                                   ((equal type "release") #'musicbrainz-format-release)
                                                   ((equal type "label") #'musicbrainz-format-label)
                                                   ((equal type "work") #'musicbrainz-format-work)
                                                   ((equal type "area") #'musicbrainz-format-area)
                                                   ((equal type "event") #'musicbrainz-format-event)
                                                   ((equal type "instrument") #'musicbrainz-format-instrument)
                                                   ((equal type "place") #'musicbrainz-format-place)
                                                   ((equal type "series") #'musicbrainz-format-series)
                                                   ((equal type "url") #'musicbrainz-format-url))
                                                 r i)
                                      "\n")))
                   (goto-char (point-min))
                   (let ((selection (read-number (format "Select (1-%d) or q to quit: " (length results)) 1)))
                     (when (and (>= selection 1) (<= selection (length results)))
                       (musicbrainz-browse
                        (oref (nth (1- selection) results) id)
                        type)))))))))

;;;###autoload
(defun musicbrainz-search-artist-interactive ()
  "Interactive artist search."
  (interactive)
  (musicbrainz-interactive-search "artist"))

;;;###autoload
(defun musicbrainz-search-release-interactive ()
  "Interactive release search."
  (interactive)
  (musicbrainz-interactive-search "release"))

;;;###autoload
(defun musicbrainz-search-label-interactive ()
  "Interactive label search."
  (interactive)
  (musicbrainz-interactive-search "label"))

;;;###autoload
(defun musicbrainz-search-work-interactive ()
  "Interactive work search."
  (interactive)
  (musicbrainz-interactive-search "work"))

;;;###autoload
(defun musicbrainz ()
  "Main entry point for MusicBrainz search."
  (interactive)
  (let ((type (completing-read "Search type: "
                               '("artist" "release" "label" "work" "area"
                                 "event" "instrument" "place" "series" "url")
                               nil t)))
    (musicbrainz-interactive-search type)))

(provide 'musicbrainz)

;;; musicbrainz.el ends here
