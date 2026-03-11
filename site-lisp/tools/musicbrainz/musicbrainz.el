;;; musicbrainz.el --- MusicBrainz API client -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: https://github.com/larsmagne/musicbrainz.el, Mimo V2 Flash Free/MiniMax M2.5 Free
;; Keywords: music, api
;; Package-Requires: ((emacs "27.1") (pdd "0.1.0"))

;;; Commentary:

;; MusicBrainz API client using pdd.el for async HTTP requests.
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

(defvar musicbrainz-api-base "https://musicbrainz.org/ws/2")

(defvar musicbrainz-user-agents
  '("curl/8.5.0"
    "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
    "Mozilla/5.0 (X11; Linux x86_64; rv:121.0) Gecko/20100101 Firefox/121.0"))

(defvar musicbrainz-user-agent (car musicbrainz-user-agents))

(defvar musicbrainz-rate-limit-delay 1.0)
(defvar musicbrainz--last-request-time nil)

(defun musicbrainz--rate-limit ()
  "Apply rate limiting between requests."
  (when-let* ((last musicbrainz--last-request-time))
    (let ((elapsed (- (float-time) last)))
      (when (< elapsed musicbrainz-rate-limit-delay)
        (sleep-for (- musicbrainz-rate-limit-delay elapsed)))))
  (setq musicbrainz--last-request-time (float-time)))

(defun musicbrainz--request (endpoint query &optional limit callback)
  "Make search request to ENDPOINT with QUERY.
If CALLBACK provided, performs async request. Returns parsed JSON response."
  (musicbrainz--rate-limit)
  (let* ((limit (or limit 25))
         (url (format "%s/%s?query=%s&fmt=json&limit=%d"
                     musicbrainz-api-base
                     endpoint
                     (url-hexify-string query)
                     limit)))
    (if callback
        (pdd url
          :headers `((User-Agent . ,musicbrainz-user-agent)
                     (Accept . "application/json"))
          :as 'json
          :done (lambda (&key body)
                  (funcall callback body)))
      (pdd url
        :headers `((User-Agent . ,musicbrainz-user-agent)
                   (Accept . "application/json"))
        :as 'json))))

(defun musicbrainz--lookup (endpoint mbid &optional inc callback)
  "Make lookup request to ENDPOINT with MBID.
If CALLBACK provided, performs async request. Returns parsed JSON response."
  (musicbrainz--rate-limit)
  (let ((url (format "%s/%s/%s?fmt=json%s"
                     musicbrainz-api-base
                     endpoint
                     mbid
                     (if inc (format "&inc=%s" inc) ""))))
    (if callback
        (pdd url
          :headers `((User-Agent . ,musicbrainz-user-agent)
                     (Accept . "application/json"))
          :as 'json
          :done (lambda (&key body)
                  (funcall callback body)))
      (pdd url
        :headers `((User-Agent . ,musicbrainz-user-agent)
                   (Accept . "application/json"))
        :as 'json))))

(defun musicbrainz--parse-artist (artist)
  "Parse ARTIST JSON into plist."
  (list :id (alist-get 'id artist)
        :name (alist-get 'name artist)
        :type (alist-get 'type artist)
        :country (alist-get 'country artist)
        :disambiguation (alist-get 'disambiguation artist)))

(defun musicbrainz--parse-release (release)
  "Parse RELEASE JSON into plist."
  (let* ((artist-credit-list (alist-get 'artist-credit release))
         (artist-name (if (consp artist-credit-list)
                          (alist-get 'name (alist-get 'artist (car artist-credit-list)))
                        "Unknown"))
         (formats (alist-get 'formats release))
         (format-info (car formats))
         (release-group (alist-get 'release-group release)))
    (list :id (alist-get 'id release)
          :title (alist-get 'title release)
          :artist artist-name
          :date (alist-get 'date release)
          :country (alist-get 'country release)
          :status (alist-get 'status release)
          :format (alist-get 'name format-info)
          :release-group-id (when release-group (alist-get 'id release-group)))))

(defun musicbrainz-search-artist (query &optional limit callback)
  "Search for artists matching QUERY.
If CALLBACK provided, performs async request."
  (if callback
      (musicbrainz--request "artist" query limit
        (lambda (json)
          (when-let* ((artists (alist-get 'artists json)))
            (funcall callback (mapcar #'musicbrainz--parse-artist artists)))))
    (when-let* ((json (musicbrainz--request "artist" query limit))
                (artists (alist-get 'artists json)))
      (mapcar #'musicbrainz--parse-artist artists))))

(defun musicbrainz-search-release (query &optional limit callback)
  "Search for releases matching QUERY.
If CALLBACK provided, performs async request."
  (if callback
      (musicbrainz--request "release" query limit
        (lambda (json)
          (when-let* ((releases (alist-get 'releases json)))
            (funcall callback (mapcar #'musicbrainz--parse-release releases)))))
    (when-let* ((json (musicbrainz--request "release" query limit))
                (releases (alist-get 'releases json)))
      (mapcar #'musicbrainz--parse-release releases))))

(defun musicbrainz-format-artist (artist index)
  "Format ARTIST for display with INDEX."
  (format "[%d] %s%s%s"
          index
          (plist-get artist :name)
          (if-let* ((type (plist-get artist :type)))
              (format " (%s)" type) "")
          (if-let* ((country (plist-get artist :country)))
              (format " [%s]" country) "")))

(defun musicbrainz-format-release (release index)
  "Format RELEASE for display with INDEX."
  (format "[%d] %s - %s (%s%s)"
          index
          (plist-get release :artist)
          (plist-get release :title)
          (plist-get release :date)
          (if-let* ((country (plist-get release :country)))
              (format ", %s" country) "")))

(defun musicbrainz--extract-year (date)
  "Extract year from DATE string (YYYY-MM-DD or YYYY or YYYY-MM)."
  (when (and date (string-match "\\([0-9]\\{4\\}\\)" date))
    (string-to-number (match-string 1 date))))

;;;###autoload
(defun musicbrainz-find-year (artist title &optional limit callback)
  "Find the earliest release year for ARTIST and TITLE.
If CALLBACK provided, performs async request."
  (interactive "sArtist: \nsTitle: ")
  (let ((query (format "artist:\"%s\" AND release:\"%s\"" artist title)))
    (if callback
        (musicbrainz--request "release" query limit
          (lambda (json)
            (when-let* ((releases (alist-get 'releases json)))
              (let ((years (cl-loop for release across releases
                                   for date = (alist-get 'date release)
                                   for year = (musicbrainz--extract-year date)
                                   when year
                                   collect year)))
                (funcall callback (when years (apply #'min years)))))))
      (let ((result (when-let* ((json (musicbrainz--request "release" query limit))
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
        (musicbrainz--request "release" query limit
          (lambda (json)
            (when-let* ((releases (alist-get 'releases json))
                        (parsed-releases (cl-map 'list #'musicbrainz--parse-release releases))
                        (release-group-id (plist-get (car parsed-releases) :release-group-id)))
              (musicbrainz--lookup "release-group" release-group-id "genres"
                (lambda (rg-json)
                  (let ((genres (alist-get 'genres rg-json)))
                    (funcall callback (mapcar (lambda (g) (alist-get 'name g)) genres))))))))
      (let ((result (when-let* ((json (musicbrainz--request "release" query limit))
                                (releases (alist-get 'releases json))
                                (parsed-releases (cl-map 'list #'musicbrainz--parse-release releases))
                                (release-group-id (plist-get (car parsed-releases) :release-group-id)))
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
  (if (consp artist-credit)
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
        (musicbrainz--request "release" query limit
          (lambda (json)
            (when-let* ((releases (alist-get 'releases json))
                        (parsed-releases (cl-map 'list #'musicbrainz--parse-release releases))
                        (release-id (plist-get (car parsed-releases) :id)))
              (musicbrainz--lookup "release" release-id "recordings"
                (lambda (release-json)
                  (funcall callback (musicbrainz--parse-tracklist release-json)))))))
      (let ((result (when-let* ((json (musicbrainz--request "release" query limit))
                                (releases (alist-get 'releases json))
                                (parsed-releases (cl-map 'list #'musicbrainz--parse-release releases))
                                (release-id (plist-get (car parsed-releases) :id)))
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
        (musicbrainz--request "release" query limit
          (lambda (json)
            (when-let* ((releases (alist-get 'releases json))
                        (parsed-releases (cl-map 'list #'musicbrainz--parse-release releases)))
              (if (= (length parsed-releases) 1)
                  (let ((release-id (plist-get (car parsed-releases) :id)))
                    (musicbrainz--lookup "release" release-id "recordings"
                      (lambda (release-json)
                        (funcall callback (musicbrainz--parse-tracklist release-json)))))
                (let* ((entries (cl-loop for release in parsed-releases
                                         collect (cons
                                                  (format "%s %s %s (%s)"
                                                          (plist-get release :title)
                                                          (plist-get release :format)
                                                          (plist-get release :date)
                                                          (plist-get release :id))
                                                  (plist-get release :id))))
                       (choice (completing-read "Choose release: " (mapcar #'car entries))))
                  (when choice
                    (let ((release-id (cdr (assoc choice entries))))
                      (musicbrainz--lookup "release" release-id "recordings"
                        (lambda (release-json)
                          (funcall callback (musicbrainz--parse-tracklist release-json)))))))))))
      (let ((result (when-let* ((json (musicbrainz--request "release" query limit))
                                (releases (alist-get 'releases json))
                                (parsed-releases (cl-map 'list #'musicbrainz--parse-release releases)))
                      (if (= (length parsed-releases) 1)
                          (let ((release-id (plist-get (car parsed-releases) :id)))
                            (when-let* ((release-json (musicbrainz--lookup "release" release-id "recordings")))
                              (musicbrainz--parse-tracklist release-json)))
                        (let* ((entries (cl-loop for release in parsed-releases
                                                 collect (cons
                                                          (format "%s %s %s (%s)"
                                                                  (plist-get release :title)
                                                                  (plist-get release :format)
                                                                  (plist-get release :date)
                                                                  (plist-get release :id))
                                                          (plist-get release :id))))
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
  "Interactive search for TYPE (artist or release)."
  (let* ((prompt (if (equal type "artist") "Artist: " "Album: "))
         (query (read-string prompt))
         (buf (get-buffer-create (format "*MusicBrainz %s*" (capitalize type))))
         results)
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert "Searching...\n"))
    (pop-to-buffer buf)
    (funcall (if (equal type "artist")
                 #'musicbrainz-search-artist
               #'musicbrainz-search-release)
             query 25
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
                            do (insert (funcall (if (equal type "artist")
                                                   #'musicbrainz-format-artist
                                                 #'musicbrainz-format-release)
                                               r i)
                                      "\n")))
                 (goto-char (point-min))
                 (let ((selection (read-number (format "Select (1-%d) or q to quit: " (length results)) 1)))
                   (when (and (>= selection 1) (<= selection (length results)))
                     (musicbrainz-browse
                      (plist-get (nth (1- selection) results) :id)
                      type))))))))

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
(defun musicbrainz ()
  "Main entry point for MusicBrainz search."
  (interactive)
  (let ((type (completing-read "Search type: " '("artist" "release") nil t)))
    (musicbrainz-interactive-search type)))

;; Note: autoload comments are added to the function definitions above

(provide 'musicbrainz)

;;; musicbrainz.el ends here
