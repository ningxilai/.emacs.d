;;; musicbrainz-org.el --- MusicBrainz org-mode interface -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Mimo V2 Flash Free/MiniMax M2.5 Free
;; Keywords: music, org-mode, musicbrainz
;; Package-Requires: ((emacs "27.1") (org "9.0") (musicbrainz "1.0"))

;;; Commentary:

;; This package provides an org-mode interface for MusicBrainz queries.
;; It allows inserting MusicBrainz data as structured Org headings.
;;
;; Features:
;; - Interactive selection interface for search results
;; - Direct insertion into Org files with structured metadata
;; - Support for all MusicBrainz entity types
;; - No database dependency - pure file storage using Org properties
;;
;; Inspired by:
;; - denote: Simple file-naming scheme for notes
;; - org-supertag: Database-backed Org workflow with structured tags
;;
;; Design principles:
;; - No database: Store data directly in Org files
;; - Structured headings: Use Org properties for metadata
;; - Link support: Use Org ID links for references
;; - Minimal dependencies: Only requires org-mode and musicbrainz

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-id)
(require 'musicbrainz)

;;; Customization

(defgroup musicbrainz-org nil
  "MusicBrainz org-mode interface."
  :group 'music
  :prefix "musicbrainz-org-")

(defcustom musicbrainz-org-default-level 2
  "Default heading level for inserted MusicBrainz data."
  :type 'integer
  :group 'musicbrainz-org)

(defcustom musicbrainz-org-insert-properties t
  "Whether to insert Org properties drawer with metadata."
  :type 'boolean
  :group 'musicbrainz-org)

(defcustom musicbrainz-org-link-to-source t
  "Whether to create links to MusicBrainz source."
  :type 'boolean
  :group 'musicbrainz-org)

(defcustom musicbrainz-org-date-format "%Y-%m-%d"
  "Date format for timestamps."
  :type 'string
  :group 'musicbrainz-org)

(defcustom musicbrainz-org-show-selection t
  "Whether to show selection interface when multiple results are found."
  :type 'boolean
  :group 'musicbrainz-org)

;;; Helper functions

(defun musicbrainz-org--format-timestamp (&optional date)
  "Format DATE as Org timestamp."
  (when date
    (format "[%s]" (format-time-string musicbrainz-org-date-format
                                       (date-to-time date)))))

(defun musicbrainz-org--create-link (type id)
  "Create MusicBrainz link for TYPE and ID."
  (when musicbrainz-org-link-to-source
    (format "https://musicbrainz.org/%s/%s" type id)))

(defun musicbrainz-org--insert-properties (entity)
  "Insert Org properties drawer for ENTITY."
  (when musicbrainz-org-insert-properties
    (insert ":PROPERTIES:\n")
    (insert (format ":ID: %s\n" (oref entity id)))
    ;; Check if slot is bound before accessing
    (when (and (slot-exists-p entity 'type)
               (slot-boundp entity 'type)
               (oref entity type))
      (insert (format ":TYPE: %s\n" (oref entity type))))
    (when (and (slot-exists-p entity 'country)
               (slot-boundp entity 'country)
               (oref entity country))
      (insert (format ":COUNTRY: %s\n" (oref entity country))))
    (when (and (slot-exists-p entity 'disambiguation)
               (slot-boundp entity 'disambiguation)
               (oref entity disambiguation))
      (insert (format ":DISAMBIGUATION: %s\n" (oref entity disambiguation))))
    (when (and (slot-exists-p entity 'begin-date)
               (slot-boundp entity 'begin-date)
               (oref entity begin-date))
      (insert (format ":BEGIN-DATE: %s\n" (oref entity begin-date))))
    (when (and (slot-exists-p entity 'end-date)
               (slot-boundp entity 'end-date)
               (oref entity end-date))
      (insert (format ":END-DATE: %s\n" (oref entity end-date))))
    (when (and (slot-exists-p entity 'country-code)
               (slot-boundp entity 'country-code)
               (oref entity country-code))
      (insert (format ":COUNTRY-CODE: %s\n" (oref entity country-code))))
    (when (and (slot-exists-p entity 'language)
               (slot-boundp entity 'language)
               (oref entity language))
      (insert (format ":LANGUAGE: %s\n" (oref entity language))))
    (when (and (slot-exists-p entity 'address)
               (slot-boundp entity 'address)
               (oref entity address))
      (insert (format ":ADDRESS: %s\n" (oref entity address))))
    (when (and (slot-exists-p entity 'coordinates)
               (slot-boundp entity 'coordinates)
               (oref entity coordinates))
      (insert (format ":COORDINATES: %s\n" (oref entity coordinates))))
    (when (and (slot-exists-p entity 'time)
               (slot-boundp entity 'time)
               (oref entity time))
      (insert (format ":TIME: %s\n" (oref entity time))))
    (when (and (slot-exists-p entity 'setlist)
               (slot-boundp entity 'setlist)
               (oref entity setlist))
      (insert (format ":SETLIST: %s\n" (oref entity setlist))))
    (when (and (slot-exists-p entity 'resource)
               (slot-boundp entity 'resource)
               (oref entity resource))
      (insert (format ":RESOURCE: %s\n" (oref entity resource))))
    ;; Release-specific properties
    (when (and (slot-exists-p entity 'status)
               (slot-boundp entity 'status)
               (oref entity status))
      (insert (format ":STATUS: %s\n" (oref entity status))))
    (when (and (slot-exists-p entity 'format)
               (slot-boundp entity 'format)
               (oref entity format))
      (insert (format ":FORMAT: %s\n" (oref entity format))))
    (when (and (slot-exists-p entity 'date)
               (slot-boundp entity 'date)
               (oref entity date))
      (insert (format ":DATE: %s\n" (oref entity date))))
    (when (and (slot-exists-p entity 'artist)
               (slot-boundp entity 'artist)
               (oref entity artist))
      (insert (format ":ARTIST: %s\n" (oref entity artist))))
    (when (and (slot-exists-p entity 'first-release-date)
               (slot-boundp entity 'first-release-date)
               (oref entity first-release-date))
      (insert (format ":FIRST-RELEASE-DATE: %s\n" (oref entity first-release-date))))
    (insert ":END:\n")))

(defun musicbrainz-org--insert-link (type id)
  "Insert MusicBrainz link."
  (when (and musicbrainz-org-link-to-source type id)
    (insert (format "[[%s][MusicBrainz %s]]\n"
                    (musicbrainz-org--create-link type id)
                    (capitalize type)))))

;;; Selection interface

(defun musicbrainz-org--select-item (items format-func)
  "Select an item from ITEMS using FORMAT-FUNC for display.
Returns the selected item or nil if cancelled."
  (if (null items)
      (progn
        (message "No items found")
        nil)
    (if (and musicbrainz-org-show-selection (> (length items) 1))
        ;; Simple completing-read with fuzzy search (no pagination or navigation)
        (let* ((formatted-items (cl-loop for item in items
                                         collect (funcall format-func item)))
               (choice (completing-read "Select item: "
                                        formatted-items
                                        nil t)))  ; Require match, fuzzy search enabled
          (when choice
            ;; Find the selected item by matching the formatted string
            (let ((pos (cl-position choice formatted-items :test 'equal)))
              (when pos
                (nth pos items)))))
      ;; Return first item if no selection needed
      (car items))))

(defun musicbrainz-org--select-item-progressive (query search-func format-func &optional offset all-items)
  "Select an item using progressive loading.
QUERY is the search query.
SEARCH-FUNC is a function that takes query, limit, and offset.
FORMAT-FUNC formats items for display.
OFFSET is the current offset (default 0).
ALL-ITEMS accumulates all loaded items."
  (let* ((offset (or offset 0))
         (page-size 20)  ; Page size for progressive loading
         (current-page (funcall search-func query page-size offset))
         (accumulated-items (append (or all-items '()) current-page)))
    (if current-page
        (let* ((formatted-items (cl-loop for item in accumulated-items
                                         collect (funcall format-func item)))
               (prompt (format "Select item (loaded %d total)%s: "
                               (length accumulated-items)
                               (if (= (length current-page) page-size) " [more available]" "")))
               (choice (completing-read prompt formatted-items nil t)))
          (if choice
              ;; Find the selected item
              (let ((pos (cl-position choice formatted-items :test 'equal)))
                (when pos
                  (nth pos accumulated-items)))
            ;; User cancelled - check if we should load more
            (when (and (= (length current-page) page-size)
                       (y-or-n-p (format "Load more items? (loaded %d so far)" (length accumulated-items))))
              (musicbrainz-org--select-item-progressive query search-func format-func (+ offset page-size) accumulated-items))))
      ;; No more items
      (if (> (length accumulated-items) 0)
          (progn
            (message "Loaded all %d items" (length accumulated-items))
            ;; Show final selection
            (let* ((formatted-items (cl-loop for item in accumulated-items
                                             collect (funcall format-func item)))
                   (choice (completing-read "Select item: " formatted-items nil t)))
              (when choice
                (let ((pos (cl-position choice formatted-items :test 'equal)))
                  (when pos
                    (nth pos accumulated-items))))))
        (message "No items found for query: %s" query)
        nil))))

(defun musicbrainz-org--format-artist-for-selection (artist)
  "Format ARTIST for selection display."
  (format "%s (%s%s%s)"
          (oref artist name)
          (if (oref artist type) (oref artist type) "Unknown")
          (if (oref artist country) (format ", %s" (oref artist country)) "")
          (if (oref artist disambiguation) (format ", %s" (oref artist disambiguation)) "")))

(defun musicbrainz-org--format-release-for-selection (release)
  "Format RELEASE for selection display."
  (format "%s - %s (%s%s%s)"
          (if (oref release artist) (oref release artist) "Unknown")
          (oref release title)
          (if (oref release date) (oref release date) "Unknown date")
          (if (oref release country) (format ", %s" (oref release country)) "")
          (if (oref release status) (format ", %s" (oref release status)) "")))

(defun musicbrainz-org--format-label-for-selection (label)
  "Format LABEL for selection display."
  (format "%s (%s%s%s)"
          (oref label name)
          (if (oref label type) (oref label type) "Unknown")
          (if (oref label country) (format ", %s" (oref label country)) "")
          (if (oref label disambiguation) (format ", %s" (oref label disambiguation)) "")))

(defun musicbrainz-org--format-work-for-selection (work)
  "Format WORK for selection display."
  (format "%s (%s%s)"
          (oref work title)
          (if (oref work type) (oref work type) "Unknown")
          (if (oref work language) (format ", %s" (oref work language)) "")))

(defun musicbrainz-org--format-area-for-selection (area)
  "Format AREA for selection display."
  (format "%s (%s%s)"
          (oref area name)
          (if (oref area type) (oref area type) "Unknown")
          (if (oref area country-code) (format ", %s" (oref area country-code)) "")))

(defun musicbrainz-org--format-event-for-selection (event)
  "Format EVENT for selection display."
  (format "%s (%s%s)"
          (oref event name)
          (if (oref event type) (oref event type) "Unknown")
          (if (oref event time) (format ", %s" (oref event time)) "")))

(defun musicbrainz-org--format-instrument-for-selection (instrument)
  "Format INSTRUMENT for selection display."
  (format "%s (%s)"
          (oref instrument name)
          (if (oref instrument type) (oref instrument type) "Unknown")))

(defun musicbrainz-org--format-place-for-selection (place)
  "Format PLACE for selection display."
  (format "%s (%s%s)"
          (oref place name)
          (if (oref place type) (oref place type) "Unknown")
          (if (oref place address) (format ", %s" (oref place address)) "")))

(defun musicbrainz-org--format-series-for-selection (series)
  "Format SERIES for selection display."
  (format "%s (%s)"
          (oref series name)
          (if (oref series type) (oref series type) "Unknown")))

(defun musicbrainz-org--format-url-for-selection (url)
  "Format URL for selection display."
  (format "%s -> %s"
          (oref url resource)
          (oref url id)))

(defun musicbrainz-org--format-recording-for-selection (recording)
  "Format RECORDING for selection display."
  (format "%s (%s%s)"
          (oref recording title)
          (if (oref recording length)
              (format "%d ms" (oref recording length))
            "Unknown length")
          (if (oref recording first-release-date)
              (format ", %s" (oref recording first-release-date))
            "")))

(defun musicbrainz-org--format-release-group-for-selection (release-group)
  "Format RELEASE-GROUP for selection display."
  (format "%s (%s)"
          (oref release-group title)
          (if (oref release-group type) (oref release-group type) "Unknown")))

(defun musicbrainz-org--format-genre-for-selection (genre)
  "Format GENRE for selection display."
  (format "%s%s"
          (oref genre name)
          (if (oref genre disambiguation)
              (format " (%s)" (oref genre disambiguation)) "")))

;;; Insert functions

(defun musicbrainz-org-insert-artist (artist &optional level)
  "Insert ARTIST as Org heading at LEVEL."
  (interactive)
  (let ((level (or level musicbrainz-org-default-level)))
    (insert (make-string level ?*) " " (oref artist name) "\n")
    (musicbrainz-org--insert-properties artist)
    (musicbrainz-org--insert-link "artist" (oref artist id))
    (insert "\n")))

(defun musicbrainz-org-insert-release (release &optional level)
  "Insert RELEASE as Org heading at LEVEL."
  (interactive)
  (let ((level (or level musicbrainz-org-default-level)))
    (insert (make-string level ?*) " " (oref release title) "\n")
    (musicbrainz-org--insert-properties release)
    (when (and (slot-exists-p release 'artist-credit)
               (slot-boundp release 'artist-credit)
               (oref release artist-credit))
      (musicbrainz-org-insert-artist-credit (oref release artist-credit) (1+ level)))
    (when (and (slot-exists-p release 'tracks)
               (slot-boundp release 'tracks)
               (oref release tracks))
      (musicbrainz-org-insert-tracklist (oref release tracks) (1+ level)))
    (musicbrainz-org--insert-link "release" (oref release id))
    (insert "\n")))

(defun musicbrainz-org-insert-label (label &optional level)
  "Insert LABEL as Org heading at LEVEL."
  (interactive)
  (let ((level (or level musicbrainz-org-default-level)))
    (insert (make-string level ?*) " " (oref label name) "\n")
    (musicbrainz-org--insert-properties label)
    (musicbrainz-org--insert-link "label" (oref label id))
    (insert "\n")))

(defun musicbrainz-org-insert-work (work &optional level)
  "Insert WORK as Org heading at LEVEL."
  (interactive)
  (let ((level (or level musicbrainz-org-default-level)))
    (insert (make-string level ?*) " " (oref work title) "\n")
    (musicbrainz-org--insert-properties work)
    (musicbrainz-org--insert-link "work" (oref work id))
    (insert "\n")))

(defun musicbrainz-org-insert-area (area &optional level)
  "Insert AREA as Org heading at LEVEL."
  (interactive)
  (let ((level (or level musicbrainz-org-default-level)))
    (insert (make-string level ?*) " " (oref area name) "\n")
    (musicbrainz-org--insert-properties area)
    (musicbrainz-org--insert-link "area" (oref area id))
    (insert "\n")))

(defun musicbrainz-org-insert-event (event &optional level)
  "Insert EVENT as Org heading at LEVEL."
  (interactive)
  (let ((level (or level musicbrainz-org-default-level)))
    (insert (make-string level ?*) " " (oref event name) "\n")
    (musicbrainz-org--insert-properties event)
    (musicbrainz-org--insert-link "event" (oref event id))
    (insert "\n")))

(defun musicbrainz-org-insert-instrument (instrument &optional level)
  "Insert INSTRUMENT as Org heading at LEVEL."
  (interactive)
  (let ((level (or level musicbrainz-org-default-level)))
    (insert (make-string level ?*) " " (oref instrument name) "\n")
    (musicbrainz-org--insert-properties instrument)
    (musicbrainz-org--insert-link "instrument" (oref instrument id))
    (insert "\n")))

(defun musicbrainz-org-insert-place (place &optional level)
  "Insert PLACE as Org heading at LEVEL."
  (interactive)
  (let ((level (or level musicbrainz-org-default-level)))
    (insert (make-string level ?*) " " (oref place name) "\n")
    (musicbrainz-org--insert-properties place)
    (musicbrainz-org--insert-link "place" (oref place id))
    (insert "\n")))

(defun musicbrainz-org-insert-series (series &optional level)
  "Insert SERIES as Org heading at LEVEL."
  (interactive)
  (let ((level (or level musicbrainz-org-default-level)))
    (insert (make-string level ?*) " " (oref series name) "\n")
    (musicbrainz-org--insert-properties series)
    (musicbrainz-org--insert-link "series" (oref series id))
    (insert "\n")))

(defun musicbrainz-org-insert-url (url &optional level)
  "Insert URL as Org heading at LEVEL."
  (interactive)
  (let ((level (or level musicbrainz-org-default-level)))
    (insert (make-string level ?*) " " (oref url resource) "\n")
    (musicbrainz-org--insert-properties url)
    (insert (format "[[%s][%s]]\n" (oref url resource) (oref url resource)))
    (insert "\n")))

(defun musicbrainz-org-insert-genre (genre &optional level)
  "Insert GENRE as Org heading at LEVEL."
  (interactive)
  (let ((level (or level musicbrainz-org-default-level)))
    (insert (make-string level ?*) " " (oref genre name) "\n")
    (musicbrainz-org--insert-properties genre)
    (musicbrainz-org--insert-link "genre" (oref genre id))
    (insert "\n")))

(defun musicbrainz-org-insert-recording (recording &optional level)
  "Insert RECORDING as Org heading at LEVEL."
  (interactive)
  (let ((level (or level musicbrainz-org-default-level)))
    (insert (make-string level ?*) " " (oref recording title) "\n")
    (musicbrainz-org--insert-properties recording)
    (when (and (slot-exists-p recording 'artist-credit)
               (slot-boundp recording 'artist-credit)
               (oref recording artist-credit))
      (musicbrainz-org-insert-artist-credit (oref recording artist-credit) (1+ level)))
    (when (oref recording first-release-date)
      (insert (format "First released: %s\n" (oref recording first-release-date))))
    (musicbrainz-org--insert-link "recording" (oref recording id))
    (insert "\n")))

(defun musicbrainz-org-insert-release-group (release-group &optional level)
  "Insert RELEASE-GROUP as Org heading at LEVEL."
  (interactive)
  (let ((level (or level musicbrainz-org-default-level)))
    (insert (make-string level ?*) " " (oref release-group title) "\n")
    (musicbrainz-org--insert-properties release-group)
    (when (and (slot-exists-p release-group 'artist-credit)
               (slot-boundp release-group 'artist-credit)
               (oref release-group artist-credit))
      (musicbrainz-org-insert-artist-credit (oref release-group artist-credit) (1+ level)))
    (musicbrainz-org--insert-link "release-group" (oref release-group id))
    (insert "\n")))

;;; Interactive functions with selection

(defun musicbrainz-org-search-and-insert-artist (query &optional level)
  "Search for artist and insert result at point with selection."
  (interactive "sSearch artist: ")
  (let ((selected (musicbrainz-org--select-item-progressive
                   query
                   (lambda (q limit offset) (musicbrainz-search-artist q limit offset))
                   #'musicbrainz-org--format-artist-for-selection)))
    (if selected
        (musicbrainz-org-insert-artist selected level)
      (message "No artist found for query: %s" query))))

(defun musicbrainz-org-search-and-insert-release (query &optional level)
  "Search for release and insert result at point with selection."
  (interactive "sSearch release: ")
  (let ((selected (musicbrainz-org--select-item-progressive
                   query
                   (lambda (q limit offset) (musicbrainz-search-release q limit offset))
                   #'musicbrainz-org--format-release-for-selection)))
    (if selected
        (musicbrainz-org-insert-release selected level)
      (message "No release found for query: %s" query))))

(defun musicbrainz-org-search-and-insert-label (query &optional level)
  "Search for label and insert result at point with selection."
  (interactive "sSearch label: ")
  (let ((selected (musicbrainz-org--select-item-progressive
                   query
                   (lambda (q limit offset) (musicbrainz-search-label q limit offset))
                   #'musicbrainz-org--format-label-for-selection)))
    (if selected
        (musicbrainz-org-insert-label selected level)
      (message "No label found for query: %s" query))))

(defun musicbrainz-org-search-and-insert-work (query &optional level)
  "Search for work and insert result at point with selection."
  (interactive "sSearch work: ")
  (let ((selected (musicbrainz-org--select-item-progressive
                   query
                   (lambda (q limit offset) (musicbrainz-search-work q limit offset))
                   #'musicbrainz-org--format-work-for-selection)))
    (if selected
        (musicbrainz-org-insert-work selected level)
      (message "No work found for query: %s" query))))

(defun musicbrainz-org-search-and-insert-area (query &optional level)
  "Search for area and insert result at point with selection."
  (interactive "sSearch area: ")
  (let ((selected (musicbrainz-org--select-item-progressive
                   query
                   (lambda (q limit offset) (musicbrainz-search-area q limit offset))
                   #'musicbrainz-org--format-area-for-selection)))
    (if selected
        (musicbrainz-org-insert-area selected level)
      (message "No area found for query: %s" query))))

(defun musicbrainz-org-search-and-insert-event (query &optional level)
  "Search for event and insert result at point with selection."
  (interactive "sSearch event: ")
  (let ((selected (musicbrainz-org--select-item-progressive
                   query
                   (lambda (q limit offset) (musicbrainz-search-event q limit offset))
                   #'musicbrainz-org--format-event-for-selection)))
    (if selected
        (musicbrainz-org-insert-event selected level)
      (message "No event found for query: %s" query))))

(defun musicbrainz-org-search-and-insert-instrument (query &optional level)
  "Search for instrument and insert result at point with selection."
  (interactive "sSearch instrument: ")
  (let ((selected (musicbrainz-org--select-item-progressive
                   query
                   (lambda (q limit offset) (musicbrainz-search-instrument q limit offset))
                   #'musicbrainz-org--format-instrument-for-selection)))
    (if selected
        (musicbrainz-org-insert-instrument selected level)
      (message "No instrument found for query: %s" query))))

(defun musicbrainz-org-search-and-insert-place (query &optional level)
  "Search for place and insert result at point with selection."
  (interactive "sSearch place: ")
  (let ((selected (musicbrainz-org--select-item-progressive
                   query
                   (lambda (q limit offset) (musicbrainz-search-place q limit offset))
                   #'musicbrainz-org--format-place-for-selection)))
    (if selected
        (musicbrainz-org-insert-place selected level)
      (message "No place found for query: %s" query))))

(defun musicbrainz-org-search-and-insert-series (query &optional level)
  "Search for series and insert result at point with selection."
  (interactive "sSearch series: ")
  (let ((selected (musicbrainz-org--select-item-progressive
                   query
                   (lambda (q limit offset) (musicbrainz-search-series q limit offset))
                   #'musicbrainz-org--format-series-for-selection)))
    (if selected
        (musicbrainz-org-insert-series selected level)
      (message "No series found for query: %s" query))))

(defun musicbrainz-org-search-and-insert-url (query &optional level)
  "Search for URL and insert result at point with selection."
  (interactive "sSearch URL: ")
  (let ((selected (musicbrainz-org--select-item-progressive
                   query
                   (lambda (q limit offset) (musicbrainz-search-url q limit offset))
                   #'musicbrainz-org--format-url-for-selection)))
    (if selected
        (musicbrainz-org-insert-url selected level)
      (message "No URL found for query: %s" query))))

(defun musicbrainz-org-search-and-insert-recording (query &optional level)
  "Search for recording and insert result at point with selection."
  (interactive "sSearch recording: ")
  (let ((selected (musicbrainz-org--select-item-progressive
                   query
                   (lambda (q limit offset) (musicbrainz-search-recording q limit offset))
                   #'musicbrainz-org--format-recording-for-selection)))
    (if selected
        (musicbrainz-org-insert-recording selected level)
      (message "No recording found for query: %s" query))))

(defun musicbrainz-org-search-and-insert-release-group (query &optional level)
  "Search for release group and insert result at point with selection."
  (interactive "sSearch release group: ")
  (let ((selected (musicbrainz-org--select-item-progressive
                   query
                   (lambda (q limit offset) (musicbrainz-search-release-group q limit offset))
                   #'musicbrainz-org--format-release-group-for-selection)))
    (if selected
        (musicbrainz-org-insert-release-group selected level)
      (message "No release group found for query: %s" query))))

(defun musicbrainz-org-insert-genre-from-release (query &optional level)
  "Search for a release by QUERY, get its genres, and insert them.
This automates getting MBID and inserting genre fields."
  (interactive "sSearch release for genres: ")
  (let* ((releases (musicbrainz-search-release query 1))
         (release (car releases)))
    (if release
        (let* ((release-group-id (oref release release-group-id))
               (rg-json (musicbrainz--lookup "release-group" release-group-id "genres"))
               (genres (alist-get 'genres rg-json)))
          (if genres
              (progn
                (insert (format "\n** Genres for %s\n" (oref release title)))
                (dolist (genre genres)
                  (let ((genre-name (alist-get 'name genre))
                        (genre-id (alist-get 'id genre)))
                    (insert (format "- %s [genre:%s]\n" genre-name genre-id))))
                (insert "\n"))
            (message "No genres found for release: %s" (oref release title))))
      (message "No release found for query: %s" query))))

(defun musicbrainz-org-insert-genre-from-artist (query &optional level)
  "Search for an artist by QUERY, get genres from their releases, and insert them.
This automates getting MBID and inserting genre fields."
  (interactive "sSearch artist for genres: ")
  (let* ((artists (musicbrainz-search-artist query 1))
         (artist (car artists)))
    (if artist
        (let* ((releases (musicbrainz-browse-releases (oref artist id) 1))
               (release (car releases)))
          (if release
              (let* ((release-group-id (oref release release-group-id))
                     (rg-json (musicbrainz--lookup "release-group" release-group-id "genres"))
                     (genres (alist-get 'genres rg-json)))
                (if genres
                    (progn
                      (insert (format "\n** Genres for %s\n" (oref artist name)))
                      (dolist (genre genres)
                        (let ((genre-name (alist-get 'name genre))
                              (genre-id (alist-get 'id genre)))
                          (insert (format "- %s [genre:%s]\n" genre-name genre-id))))
                      (insert "\n"))
                  (message "No genres found for artist: %s" (oref artist name))))
            (message "No releases found for artist: %s" (oref artist name))))
      (message "No artist found for query: %s" query))))

(defun musicbrainz-org-browse-all-genres (&optional level)
  "Browse all available genres from MusicBrainz and insert them."
  (interactive)
  (let ((genres (musicbrainz-browse-genres)))
    (if genres
        (progn
          (insert (format "\n** All MusicBrainz Genres\n"))
          (dolist (genre genres)
            (let ((genre-name (oref genre name))
                  (genre-id (oref genre id))
                  (disambig (oref genre disambiguation)))
              (insert (format "- %s [genre:%s]%s\n"
                              genre-name
                              genre-id
                              (if disambig (format " (%s)" disambig) "")))))
          (insert "\n")
          (message "Inserted %d genres" (length genres)))
      (message "No genres found"))))

;;; Batch insertion functions

(defun musicbrainz-org-insert-search-results (entity-type query &optional level)
  "Insert multiple search results for ENTITY-TYPE and QUERY.
Shows selection interface if multiple results are found."
   (interactive
    (list (completing-read "Entity type: "
                           '("artist" "release" "release-group" "recording" "label" "work" "area"
                             "event" "instrument" "place" "series" "url" "genre"))
          (read-string "Search query: ")))
    (let* ((level (or level musicbrainz-org-default-level))
           (results (cond
                     ((equal entity-type "artist") (musicbrainz-search-artist query))
                     ((equal entity-type "release") (musicbrainz-search-release query))
                     ((equal entity-type "release-group") (musicbrainz-search-release-group query))
                     ((equal entity-type "recording") (musicbrainz-search-recording query))
                     ((equal entity-type "label") (musicbrainz-search-label query))
                     ((equal entity-type "work") (musicbrainz-search-work query))
                     ((equal entity-type "area") (musicbrainz-search-area query))
                     ((equal entity-type "event") (musicbrainz-search-event query))
                     ((equal entity-type "instrument") (musicbrainz-search-instrument query))
                     ((equal entity-type "place") (musicbrainz-search-place query))
                     ((equal entity-type "series") (musicbrainz-search-series query))
                     ((equal entity-type "url") (musicbrainz-search-url query))
                     ((equal entity-type "genre") (message "Genre search is not supported by MusicBrainz API") nil))))
      (if results
          (progn
            (insert (format "\n* MusicBrainz %s search results for \"%s\"\n\n"
                            (capitalize entity-type) query))
            (dolist (result results)
              (cond
               ((equal entity-type "artist") (musicbrainz-org-insert-artist result level))
               ((equal entity-type "release") (musicbrainz-org-insert-release result level))
               ((equal entity-type "release-group") (musicbrainz-org-insert-release-group result level))
               ((equal entity-type "recording") (musicbrainz-org-insert-recording result level))
               ((equal entity-type "label") (musicbrainz-org-insert-label result level))
               ((equal entity-type "work") (musicbrainz-org-insert-work result level))
               ((equal entity-type "area") (musicbrainz-org-insert-area result level))
               ((equal entity-type "event") (musicbrainz-org-insert-event result level))
               ((equal entity-type "instrument") (musicbrainz-org-insert-instrument result level))
               ((equal entity-type "place") (musicbrainz-org-insert-place result level))
               ((equal entity-type "series") (musicbrainz-org-insert-series result level))
               ((equal entity-type "url") (musicbrainz-org-insert-url result level))))
            (message "Inserted %d %s results" (length results) entity-type))
       (message "No results found for query: %s" query))))

(defun musicbrainz-org-insert-lookup (entity-type mbid &optional level)
  "Insert lookup result for ENTITY-TYPE and MBID."
   (interactive
    (list (completing-read "Entity type: "
                           '("artist" "release" "release-group" "recording" "label" "work"
                             "area" "event" "instrument" "place" "series" "url" "genre"))
          (read-string "MBID: ")))
   (let* ((level (or level musicbrainz-org-default-level))
          (result (cond
                   ((equal entity-type "artist") (musicbrainz-lookup-artist mbid))
                   ((equal entity-type "release") (musicbrainz-lookup-release mbid))
                   ((equal entity-type "release-group") (musicbrainz-lookup-release-group mbid))
                   ((equal entity-type "recording") (musicbrainz-lookup-recording mbid))
                   ((equal entity-type "label") (musicbrainz-lookup-label mbid))
                   ((equal entity-type "work") (musicbrainz-lookup-work mbid))
                   ((equal entity-type "area") (musicbrainz-lookup-area mbid))
                   ((equal entity-type "event") (musicbrainz-lookup-event mbid))
                   ((equal entity-type "instrument") (musicbrainz-lookup-instrument mbid))
                   ((equal entity-type "place") (musicbrainz-lookup-place mbid))
                    ((equal entity-type "series") (musicbrainz-lookup-series mbid))
                    ((equal entity-type "url") (musicbrainz-lookup-url mbid))
                    ((equal entity-type "genre") (musicbrainz-lookup-genre mbid)))))
     (if result
         (cond
          ((equal entity-type "artist") (musicbrainz-org-insert-artist result level))
          ((equal entity-type "release") (musicbrainz-org-insert-release result level))
          ((equal entity-type "release-group") (musicbrainz-org-insert-release-group result level))
          ((equal entity-type "recording") (musicbrainz-org-insert-recording result level))
          ((equal entity-type "label") (musicbrainz-org-insert-label result level))
          ((equal entity-type "work") (musicbrainz-org-insert-work result level))
          ((equal entity-type "area") (musicbrainz-org-insert-area result level))
          ((equal entity-type "event") (musicbrainz-org-insert-event result level))
          ((equal entity-type "instrument") (musicbrainz-org-insert-instrument result level))
          ((equal entity-type "place") (musicbrainz-org-insert-place result level))
           ((equal entity-type "series") (musicbrainz-org-insert-series result level))
           ((equal entity-type "url") (musicbrainz-org-insert-url result level))
           ((equal entity-type "genre") (musicbrainz-org-insert-genre result level)))
       (message "No result found for MBID: %s" mbid))))

;;; Org capture integration

(defun musicbrainz-org-capture-artist (query)
  "Capture artist search result to Org capture template."
  (interactive "sSearch artist: ")
  (let ((artists (musicbrainz-search-artist query 1)))
    (if artists
        (let ((artist (car artists)))
          (org-capture nil "a")
          (insert (oref artist name) "\n")
          (musicbrainz-org--insert-properties artist)
          (musicbrainz-org--insert-link "artist" (oref artist id)))
      (message "No artist found for query: %s" query))))

(defun musicbrainz-org-capture-release (query)
  "Capture release search result to Org capture template."
  (interactive "sSearch release: ")
  (let ((releases (musicbrainz-search-release query 1)))
    (if releases
        (let ((release (car releases)))
          (org-capture nil "a")
          (insert (oref release title) "\n")
          (musicbrainz-org--insert-properties release)
          (musicbrainz-org--insert-link "release" (oref release id)))
      (message "No release found for query: %s" query))))

;;; Utility functions

(defun musicbrainz-org-insert-artist-credit (artist-credit &optional level)
  "Insert artist credit as Org formatted text."
  (when artist-credit
    (let ((level (or level (1+ musicbrainz-org-default-level))))
      (insert (make-string level ?*) " Artist Credit\n")
      (mapc (lambda (credit)
              (let* ((artist (alist-get 'artist credit))
                     (name (alist-get 'name credit))
                     (artist-name (or name (alist-get 'name artist))))
                (insert (format "  - %s\n" artist-name))))
            artist-credit)
      (insert "\n"))))

(defun musicbrainz-org-insert-tracklist (tracks &optional level)
  "Insert tracklist as Org list."
  (when tracks
    (let ((level (or level (1+ musicbrainz-org-default-level))))
      (insert (make-string level ?*) " Tracklist\n")
      (mapc (lambda (track)
              (let ((track-name (alist-get 'title track)))
                (insert (format "  - %s\n" track-name))))
            tracks)
      (insert "\n"))))

(provide 'musicbrainz-org)

;;; musicbrainz-org.el ends here
