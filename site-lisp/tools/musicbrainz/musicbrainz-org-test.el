;;; musicbrainz-org-test.el --- Tests for musicbrainz-org.el -*- lexical-binding: t; -*-

(require 'musicbrainz-org)

(ert-deftest musicbrainz-org-test-format-timestamp ()
  "Test timestamp formatting."
  (let ((musicbrainz-org-date-format "%Y-%m-%d"))
    (should (string-match-p "\\[20[0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\]"
                           (musicbrainz-org--format-timestamp "2023-01-01")))))

(ert-deftest musicbrainz-org-test-create-link ()
  "Test link creation."
  (let ((musicbrainz-org-link-to-source t))
    (should (equal (musicbrainz-org--create-link "artist" "test-id")
                   "https://musicbrainz.org/artist/test-id"))))

(ert-deftest musicbrainz-org-test-insert-artist ()
  "Test inserting artist into Org buffer."
  (with-temp-buffer
    (org-mode)
    (let ((artist (make-instance 'musicbrainz-artist
                                 :id "test-id"
                                 :name "Test Artist"
                                 :type "Person"
                                 :country "US"
                                 :disambiguation "test")))
      (musicbrainz-org-insert-artist artist 2)
      (should (string-match-p "\\* Test Artist" (buffer-string)))
      (should (string-match-p ":ID: test-id" (buffer-string)))
      (should (string-match-p ":TYPE: Person" (buffer-string)))
      (should (string-match-p ":COUNTRY: US" (buffer-string)))
      (should (string-match-p ":DISAMBIGUATION: test" (buffer-string))))))

(ert-deftest musicbrainz-org-test-insert-release ()
  "Test inserting release into Org buffer."
  (with-temp-buffer
    (org-mode)
    (let ((release (make-instance 'musicbrainz-release
                                  :id "test-id"
                                  :title "Test Album"
                                  :artist "Test Artist"
                                  :date "2023-01-01"
                                  :country "US"
                                  :status "Official")))
      (musicbrainz-org-insert-release release 2)
      (should (string-match-p "\\* Test Album" (buffer-string)))
      (should (string-match-p ":ID: test-id" (buffer-string)))
      (should (string-match-p "Artist: Test Artist" (buffer-string)))
      (should (string-match-p "Date: 2023-01-01" (buffer-string)))
      (should (string-match-p "Status: Official" (buffer-string))))))

(ert-deftest musicbrainz-org-test-insert-label ()
  "Test inserting label into Org buffer."
  (with-temp-buffer
    (org-mode)
    (let ((label (make-instance 'musicbrainz-label
                                :id "test-id"
                                :name "Test Label"
                                :type "Production"
                                :country "US"
                                :disambiguation "test")))
      (musicbrainz-org-insert-label label 2)
      (should (string-match-p "\\* Test Label" (buffer-string)))
      (should (string-match-p ":ID: test-id" (buffer-string)))
      (should (string-match-p ":TYPE: Production" (buffer-string)))
      (should (string-match-p ":DISAMBIGUATION: test" (buffer-string))))))

(ert-deftest musicbrainz-org-test-insert-work ()
  "Test inserting work into Org buffer."
  (with-temp-buffer
    (org-mode)
    (let ((work (make-instance 'musicbrainz-work
                               :id "test-id"
                               :title "Test Work"
                               :type "Song"
                               :language "eng"
                               :disambiguation "test")))
      (musicbrainz-org-insert-work work 2)
      (should (string-match-p "\\* Test Work" (buffer-string)))
      (should (string-match-p ":ID: test-id" (buffer-string)))
      (should (string-match-p "Language: eng" (buffer-string)))
      (should (string-match-p ":DISAMBIGUATION: test" (buffer-string))))))

(ert-deftest musicbrainz-org-test-insert-area ()
  "Test inserting area into Org buffer."
  (with-temp-buffer
    (org-mode)
    (let ((area (make-instance 'musicbrainz-area
                               :id "test-id"
                               :name "Test Area"
                               :type "Country"
                               :country-code "US"
                               :disambiguation "test")))
      (musicbrainz-org-insert-area area 2)
      (should (string-match-p "\\* Test Area" (buffer-string)))
      (should (string-match-p ":ID: test-id" (buffer-string)))
      (should (string-match-p ":COUNTRY-CODE: US" (buffer-string)))
      (should (string-match-p ":DISAMBIGUATION: test" (buffer-string))))))

(ert-deftest musicbrainz-org-test-insert-event ()
  "Test inserting event into Org buffer."
  (with-temp-buffer
    (org-mode)
    (let ((event (make-instance 'musicbrainz-event
                               :id "test-id"
                               :name "Test Event"
                               :type "Concert"
                               :time "2023-01-01T20:00:00Z"
                               :disambiguation "test")))
      (musicbrainz-org-insert-event event 2)
      (should (string-match-p "\\* Test Event" (buffer-string)))
      (should (string-match-p ":ID: test-id" (buffer-string)))
      (should (string-match-p ":TYPE: Concert" (buffer-string)))
      (should (string-match-p ":TIME: 2023-01-01T20:00:00Z" (buffer-string))))))

(ert-deftest musicbrainz-org-test-insert-instrument ()
  "Test inserting instrument into Org buffer."
  (with-temp-buffer
    (org-mode)
    (let ((instrument (make-instance 'musicbrainz-instrument
                                    :id "test-id"
                                    :name "Test Instrument"
                                    :type "String")))
      (musicbrainz-org-insert-instrument instrument 2)
      (should (string-match-p "\\* Test Instrument" (buffer-string)))
      (should (string-match-p ":ID: test-id" (buffer-string)))
      (should (string-match-p ":TYPE: String" (buffer-string))))))

(ert-deftest musicbrainz-org-test-insert-place ()
  "Test inserting place into Org buffer."
  (with-temp-buffer
    (org-mode)
    (let ((place (make-instance 'musicbrainz-place
                               :id "test-id"
                               :name "Test Place"
                               :type "Venue"
                               :address "123 Test St"
                               :disambiguation "test")))
      (musicbrainz-org-insert-place place 2)
      (should (string-match-p "\\* Test Place" (buffer-string)))
      (should (string-match-p ":ID: test-id" (buffer-string)))
      (should (string-match-p ":TYPE: Venue" (buffer-string)))
      (should (string-match-p ":ADDRESS: 123 Test St" (buffer-string))))))

(ert-deftest musicbrainz-org-test-insert-series ()
  "Test inserting series into Org buffer."
  (with-temp-buffer
    (org-mode)
    (let ((series (make-instance 'musicbrainz-series
                                :id "test-id"
                                :name "Test Series"
                                :type "Album series")))
      (musicbrainz-org-insert-series series 2)
      (should (string-match-p "\\* Test Series" (buffer-string)))
      (should (string-match-p ":ID: test-id" (buffer-string)))
      (should (string-match-p ":TYPE: Album series" (buffer-string))))))

(ert-deftest musicbrainz-org-test-insert-url ()
  "Test inserting URL into Org buffer."
  (with-temp-buffer
    (org-mode)
    (let ((url (make-instance 'musicbrainz-url
                             :id "test-id"
                             :resource "https://example.com")))
      (musicbrainz-org-insert-url url 2)
      (should (string-match-p "\\* https://example.com" (buffer-string)))
      (should (string-match-p ":ID: test-id" (buffer-string)))
      (should (string-match-p "https://example.com" (buffer-string))))))

(ert-deftest musicbrainz-org-test-insert-recording ()
  "Test inserting recording into Org buffer."
  (with-temp-buffer
    (org-mode)
    (let ((recording (make-instance 'musicbrainz-recording
                                   :id "test-id"
                                   :title "Test Recording"
                                   :length 180000
                                   :first-release-date "2023-01-01")))
      (musicbrainz-org-insert-recording recording 2)
      (should (string-match-p "\\* Test Recording" (buffer-string)))
      (should (string-match-p ":ID: test-id" (buffer-string)))
      (should (string-match-p "First released: 2023-01-01" (buffer-string))))))

(ert-deftest musicbrainz-org-test-insert-release-group ()
  "Test inserting release group into Org buffer."
  (with-temp-buffer
    (org-mode)
    (let ((release-group (make-instance 'musicbrainz-release-group
                                       :id "test-id"
                                       :title "Test Release Group"
                                       :type "Album"
                                       :first-release-date "2023-01-01")))
      (musicbrainz-org-insert-release-group release-group 2)
      (should (string-match-p "\\* Test Release Group" (buffer-string)))
      (should (string-match-p ":ID: test-id" (buffer-string)))
      (should (string-match-p ":TYPE: Album" (buffer-string)))
      (should (string-match-p ":FIRST-RELEASE-DATE: 2023-01-01" (buffer-string))))))

(ert-deftest musicbrainz-org-test-insert-genre ()
  "Test inserting genre into Org buffer."
  (with-temp-buffer
    (org-mode)
    (let ((genre (make-instance 'musicbrainz-genre
                               :id "test-id"
                               :name "Test Genre"
                               :disambiguation "test genre")))
      (musicbrainz-org-insert-genre genre 2)
      (should (string-match-p "\\* Test Genre" (buffer-string)))
      (should (string-match-p ":ID: test-id" (buffer-string)))
      (should (string-match-p ":DISAMBIGUATION: test genre" (buffer-string))))))

(provide 'musicbrainz-org-test)
