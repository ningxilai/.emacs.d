;;;  -*- lexical-binding: t; -*-

;;; Code:

;; MusicBrainz

(setup (:elpaca pdd))

(setup (:elpaca musicbrainz :repo "ningxilai/musicbrainz" :files ( :defaults "musicbrainz-url.el" "musicbrainz.el" "musicbrainz-org.el" (:exclude "README.md" "LICENSE"))))

(provide 'tool-musicbrainz)

;; ends here.
