;;;  -*- lexical-binding: t; -*-

;; fanyi/gt

(setup (:elpaca fanyi)
  (:custom fanyi-providers '(;; 海词
                             fanyi-haici-provider
                             ;; 有道同义词词典
                             fanyi-youdao-thesaurus-provider
                             ;; Etymonline
                             fanyi-etymon-provider
                             ;; Longman
                             fanyi-longman-provider
                             ;; English-English dictionary
                             fanyi-etymon-provider
                             fanyi-longman-provider))
  (with-eval-after-load 'org
    (add-to-list 'org-modules 'ol-fanyi))
  (:when-loaded
    (require 'fanyi-longman)
    (require 'fanyi-etymon))
  (:with-map help-map (:bind "M-d" fanyi-dwim)))

(setup (:elpaca gt :host github :repo "lorniu/gt.el")
  (:require gt-engine-bing
            gt-engine-chatgpt
            gt-engine-deepl
            gt-engine-echo
            gt-engine-google-rpc
            gt-engine-google
            gt-engine-libre
            gt-engine-osxdict
            gt-engine-stardict
            gt-engine-youdao)
  (:custom gt-default-translator (gt-translator
                                  :taker (gt-taker :text 'buffer :pick 'paragraph)
                                  :engines (gt-bing-engine)
                                  :render (gt-buffer-render)))
  (:option gt-langs '(en zh)))

(provide 'tool-fanyi)
;; ends here.
