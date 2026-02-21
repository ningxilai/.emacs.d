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
                             fanyi-longman-provider)))

(setup (:elpaca gt :host github :repo "lorniu/gt.el")
  (:custom gt-langs '(en zh)
           gt-default-translator '(gt-translator :engines (gt-youdao-dict-engine))))


(provide 'tool-fanyi)
;; ends here.
