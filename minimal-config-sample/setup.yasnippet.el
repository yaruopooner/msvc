;;; -*- mode: emacs-lisp ; coding: utf-8-unix -*-
;;; last updated : 2015/04/13.02:12:16


;;==================================================================================================
;; yasnippet setup                                                                                      
;; 
;; jp translated doc
;; http://yasnippet-doc-jp.googlecode.com/svn/trunk/doc-jp/index.html
;;==================================================================================================


;;------------------------------------------------------------------------------
;; prepare variable setting                                                           
;;------------------------------------------------------------------------------


;; load path addition
(add-to-list 'load-path (locate-user-emacs-file "yasnippet/"))




;;------------------------------------------------------------------------------
;; load                                                                         
;;------------------------------------------------------------------------------


;; Load Module
(require 'yasnippet)


;; (require 'dropdown-list)




;;------------------------------------------------------------------------------
;; basic setting                                                       
;;------------------------------------------------------------------------------


;; オフィシャルのスニペットディレクトリを登録させない
;; (setq yas-snippet-dirs `(,(expand-file-name "snippets" de:path-dot-emacs-root)))

;; 同一名を持つ key を展開しようとすると x-window のドロップダウンGUIが表示されるのを削除
;; C-n,C-p で操作できるCUIドロップダウンリストにする
(setq yas-prompt-functions '(
                             ;; yas-dropdown-prompt
                             yas-ido-prompt
                             yas-completing-prompt))


;; default snippets directory
;; (yas-load-directory (expand-file-name "yasnippet/snippets" de:path-plugins))

;; user custom snippets directory
;; (setq yas-snippet-dirs `(,(expand-file-name "yasnippet/snippets" de:path-plugins) ,(expand-file-name "snippets" de:path-dot-emacs-root)))
;; (setq yas-snippet-dirs (append yas-snippet-dirs `(,(expand-file-name "yasnippet" de:path-dot-emacs-root))))
;; (setq yas-snippet-dirs `(,(expand-file-name "snippets" de:path-dot-emacs-root)))




;; 上記設定に基づいて初期化
(yas-initialize)






(provide 'setup.yasnippet)
;;--------------------------------------------------------------------------------------------------
;; EOF
