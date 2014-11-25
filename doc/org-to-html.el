;;; -*- mode: emacs-lisp ; coding: utf-8-unix -*-
;;; last updated : 2014/11/25.03:23:35


;;--------------------------------------------------------------------------------------------------
;; org document convert to html project file
;;--------------------------------------------------------------------------------------------------

(require 'ox-publish)


(let* ((link "<link rel=\"stylesheet\" type=\"text/css\" href=\"./common.css\">")
       (org-html-style-default (concat org-html-style-default link)))
  ;; 全ファイル強制更新
  (shell-command "find . -type f -regex \".*\\.\\(org\\)$\" -print0 | xargs -0 -e touch")

                                        ;  (setq style (format "<link rel=\"stylesheet\" type=\"text/css\" href=\"%s\">" (expand-file-name "./org-html-data/css/common.css" default-directory)))

  (setq org-publish-project-alist
        `(("MSVC-document-establish"
           :publishing-function org-html-publish-to-html
           :base-directory ,(expand-file-name "./" default-directory)
           :base-extension "org$"
           :publishing-directory ,(expand-file-name "./" default-directory)
           ;;         :publishing-directory "./html/"
           :recursive t
           :index-title "Microsoft Visual C/C++ Mode User Manual"
           :index-style list
           ;; :style "<link rel=\"stylesheet\" type=\"text/css\" href=\"html-data/css/common.css\">"
           :language "en"
           :author ""
           :author-info nil
           :email "@"
           :email-info nil
           :time-stamp-file t
           :table-of-contents t
           :tables t
           :preserve-breaks t
           :sub-superscript nil
           ;; :style ,(concat
           ;;          "<link rel=\"stylesheet\" type=\"text/css\" href=\"./common.css\">"
           ;;          ""
           ;;          )
           )))

  ;;(org-publish-current-project)
  ;;(org-publish-projects "MSVC-document-establish")
  (org-publish "MSVC-document-establish"))



;;--------------------------------------------------------------------------------------------------
;; EOF
