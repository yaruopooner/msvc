;;; -*- Mode: Emacs-Lisp ; Coding: utf-8-unix -*-
;;; last updated : 2014/09/25.03:22:29


;;==============================================================================
;; msvc minimal configuration sample
;;==============================================================================


(add-to-list 'load-path user-emacs-directory)


(setq explicit-shell-file-name "bash")
(setq shell-file-name "bash")
(setq shell-command-switch "-c")


(require 'package)

(add-to-list 'package-archives '("melpa"     . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)


(defun package-installer (packages)
  (let (targets)
    (dolist (name packages)
      (unless (package-installed-p name)
        (message "Package: %s : not installed. " name)
        (add-to-list 'targets name t)))

    (when targets
      (package-refresh-contents)

      (dolist (name targets)
        (package-install name)))))


(defconst msvc-dependency-package-list '(auto-complete
                                         fuzzy
                                         yasnippet
                                         dropdown-list))


(package-installer msvc-dependency-package-list)


;; built-in packages
(require 'cedet.config)
(require 'flymake.config)

;; dependency packages
(require 'setup.auto-complete)
(require 'setup.yasnippet)

(require 'setup.ac-clang)
(require 'setup.msvc)



;; EOF
