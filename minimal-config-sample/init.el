;;; -*- Mode: Emacs-Lisp ; Coding: utf-8-unix -*-
;;; last updated : 2015/02/25.03:23:25


;;==============================================================================
;; msvc minimal configuration sample
;;==============================================================================


(add-to-list 'load-path user-emacs-directory)


(setq explicit-shell-file-name "bash")
(setq shell-file-name "bash")
(setq shell-command-switch "-c")


(require 'package)

(add-to-list 'package-archives '("melpa"     . "http://melpa.milkbox.net/packages/"))

;; signature test pass(for gnu)
(setq package-check-signature nil)

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
                                         ac-clang
                                         ))


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
