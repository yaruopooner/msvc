;;; -*- Mode: Emacs-Lisp ; Coding: utf-8-unix -*-
;;; last updated : 2014/03/07.13:16:58


;;==============================================================================
;; msvc minimal configuration sample
;;==============================================================================


(add-to-list 'load-path user-emacs-directory)


(setq explicit-shell-file-name "bash")
(setq shell-file-name "bash")
(setq shell-command-switch "-c")


;; built-in packages
(require 'cedet.config)
(require 'flymake.config)

;; plug-in packages
(require 'setup.auto-complete)
(require 'setup.yasnippet)

(require 'setup.ac-clang)
(require 'setup.msvc)



;; EOF
