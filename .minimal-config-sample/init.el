;;; -*- Mode: Emacs-Lisp ; Coding: utf-8-unix -*-
;;; last updated : 2013/09/24.01:12:37


;;==============================================================================
;; msvc minimal configuration sample
;;==============================================================================


(add-to-list 'load-path "~/.emacs.d/")


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
