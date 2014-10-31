;;; -*- mode: emacs-lisp ; coding: utf-8-unix -*-
;;; last updated : 2013/09/24.01:11:40


;;==================================================================================================
;; ac-clang setup                                                                                      
;;==================================================================================================


;;------------------------------------------------------------------------------
;; prepare variable setting                                                           
;;------------------------------------------------------------------------------


;; load path addition
(add-to-list 'load-path (locate-user-emacs-file "ac-clang/"))



;;------------------------------------------------------------------------------
;; load                                                                         
;;------------------------------------------------------------------------------

;; Load Module
(require 'ac-clang)





;;------------------------------------------------------------------------------
;; basic setting                                                       
;;------------------------------------------------------------------------------

(ac-clang:initialize)






(provide 'setup.ac-clang)
;;--------------------------------------------------------------------------------------------------
;; EOF