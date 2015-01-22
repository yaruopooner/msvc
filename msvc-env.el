;;; -*- mode: emacs-lisp ; coding: utf-8-unix ; lexical-binding: nil -*-
;;; last updated : 2015/01/22.23:38:48

;; Copyright (C) 2013-2015  yaruopooner
;; 
;; Author          : yaruopooner [https://github.com/yaruopooner]
;; Keywords        : languages, completion, syntax check, mode, convenience

;; This file is part of MSVC.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:



(require 'cl-lib)


;; Microsoft Visual C/C++ Product information
(defvar msvc-env:product-detected-p nil)
(defvar msvc-env:product-version nil)
(defvar msvc-env:default-use-version nil)

(defconst msvc-env:product-details '((:version "2015" :env-var "VS130COMNTOOLS")
                                     (:version "2013" :env-var "VS120COMNTOOLS")
                                     (:version "2012" :env-var "VS110COMNTOOLS")
                                     (:version "2010" :env-var "VS100COMNTOOLS")
                                     (:version "2008" :env-var "VS90COMNTOOLS")))


;; Microsoft Visual C/C++ Command Prompt 
(defvar msvc-env:shell-msvc nil)
(defvar msvc-env:shell-msvc-arg 'amd64
  " MSVC shell argument symbols
`x86'          : x86
`amd64'        : amd64
`x64'          : same `amd64'
`ia64'         : ia64
`x86_amd64'    : x86_amd64
`x86_ia64'     : x86_ia64
")


(defconst msvc-env:package-directory (file-name-directory (or (buffer-file-name) load-file-name)) "Package Installed Directory.")


;; invoke program
(defconst msvc-env:invoke-command "cmd")


;; MSBuild invoke shell
(defconst msvc-env:shell-msbuild-name "invoke-msbuild.bat")
(defconst msvc-env:shell-msbuild (expand-file-name msvc-env:shell-msbuild-name msvc-env:package-directory))




(defun msvc-env:detect-product ()
  (cl-dolist (detail msvc-env:product-details)
    (let ((version (plist-get detail :version))
          (path (getenv (plist-get detail :env-var))))
      (when path
        (setq path (expand-file-name "../../VC/vcvarsall.bat" path))
        (when (file-exists-p path)
          (setq msvc-env:product-detected-p t)
          (add-to-list 'msvc-env:product-version version t)
          (setq msvc-env:shell-msvc (plist-put msvc-env:shell-msvc (intern (concat ":" version)) path))))))
  msvc-env:product-detected-p)



;; utilities
(defun msvc-env:normalize-path (paths safe-path)
  (let* (result
         (converter '(lambda (path safe-path)
                       (if (file-name-absolute-p path)
                           path
                         (expand-file-name path safe-path)))))

    (if (listp paths)
        (cl-dolist (path paths)
          (add-to-list 'result (funcall converter path safe-path) t))
      (setq result (funcall converter paths safe-path)))
    result))


(defun msvc-env:convert-to-posix-style-path (paths)
  (let* (result
         (converter '(lambda (path)
                       (replace-regexp-in-string "^\\([a-zA-Z]\\):" 
                                                 (lambda (match) (downcase (format "/cygdrive/%s" (match-string 1 path))))
                                                 path
                                                 t))))
    (if (listp paths)
        (cl-dolist (path paths)
          (add-to-list 'result (funcall converter path) t))
      (setq result (funcall converter paths)))
    result))



;; for MSBuild property flag
(defun msvc-env:create-msb-flags (switch parameters)
  (let* ((flags switch))
    (cl-dolist (parameter parameters)
      (setq flags (concat flags (format (car parameter) (cdr parameter)) ";")))
    ;; 末尾にparameter-separator ';' があるとエラーになる
    ;; 削除して後続スイッチ用に空白で置き換える
    (replace-regexp-in-string ";$" " " flags)))


(defun msvc-env:create-msb-rsp-file (msb-rsp-file msb-target-file msb-flags)
  (with-temp-file msb-rsp-file
    (insert msb-target-file "\n")
    (cl-dolist (flag msb-flags)
      (insert flag "\n")))
  msb-rsp-file)


(defun msvc-env:remove-msb-rsp-files (path)
  (let* ((files (directory-files path t "\.rsp$")))
    (cl-dolist (file files)
      (delete-file file))))



;; log-file は cmd から type されるのでパスセパレーターが \ である必要がある
(defun msvc-env:build-msb-command-args (version msb-rsp-file log-file)
  (interactive)
  (list
   "/c"
   msvc-env:shell-msbuild
   (plist-get msvc-env:shell-msvc (intern (concat ":" version)))
   (symbol-name msvc-env:shell-msvc-arg)
   msb-rsp-file
   (replace-regexp-in-string "/" "\\\\" log-file)))



;; setup functions
(defun msvc-env:clear-variables ()
  (setq msvc-env:product-detected-p nil)
  (setq msvc-env:product-version nil)
  (setq msvc-env:default-use-version nil)
  (setq msvc-env:shell-msvc nil)
  (setq msvc-env:shell-msvc-arg nil))


(cl-defun msvc-env:initialize ()
  (if (eq system-type 'windows-nt)
      (when (and (boundp 'w32-pipe-read-delay) (> w32-pipe-read-delay 0))
        (display-warning 'msvc "Please set the appropriate value for `w32-pipe-read-delay'. Because a pipe delay value is large value."))
    
    (display-warning 'msvc "This environment is not a Microsoft Windows."))

  (unless (msvc-env:detect-product)
    (display-warning 'msvc "msvc-env : product not detected : Microsoft Visual Studio")
    (cl-return-from msvc-env:initialize nil))

  ;; default is latest product
  (setq msvc-env:default-use-version (nth 0 msvc-env:product-version))

  (message "msvc-env : product detected : Microsoft Visual Studio %s" msvc-env:product-version)
  t)






(provide 'msvc-env)
;;--------------------------------------------------------------------------------------------------
;; EOF
