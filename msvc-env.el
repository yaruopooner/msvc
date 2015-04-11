;;; msvc-env.el --- MSVC basic environment -*- lexical-binding: t; -*-

;;; last updated : 2015/04/12.02:55:14

;; Copyright (C) 2013-2015  yaruopooner
;; 
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
(defvar msvc-env-product-detected-p nil)
(defvar msvc-env--product-version nil)
(defvar msvc-env-default-use-version nil
  "MSVC default use version string
`2015'
`2013'
`2012'
`2010'
`2008'
If the value is nil, latest version will be used.
")

(defconst msvc-env--product-details '((:version "2015" :env-var "VS140COMNTOOLS")
                                      (:version "2013" :env-var "VS120COMNTOOLS")
                                      (:version "2012" :env-var "VS110COMNTOOLS")
                                      (:version "2010" :env-var "VS100COMNTOOLS")
                                      (:version "2008" :env-var "VS90COMNTOOLS")))


;; Microsoft Visual C/C++ Toolset Shell List &  Toolset type
(defvar msvc-env--toolset-shells nil)
(defvar msvc-env-default-use-toolset "x86_amd64"
  "MSVC toolset shell argument string
toolset-name   : support product
`x86'          : (2015/2013/2012/2010)
`x86_amd64'    : (2015/2013/2012/2010)
`x86_arm'      : (2015/2013/2012)
`x86_ia64'     : (2010)
`amd64'        : (2015/2013/2012/2010)
`amd64_x86'    : (2015/2013)
`amd64_arm'    : (2015/2013)
`arm'          : (2015/2013/2012)
`ia64'         : (2010)
see this page.
https://msdn.microsoft.com/library/f2ccy3wt.aspx
")


(defconst msvc-env--package-directory (file-name-directory (or (buffer-file-name) load-file-name)) "Package Installed Directory.")


;; invoke program
(defconst msvc-env--invoke-command "cmd")


;; MSBuild invoke shell
(defconst msvc-env--shell-msbuild-name "invoke-msbuild.bat")
(defconst msvc-env--shell-msbuild (expand-file-name msvc-env--shell-msbuild-name msvc-env--package-directory))



;; for lexical-binding 
(defmacro msvc-env--add-to-list (list-var element &optional append)
  `(if (member ,element ,list-var)
       ,list-var
     (if ,append
         (setq ,list-var (append ,list-var (list ,element)))
       (push ,element ,list-var))))



(defun msvc-env--detect-product ()
  (cl-dolist (detail msvc-env--product-details)
    (let ((version (plist-get detail :version))
          (path (getenv (plist-get detail :env-var))))
      (when path
        (setq path (expand-file-name "../../VC/vcvarsall.bat" path))
        (when (file-exists-p path)
          (setq msvc-env-product-detected-p t)
          (add-to-list 'msvc-env--product-version version t)
          (setq msvc-env--toolset-shells (plist-put msvc-env--toolset-shells (intern (concat ":" version)) path))))))
  msvc-env-product-detected-p)



;; utilities
(defun msvc-env--normalize-path (paths safe-path)
  (unless (listp paths)
    (setq paths (list paths)))
  (mapcar (lambda (path)
            (if (file-name-absolute-p path)
                path
              (expand-file-name path safe-path))) paths))


(defun msvc-env--convert-to-posix-style-path (paths)
  (unless (listp paths)
    (setq paths (list paths)))
  (mapcar (lambda (path)
            (replace-regexp-in-string "^\\([a-zA-Z]\\):"
                                      (lambda (match) (downcase (format "/cygdrive/%s" (match-string 1 match))))
                                      path
                                      t)) paths))



;; for MSBuild property flag
(defun msvc-env--create-msb-flags (switch parameters)
  (let* ((flags switch))
    (cl-dolist (parameter parameters)
      (setq flags (concat flags (format (car parameter) (cdr parameter)) ";")))
    ;; 末尾にparameter-separator ';' があるとエラーになる
    ;; 削除して後続スイッチ用に空白で置き換える
    (replace-regexp-in-string ";$" " " flags)))


(defun msvc-env--create-msb-rsp-file (msb-rsp-file msb-target-file msb-flags)
  (with-temp-file msb-rsp-file
    (insert (format "%S" msb-target-file) "\n")
    (cl-dolist (flag msb-flags)
      (insert flag "\n")))
  msb-rsp-file)


(defun msvc-env--remove-msb-rsp-files (path)
  (let* ((files (directory-files path t "\.rsp\.msvc$")))
    (cl-dolist (file files)
      (delete-file file))))



;; log-file は cmd から type されるのでパスセパレーターが \ である必要がある
(defun msvc-env--build-msb-command-args (version toolset msb-rsp-file log-file)
  (interactive)
  (list
   "/c"
   msvc-env--shell-msbuild
   (plist-get msvc-env--toolset-shells (intern (concat ":" version)))
   ;; (symbol-name toolset)
   toolset
   msb-rsp-file
   (replace-regexp-in-string "/" "\\\\" log-file)))



;; setup functions
(defun msvc-env--clear-variables ()
  (setq msvc-env-product-detected-p nil)
  (setq msvc-env--product-version nil)
  (setq msvc-env-default-use-version nil)
  (setq msvc-env--toolset-shells nil)
  (setq msvc-env-default-use-toolset nil))


(cl-defun msvc-env--initialize ()
  (if (eq system-type 'windows-nt)
      (when (and (boundp 'w32-pipe-read-delay) (> w32-pipe-read-delay 0))
        (display-warning 'msvc "Please set the appropriate value for `w32-pipe-read-delay'. Because a pipe delay value is large value. Ideal value is 0. see help of `w32-pipe-read-delay'."))
    (display-warning 'msvc "This environment is not a Microsoft Windows."))

  (unless (msvc-env--detect-product)
    (display-warning 'msvc "msvc-env : product not detected : Microsoft Visual Studio")
    (cl-return-from msvc-env--initialize nil))

  ;; default is latest product
  (setq msvc-env-default-use-version (nth 0 msvc-env--product-version))

  (message "msvc-env : product detected : Microsoft Visual Studio %s" msvc-env--product-version)
  t)






(provide 'msvc-env)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; msvc-env.el ends here
