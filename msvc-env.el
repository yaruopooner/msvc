;;; msvc-env.el --- MSVC basic environment -*- lexical-binding: t; -*-

;;; last updated : 2019/04/11.20:56:21

;; Copyright (C) 2013-2019  yaruopooner
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
(defvar msvc-env--product-names nil)
(defvar msvc-env--product-versions nil)
(defvar msvc-env-default-use-product-name nil
  "MSVC default use product name
Specifiable type [String]
`2019'
`2017'
`2015'
`2013'
`2012'
`2010'
`2008'
If the value is nil, latest version will be used.
")

(defconst msvc-env--product-details '((:name "2019" :version "16" :env-var "VS160COMNTOOLS" :vcvars-rpath "VC/Auxiliary/Build/vcvarsall.bat")
                                      (:name "2017" :version "15" :env-var "VS150COMNTOOLS" :vcvars-rpath "VC/Auxiliary/Build/vcvarsall.bat")
                                      (:name "2015" :version "14" :env-var "VS140COMNTOOLS" :vcvars-rpath "VC/vcvarsall.bat")
                                      (:name "2013" :version "12" :env-var "VS120COMNTOOLS" :vcvars-rpath "VC/vcvarsall.bat")
                                      (:name "2012" :version "11" :env-var "VS110COMNTOOLS" :vcvars-rpath "VC/vcvarsall.bat")
                                      (:name "2010" :version "10" :env-var "VS100COMNTOOLS" :vcvars-rpath "VC/vcvarsall.bat")
                                      (:name "2008" :version "9" :env-var "VS90COMNTOOLS" :vcvars-rpath "VC/vcvarsall.bat")))

(defcustom msvc-env--vswhere-executable "c:/Program Files (x86)/Microsoft Visual Studio/Installer/vswhere.exe"
  "Location of vswhere executable."
  :group 'msvc-env
  :type 'file)

(defconst msvc-env--product-query-details '((:platform 64 :sub-key "\"%s\\SOFTWARE\\Wow6432Node\\Microsoft\\VisualStudio\\SxS\\VS7\"" :root-keys ("HKLM" "HKCU"))
                                            (:platform 32 :sub-key "\"%s\\SOFTWARE\\Microsoft\\VisualStudio\\SxS\\VS7\"" :root-keys ("HKLM" "HKCU"))))

(defvar msvc-env--product-info-source 'vswhere
  "MSVC product information source
Specifiable type [Symbol]
`vswhere'        : search from vswhere
`registry'       : search from Windows registry
`env-var'        : search from environment variable
")



;; Microsoft Visual C/C++ Toolset Shell List &  Toolset type
(defvar msvc-env--toolset-shells nil)
(defvar msvc-env-default-use-toolset "x86_amd64"
  "MSVC toolset shell argument
Specifiable type [String]
toolset-name   : support product
`x86'          : (2019/2017/2015/2013/2012/2010)
`x86_x64'      : (2019/2017)
`x86_amd64'    : (2019/2017/2015/2013/2012/2010)
`x86_arm'      : (2019/2017/2015/2013/2012)
`x86_ia64'     : (2010)
`x64'          : (2019/2017)
`x64_x86'      : (2019/2017)
`x64_arm'      : (2019/2017)
`amd64'        : (2019/2017/2015/2013/2012/2010)
`amd64_x86'    : (2019/2017/2015/2013)
`amd64_arm'    : (2019/2017/2015/2013)
`arm'          : (2019/2017/2015/2013/2012)
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



(defun msvc-env--query-product-at-vswhere ()
  (when (file-exists-p msvc-env--vswhere-executable)
    (let* ((results (make-hash-table :test 'equal))
           (cmd-result (shell-command-to-string (mapconcat #'shell-quote-argument (list msvc-env--vswhere-executable "-legacy") " ")))
           (entries (split-string cmd-result "^\n" t))
           (header-pattern "^instanceId:")
           (major-version-pattern "^installationVersion:\\s-\\([0-9]+\\).+$")
           (path-pattern "^installationPath:\\s-+\\(.+\\)$"))
      (when entries
        (cl-dolist (entry entries)
          (when (string-match header-pattern entry)
            (let (version
                  path)
              (when (string-match major-version-pattern entry)
                (setq version (match-string-no-properties 1 entry)))
              (when (string-match path-pattern entry)
                (setq path (match-string-no-properties 1 entry)))

              (when (and version path)
                ;; (print (format "%s : %s" version path))
                (puthash version path results))))))
      results)))


(defun msvc-env--detect-product-from-vswhere ()
  (let ((query-results (msvc-env--query-product-at-vswhere)))
    (cl-dolist (detail msvc-env--product-details)
      (let* ((name (plist-get detail :name))
             (version (plist-get detail :version))
             (vcvars-rpath (plist-get detail :vcvars-rpath))
             (path (gethash version query-results)))
        (when path
          (setq path (expand-file-name vcvars-rpath path))
          (when (file-exists-p path)
            (setq msvc-env-product-detected-p t)
            (add-to-list 'msvc-env--product-names name t)
            (add-to-list 'msvc-env--product-versions version t)
            (setq msvc-env--toolset-shells (plist-put msvc-env--toolset-shells (intern (concat ":" name)) path)))))))
  msvc-env-product-detected-p)



(cl-defun msvc-env--query-product-at-registry ()
  (let ((results (make-hash-table :test 'equal))
        (pattern "^\\s-+\\([0-9]+\\)\\S-+\\s-+REG_SZ\\s-+\\(.+\\)$"))
    (cl-dolist (detail msvc-env--product-query-details)
      (let* ((sub-key (plist-get detail :sub-key))
             (root-keys (plist-get detail :root-keys)))
        (cl-dolist (root-key root-keys)
          (let* ((full-key-name (format sub-key root-key))
                 (cmd-result (shell-command-to-string (concat "reg query " full-key-name)))
                 (entries (cdr (split-string cmd-result "\n" t))) ; split after excluded 1st element
                 version
                 path)
            ;; (print full-key-name)
            ;; (print cmd-result)
            ;; (print (length entries))
            (when entries
              (cl-dolist (entry entries)
                (string-match pattern entry)
                (setq version (match-string-no-properties 1 entry)
                      path (match-string-no-properties 2 entry))
                (puthash version path results))
              ;; (print (format "%s : %s" version path))
              (cl-return-from msvc-env--query-product-at-registry results))))))
    results))


(defun msvc-env--detect-product-from-registry ()
  (let ((query-results (msvc-env--query-product-at-registry)))
    (cl-dolist (detail msvc-env--product-details)
      (let* ((name (plist-get detail :name))
             (version (plist-get detail :version))
             (vcvars-rpath (plist-get detail :vcvars-rpath))
             (path (gethash version query-results)))
        (when path
          (setq path (expand-file-name vcvars-rpath path))
          (when (file-exists-p path)
            (setq msvc-env-product-detected-p t)
            (add-to-list 'msvc-env--product-names name t)
            (add-to-list 'msvc-env--product-versions version t)
            (setq msvc-env--toolset-shells (plist-put msvc-env--toolset-shells (intern (concat ":" name)) path)))))))
  msvc-env-product-detected-p)


(defun msvc-env--detect-product-from-env-var ()
  (cl-dolist (detail msvc-env--product-details)
    (let ((name (plist-get detail :name))
          (version (plist-get detail :version))
          (vcvars-rpath (plist-get detail :vcvars-rpath))
          (path (getenv (plist-get detail :env-var))))
      (when path
        (setq path (expand-file-name (concat "../../" vcvars-rpath) path))
        (when (file-exists-p path)
          (setq msvc-env-product-detected-p t)
          (add-to-list 'msvc-env--product-names name t)
          (add-to-list 'msvc-env--product-versions version t)
          (setq msvc-env--toolset-shells (plist-put msvc-env--toolset-shells (intern (concat ":" name)) path))))))
  msvc-env-product-detected-p)


(defun msvc-env--detect-product ()
  (cl-case msvc-env--product-info-source
    (vswhere
     (msvc-env--detect-product-from-vswhere))
    (registry
     (msvc-env--detect-product-from-registry))
    (env-var
     (msvc-env--detect-product-from-env-var))))


(defun msvc-env--query-detected-product-name-p (name)
  (member name msvc-env--product-names))

(defun msvc-env--query-detected-product-version-p (version)
  (member version msvc-env--product-versions))





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
(defun msvc-env--build-msb-command-args (product-name toolset msb-rsp-file log-file)
  (interactive)
  (list
   "/c"
   msvc-env--shell-msbuild
   (plist-get msvc-env--toolset-shells (intern (concat ":" product-name)))
   ;; (symbol-name toolset)
   toolset
   msb-rsp-file
   (replace-regexp-in-string "/" "\\\\" log-file)))



;; setup functions
(defun msvc-env--clear-variables ()
  (setq msvc-env-product-detected-p nil)
  (setq msvc-env--product-names nil)
  (setq msvc-env--product-versions nil)
  (setq msvc-env-default-use-product-name nil)
  (setq msvc-env--toolset-shells nil)
  (setq msvc-env-default-use-toolset nil))


(cl-defun msvc-env--initialize ()
  (unless (eq system-type 'windows-nt)
    ;; (display-warning 'msvc "msvc-env : This environment is not a Microsoft Windows.")
    (message "msvc-env : This environment is not a Microsoft Windows.")
    (cl-return-from msvc-env--initialize nil))

  (when (and (boundp 'w32-pipe-read-delay) (> w32-pipe-read-delay 0))
    (display-warning 'msvc "Please set the appropriate value for `w32-pipe-read-delay'. Because a pipe delay value is large value. Ideal value is 0. see help of `w32-pipe-read-delay'."))

  (unless (msvc-env--detect-product)
    (display-warning 'msvc "msvc-env : product not detected : Microsoft Visual Studio")
    (cl-return-from msvc-env--initialize nil))

  ;; default is latest product
  (setq msvc-env-default-use-product-name (nth 0 msvc-env--product-names))

  (message "msvc-env : product detected : Microsoft Visual Studio %s" msvc-env--product-names)
  t)






(provide 'msvc-env)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; msvc-env.el ends here
