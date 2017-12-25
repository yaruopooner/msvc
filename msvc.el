;;; msvc.el --- Microsoft Visual C/C++ mode -*- lexical-binding: t; -*-

;;; last updated : 2017/12/26.00:32:01


;; Copyright (C) 2013-2017  yaruopooner
;; 
;; Author: yaruopooner [https://github.com/yaruopooner]
;; URL: https://github.com/yaruopooner/msvc
;; Keywords: languages, completion, syntax check, mode, intellisense
;; Version: 1.3.7
;; Package-Requires: ((emacs "24") (cl-lib "0.5") (cedet "1.0") (ac-clang "2.0.0"))

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


;;; Commentary:
;; 
;; * INTRODUCTION:
;;   msvc parse the project file or the solution file of Visual Studio.
;;   msvc-mode becomes effective when you open the file belonging to a project.
;;   This mode provides the following features.
;; 
;; * FEATURES:
;;   - Visual Studio project file manager
;;     backend: msvc + ede
;;   - coexistence of different versions
;;     2017/2015/2013/2012/2010
;;   - code completion (auto / manual)
;;     backend: ac-clang
;;     ac-sources: ac-clang or semantic
;;   - syntax check (auto / manual)
;;     backend: msbuild or ac-clang
;;   - jump to declaration or definition. return from jumped location.
;;     backend: ac-clang
;;   - jump to include file. return from jumped include file.
;;     backend: semantic
;;   - build Solution or Project on Emacs
;;     backend: msbuild
;;   - jump to error buffer from build report
;;     look like a grep buffer
;;   - launch Visual Studio from Solution or Project
;;     backend: Windows file association
;; 
;; * REQUIRED ENVIRONMENT
;;   - Microsoft Windows 64/32bit
;;     10/8/7/Vista
;;   - Microsoft Visual Studio Community/Professional/Enterprise
;;     2017/2015/2013/2012/2010
;;   - Shell 64/32bit
;;     CYGWIN/MSYS/CMD(cmdproxy)
;;     CYGWIN's bash recommended
;; 
;; * REQUIRED EMACS LISP PACKAGE
;;   - ac-clang
;;     ac-clang provide code completion and arguments expand.
;;     This program consists of the client(elisp) and server(binary).
;;     The server is executable file, and a self-build is necessary.
;;     The server achieve code completion using libclang of LLVM.
;;     For ac-clang information and detailed usage, 
;;     refer to header document of ac-clang.el or the project page.
;;     [https://github.com/yaruopooner/ac-clang]
;;  
;; * TESTED SDK:
;;   completion test, syntax check test
;;   - Windows SDK 7.1/7.0A
;;   - Direct X SDK(June 2010)
;;   - STL,std::tr1
;; 


;; Usage:
;; * DETAILED MANUAL:
;;   For more information and detailed usage, refer to the project page:
;;   [https://github.com/yaruopooner/msvc]
;; 
;; * INSTALLATION:
;;   If you use auto-complete by ac-clang, you will need an external program.
;;   It's necessary to download or self-build the external program.
;; 
;;   - download
;;     [https://github.com/yaruopooner/ac-clang/releases]
;;   - self-build
;;     [https://github.com/yaruopooner/llvm-build-shells]
;; 
;; * SETUP:
;;   (require 'msvc)
;; 
;;   (setq w32-pipe-read-delay 0)
;;   (when (msvc-initialize)
;;     (msvc-flags-load-db :parsing-buffer-delete-p t)
;;     (add-hook 'c-mode-common-hook #'msvc-mode-on t))
;; 
;;   For more samples, please refer the following URL.
;;   [https://github.com/yaruopooner/msvc/tree/master/minimal-config-sample]
;;   If you want to test a sample, please checkout.
;;   $ git clone https://github.com/yaruopooner/msvc.git
;;   Look at the file in 'msvc/minimal-config-sample' directory.
;; 
;; * REGISTRATION OF PROJECT OR SOLUTION
;;   (msvc-activate-projects-after-parse :solution-file "d:/DirectXSamples/SubD11/SubD11_2010.sln"
;;                                       :project-file "d:/DirectXSamples/SubD11/SubD11_2010.vcxproj"
;;                                       :platform "x64"
;;                                       :configuration "Release" 
;;                                       :version "2013" 
;;                                       :toolset 'x86_amd64
;;                                       :md5-name-p nil
;;                                       :force-parse-p nil
;;                                       :allow-cedet-p t
;;                                       :allow-ac-clang-p t
;;                                       :allow-flymake-p t
;;                                       :cedet-root-path "d:/DirectXSamples/SubD11"
;;                                       :cedet-spp-table nil
;;                                       :flymake-manually-p nil)
;; 
;;   When the project is active , buffer with the appropriate project name will be created.
;;   The project buffer name is based on the following format.
;;   *MSVC Project <`db-name`>*
;;   msvc-mode will be applied automatically when source code belonging to the project has been opened.
;;   msvc-mode has been applied buffer in the mode line MSVC`version`[platform|configuration] and will be displayed.
;;   You can activate a lot of projects.
;; 
;; * REQUIRED PROPERTIES
;;   - :solution-file
;;      If you don't use :project-file,
;;      all projects that are included in the Solution is parsed, it will be activated.
;;   - :project-file
;;      If you don't use :solution-file,
;;      Only the specified project is parsed, it will be activated.
;;      Feature associated with the Solution you will not be able to run.
;;   - :solution-file & :project-file
;;      You have the same effect as if you had specified a :solution-file only,
;;      but only a designated project will be parsed and activated.
;;      In the case that there are many projects in solution, this way is recommended.
;;   - :platform
;;      Must be a platform that exists in the project file.
;;   - :configuration
;;      Must be a configuration that exists in the project file.
;; 
;; * OPTIONAL PROPERTIES
;;   - :version
;;     Specifies the version of Visual Studio to be used.
;;     If you do not specify or nil used, the value used is `msvc-env-default-use-version'.
;;   - :toolset
;;     Specifies the toolset of Visual Studio to be used.
;;     If you do not specify or nil used, the value used is `msvc-env-default-use-toolset'.
;;   - :md5-name-p
;;     nil recommended.
;;     If value is t, generate a database directory and file name by MD5.
;;     This attribute solves a database absolute path longer than MAX_PATH(260 bytes).
;;   - :force-parse-p
;;     nil recommended. force parse and activate.
;;     It is primarily for debugging applications.
;;   - :allow-cedet-p
;;     t recommended. use the CEDET. 
;;     In the case of nil you will not be able to use the jump to include files.
;;   - :allow-ac-clang-p
;;     t recommended. 
;;     If value is t, use the ac-clang.
;;     If value is nil, use the semantic.
;;   - :allow-flymake-p
;;     t recommended. use the flymake. syntax check by MSBuild.
;;   - :cedet-root-path
;;     It is referenced only when the allow-cedet-p t.
;;     You specify the CEDET ede project base directory *.ede.
;;     File is generated in the specified directory.
;;     It is most likely not a problem in the directory where the project file is located.
;;     However, if the location of the source code is not a project file placement directory
;;     at the same level or descendants you will need to be careful.  
;;     In this case you will need to specify a common parent directory such that the same hierarchy or descendants.
;;   - :cedet-spp-table
;;     nil recommended. 
;;     It is referenced only when the allow-cedet-p t.
;;     Word associative table that you want to replace when the semantic is to parse the source.
;;     It is a table replacing define which cannot parsed a semantic.
;;     If semantic.cache can not be created successfully requires this setting.
;;     The following description sample
;;       :cedet-spp-table '(
;;                          ("ALIGN"              . "")
;;                          ("FORCE_INLINE"       . "")
;;                          ("NO_INLINE"          . "")
;;                          ("THREAD_LOCAL"       . "")
;;                          ("DLL_IMPORT"         . "")
;;                          ("DLL_EXPORT"         . "")
;;                          ("RESTRICT"           . ""))
;;     For details, refer to CEDET manual.
;;   - :flymake-back-end
;;     nil recommended. 
;;     specifiable : `msbuild' `clang' `nil'
;;     refer to `msvc--flymake-back-end'
;;   - :flymake-manually-p
;;     nil recommended. 
;;     If value is t, manual syntax check only.
;;   - :flymake-manually-back-end
;;     nil recommended.
;;     specifiable : `msbuild' `clang' `nil'
;;     refer to `msvc--flymake-manually-back-end'
;; 
;; * DEFAULT KEYBIND(msvc on Source Code Buffer)
;;   - start auto completion
;;     code completion & arguments expand
;;     `.` `->` `::`
;;   - start manual completion
;;     code completion & arguments expand
;;     `<TAB>`
;;   - jump to definition / return from definition
;;     this is nestable jump.
;;     target is type, function, enum, macro, include, misc.
;;     visit to definition file / return from definition file.
;;     `M-.` / `M-,`
;;   - visit to include file / return from include file(CEDET)
;;     `M-i` / `M-I`
;;   - goto error line prev / next
;;     `M-[` / `M-]`
;;   - manual syntax check
;;    `<f5>`
;;   - build solution
;;    `C-<f5>`
;; 
;; * DEFAULT KEYBIND(msvc on Project Buffer)
;;   - jump to buffer
;;     `RET` `mouse-1`
;;   - refer to buffer
;;     `C-z`
;; 
;; * DEFAULT KEYBIND(msvc on Build Report)
;;   - goto error line prev / next
;;     `[` / `]`
;;   - refer error line & buffer prev / next
;;     `M-[` / `M-]`
;;   - jump to buffer
;;     `RET` `mouse-1`
;;   - refer to buffer
;;     `C-z`
;; 


;;; Code:



(require 'cl-lib)
(require 'flymake)
(require 'msvc-env)
(require 'msvc-flags)
(require 'ac-clang)




(defconst msvc-version "1.3.7")


(defconst msvc--project-buffer-name-fmt "*MSVC Project<%s>*")

;; active projects database
(defvar msvc--active-projects nil
  "active project details")

;; '(db-name . 
;;         ((db-path . db-path)
;;          (project-buffer  . project-buffer)
;;          (project-file . project-file)
;;          (platform . nil)
;;          (configuration . nil)
;;          (version . nil)
;;          (toolset . nil)
;;          (allow-cedet-p . t)
;;          (allow-ac-clang-p . t)
;;          (allow-flymake-p . t)
;;          (cedet-spp-table . nil)
;;          (target-buffers . ())
;;         ))


;; the project name(per MSVC buffer)
(defvar-local msvc--db-name nil)


;; source code buffer belonging to the project name(per source code buffer)
(defvar-local msvc--source-code-belonging-db-name nil)



;; auto-complete ac-sources backup
(defvar-local msvc--ac-sources-backup nil)

;; ac-clang cflags backup
(defvar-local msvc--ac-clang-cflags-backup nil)




;; project buffer display update var.
;; usage: the control usually use let bind.
(defvar msvc-display-update-p t)

;; diplay allow & order
(defvar msvc-display-allow-properties '(:db-path
                                        :project-buffer
                                        :solution-file
                                        :project-file
                                        :platform
                                        :configuration
                                        :version
                                        :toolset
                                        :md5-name-p
                                        :allow-cedet-p
                                        :allow-ac-clang-p
                                        :allow-flymake-p
                                        :cedet-root-path
                                        :cedet-spp-table
                                        :flymake-back-end
                                        :flymake-manually-p
                                        :flymake-manually-back-end
                                        :target-buffers))
                                        
;; diplay properties
(defvar msvc--display-property-types '(:db-path path
                                       :project-buffer value
                                       :solution-file path
                                       :project-file path
                                       :platform value
                                       :configuration value
                                       :version value
                                       :toolset value
                                       :md5-name-p value
                                       :allow-cedet-p value
                                       :allow-ac-clang-p value
                                       :allow-flymake-p value
                                       :cedet-root-path path
                                       :cedet-spp-table value
                                       :flymake-back-end value
                                       :flymake-manually-p value
                                       :flymake-manually-back-end value
                                       :target-buffers buffer))



;; using path style
(defvar msvc-target-buffer-path-format nil
  "project's target source code buffer path style
`nil'          : native style
`posix'        : posix style
")

(defvar msvc-cedet-path-format nil
  "CEDET project & include path style
`nil'          : native style
`posix'        : posix style
")



;; project file importer
(defconst msvc--flymake-vcx-proj-name "msvc-extractor.flymake.vcxproj")
(defconst msvc--flymake-vcx-proj-file (expand-file-name msvc--flymake-vcx-proj-name msvc-env--package-directory))


(defvar msvc-flymake-error-display-style 'popup
  "flymake error message display style symbols
`popup'       : popup display
`mini-buffer' : mini-buffer display
`nil'         : user default style")


(defvar-local msvc--flymake-back-end 'msbuild
  "flymake back-end symbols
`msbuild'     : MSBuild
`clang'       : clang
`nil'         : native back-end")

(defvar-local msvc--flymake-manually-back-end nil
  "flymake manually mode back-end symbols
`msbuild'     : MSBuild
`clang'       : clang
`nil'         : inherit msvc--flymake-back-end value")



(defconst msvc--after-init-file (locate-user-emacs-file ".msvc"))


;; This is activation request queue after parse.
(defvar msvc--activation-requests nil)

;; Activation executor after all parse.
(defvar msvc--activation-timer nil)



(defvar msvc-solution-build-report-display-timing nil
  "build report display timing symbols
`nil'      : not foreground.
`before'   : when the build is starts.
`after'    : when the build is done.")

(defvar msvc-solution-build-report-display-target nil
  "build report display target symbols
`nil'         : self
`other-frame' : other frame.")

(defvar msvc-solution-build-report-realtime-display-p t)

(defvar msvc-solution-build-report-verbosity 'normal
  "build report verbosity symbols
`quiet'
`minimal'
`normal'
`detailed'
`diagnostic'")



;; for Project Buffer keymap
(defvar msvc-mode-filter-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'msvc--keyboard-visit-target)
    (define-key map (kbd "C-z") #'msvc--keyboard-visit-target-other-window)
    ;; (define-key map [(mouse-1)] #'ibuffer-mouse-toggle-mark)
    (define-key map [(mouse-1)] #'msvc--mouse-visit-target)
    ;; (define-key map [down-mouse-3] #'ibuffer-mouse-popup-menu)
    map))



;; (defsubst msvc--get-alist-value (alist key)
;;   (cdr (assq key alist)))



(defun msvc--regist-project (db-name details)
  (msvc--unregist-project db-name)
  (add-to-list 'msvc--active-projects `(,db-name . ,details)))

(defun msvc--unregist-project (db-name)
  (setq msvc--active-projects (delete (assoc-string db-name msvc--active-projects) msvc--active-projects)))


(defun msvc--query-project (db-name)
  (cdr (assoc-string db-name msvc--active-projects)))

(defun msvc--query-current-project ()
  (msvc--query-project (or msvc--db-name msvc--source-code-belonging-db-name)))


(defsubst msvc--query-project-db-name (project)
  (car project))



(defsubst msvc--convert-to-target-buffer-style-path (paths)
  (if (eq msvc-target-buffer-path-format 'posix)
      (msvc-env--convert-to-posix-style-path paths)
    paths))

(defsubst msvc--convert-to-cedet-style-path (paths &optional safe-path)
  (when safe-path
    (setq paths (msvc-env--normalize-path paths safe-path)))

  (if (eq msvc-cedet-path-format 'posix)
      (msvc-env--convert-to-posix-style-path paths)
    paths))



;; カレントバッファが指定プロジェクトに属しているかチェックする
(defun msvc--target-buffer-p (db-name &optional target-files)
  ;; major-mode check
  (when (and (memq major-mode '(c++-mode c-mode)) buffer-file-name)
    (unless target-files
      (setq target-files (msvc--convert-to-target-buffer-style-path (msvc-flags--query-cflag db-name "CFLAG_TargetFilesAbs"))))

    (when (member-ignore-case buffer-file-name target-files)
      buffer-file-name)))

;; すでにオープンされているバッファでプロジェクトに所属しているものを集める
(defun msvc--collect-target-buffer (db-name)
  (let* ((buffers (buffer-list))
         target-buffers
         (target-files (msvc--convert-to-target-buffer-style-path (msvc-flags--query-cflag db-name "CFLAG_TargetFilesAbs"))))

    (cl-dolist (buffer buffers)
      (with-current-buffer buffer
        ;; file belonging check
        (when (msvc--target-buffer-p db-name target-files)
          (push buffer target-buffers))))
    target-buffers))





(defun msvc--switch-to-buffer-other-frame (buffer)
  (let ((current-frame (selected-frame)))
    (switch-to-buffer-other-frame buffer)
    (raise-frame current-frame)))

(defun msvc--split-window (buffer)
  (unless (get-buffer-window-list buffer)
    (let ((target-window (if (one-window-p) (split-window-below) (next-window))))
      (set-window-buffer target-window buffer))))

(defun msvc--visit-buffer (point switch-function)
  (let* ((target-buffer (get-text-property point 'value)))
    (if target-buffer
        (apply switch-function target-buffer nil)
      (error "buffer no present"))))

(defun msvc--visit-path (point switch-function)
  (let* ((target-path (get-text-property point 'value)))
    (if target-path
        (apply switch-function target-path nil)
      (error "path no present"))))


(defun msvc--keyboard-visit-target ()
  "Toggle the display status of the filter group on this line."
  (interactive)

  (cl-case (get-text-property (point) 'target)
    (buffer
     (msvc--visit-buffer (point) #'switch-to-buffer))
    (path
     (msvc--visit-path (point) #'find-file))))
     

(defun msvc--keyboard-visit-target-other-window ()
  "Toggle the display status of the filter group on this line."
  (interactive)

  (cl-case (get-text-property (point) 'target)
    (buffer
     (msvc--visit-buffer (point) #'msvc--split-window))
    (path
     (msvc--visit-path (point) #'find-file-other-window))))
    

(defun msvc--mouse-visit-target (_event)
  "Toggle the display status of the filter group chosen with the mouse."
  (interactive "e")

  (cl-case (get-text-property (point) 'target)
    (buffer
     (msvc--visit-buffer (point) #'switch-to-buffer))
    (path
     (msvc--visit-path (point) #'find-file))))



;; プロジェクトディテールをプロジェクトバッファに表示する
(defun msvc--display-project-details (db-name)
  (when msvc-display-update-p
    (let* ((details (msvc--query-project db-name))
           (project-buffer (plist-get details :project-buffer)))
      (when project-buffer
        (with-current-buffer project-buffer
          (let ((buffer-read-only nil))
            (erase-buffer)
            (goto-char (point-min))

            (cl-dolist (property msvc-display-allow-properties)
              (let ((type (plist-get msvc--display-property-types property))
                    (value (plist-get details property))
                    (start-pos (point)))
                (cl-case type
                  (path
                   (insert
                    (format "%-30s : " property)
                    (propertize (format "%s" value) 'face 'font-lock-keyword-face)
                    "\n")
                   (add-text-properties start-pos (1- (point)) `(mouse-face highlight))
                   (add-text-properties start-pos (point) `(target ,type value ,value keymap ,msvc-mode-filter-map)))
                  (buffer
                   (insert (format "%-30s :\n" property))
                   (cl-dolist (buffer value)
                     (setq start-pos (point))
                     (insert
                      (format " -%-28s : " "buffer-name")
                      (propertize (format "%-30s : %s" buffer (buffer-file-name buffer)) 'face 'font-lock-keyword-face)
                      "\n")
                     (add-text-properties start-pos (1- (point)) `(mouse-face highlight))
                     (add-text-properties start-pos (point) `(target ,type value ,buffer keymap ,msvc-mode-filter-map))))
                  (value
                   (insert (format "%-30s : %s\n" property value))))))))))))



;; CEDET Project.ede を生成する
(defun msvc--create-ede-project-file (ede-proj-file db-name)
  (let* ((proj-name db-name)
         (file-name (file-name-nondirectory ede-proj-file)))
    (with-temp-file ede-proj-file
      (insert ";; Object " proj-name "\n"
              ";; EDE Project File.\n"
              "(ede-proj-project \"" proj-name "\"
                  :file \"" file-name "\"
                  :name \"" proj-name "\"
                  :targets 'nil
                  )"))))




;; replace start-file-process => start-file-process-shell-command (flymake original function)
(defun msvc--flymake-start-syntax-check-process (cmd args dir)
  "Start syntax check process."
  ;; (print (format "cmd = %s" cmd))
  ;; (print (format "args = %s" args))
  ;; (print (format "dir = %s" dir))
  (condition-case err
      (let* (;; bind connection type (use pipe)
             (process-connection-type nil)
             ;; bind encoding system (logfile:utf-8-dos, buffer:utf-8-unix)
             (default-process-coding-system '(utf-8-dos . utf-8-unix))
             (process
              (let ((default-directory (or dir default-directory)))
                (when dir
                  (flymake-log 3 "starting process on dir %s" dir))
                (apply #'start-file-process
                       "flymake-proc" (current-buffer) cmd args))))
        (set-process-sentinel process 'flymake-process-sentinel)
        (set-process-filter process 'flymake-process-filter)
        (push process flymake-processes)

        (setq flymake-is-running t)
        (setq flymake-last-change-time nil)
        (setq flymake-check-start-time (float-time))

        (flymake-report-status nil "*")
        (flymake-log 2 "started process %d, command=%s, dir=%s"
                     (process-id process) (process-command process)
                     default-directory)
        process)
    (error
     (let* ((err-str (format "Failed to launch syntax check process '%s' with args %s: %s"
                             cmd args (error-message-string err)))
            (source-file-name buffer-file-name)
            (cleanup-f (flymake-get-cleanup-function source-file-name)))
       (flymake-log 0 err-str)
       (funcall cleanup-f)
       (flymake-report-fatal-status "PROCERR" err-str)))))


(defadvice flymake-start-syntax-check-process (around msvc--flymake-start-syntax-check-process-advice (cmd args dir) activate)
  (cl-case msvc--flymake-back-end
    (msbuild
     (msvc--flymake-start-syntax-check-process cmd args dir))
    (clang
     (ac-clang-diagnostics))
    (t
     ad-do-it)))


(defconst msvc--flymake-allowed-file-name-masks '(("\\.\\(?:[ch]\\(?:pp\\|xx\\|\\+\\+\\)?\\|CC\\)\\'" msvc--flymake-command-generator)))

(defconst msvc--flymake-err-line-patterns
  '(
    ;; Visual C/C++ 2013/2012/2010
    msbuild
    ;; (1:file, 2:line, 3:error-text) flymake only support
    ;; (("^\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\))[ \t\n]*\:[ \t\n]*\\(\\(?:error\\|warning\\|fatal error\\) \\(?:C[0-9]+\\):[ \t\n]*\\(?:[^[]+\\)\\)" 1 2 nil 3))
    ;; (1:file, 2:line, 3:error-text, 4:project) flymake & solution build both support
    (("^[ 0-9>]*\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\))[ \t\n]*\:[ \t\n]*\\(\\(?:error\\|warning\\|fatal error\\) \\(?:C[0-9]+\\):[ \t\n]*\\(?:[^[]+\\)\\)\\[\\(.+\\)\\]" 1 2 nil 3))

    ;; clang 3.3
    clang
    (("^\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\):\\([0-9]+\\):\\([0-9]+\\)[ \t\n]*:[ \t\n]*\\(\\(?:error\\|warning\\|fatal error\\):\\(?:.*\\)\\)" 1 2 3 4)))
  "  (REGEXP FILE-IDX LINE-IDX COL-IDX ERR-TEXT-IDX).")


(defun msvc--flymake-command-generator ()
  (interactive)
  (let* ((db-name msvc--source-code-belonging-db-name)
         (compile-file (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))

         (cedet-file-name (cedet-directory-name-to-file-name compile-file))
         (cedet-project-path (cedet-directory-name-to-file-name (msvc-flags--create-project-path db-name)))
         (extract-file-name (substring cedet-file-name (1- (abs (compare-strings cedet-project-path nil nil cedet-file-name nil nil)))))

         (details (msvc--query-project db-name))
         (db-path (plist-get details :db-path))
         (version (plist-get details :version))
         (toolset (plist-get details :toolset))
         (md5-name-p (plist-get details :md5-name-p))
         (fix-file-name (if md5-name-p (md5 extract-file-name) extract-file-name))
         (msb-rsp-file (expand-file-name (concat fix-file-name ".flymake.rsp.msvc") db-path))
         (log-file (expand-file-name (concat fix-file-name ".flymake.log.msvc") db-path)))

    ;; create rsp file
    (unless (file-exists-p msb-rsp-file)
      (let* ((project-file (plist-get details :project-file))
             (platform (plist-get details :platform))
             (configuration (plist-get details :configuration))

             (logger-encoding "UTF-8")
             (project-path (file-name-directory project-file))
             (msb-target-file (expand-file-name msvc--flymake-vcx-proj-name project-path))
             (msb-flags (list
                         (msvc-env--create-msb-flags "/p:"
                                                     `(("ImportProjectFile=%S"       .       ,project-file)
                                                       ("Platform=%S"                .       ,platform)
                                                       ("Configuration=%S"           .       ,configuration)
                                                       ("CompileFile=%S"             .       ,compile-file)
                                                       ;; IntDir,OutDirは末尾にスラッシュが必須(MSBuildの仕様)
                                                       ("IntDir=%S"                  .       ,db-path)
                                                       ("OutDir=%S"                  .       ,db-path)))
                         (msvc-env--create-msb-flags "/flp:"
                                                     `(("Verbosity=%s"               .       "normal")
                                                       ("LogFile=%S"                 .       ,log-file)
                                                       ("Encoding=%s"                .       ,logger-encoding)
                                                       ("%s"                         .       "NoSummary")))
                         "/noconsolelogger"
                         "/nologo")))

        (msvc-env--create-msb-rsp-file msb-rsp-file msb-target-file msb-flags)))

    (list 
     msvc-env--invoke-command
     (msvc-env--build-msb-command-args version toolset msb-rsp-file log-file))))


;; error message display to Minibuf
(defun msvc--flymake-display-current-line-error-by-minibuf ()
  "Displays the error/warning for the current line in the minibuffer"

  (let* ((line-no (line-number-at-pos))
         (line-err-info-list (nth 0 (flymake-find-err-info flymake-err-info line-no)))
         (count (length line-err-info-list)))
    (while (> count 0)
      (when line-err-info-list
        (let* ((text (flymake-ler-text (nth (1- count) line-err-info-list)))
               (line (flymake-ler-line (nth (1- count) line-err-info-list))))
          (message "[%s] %s" line text)))
      (setq count (1- count)))))

;; error message display to popup-tip
;; use popup.el (include auto-complete packages)
(defun msvc--flymake-display-current-line-error-by-popup ()
  "Display a menu with errors/warnings for current line if it has errors and/or warnings."

  (let* ((line-no (line-number-at-pos))
         (errors (nth 0 (flymake-find-err-info flymake-err-info line-no)))
         (texts (mapconcat (lambda (x) (flymake-ler-text x)) errors "\n")))
    (when texts
      (popup-tip texts))))

(defun msvc--flymake-display-current-line-error ()
  (cl-case msvc-flymake-error-display-style
    (popup
     (msvc--flymake-display-current-line-error-by-popup))
    (mini-buffer
     (msvc--flymake-display-current-line-error-by-minibuf))))



(defun msvc--setup-project-feature-ac-clang (_db-name status)
  (cl-case status
    (enable
     nil
     )
    (disable
     nil)))

(defun msvc--setup-buffer-feature-ac-clang (db-name status)
  (cl-case status
    (enable
     ;; backup value
     ;; (push ac-sources msvc--ac-sources-backup)
     (push ac-clang-cflags msvc--ac-clang-cflags-backup)

     ;; set database value
     ;; (setq ac-sources '(ac-source-clang-async))
     (setq ac-clang-cflags (msvc-flags-create-ac-clang-cflags db-name))

     ;; buffer modified > do activation
     ;; buffer not modify > delay activation
     (ac-clang-activate-after-modify))
    (disable
     ;; always deactivation
     (ac-clang-deactivate)

     ;; restore value
     ;; (setq ac-sources (pop msvc--ac-sources-backup))
     (setq ac-clang-cflags (pop msvc--ac-clang-cflags-backup)))))


;; CEDET セットアップ関数
(defun msvc--setup-project-feature-cedet (db-name status)
  (let* ((details (msvc--query-project db-name))
         (project-path (file-name-directory (plist-get details :project-file)))

         ;; cedet-root-path が未設定の場合はプロジェクトファイルのパスから生成する
         ;; この場合、ディレクトリ構成によっては正常に動作しないケースもある
         (cedet-root-path (or (plist-get details :cedet-root-path) project-path))
         (cedet-spp-table (plist-get details :cedet-spp-table))
         (system-inc-paths (msvc--convert-to-cedet-style-path (msvc-flags--query-cflag db-name "CFLAG_SystemIncludePath")))
         (additional-inc-paths (msvc--convert-to-cedet-style-path (msvc-flags--query-cflag db-name "CFLAG_AdditionalIncludePath") project-path))
         ;; (project-header-match-regexp "\\.\\(h\\(h\\|xx\\|pp\\|\\+\\+\\)?\\|H\\|inl\\)$\\|\\<\\w+$")
         (project-header-match-regexp "\\.\\(h\\(h\\|xx\\|pp\\|\\+\\+\\)?\\|H\\|inl\\)$")
         (ede-proj-file (expand-file-name (concat db-name ".ede") cedet-root-path))
         additional-inc-rpaths)

    (cl-case status
      (enable
       ;; generate relative path(CEDET format)
       (cl-dolist (path additional-inc-paths)
         (setq path (file-relative-name path cedet-root-path))
         ;; All path is relative from cedet-root-path.
         ;; And relative path string require starts with "/". (CEDET :include-path format specification)
         (setq path (concat "/" path))
         (msvc-env--add-to-list additional-inc-rpaths (file-name-as-directory path) t))

       ;; generate Project.ede file
       ;; (print "ede-proj-file")
       ;; (print ede-proj-file)
       (unless (file-readable-p ede-proj-file)
         (msvc--create-ede-project-file ede-proj-file db-name))

       ;; (print "ede-cpp-root-project")
       ;; (print ede-proj-file)
       ;; (print cedet-root-path)
       ;; (print additional-inc-rpaths)
       ;; (print system-inc-paths)
       ;; (print project-header-match-regexp)
       ;; (print cedet-spp-table)

       (semantic-reset-system-include 'c-mode)
       (semantic-reset-system-include 'c++-mode)
       (setq semantic-c-dependency-system-include-path nil)
       (ede-cpp-root-project db-name ;ok
                             :file ede-proj-file ;ok
                             :directory cedet-root-path ; ok
                             :include-path additional-inc-rpaths ; :directoryからの相対パスで指定
                             :system-include-path system-inc-paths ;ok
                             :header-match-regexp project-header-match-regexp ;ok
                             :spp-table cedet-spp-table ;ok
                             :spp-files nil ;ok
                             :local-variables nil ;ok ede:project-local-variables
                             ))
      (disable
       nil))))

(defun msvc--setup-buffer-feature-cedet (_db-name status)
  (cl-case status
    (enable
     ;; backup value
     (push ac-sources msvc--ac-sources-backup)
     ;; auto-complete ac-sources setup(use semantic)
     (setq ac-sources '(
                        ;; ac-source-dictionary
                        ac-source-semantic
                        ac-source-semantic-raw
                        ac-source-imenu
                        ;; ac-source-words-in-buffer
                        ;; ac-source-words-in-same-mode-buffers
                        ))

     ;; Force a full refresh of the current buffer's tags.
     ;; (semantic-force-refresh)
     )
    (disable
     ;; restore value
     (setq ac-sources (pop msvc--ac-sources-backup)))))


;; flymake セットアップ関数
(defun msvc--setup-project-feature-flymake (db-name status)
  (cl-case status
    (enable
     ;; (unless (rassoc '(msvc--flymake-command-generator) flymake-allowed-file-name-masks)
     ;;   (push `(,msvc--flymake-target-pattern msvc--flymake-command-generator) flymake-allowed-file-name-masks)))

     ;; プロジェクトファイルと同じ場所にインポートプロジェクトが配置されている必要がある
     ;; MSBuild の仕様のため(詳細後述)
     (let* ((project-file (plist-get (msvc-flags--create-project-property db-name) :project-file))
            (project-path (file-name-directory project-file))
            (msb-target-file (expand-file-name msvc--flymake-vcx-proj-name project-path)))
       (when (file-newer-than-file-p msvc--flymake-vcx-proj-file msb-target-file)
         (copy-file msvc--flymake-vcx-proj-file msb-target-file t t)))
     nil)
    (disable
     ;; (setq flymake-allowed-file-name-masks (delete (rassoc '(msvc--flymake-command-generator) flymake-allowed-file-name-masks) flymake-allowed-file-name-masks)))
     nil)))

(defun msvc--setup-buffer-feature-flymake (db-name status)
  (let* ((details (msvc--query-project db-name))
         (back-end (plist-get details :flymake-back-end))
         (manually-p (plist-get details :flymake-manually-p))
         (manually-back-end (plist-get details :flymake-manually-back-end)))

    (cl-case status
      (enable
       (when back-end
         (setq msvc--flymake-back-end back-end))
       (setq msvc--flymake-manually-back-end (or manually-back-end msvc--flymake-back-end))
       (set (make-local-variable 'flymake-allowed-file-name-masks) msvc--flymake-allowed-file-name-masks)
       (set (make-local-variable 'flymake-err-line-patterns) (plist-get msvc--flymake-err-line-patterns msvc--flymake-manually-back-end))
       ;; 複数バッファのflymakeが同時にenableになるとflymake-processでpipe errorになるのを抑制
       (set (make-local-variable 'flymake-start-syntax-check-on-find-file) nil)

       (flymake-mode-on)
       (when manually-p
         (defadvice flymake-on-timer-event (around msvc--flymake-suspend-advice last activate)
           (let* ((details (msvc--query-current-project))
                  (manually-p (plist-get details :flymake-manually-p)))
             ;; (when (and details (not manually-p))
             (unless manually-p
               ad-do-it)))))
      ;; (let ((flymake-start-syntax-check-on-find-file nil))
      ;;   (flymake-mode-on)))
      (disable
       (when manually-p
         (flymake-delete-own-overlays)
         (ad-disable-advice 'flymake-on-timer-event 'around 'msvc--flymake-suspend-advice))
       (flymake-mode-off)

       (setq msvc--flymake-back-end nil)
       (setq msvc--flymake-manually-back-end nil)
       (set (make-local-variable 'flymake-allowed-file-name-masks) (default-value 'flymake-allowed-file-name-masks))
       (set (make-local-variable 'flymake-err-line-patterns) (default-value 'flymake-err-line-patterns))
       (set (make-local-variable 'flymake-start-syntax-check-on-find-file) (default-value 'flymake-start-syntax-check-on-find-file))))))




;; カレントバッファをプロジェクトにアタッチする
(defun msvc--attach-to-project (db-name)
  (interactive)

  (let* ((details (msvc--query-project db-name))
         (allow-cedet-p (plist-get details :allow-cedet-p))
         (allow-ac-clang-p (plist-get details :allow-ac-clang-p))
         (allow-flymake-p (plist-get details :allow-flymake-p))
         (target-buffers (plist-get details :target-buffers)))
    ;; (print db-name)
    ;; (print details)
    ;; (print target-buffers)

    (unless msvc--source-code-belonging-db-name
      ;; db-name set to local-var for project target buffer.
      (setq msvc--source-code-belonging-db-name db-name)

      ;; attach to project
      (push (current-buffer) target-buffers)
      (setq details (plist-put details :target-buffers target-buffers))
      ;; (print target-buffers)

      ;; (add-hook 'kill-buffer-hook #'msvc--detach-from-project nil t)
      ;; (add-hook 'before-revert-hook #'msvc--detach-from-project nil t)
      (add-hook 'kill-buffer-hook #'msvc-mode-off nil t)
      (add-hook 'before-revert-hook #'msvc-mode-off nil t)

      ;; launch allow features(launch order low > high)

      ;; ---- CEDET ----
      (when allow-cedet-p
        (msvc--setup-buffer-feature-cedet db-name 'enable))
      ;; ---- ac-clang ----
      (when allow-ac-clang-p
        (msvc--setup-buffer-feature-ac-clang db-name 'enable))
      ;; ---- flymake ----
      (when allow-flymake-p
        (msvc--setup-buffer-feature-flymake db-name 'enable))

      ;; プロジェクト状態をバッファへ表示
      (msvc--display-project-details db-name)

      t)))



;; カレントバッファをプロジェクトからデタッチする
(defun msvc--detach-from-project ()
  (interactive)

  (when msvc--source-code-belonging-db-name
    (let* ((db-name msvc--source-code-belonging-db-name)
           (details (msvc--query-project db-name))
           (allow-cedet-p (plist-get details :allow-cedet-p))
           (allow-ac-clang-p (plist-get details :allow-ac-clang-p))
           (allow-flymake-p (plist-get details :allow-flymake-p))
           (target-buffers (plist-get details :target-buffers)))

      ;; clear beglonging db-name
      (setq msvc--source-code-belonging-db-name nil)

      ;; detach from project
      (setq target-buffers (delete (current-buffer) target-buffers))
      (setq details (plist-put details :target-buffers target-buffers))

      (remove-hook 'kill-buffer-hook #'msvc-mode-off t)
      (remove-hook 'before-revert-hook #'msvc-mode-off t)

      ;; shutdown allow features(order hight > low)

      ;; ---- flymake ----
      (when allow-flymake-p
        (msvc--setup-buffer-feature-flymake db-name 'disable))
      ;; ---- ac-clang ----
      (when allow-ac-clang-p
        (msvc--setup-buffer-feature-ac-clang db-name 'disable))
      ;; ---- CEDET ----
      (when allow-cedet-p
        (msvc--setup-buffer-feature-cedet db-name 'disable))

      ;; プロジェクト状態をバッファへ表示
      (msvc--display-project-details db-name)

      t)))



;; バッファ起動時のフックで実行する関数
(cl-defun msvc--evaluate-buffer ()
  (interactive)

  (unless msvc--source-code-belonging-db-name
    (cl-dolist (project msvc--active-projects)
      (let* ((db-name (car project)))
        (when (msvc--target-buffer-p db-name)
          (msvc--attach-to-project db-name)
          (cl-return-from msvc--evaluate-buffer t))))))




(defun msvc--parsed-activator ()
  (unless msvc-flags--parsing-p
    ;; (message "parsed-activator")
    ;; (print msvc--activation-requests)
    (cl-dolist (request msvc--activation-requests)
      ;; (print request)
      (let ((db-names (plist-get request :db-names))
            (args (plist-get request :args)))
        (cl-dolist (db-name db-names)
          (apply #'msvc-activate-project db-name args))))
    (setq msvc--activation-requests nil)

    (when msvc--activation-timer
      (cancel-timer msvc--activation-timer)
      (setq msvc--activation-timer nil))))



;; プロジェクトのアクティベーション(アクティブリストへ登録)
(cl-defun msvc-activate-projects-after-parse (&rest args)
  "attributes
-requires
:solution-file
:project-file
:platform
:configuration

-optionals
:version
:toolset
:md5-name-p
:force-parse-p
:sync-p
:allow-cedet-p
:allow-ac-clang-p
:allow-flymake-p
:cedet-root-path
:cedet-spp-table
:flymake-back-end
:flymake-manually-p
:flymake-manually-back-end
"
  (interactive)

  (let* ((solution-file (plist-get args :solution-file))
         (project-file (plist-get args :project-file))
         (platform (plist-get args :platform))
         (configuration (plist-get args :configuration))
         db-names)

    (unless (or solution-file project-file)
      (cl-return-from msvc-activate-projects-after-parse nil))

    (unless (and platform configuration)
      (cl-return-from msvc-activate-projects-after-parse nil))

    ;; args check & modify

    ;; add force delete
    (setq args (plist-put args :parsing-buffer-delete-p t))

    ;; check version
    (unless (plist-get args :version)
      (setq args (plist-put args :version msvc-env-default-use-version)))
    
    ;; check toolset
    (unless (plist-get args :toolset)
      (setq args (plist-put args :toolset msvc-env-default-use-toolset)))
    
    ;; 指定ソリューションorプロジェクトのパース
    (when (and solution-file (not project-file))
      (setq db-names (apply #'msvc-flags-parse-vcx-solution args)))

    (when project-file
      (setq db-names (apply #'msvc-flags-parse-vcx-project args))
      (setq db-names (when db-names (list db-names))))

    (when db-names
      (add-to-list 'msvc--activation-requests `(:db-names ,db-names :args ,args) t)
      (unless msvc--activation-timer
        (setq msvc--activation-timer (run-at-time nil 1 #'msvc--parsed-activator))))

    db-names))




(cl-defun msvc-activate-project (db-name &rest args)
  "attributes
-optionals
:solution-file
:allow-cedet-p
:allow-ac-clang-p
:allow-flymake-p
:cedet-root-path
:cedet-spp-table
:flymake-back-end
:flymake-manually-p
:flymake-manually-back-end
"
  (interactive)

  ;; (message (format "allow-ac-clang-p = %s, allow-cedet-p = %s, allow-flymake-p = %s\n" allow-ac-clang-p allow-cedet-p allow-flymake-p))

  (unless db-name
    (message "msvc : db-name is nil.")
    (cl-return-from msvc-activate-project nil))

  ;; DBリストからプロジェクトマネージャーを生成
  (let* ((property (msvc-flags--create-project-property db-name))

         ;; project basic information(from property)
         (project-buffer (format msvc--project-buffer-name-fmt db-name))
         (project-file (plist-get property :project-file))
         (platform (plist-get property :platform))
         (configuration (plist-get property :configuration))
         (version (plist-get property :version))
         (toolset (plist-get property :toolset))

         ;; project basic information(from args)
         (md5-name-p (plist-get args :md5-name-p))
         (dir-name (if md5-name-p (md5 db-name) db-name))
         (db-path (msvc-flags--create-db-path dir-name))

         (solution-file (plist-get args :solution-file))

         ;; project allow feature(from args)
         (allow-cedet-p (plist-get args :allow-cedet-p))
         (allow-ac-clang-p (plist-get args :allow-ac-clang-p))
         (allow-flymake-p (plist-get args :allow-flymake-p))
         (cedet-root-path (plist-get args :cedet-root-path))
         (cedet-spp-table (plist-get args :cedet-spp-table))
         (flymake-back-end (plist-get args :flymake-back-end))
         (flymake-manually-p (plist-get args :flymake-manually-p))
         (flymake-manually-back-end (plist-get args :flymake-manually-back-end))

         (target-buffers nil)
         ;; details
         )

    ;; CFLAGS exist check
    (unless (msvc-flags--query-cflags db-name)
      (message "msvc : db-name not found in CFLAGS database. : %s" db-name)
      (cl-return-from msvc-activate-project nil))

    ;; 既存バッファは削除（削除によって既存プロジェクトの削除も動作するはず）
    (when (get-buffer project-buffer)
      (kill-buffer project-buffer))

    (get-buffer-create project-buffer)

    ;; dbへ登録のみ
    ;; value が最初はnilだとわかっていても変数を入れておかないと評価時におかしくなる・・
    ;; args をそのまま渡したいが、 意図しないpropertyが紛れ込みそうなのでちゃんと指定する
    (msvc--regist-project db-name `(
                                    :db-path ,db-path
                                    :project-buffer ,project-buffer
                                    :solution-file ,solution-file
                                    :project-file ,project-file
                                    :platform ,platform
                                    :configuration ,configuration
                                    :version ,version
                                    :toolset ,toolset
                                    :md5-name-p ,md5-name-p
                                    :allow-cedet-p ,allow-cedet-p
                                    :allow-ac-clang-p ,allow-ac-clang-p
                                    :allow-flymake-p ,allow-flymake-p
                                    :cedet-root-path ,cedet-root-path
                                    :cedet-spp-table ,cedet-spp-table
                                    :flymake-back-end ,flymake-back-end
                                    :flymake-manually-p ,flymake-manually-p
                                    :flymake-manually-back-end ,flymake-manually-back-end
                                    :target-buffers ,target-buffers
                                    ))

    ;; setup project buffer
    (with-current-buffer project-buffer
      ;; db-name set local-var for MSVC buffer
      (setq msvc--db-name db-name)

      ;; (add-to-list 'msvc--active-projects project-buffer)
      ;; 該当バッファが消されたらマネージャーから外す
      (add-hook 'kill-buffer-hook `(lambda () (msvc--deactivate-project ,db-name)) nil t)

      ;; launch features (per project)

      ;; ---- CEDET ----
      (when allow-cedet-p
        (msvc--setup-project-feature-cedet db-name 'enable))
      ;; ---- ac-clang ----
      (when allow-ac-clang-p
        (msvc--setup-project-feature-ac-clang db-name 'enable))
      ;; ---- flymake ----
      (when allow-flymake-p
        (msvc--setup-project-feature-flymake db-name 'enable))

      ;; 編集させない
      (setq buffer-read-only t))

    ;; 以下プロジェクトのセットアップが終わってから行う(CEDETなどのプロジェクト付機能のセットアップも終わっていないとだめ)
    ;; オープン済みで所属バッファを収集
    (setq target-buffers (msvc--collect-target-buffer db-name))

    ;; target buffer all attach
    (let ((msvc-display-update-p nil))
      (cl-dolist (buffer target-buffers)
        (with-current-buffer buffer
          (msvc-mode-on))))
    
    ;; プロジェクト状態をバッファへ表示
    (msvc--display-project-details db-name)

    t))


;; プロジェクトのデアクティベーション
(defun msvc--deactivate-project (db-name)
  (interactive)

  (let ((details (msvc--query-project db-name)))
    (when details
      (let* ((project-buffer (format msvc--project-buffer-name-fmt db-name))
             (allow-cedet-p (plist-get details :allow-cedet-p))
             (allow-ac-clang-p (plist-get details :allow-ac-clang-p))
             (allow-flymake-p (plist-get details :allow-flymake-p))
             (target-buffers (plist-get details :target-buffers)))

        ;; target buffers all detach
        (let ((msvc-display-update-p nil))
          (cl-dolist (buffer target-buffers)
            (with-current-buffer buffer
              (msvc-mode-off))))


        ;; shutdown features (per project)
        ;; allowed features are necessary to shutdown.
        (with-current-buffer project-buffer
          ;; ---- flymake ----
          (when allow-flymake-p
            (msvc--setup-project-feature-flymake db-name 'disable))
          ;; ---- ac-clang ----
          (when allow-ac-clang-p
            (msvc--setup-project-feature-ac-clang db-name 'disable))
          ;; ---- CEDET ----
          (when allow-cedet-p
            (msvc--setup-project-feature-cedet db-name 'disable))))


      ;; a project is removed from database.
      ;; (print (format "msvc--deactivate-project %s" db-name))
      (msvc--unregist-project db-name)
      t)))


;; 現在アクティブなプロジェクトを再パース
(defun msvc-reparse-active-projects ()
  (interactive)

  ;; msvc-activate-projects-after-parseでmsvc--active-projectsに対してadd/removeされるので
  ;; msvc--active-projects を参照しながら msvc-activate-projects-after-parse を実行すると問題がでる可能性がある
  ;; なので一旦対象db-nameだけを集めてから処理する
  (let ((db-names (mapcar (lambda (project)
                            (car project))
                          msvc--active-projects)))
    (cl-dolist (db-name db-names)
      (apply #'msvc-activate-projects-after-parse (msvc--query-project db-name)))))



(defvar msvc--mode-feature-include-visit-stack nil)

(defun msvc-mode-feature-visit-to-include ()
  (interactive)
  (push (current-buffer) msvc--mode-feature-include-visit-stack)
  (semantic-decoration-include-visit))

(defun msvc-mode-feature-return-from-include ()
  (interactive)
  (let ((buffer (pop msvc--mode-feature-include-visit-stack)))
    (when (buffer-live-p buffer)
      (set-window-buffer nil buffer))))

(defun msvc-mode-feature-manually-ac-clang-complete ()
  (interactive)
  )


(defun msvc-mode-feature-flymake-goto-prev-error ()
  (interactive)

  (flymake-goto-prev-error)
  (msvc--flymake-display-current-line-error))

(defun msvc-mode-feature-flymake-goto-next-error ()
  (interactive)

  (flymake-goto-next-error)
  (msvc--flymake-display-current-line-error))


(defun msvc-mode-feature-manually-flymake ()
  (interactive)
  (cl-case msvc--flymake-manually-back-end
    (msbuild
     ;; back end : MSBuild
     (flymake-start-syntax-check))
    (clang
     ;; back end : clang
     (ac-clang-diagnostics))))

(defun msvc-mode-feature-jump-to-project-buffer ()
  (interactive)
  (when msvc--source-code-belonging-db-name
    (switch-to-buffer (format msvc--project-buffer-name-fmt msvc--source-code-belonging-db-name))
    ;; (switch-to-buffer-other-window (format msvc--project-buffer-name-fmt msvc--source-code-belonging-db-name))
    ))

(defun msvc--mode-feature-reparse-project ()
  (interactive)
  (let* ((details (msvc--query-current-project)))
    (apply #'msvc-activate-projects-after-parse details)))


(defun msvc-mode-feature-launch-msvs-by-project ()
  (interactive)
  (let* ((details (msvc--query-current-project))
         (project-file (plist-get details :project-file)))
    (w32-shell-execute "open" project-file)))

(defun msvc-mode-feature-launch-msvs-by-solution ()
  (interactive)
  (let* ((details (msvc--query-current-project))
         (solution-file (plist-get details :solution-file)))
    (when solution-file
      (w32-shell-execute "open" solution-file))))

(defun msvc-mode-feature-launch-msvs ()
  (interactive)
  (let* ((details (msvc--query-current-project))
         (target-file (or (plist-get details :solution-file) (plist-get details :project-file))))
    (when target-file
      (w32-shell-execute "open" target-file))))


(defun msvc--build-solution-sentinel (process _event)
  (when (memq (process-status process) '(signal exit))
    (let* (;; (exit-status (process-exit-status process))
           (bind-buffer (process-buffer process)))
      ;; プロセスバッファを終了時に表示
      (msvc--parse-solution-build-report bind-buffer)
      (when (eq msvc-solution-build-report-display-timing 'after)
        (if (eq msvc-solution-build-report-display-target 'other-frame)
            (msvc--switch-to-buffer-other-frame bind-buffer)
          (msvc--split-window bind-buffer))))))


(defun msvc-mode-feature-solution-goto-prev-error ()
  (interactive)

  (move-to-column 0)
  (let ((pos (previous-single-property-change (point) 'error-info)))
    (unless pos
      (setq pos (previous-single-property-change (point-max) 'error-info)))
    (when pos
      (goto-char pos)
      (move-to-column 0))))


(defun msvc-mode-feature-solution-goto-next-error ()
  (interactive)

  (move-to-column 0)
  (when (get-text-property (point) 'error-info)
    (goto-char (next-single-property-change (point) 'error-info)))
  (let ((pos (next-single-property-change (point) 'error-info)))
    (unless pos
      (setq pos (next-single-property-change (point-min) 'error-info)))
    (when pos
      (goto-char pos))))


(defun msvc-mode-feature-solution-jump-to-error-file ()
  (interactive)

  (let ((info (get-text-property (point) 'error-info)))
    (when info
      (let* ((file (plist-get info :src-file))
             (line (plist-get info :src-line))
             (buffer (find-file-noselect file)))
        (msvc--split-window buffer)
        (select-window (get-buffer-window buffer))
        (goto-char (point-min))
        (forward-line (1- line))))))


(defun msvc-mode-feature-solution-view-error-file ()
  (interactive)

  (let ((info (get-text-property (point) 'error-info)))
    (when info
      (let* ((file (plist-get info :src-file))
             (line (plist-get info :src-line))
             (buffer (find-file-noselect file)))
        (msvc--split-window buffer)
        (with-selected-window (get-buffer-window buffer)
          (goto-char (point-min))
          (forward-line (1- line)))))))


(defun msvc-mode-feature-solution-view-prev-error ()
  (interactive)
  (msvc-mode-feature-solution-goto-prev-error)
  (msvc-mode-feature-solution-view-error-file))

(defun msvc-mode-feature-solution-view-next-error ()
  (interactive)
  (msvc-mode-feature-solution-goto-next-error)
  (msvc-mode-feature-solution-view-error-file))


(defun msvc-mode-feature-solution-jump-to-error-file-by-mouse (event)
  (interactive "e")

  (mouse-set-point event)
  (msvc-mode-feature-solution-jump-to-error-file))


(defun msvc--parse-solution-build-report (buffer)
  (let* (
         ;; (pattern (concat (caar (plist-get msvc--flymake-err-line-patterns 'msbuild)) "\\[\\(.+\\)\\]"))
         (pattern (caar (plist-get msvc--flymake-err-line-patterns 'msbuild)))
         src-file
         src-line
         project-path
         msg-start
         msg-end
         ;; log-line
         log-start
         log-end
         (map (make-sparse-keymap)))

    (define-key map (kbd "[") #'msvc-mode-feature-solution-goto-prev-error)
    (define-key map (kbd "]") #'msvc-mode-feature-solution-goto-next-error)
    (define-key map (kbd "C-z") #'msvc-mode-feature-solution-view-error-file)
    (define-key map (kbd "M-[") #'msvc-mode-feature-solution-view-prev-error)
    (define-key map (kbd "M-]") #'msvc-mode-feature-solution-view-next-error)
    (define-key map (kbd "RET") #'msvc-mode-feature-solution-jump-to-error-file)
    (define-key map [(mouse-1)] #'msvc-mode-feature-solution-jump-to-error-file-by-mouse)

    (with-current-buffer buffer
      (use-local-map map)

      (setq buffer-read-only nil)
      (goto-char (point-min))

      (while (re-search-forward pattern nil t)
        (setq src-file (match-string 1))
        (setq src-line (string-to-number (match-string 2)))
        (setq log-start (match-beginning 1))
        (setq log-end (match-end 1))
        (setq msg-start (match-beginning 3))
        (setq msg-end (match-end 3))
        (setq project-path (match-string 4))

        ;; (setq log-line (line-number-at-pos log-start))
        (setq src-file (replace-regexp-in-string "[\\\\]+" "/" src-file))
        ;; (setq src-file (replace-regexp-in-string "^\\s-+" "" src-file))

        (unless (file-name-absolute-p src-file)
          (setq project-path (replace-regexp-in-string "[\\\\]+" "/" project-path))
          (setq project-path (file-name-directory project-path))
          (setq src-file (expand-file-name src-file project-path)))

        (set-text-properties (line-beginning-position) (line-end-position) `(mouse-face highlight error-info (:src-file ,src-file :src-line ,src-line)))
        (add-text-properties log-start log-end `(face dired-directory))
        (add-text-properties msg-start msg-end `(face font-lock-keyword-face)))

      (setq buffer-read-only t))))



(cl-defun msvc-mode-feature-build (&key project-only-p (target "Build"))
  (interactive)
  (let ((db-name (or msvc--db-name msvc--source-code-belonging-db-name)))
    (when db-name
      (let* ((details (msvc--query-project db-name))
             (solution-file (plist-get details :solution-file))
             (project-file (plist-get details :project-file))
             (target-file (if project-only-p project-file solution-file))
             (process-bind-buffer (format "*MSVC Build<%s>*" db-name)))

        (unless target-file
          (message "The solution or project name not found on active project.")
          (cl-return-from msvc-mode-feature-build nil))

        (when (process-live-p (get-buffer-process process-bind-buffer))
          (message "The solution is already building.")
          (cl-return-from msvc-mode-feature-build nil))
          
        (let* ((db-path (plist-get details :db-path))
               (platform (plist-get details :platform))
               (configuration (plist-get details :configuration))
               (version (plist-get details :version))
               (toolset (plist-get details :toolset))

               (dst-file-base-name (file-name-nondirectory target-file))
               (log-file (expand-file-name (concat dst-file-base-name ".build.log.msvc") db-path))
               (logger-encoding "UTF-8")

               (msb-rsp-file (expand-file-name (concat dst-file-base-name ".build.rsp.msvc") db-path))
               (msb-target-file target-file)
               (msb-flags (list
                           (msvc-env--create-msb-flags "/t:"
                                                       `(("%s"               .       ,target)))
                           (msvc-env--create-msb-flags "/p:"
                                                       `(("Platform=%S"      .       ,platform)
                                                         ("Configuration=%S" .       ,configuration)))
                           (msvc-env--create-msb-flags "/flp:"
                                                       `(("Verbosity=%s"     .       ,(symbol-name msvc-solution-build-report-verbosity))
                                                         ("LogFile=%S"       .       ,log-file)
                                                         ("Encoding=%s"      .       ,logger-encoding)
                                                         ("%s"               .       "NoSummary")))
                           (if msvc-solution-build-report-realtime-display-p
                               (msvc-env--create-msb-flags "/clp:"
                                                           `(("Verbosity=%s" .       ,(symbol-name msvc-solution-build-report-verbosity))))
                             "/noconsolelogger")
                           "/nologo"
                           "/maxcpucount"))

               (process-name "msvc-build")
               ;; bind connection type (use pipe)
               (process-connection-type nil)
               ;; bind encoding system (logfile:utf-8-dos, buffer:utf-8-unix)
               (default-process-coding-system (if msvc-solution-build-report-realtime-display-p default-process-coding-system '(utf-8-dos . utf-8-unix)))
               (display-file (if msvc-solution-build-report-realtime-display-p "" log-file))

               (command msvc-env--invoke-command)
               (command-args (msvc-env--build-msb-command-args version toolset msb-rsp-file display-file)))

          ;; create rsp file(always create)
          (msvc-env--create-msb-rsp-file msb-rsp-file msb-target-file msb-flags)

          (when (get-buffer process-bind-buffer)
            (kill-buffer process-bind-buffer))

          (let ((process (apply #'start-process process-name process-bind-buffer command command-args)))
            (set-process-sentinel process #'msvc--build-solution-sentinel))

          ;; プロセスバッファを最初に表示
          (when (eq msvc-solution-build-report-display-timing 'before)
            (if (eq msvc-solution-build-report-display-target 'other-frame)
                (msvc--switch-to-buffer-other-frame process-bind-buffer)
              (msvc--split-window process-bind-buffer)))

          (with-current-buffer process-bind-buffer
            ;; buffer sentinelで終了検知後に、文字列propertize & read-only化が望ましい
            (setq buffer-read-only t))
          t)
        ))))


(defun msvc-mode-feature-build-solution ()
  (interactive)
  (msvc-mode-feature-build))

(defun msvc-mode-feature-rebuild-solution ()
  (interactive)
  (msvc-mode-feature-build :target "Rebuild"))

(defun msvc-mode-feature-clean-solution ()
  (interactive)
  (msvc-mode-feature-build :target "Clean"))


(defun msvc-mode-feature-build-project ()
  (interactive)
  (msvc-mode-feature-build :project-only-p t))

(defun msvc-mode-feature-rebuild-project ()
  (interactive)
  (msvc-mode-feature-build :project-only-p t :target "Rebuild"))

(defun msvc-mode-feature-clean-project ()
  (interactive)
  (msvc-mode-feature-build :project-only-p t :target "Clean"))




;; mode definitions
(defvar-local msvc--mode-line nil)


(defvar msvc--mode-key-map 
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-i") #'msvc-mode-feature-visit-to-include)
    (define-key map (kbd "M-I") #'msvc-mode-feature-return-from-include)
    (define-key map (kbd "M-[") #'msvc-mode-feature-flymake-goto-prev-error)
    (define-key map (kbd "M-]") #'msvc-mode-feature-flymake-goto-next-error)
    (define-key map (kbd "<f5>") #'msvc-mode-feature-manually-flymake)
    (define-key map (kbd "<C-f5>") #'msvc-mode-feature-build-solution)
    ;; (define-key map (kbd "<f6>") #'msvc-mode-feature-manually-ac-clang-complete)
    ;; (define-key map (kbd "<f7>") #'semantic-force-refresh)
    ;; (define-key map (kbd "C-j") #'msvc-mode-feature-jump-to-project-buffer)
    ;; (define-key map (kbd "C-j") #'msvc-mode-feature-launch-msvs)
    map)
  "MSVC mode key map")


(defun msvc--update-mode-line (version platform configuration)
  (setq msvc--mode-line (format " MSVC%s[%s|%s]" version platform configuration))
  (force-mode-line-update))


(define-minor-mode msvc-mode
  "Microsoft Visual C/C++ mode"
  :lighter msvc--mode-line
  :keymap msvc--mode-key-map
  :group 'msvc
  (if msvc-mode
      (progn
        (if (msvc--evaluate-buffer)
            (let* ((property (msvc-flags--create-project-property msvc--source-code-belonging-db-name))
                   (version (plist-get property :version))
                   (platform (plist-get property :platform))
                   (configuration (plist-get property :configuration)))
              (msvc--update-mode-line version platform configuration))
          (progn
            (msvc--update-mode-line "" "-" "-")
            (message "This buffer don't belonging to the active projects.")
            (msvc-mode-off))))
    (progn
      (msvc--detach-from-project))))


(defun msvc-mode-on ()
  (interactive)
  (msvc-mode 1))

(defun msvc-mode-off ()
  (interactive)
  (msvc-mode 0))



(defun msvc-initialize ()
  (when (msvc-env--initialize)
    (msvc-flags--initialize)

    (add-hook 'after-init-hook
              '(lambda ()
                 (when (file-readable-p msvc--after-init-file)
                   (let ((result (ignore-errors
                                   (load-library msvc--after-init-file)
                                   t)))
                     (unless result
                       (message ".msvc error!")))))
              t)))






(provide 'msvc)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; msvc.el ends here
