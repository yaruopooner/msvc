;;; -*- mode: emacs-lisp ; coding: utf-8-unix -*-
;;; last updated : 2014/09/25.03:22:15


;;==================================================================================================
;; CEDET(Collection of Emacs Development Environment Tools) setup                                   
;;==================================================================================================


;;------------------------------------------------------------------------------
;; prepare variable setting                                                           
;;------------------------------------------------------------------------------

;; Configuration variables here:


;; ロード前設定が必要な変数, defcustom系変数
(setq semantic-default-submodes
      '(
        ;; Semanticデータベース
        global-semanticdb-minor-mode
        ;; アイドルタイムにsemantic-add-system-includeで追加されたパスグループを再解析
        global-semantic-idle-scheduler-mode
        ;; タグのサマリを表示
        global-semantic-idle-summary-mode
        ;; タグの補完を表示(plugin auto-completeでac-source-semanticを使うのでdisable)
        ;; global-semantic-idle-completions-mode
        ;; タグを装飾
        global-semantic-decoration-mode
        ;; 現在カーソルでポイントされているfunctionの宣言をハイライト
        global-semantic-highlight-func-mode
        ;; ?
        global-semantic-mru-bookmark-mode
        ;; ?
        global-semantic-stickyfunc-mode
        ))


;; アイドルタイムにsemantic-add-system-includeで追加されたパスグループをパースする
(setq semantic-idle-work-update-headers-flag t)

;; アイドルタイムを指定

;; Time in seconds of idle before scheduling events.
;; This time should be short enough to ensure that idle-scheduler will be
;; run as soon as Emacs is idle.
(setq semantic-idle-scheduler-idle-time 10)

;; Time in seconds of idle before scheduling big work.
;; This time should be long enough that once any big work is started, it is
;; unlikely the user would be ready to type again right away.
(setq semantic-idle-scheduler-work-idle-time 60)



;;------------------------------------------------------------------------------
;; load & enable modules                                                        
;;------------------------------------------------------------------------------


;; Load CEDET
(require 'cedet)




;; プロジェクト管理：有効化
;; Enable the Project management system(Emacs Development Environment)
(global-ede-mode 1)


;; セマンティック：有効化
;; !!!!!!!重要!!!!!!!
;; これを実行する前に semantic 関連のパラメーターは必ずセットしておく事
;; Enable Semantic Mode
(semantic-mode 1)


;; !!!!!!!重要!!!!!!!
;; 必ず ede => semantic の順番で有効化する事
;; 逆になったりすると ede-cpp-root-projectなどがうまく動作しない
;; エラーなどが一切で出力されないので、わかりずらいが
;; :include-path や :system-include-path などが設定されているにもかかわらず
;; semantic.cache が生成されない



;;------------------------------------------------------------------------------
;; ede                                                           
;;------------------------------------------------------------------------------



;;------------------------------------------------------------------------------
;; semantic                                                           
;;------------------------------------------------------------------------------


;; (setq semanticdb-project-roots '("~/"))
;; semantic cache files location(指定パスに生成されたcacheファイルが置かれる, 指定がない場合は ~/.emacs.d/semanticdb)
(setq semanticdb-default-save-directory (locate-user-emacs-file "semanticdb"))
(setq srecode-map-save-file (locate-user-emacs-file "srecode/srecode-map"))

;; semantic database生成用にパースさせるプロジェクトルートディレクトリを設定
;; (semantic-add-system-include "/usr/include" 'c-mode)
;; (semantic-remove-system-include "/usr/include" 'c-mode)


;; (setq ac-sources (append ac-sources '(ac-source-semantic)))
;; (setq ac-sources '(ac-source-semantic))





;;------------------------------------------------------------------------------
;; speedbar                                                           
;;------------------------------------------------------------------------------


;; speedbar フレーム設定
(custom-set-variables
 '(speedbar-frame-parameters
   '(
     (minibuffer)
     (width . 50)
     (border-width . 0)
     (menu-bar-lines . 0)
     (tool-bar-lines . 0)
     (unsplittable . t)
     (left-fringe . 0)
     )))


;; speedbar automatic update
;; (speedbar-toggle-updates)


;; (require 'semantic-imenu)

;; (setq semantic-imenu-summary-function
;;      (lambda (tag)
;;        (semantic-format-tag-summarize tag nil t)))





(provide 'cedet.config)
;;--------------------------------------------------------------------------------------------------
;; EOF
