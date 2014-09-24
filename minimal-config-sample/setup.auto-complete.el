;;; -*- mode: emacs-lisp ; coding: utf-8-unix -*-
;;; last updated : 2014/09/25.03:22:43


;;==================================================================================================
;; auto-complete setup                                                                                      
;;==================================================================================================


;;------------------------------------------------------------------------------
;; prepare variable setting                                                           
;;------------------------------------------------------------------------------


;; load path addition
(add-to-list 'load-path (locate-user-emacs-file "auto-complete/"))




;;------------------------------------------------------------------------------
;; load                                                                         
;;------------------------------------------------------------------------------


;; Load Module
(require 'auto-complete-config)


;; add feature
(defvar ac-quick-help-delay-from-show-menu 0.1)


(defun ac-set-show-menu-delay (show-menu-delay &optional quick-help-delay-from-show-menu)
  (interactive)

  (setq ac-auto-show-menu show-menu-delay)
  (when quick-help-delay-from-show-menu
    (setq ac-quick-help-delay-from-show-menu quick-help-delay-from-show-menu))
  (setq ac-quick-help-delay (+ ac-auto-show-menu ac-quick-help-delay-from-show-menu)))



;;------------------------------------------------------------------------------
;; basic setting                                                       
;;------------------------------------------------------------------------------


;; function call
(ac-config-default)


;; 補完機能手動呼び出しキーバインド
(global-unset-key (kbd "M-\\"))
(ac-set-trigger-key "M-\\")


;; 補完開始有効文字数：指定文字数以上で補完が作動, nilで作動しない
(setq ac-auto-start nil)


;; 補完可能になるまでの遅延時間(sec)
(setq ac-delay 0.1)

;; 補完メニュー表示開始までの時間(sec)：nilで表示なし
;; (setq ac-auto-show-menu 0.8)

;; 補完メニュー表示時のみC-n/C-pで補完候補を選択する
(setq ac-use-menu-map t)

;; 補完候補選択中にポップアプされるヘルプ on/off
(setq ac-use-quick-help t)

;; ヘルプ表示開始までの時間(sec)
;; NOTICE:
;; ac-auto-show-menu 以下の値にするとポップアップ表示が補完メニュー表示位置と被ってしまう
;; (setq ac-quick-help-delay (+ ac-auto-show-menu 0.1))

;; 補完メニューの高さ行数：一度に表示される補完対象数
(setq ac-menu-height 20)

;; 大文字・小文字を区別： t :しない, nil :する, 'smart :補完対象に大文字が含まれる場合のみ区別する
(setq ac-ignore-case 'nil)


;; 補完メニュー表示開始時間とヘルプ表示時間の設定
(ac-set-show-menu-delay 0.1 0.1)


;; face-name
;; ac-completion-face   インライン補完の文字色
;; ac-candidate-face    補完メニューの背景色
;; ac-selection-face    補完メニューの選択色

;; インライン補完の文字色
(set-face-foreground 'ac-completion-face "darkgray")
;; (set-face-background 'ac-completion-face "lightgray")
(set-face-underline 'ac-completion-face "white")

;; 補完メニューの背景色
(set-face-foreground 'ac-candidate-face "black")
(set-face-background 'ac-candidate-face "lightgray")
;; (set-face-underline 'ac-candidate-face "black")

;; 補完メニューの選択色
(set-face-foreground 'ac-selection-face "white")
(set-face-background 'ac-selection-face "lightgray")
;; (set-face-underline 'ac-selection-face "black")



(global-auto-complete-mode t)



;;------------------------------------------------------------------------------
;; auto complete behavior setting                                               
;;------------------------------------------------------------------------------



;; 標準的な情報源の名前と説明を列挙します.
;; それぞれ利用したいものをピックアップして ac-sources に設定してください.
;; 
;; |--------------------------------------+---------------------------------------------------------------------------------------|
;; | 名前                                 | 機能                                                                                  |
;; |--------------------------------------+---------------------------------------------------------------------------------------|
;; | ac-source-dictionary                 | メジャーモード辞書・拡張子辞書                                                        |
;; | ac-source-words-in-buffer            | 現在のバッファ内の単語を補完                                                          |
;; | ac-source-words-in-all-buffer        | 全てのバッファ内の単語を補完                                                          |
;; | ac-source-words-in-same-mode-buffers | 同じメジャーモードのバッファ内の単語を補完。プログラミングに便利                      |
;; | ac-source-filename                   | ファイル名補完                                                                        |
;; | ac-source-semantic                   | CEDET:Semanticのための情報源です。C/C++でメンバー名補完として利用                     |
;; | ac-source-semantic-raw               | ac-source-semantic と違って、この情報源は生の名前空間でシンボルを補完するのに使います |
;; | ac-source-gtags                      | gtagsの補完(cc-mode は defaultで組み込まれている)                                     |
;; | ac-source-symbols                    | Lispのシンボルを補完。Emacs Lispを書く人向け                                          |
;; | ac-source-abbrev                     | abbrevを補完                                                                          |
;; | ac-source-imenu                      | imenuの補完                                                                           |
;; | ac-source-yasnippet                  | yasnippetの補完(cc-mode は defaultで組み込まれている)                                 |
;; |--------------------------------------+---------------------------------------------------------------------------------------|
;;


;; C/C++ Mode 時の補完ソース拡張
(add-hook 'c-mode-common-hook
          '(lambda ()
             (setq ac-sources '(
                                ;; ac-source-dictionary
                                ac-source-semantic
                                ac-source-semantic-raw
                                ac-source-imenu
                                ;; ac-source-words-in-buffer
                                ;; ac-source-words-in-same-mode-buffers
                                )))
          ;; add to hook list end
          t)






(provide 'setup.auto-complete)
;;--------------------------------------------------------------------------------------------------
;; EOF
