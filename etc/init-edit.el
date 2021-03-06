;;; init-edit.el --- Another improve edit -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;----------------------------------------------------------------
;; beacon 像一个彗星尾巴一样闪烁当前行
(use-package beacon
  :ensure t
  :diminish
  :init
  (setq-default beacon-lighter ""
                beacon-size 20)
  :config
  (beacon-mode 1))


;;----------------------------------------------------------------
;; Zap and browse-kill-ring 为 M-y 提供更好的交互
(use-package browse-kill-ring
  :ensure t
  :diminish
  :init
  (setq browse-kill-ring-separator "\f")

  ;; Zap *up* to char is a handy pair for zap-to-char
  (autoload 'zap-up-to-char "misc" "Kill up to, but not including ARGth occurrence of CHAR.")
  :bind (("M-Y" . browse-kill-ring)
         ("M-z" . zap-up-to-char)
         :map browse-kill-ring-mode-map
         ("C-g" . browse-kill-ring-quit)
         ("M-n" . browse-kill-ring-forward)
         ("M-p" . browse-kill-ring-previous))
  :config
  ;; 会将分隔符号 "^L" 替换成一条水平线
  (with-eval-after-load 'page-break-lines
    (push 'browse-kill-ring-mode page-break-lines-modes)))


;;----------------------------------------------------------------
;; csharp
(use-package csharp-mode
  :ensure t
  :config
  (defun my-csharp-mode-hook ()
    (electric-pair-local-mode 1) ;; Emacs 25
    )
  (add-hook 'csharp-mode-hook 'my-csharp-mode-hook))


;;----------------------------------------------------------------
;; expand-region
(use-package expand-region
  :ensure t
  :diminish
  :bind ("C-=" . er/expand-region))


;;----------------------------------------------------------------
;; focus ( M-x focus-mode)
(use-package focus
  :ensure t
  :defer t)


;;----------------------------------------------------------------
;; goto-line-preview
;; (use-package goto-line-preview
;;   :ensure t
;;   :bind ([remap goto-line] . goto-line-preview)
;;   :config
;;   (when (fboundp 'display-line-numbers-mode)
;;     (defun yantree/with-display-line-numbers (f &rest args)
;;       (let ((display-line-numbers t))
;;         (apply f args)))
;;     (advice-add 'goto-line-preview :around #'yantree/with-display-line-numbers)))


;;----------------------------------------------------------------
;; hungry-delete
(use-package hungry-delete
  :ensure t
  :diminish hungry-delete-mode
  :config
  (global-hungry-delete-mode 1))


;;----------------------------------------------------------------
;; move-dup
(use-package move-dup
  :ensure t
  :config
  (global-set-key (kbd "M-<up>") 'move-dup-move-lines-up)
  (global-set-key (kbd "M-<down>") 'move-dup-move-lines-down)
  (global-set-key (kbd "M-S-<up>") 'move-dup-duplicate-up)
  (global-set-key (kbd "M-S-<down>") 'move-dup-duplicate-down))


;;----------------------------------------------------------------
;;  multiple-cursors
(use-package multiple-cursors
  :ensure t
  :diminish
  :init
  (setq mc/list-file "~/.emacs.d/lib/.mc-lists.el")  ;;改变配置文件位置的位置
  :bind (;; From active region to multiple cursors:
         ("C-S-c C-S-c"  . mc/edit-lines)
         ("C-S-c C-S-a"  . mc/edit-beginnings-of-lines)
         ("C-S-c C-S-e"  . mc/edit-ends-of-lines)
         ;; multiple-cursors
         ("C-<"          . mc/mark-previous-like-this)
         ("C->"          . mc/mark-next-like-this)
         ("C-+"          . mc/mark-next-like-this)
         ("C-c C-<"      . mc/mark-all-like-this)
         ("C-r"          . mc/mark-all-dwim)))


;;----------------------------------------------------------------
;; smartparens
(use-package smartparens
  :ensure t
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config)
  (require 'smartparens-html)
  :bind (("C-M-f"   . sp-forward-sexp)
         ("C-M-b"   . sp-backward-sexp)

         ("C-M-d"   . sp-down-sexp)
         ("C-M-u"   . sp-backward-up-sexp)

         ("C-M-n"   . sp-up-sexp)
         ("C-M-p"   . sp-backward-down-sexp)

         ("C-M-k"   . sp-kill-sexp)
         ("C-M-w"   . sp-copy-sexp)
         ("C-M-SPC" . sp-mark-sexp)

         ("C-M-t"   . sp-transpose-sexp)

         ("M-("     . sp-wrap-round)
         ("M-<delete>" . sp-unwrap-sexp)))


;;----------------------------------------------------------------
;; symbol-overlay 高亮同一个symbol,并对其编辑(例如:下面的 define-key)
(use-package symbol-overlay
  :ensure t
  :diminish symbol-overlay-mode
  :hook ((prog-mode html-mode yaml-mode conf-mode) . symbol-overlay-mode)
  :init
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<up>") 'symbol-overlay-jump-prev)
    (define-key map (kbd "M-p") 'symbol-overlay-jump-prev)
    (define-key map (kbd "<down>") 'symbol-overlay-jump-next)
    (define-key map (kbd "M-n") 'symbol-overlay-jump-next)
    (define-key map (kbd "<") 'symbol-overlay-jump-first)
    (define-key map (kbd ">") 'symbol-overlay-jump-last)
    (define-key map (kbd "r") 'symbol-overlay-rename)
    (define-key map (kbd "s") 'symbol-overlay-save-symbol)
    (setq symbol-overlay-map map))
  (setq symbol-overlay-idle-time 0.01)
  :custom-face
  (symbol-overlay-default-face ((t (:inherit 'background-color :underline t))))
  :bind ("M-i" . symbol-overlay-put))


;;----------------------------------------------------------------
;; volatile-highlights  高亮显示一些操作(例如：C-y) 很有用
(use-package volatile-highlights
  :ensure t
  :diminish
  :config
  (volatile-highlights-mode 1))


;;----------------------------------------------------------------
;; wgrep and rg
(use-package wgrep
  :ensure t
  :defer t
  :init
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t))

;; `ripgrep'
(when (executable-find "rg")
  (use-package rg
    :ensure t
    :defer t
    :config
    (setq rg-group-result t
          rg-show-columns t)))


;;----------------------------------------------------------------
;; whole-line-or-region (Cut/copy the current line if no region is active)
(use-package whole-line-or-region
  :ensure t
  :diminish whole-line-or-region-local-mode
  :hook (after-init . whole-line-or-region-global-mode)
  :bind (("M-;"     . whole-line-or-region-comment-dwim)))


;;----------------------------------------------------------------
;; yasnippet
(use-package yasnippet
  :ensure t
  :commands (yas-reload-all)
  :diminish yas-minor-mode
  :hook ((org-mode prog-mode snippet-mode) . yas-minor-mode)
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/lib/snippets"))
  :config
  (yas-reload-all))

(provide 'init-edit)
;;; init-edit.el ends here
