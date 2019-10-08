;;; -*- lexical-binding: t; -*-

;;----------------------------------------------------------------------------
;; C/C++ Mode
(use-package cc-mode
  :ensure nil
  :bind (:map c-mode-base-map
              ("C-c c" . compile))
  :hook (c-mode-common . (lambda ()
                           (c-set-style "bsd")
                           (setq tab-width 4)
                           (setq c-basic-offset 4)))
  ;; :config
  ;; (use-package modern-cpp-font-lock
  ;;   :diminish
  ;;   :init (modern-c++-font-lock-global-mode t))
  )

(provide 'init-c)
;;; init-c.el ends here
