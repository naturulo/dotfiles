;;;; package
(require 'package)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;;;; editing setting
(prefer-coding-system 'utf-8)
(setq-default tab-width 2 indent-tabs-mode nil)

;;;; helm
(require 'helm-config)

;;;; auto-complete
(when (require 'auto-complete-config nil t)
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (ac-config-default)
  (setq ac-use-menu-map t)
  (setq ac-ignore-case nil))

;;;; wgrep
(require 'wgrep nil t)

;;;; undohist
(when (require 'undohist nil t)
  (undohist-initialize))

;;;; undotree
(when (require 'undo-tree nil t)
  (define-key global-map (kbd "C-'") 'undo-tree-redo)
  (global-undo-tree-mode))

;;;; cua-mode
(cua-mode t)
(setq cua-enable-cua-keys nil)
(define-key global-map (kbd "C-x SPC") 'cua-set-rectangle-mark)

;;; flycheck
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (magit flycheck wgrep undohist undo-tree org-link-minor-mode helm auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(add-hook 'after-init-hook #'global-flycheck-mode)

;;;; magit
(setq-default magit-auto-revert-mode nil)
(setq vc-handled-backends '())
(eval-after-load "vc" '(remote-hook 'find-file-hooks 'vc-find-file-hook))
(bind-key "C-x m" 'magit-status)
(bind-key "C-c l" 'magit-blame)
(custom-set-faces
 '(magit-diff-added ((t (:background "black" :foreground "green"))))
 '(magit-diff-added-highlight ((t (:background "white" :foreground "green"))))
 '(magit-diff-removed ((t (:background "black" :foreground "blue"))))
 '(magit-diff-removed-highlight ((t (:background "white" :foreground "blue"))))
 '(magit-hash ((t (:foreground "red")))))
