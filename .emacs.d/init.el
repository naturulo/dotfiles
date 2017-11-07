;;;; Initial Settings
;;; package
(require 'package)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
;;; editing setting
(prefer-coding-system 'utf-8)
(setq-default tab-width 2 indent-tabs-mode nil)
;;; helm
(require 'helm-config)
;;; auto-complete
(when (require 'auto-complete-config nil t)
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (ac-config-default)
  (setq ac-use-menu-map t)
  (setq ac-ignore-case nil))
;;; wgrep
(require 'wgrep nil t)
;;; undohist
(when (require 'undohist nil t)
  (undohist-initialize))
;;; undotree
(when (require 'undo-tree nil t)
  (define-key global-map (kbd "C-'") 'undo-tree-redo)
  (global-undo-tree-mode))
;;; cua-mode
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
    (cider clj-refactor clojure-mode company smartparens ac-slime slime use-package magit flycheck wgrep undohist undo-tree org-link-minor-mode helm auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-diff-added ((t (:background "black" :foreground "green"))))
 '(magit-diff-added-highlight ((t (:background "white" :foreground "green"))))
 '(magit-diff-removed ((t (:background "black" :foreground "blue"))))
 '(magit-diff-removed-highlight ((t (:background "white" :foreground "blue"))))
 '(magit-hash ((t (:foreground "red")))))
(add-hook 'after-init-hook #'global-flycheck-mode)
;;; magit
(setq-default magit-auto-revert-mode nil)
(setq vc-handled-backends '())
(eval-after-load "vc" '(remote-hook 'find-file-hooks 'vc-find-file-hook))
(bind-key "C-x m" 'magit-status)
(bind-key "C-c l" 'magit-blame)

;;;; Common Lisp dev-env settings
;;; slime
(require 'slime)
(setq inferior-lisp-program "sbcl")
(slime-setup '(slime-repl slime-fancy slime-banner))
(add-hook 'slime-mode-hook #'smartparens-mode)
;;; ac-slime
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete" '(add-to-list 'ac-modes 'slime-repl-mode))

;;;; Clojure dev-env settings
;;; clojure-mode
(use-package clojure-mode
  :init
  (add-hook 'clojure-mode-hook #'yas-minor-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'smartparens-mode))
;;; cider
(use-package cider
  :init
  (add-hook 'cider-mode-hook #'clj-refactor-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  :diminish subword-mode
  :config
  (setq nrepl-log-messages t
        cider-repl-display-in-current-window t
        cider-repl-use-clojure-font-lock t
        cider-prompt-save-file-on-load 'always-save
        cider-font-lock-dynamically '(macro core function var)
        cider-overlays-use-font-lock t)
  (cider-repl-toggle-pretty-printing))
;;; clj-refactor
(use-package clj-refactor
  :diminish clj-refactor-mode
  :config (cljr-add-keybindings-with-prefix "C-c j"))
;;; company-mode
(use-package company
  :config
  (global-company-mode)
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2
        company-selection-wrap-around t)
  (bind-keys :map company-mode-map
             ("C-i" . company-complet))
  (bind-keys :map company-active-map
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous)
             ("C-s" . company-search-words-regexp))
  (bind-keys :map company-search-map
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous)))

