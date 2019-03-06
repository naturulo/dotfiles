;;;; Initial Settings
;;;; https://uwabami.github.io/cc-env/Emacs.html
;;;; https://qiita.com/Ladicle/items/feb5f9dce9adf89652cf
;;; editing setting
(prefer-coding-system 'utf-8)
(setq-default tab-width 2 indent-tabs-mode nil)
;;; don't show some mode
(setq inhibit-startup-screen t
      inhibit-startup-message t)
(menu-bar-mode -1)
(column-number-mode -1)
;;; show line number
(global-linum-mode t)
(setq linum-format "%5d")
;;; don't ring bells
(setq ring-bell-function 'ignore)
;;; highlight current row
(global-hl-line-mode t)
;;; highlight parans
(show-paren-mode t)
(setq show-paren-style 'mixed)
;;; auto completion parens
(electric-pair-mode t)
;;; enable y-or-n instead of yes-or-no
(fset 'yes-or-no-p 'y-or-n-p)
;;; truncating lines
(set-default 'truncate-lines t)
(set-default 'truncate-partial-width-windows t)
;;; always follow symlinks
(setq vc-follow-symlinks t)
;;; don't create systemfiles
(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq auto-save-list-file-prefix nil)
;;; display time in mode-line
(display-time)

;;;; Package
;;; package settigns
(require 'package nil t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
        (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;;; delete empty files
(defun my:delete-file-if-no-contents ()
  (when (and (buffer-file-name (current-buffer))
             (= (point-min) (point-max)))
    (delete-file
     (buffer-file-name (current-buffer)))))
(if (not (memq 'my:delete-file-if-no-contents after-save-hook))
    (setq after-save-hook
          (cons 'my:delete-file-if-no-contents after-save-hook)))

;;;; install doom-theme
;(use-package doom-themes
;  :ensure t
;  :custom
;  (doom-themes-enable-italic t)
;  (doom-themes-enable-bold t)
;  :custom-face
;  (doom-modeline-bar ((t (:background "#6272a4"))))
;  :config
;  (load-theme 'doom-dracula t)
;  (doom-themes-neotree-config)
;  (doom-themes-org-config))
;;;; install doom-modeline
;(use-package doom-modeline
;  :ensure t
;  :custom
;  (doom-modeline-buffer-file-name-style 'truncate-with-project)
;  (doom-modeline-icon t)
;  (doom-modeline-major-mode-icon nil)
;  (doom-modeline-minor-modes nil)
;  :hook
;  (after-init . doom-modeline-mode)
;  :config
;  (line-number-mode 0)
;  (column-number-mode 0)
;  (doom-modeline-def-modeline 'main
;  '(bar workspace-number window-number evil-state god-state ryo-modal xah-fly-keys matches buffer-info remote-host buffer-position parrot selection-info)
;  '(misc-info persp-name lsp github debug minor-modes input-method major-mode process vcs checker)))
  
;;; install hide-mode-line
(use-package hide-mode-line
  :ensure t
  :hook
  ((neotree-mode imenu-list-minor-mode minimap-mode) . hide-mode-line-mode))
;;; install which-key
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :hook (after-init . which-key-mode))
;;; install neotree to view easily directory
(use-package neotree
  :ensure t
  :after
  projectile
  :commands
  (neotree-show neotree-hide neotree-dir neotree-find)
  :custom
  (neo-theme 'nerd2)
  :bind
  ("<f9>" . neotree-projectile-toggle)
  :preface
  (defun neotree-projectile-toggle ()
    (interactive)
    (let ((project-dir
       (ignore-errors
       ;;; Pick one: projectile or find-file-in-project
         (projectile-project-root)
         ))
      (file-name (buffer-file-name))
      (neo-smart-open t))
  (if (and (fboundp 'neo-global--window-exists-p)
       (neo-global--window-exists-p))
      (neotree-hide)
    (progn
      (neotree-show)
      (if project-dir
      (neotree-dir project-dir))
      (if file-name
	  (neotree-find file-name)))))))
;;; install highlight-indent-guides to view indentation
(use-package highlight-indent-guides
  :ensure t
  :diminish
  :hook
  ((prog-mode yaml-mode) . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-auto-enabled t)
  (highlight-indent-guides-responsive t)
  (highlight-indent-guides-method 'character)) ; column
;;; View easily rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-themes-enable-bold t)
 '(doom-themes-enable-italic t)
 '(package-selected-packages
   (quote
    (rainbow-delimiters highlight-indent-guides neotree which-key dracula-theme use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-modeline-bar ((t (:background "#6272a4"))))
 '(show-paren-match ((nil (:background "#44475a" :foreground "#f1fa8c")))))
