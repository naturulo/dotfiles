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
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
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

;;; install dravula theme
(use-package dracula-theme)
;;; install hide-mode-line
(use-package hide-mode-line
    :hook
    ((neotree-mode imenu-list-minor-mode minimap-mode) . hide-mode-line-mode))
;;; install which-key
(use-package which-key
    :diminish which-key-mode
    :hook (after-init . which-key-mode))
;;; install neotree to view easily directory
(use-package neotree
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
    :diminish
    :hook
    ((prog-mode yaml-mode) . highlight-indent-guides-mode)
    :custom
    (highlight-indent-guides-auto-enabled t)
    (highlight-indent-guides-responsive t)
    (highlight-indent-guides-method 'character)) ; column
;;; View easily rainbow delimiters
(use-package rainbow-delimiters
    :hook
    (prog-mode . rainbow-delimiters-mode))
;;; Easy to use parenthesis
(use-package paren
   :ensure nil
   :hook
   (after-init . show-paren-mode)
   :custom-face
   (show-paren-match ((nil (:background "#44475a" :foreground "#f1fa8c"))))
   :custom
   (show-paren-style 'mixed)
   (show-paren-when-point-inside-paren t)
   (show-paren-when-point-in-periphery t))

