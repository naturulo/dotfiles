;;;; Initial Settings
;;;; https://uwabami.github.io/cc-env/Emacs.html
;;;; https://qiita.com/Ladicle/items/feb5f9dce9adf89652cf
;;; editing setting
(prefer-coding-system 'utf-8)
(setq-default tab-width 2 indent-tabs-mode nil)
;;; don't show some mode
(setq inhibit-startup-screen t
      inhibit-startup-message t)
;(tool-bar-mode -1)
;(scroll-bar-mode -1)
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
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(use-package bind-key
  :ensure t)
(use-package diminish
  :ensure t)
;;; install quelpa and quelpa-use-package
(use-package quelpa-use-package
	     :ensure t
	     :init
	     (setq quelpa-upgrade-p nil
		   quelpa-checkout-melpa-p nil
		   quelpa-update-melpa-p nil
		   quelpa-melpa-recipe-stores nil))
;;; measure uptime
(use-package esup :ensure t)
;;; delete empty files
(defun my:delete-file-if-no-contents ()
  (when (and (buffer-file-name (current-buffer))
             (= (point-min) (point-max)))
    (delete-file
     (buffer-file-name (current-buffer)))))
(if (not (memq 'my:delete-file-if-no-contents after-save-hook))
    (setq after-save-hook
          (cons 'my:delete-file-if-no-contents after-save-hook)))
;;; don't erase scratch. if erased, regenerate
(defun my:make-scratch (&optional arg)
  (interactive)
  (progn
    ;; "*scratch*" を作成して buffer-list に放り込む
    (set-buffer (get-buffer-create "*scratch*"))
    (funcall initial-major-mode)
    (erase-buffer)
    (when (and initial-scratch-message (not inhibit-startup-message))
      (insert initial-scratch-message))
    (or arg
        (progn
          (setq arg 0)
          (switch-to-buffer "*scratch*")))
    (cond ((= arg 0) (message "*scratch* is cleared up."))
          ((= arg 1) (message "another *scratch* is created")))))

(defun my:buffer-name-list ()
  (mapcar (function buffer-name) (buffer-list)))
(add-hook 'kill-buffer-query-functions
          ;; *scratch* バッファで kill-buffer したら内容を消去するだけにする
          (function (lambda ()
                      (if (string= "*scratch*" (buffer-name))
                          (progn (my:make-scratch 0) nil)
                        t))))
(add-hook 'after-save-hook
          ;; *scratch* バッファの内容を保存したら
          ;; *scratch* バッファを新しく作る.
          (function
           (lambda ()
             (unless (member "*scratch*" (my:buffer-name-list))
               (my:make-scratch 1)))))
;;; install doom theme
(use-package doom-themes
    :custom
    (doom-themes-enable-italic t)
    (doom-themes-enable-bold t)
    :custom-face
    (doom-modeline-bar ((t (:background "#6272a4"))))
    :config
    (load-theme 'doom-dracula t)
    (doom-themes-neotree-config)
    (doom-themes-org-config))
(use-package doom-modeline
      :custom
      (doom-modeline-buffer-file-name-style 'truncate-with-project)
      (doom-modeline-icon t)
      (doom-modeline-major-mode-icon nil)
      (doom-modeline-minor-modes nil)
      :hook
      (after-init . doom-modeline-mode)
      :config
      (line-number-mode 0)
      (column-number-mode 0)
      (doom-modeline-def-modeline 'main
    '(bar workspace-number window-number evil-state god-state ryo-modal xah-fly-keys matches buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info persp-name lsp github debug minor-modes input-method major-mode process vcs checker)))
(use-package hide-mode-line
    :hook
    ((neotree-mode imenu-list-minor-mode minimap-mode) . hide-mode-line-mode))
(use-package which-key
    :diminish which-key-mode
    :hook (after-init . which-key-mode))
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
(use-package highlight-indent-guides
    :diminish
    :hook
    ((prog-mode yaml-mode) . highlight-indent-guides-mode)
    :custom
    (highlight-indent-guides-auto-enabled t)
    (highlight-indent-guides-responsive t)
    (highlight-indent-guides-method 'character)) ; column
(use-package rainbow-delimiters
    :hook
    (prog-mode . rainbow-delimiters-mode))
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (doom-themes quelpa-use-package esup diminish))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-modeline-bar ((t (:background "#6272a4"))))
 '(show-paren-match ((nil (:background "#44475a" :foreground "#f1fa8c")))))
