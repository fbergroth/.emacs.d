;;; init.el --- user-init-file                    -*- lexical-binding: t -*-
;;; Early birds
(progn ;     startup
  (defvar before-user-init-time (current-time)
    "Value of `current-time' when Emacs begins loading `user-init-file'.")
  (message "Loading Emacs...done (%.3fs)"
           (float-time (time-subtract before-user-init-time
                                      before-init-time)))
  (setq user-init-file (or load-file-name buffer-file-name))
  (setq user-emacs-directory (file-name-directory user-init-file))
  (message "Loading %s..." user-init-file)
  (setq package-enable-at-startup nil)
  (setq inhibit-startup-buffer-menu t)
  (setq inhibit-startup-screen t)
  (setq inhibit-startup-echo-area-message "locutus")
  (setq initial-buffer-choice t)
  (setq initial-scratch-message "")
  (setq load-prefer-newer t)
  (setq enable-recursive-minibuffers t)
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (blink-cursor-mode 0)
  (fset 'yes-or-no-p #'y-or-n-p)
  (setq ring-bell-function 'ignore)
  (defmacro fn! (&rest body) `(lambda () (interactive) ,@body)))

(progn ;    `borg'
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require  'borg)
  (borg-initialize))

(progn ;    `use-package'
  (require  'use-package)
  (setq use-package-verbose t))

(use-package auto-compile
  :demand t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  (setq auto-compile-display-buffer               nil)
  (setq auto-compile-mode-line-counter            t)
  (setq auto-compile-source-recreate-deletes-dest t)
  (setq auto-compile-toggle-deletes-nonlib-dest   t)
  (setq auto-compile-update-autoloads             t)
  (add-hook 'auto-compile-inhibit-compile-hook
            'auto-compile-inhibit-compile-detached-git-head))

(use-package no-littering)

(use-package epkg
  :defer t
  :init (setq epkg-repository
              (expand-file-name "var/epkgs/" user-emacs-directory)))

(use-package custom
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package server
  :config
  (unless (server-running-p)
    (server-mode)))

(progn ;     startup
  (message "Loading early birds...done (%.3fs)"
           (float-time (time-subtract (current-time)
                                      before-user-init-time))))

;;; Long tail

(use-package dash
  :config (dash-enable-font-lock))

(use-package diff-hl
  :config
  (setq diff-hl-draw-borders nil)
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh t))

(use-package dired
  :defer t
  :config (setq dired-listing-switches "-alh"))

(use-package eldoc
  :config (global-eldoc-mode))

(use-package evil
  :init
  (setq evil-cross-lines t
	evil-ex-substitute-global t
	evil-want-C-u-scroll t
	evil-want-C-i-jump nil
	evil-want-C-w-in-emacs-state t
        evil-respect-visual-line-mode t)
  (evil-mode)
  (setq-default evil-symbol-word-search t)
  :config
  (define-key evil-normal-state-map (kbd "M-.") nil))

(use-package evil-collection
  :demand t
  :config (evil-collection-init))

(use-package evil-anzu
  :config
  (setq anzu-cons-mode-line-p nil))

(use-package evil-multiedit
  :demand t
  :config
  (setq evil-multiedit-follow-matches t)
  (evil-multiedit-default-keybinds))

(use-package evil-surround
  :init
  (global-evil-surround-mode)
  (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)
  (evil-define-key 'visual evil-surround-mode-map "S" 'evil-substitute))

(use-package evil-visualstar
  :init
  (global-evil-visualstar-mode))

(use-package git-commit
  :defer t
  :config
  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell t))

(use-package help
  :defer t
  :config (temp-buffer-resize-mode))

(progn ;    `isearch'
  (setq isearch-allow-scroll t))

(use-package lisp-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'reveal-mode)
  (defun indent-spaces-mode ()
    (setq indent-tabs-mode nil))
  (add-hook 'lisp-interaction-mode-hook #'indent-spaces-mode))

(use-package magit
  :defer t
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1
        magit-diff-refine-hunk 'all)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append))

(use-package man
  :defer t
  :config (setq Man-width 80))

(use-package paren
  :config (show-paren-mode))

(use-package prog-mode
  :config
  (defun indicate-buffer-boundaries-left ()
    (setq indicate-buffer-boundaries 'left))
  (add-hook 'prog-mode-hook #'indicate-buffer-boundaries-left))

(use-package recentf
  :demand t
  :init (recentf-mode)
  :config (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:"))

(use-package savehist
  :config (savehist-mode))

(use-package saveplace
  :config (save-place-mode))

(use-package simple
  :config
  (line-number-mode)
  (column-number-mode)
  (size-indication-mode))

(progn ;    `text-mode'
  (add-hook 'text-mode-hook #'indicate-buffer-boundaries-left))

(use-package tramp
  :defer t
  :config
  (add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
  (add-to-list 'tramp-default-proxies-alist '("localhost" nil nil))
  (add-to-list 'tramp-default-proxies-alist
               (list (regexp-quote (system-name)) nil nil)))

(use-package general
  :config
  (setq general-default-prefix "SPC"
        general-default-states '(normal motion)))

(defun my-init-file ()
  (interactive)
  (find-file user-init-file))

(defun my-buffer-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(general-define-key
 "u"   'universal-argument
 "fi"  'my-init-file
 "bd"  'kill-this-buffer
 "bs"  'my-buffer-scratch
 "bm"  'view-echo-area-messages
 "d"   'dired-jump
 "TAB" 'mode-line-other-buffer)

(use-package eyebrowse
  :config
  (eyebrowse-setup-opinionated-keys)
  (eyebrowse-mode)
  (eyebrowse-switch-to-window-config-1))

(use-package spaceline-config
  :config
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (setq powerline-default-separator 'alternate)
  (setq spaceline-window-numbers-unicode t
        spaceline-workspace-numbers-unicode t)
  (spaceline-spacemacs-theme))

(use-package which-key
  :config
  (which-key-mode)
  ;; (setq which-key-description-replacement-alist
  ;;       '(("Prefix Command" . "prefix")
  ;;         ("\\`\\?\\?\\'"   . "λ")
  ;;         ("/body\\'"       . "|=")
  ;;         ("projectile-"    . "")))
  )

(use-package evil-magit
  :after magit
  :defer t)

(use-package ediff
  :config
  (progn
    (setq ediff-diff-options "-w"
	  ediff-split-window-function 'split-window-horizontally
	  ediff-window-setup-function 'ediff-setup-windows-plain)
    (with-eval-after-load 'winner
      (add-hook 'ediff-after-quit-hook-internal #'winner-undo))))


(use-package evil-ediff
  :config
  (evil-ediff-init))


(use-package flycheck
  :general
  (:keymaps 'flycheck-mode-map
	    "e"  '(:ignore t :which-key "errors")
	    "ev" 'flycheck-verify-setup
	    "el" 'flycheck-list-errors
	    "ey" 'flycheck-copy-errors-as-kill)
  :diminish flycheck-mode
  :init
  (defun pkg-info-version-info (_arg) "hack")
  (global-flycheck-mode)
  :config
  (evil-set-initial-state 'flycheck-error-list-mode 'motion)
  (evil-make-overriding-map flycheck-error-list-mode-map 'motion))


(use-package flycheck-pos-tip
  :after flycheck
  :init
  (flycheck-pos-tip-mode))


(use-package elide-head
  :init (add-hook 'prog-mode-hook #'elide-head))


(use-package elisp-slime-nav
  :general
  (:keymaps 'emacs-lisp-mode-map
	    "h" 'elisp-slime-nav-describe-elisp-thing-at-point
	    "g" 'elisp-slime-nav-find-elisp-thing-at-point)
  :diminish elisp-slime-nav-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode))


(use-package lisp-extra-font-lock
  :disabled t
  :init (lisp-extra-font-lock-global-mode))

(use-package cl-lib-highlight
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook #'cl-lib-highlight-initialize)
    (add-hook 'emacs-lisp-mode-hook #'cl-lib-highlight-warn-cl-initialize)))

(use-package macrostep
  :general
  (:keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
	    "m" 'macrostep-expand)
  :config
  (evil-set-initial-state 'macrostep-mode 'motion)
  (evil-make-overriding-map macrostep-keymap 'motion))

(use-package flycheck-cask
  :defer t
  :init (add-hook 'flycheck-mode-hook #'flycheck-cask-setup))

(use-package flycheck-package
  :init
  (with-eval-after-load 'flycheck
    (flycheck-package-setup)))

(use-package hl-line
  :init (global-hl-line-mode))

(use-package hl-todo
  :init
  (global-hl-todo-mode)
  :config
  (setq hl-todo-activate-in-modes '(prog-mode)))


(use-package yaml-mode
  :defer t)


(use-package autorevert
  :init
  (global-auto-revert-mode)
  :config
  (setq auto-revert-verbose nil
        global-auto-revert-non-file-buffers t))

(use-package image-file
  :init (auto-image-file-mode))

(use-package whitespace-cleanup-mode
  :init (global-whitespace-cleanup-mode)
  :diminish whitespace-cleanup-mode)


(use-package ivy
  :general
  (:prefix nil :states nil :keymaps 'ivy-minibuffer-map
           "<escape>" 'minibuffer-keyboard-quit)

  :init (ivy-mode)
  :config
  (setq ivy-use-virtual-buffers t
        ivy-re-builders-alist '((t . ivy--regex-fuzzy))))

(use-package ivy-hydra)

(use-package hydra)

(use-package counsel
  :general
  ("SPC" 'counsel-M-x
   "r"  'ivy-resume
   "bb"  'ivy-switch-buffer
   "fg"  'counsel-git
   "ff"  'counsel-find-file
   "fl"  'counsel-file-jump
   "fL"  'counsel-locate
   "fr"  'counsel-recentf
   "ss"  'counsel-ag
   "sg"  'counsel-git-grep)
  :init (counsel-mode)
  :config
  (setq counsel-git-cmd "git ls-files --recurse-submodules --")
  (setq counsel-rg-base-command "rg -i --no-heading --line-number --color never --max-columns 120 %s ."))

(use-package flx)

(use-package smex)

(use-package company
  :diminish company-mode
  :general (:prefix nil :states nil :keymaps 'company-active-map
                    "C-/" 'counsel-company)
  ;;:evil-bind (insert "M-TAB" company-complete)
  :defer t
  :init
  (global-company-mode)
  :config
  (progn
    (setq company-minimum-prefix-length 2
          company-idle-delay 0.1
          company-tooltip-align-annotations t
	  company-tooltip-flip-when-above t)))

(use-package company-quickhelp
  :after company
  :init
  (company-quickhelp-mode 1)
  (define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin))

(use-package company-shell
  :init
  (add-to-list 'company-backends 'company-shell))


(use-package hippie-exp
  :bind ([remap dabbrev-expand] . hippie-expand)
  :config
  (setq hippie-expand-try-functions-list
	'(try-expand-dabbrev
	  try-expand-dabbrev-all-buffers
	  try-expand-dabbrev-from-kill
	  try-complete-file-name-partially
	  try-complete-file-name
	  try-expand-all-abbrevs
	  try-expand-list
	  try-complete-lisp-symbol-partially
	  try-complete-lisp-symbol
	  try-expand-line))
  (with-eval-after-load 'yasnippet
    (add-to-list 'hippie-expand-try-functions-list
		 'yas-hippie-try-expand)))

(use-package projectile
  :general
  (:keymaps 'projectile-mode-map
	    "p" '(projectile-command-map :which-key "projectile"))
  :init (projectile-mode)
  :config
  (setq projectile-completion-system 'ivy)

  :diminish projectile-mode)

(use-package projectile-git-autofetch
  :disabled t
  :after magit
  :config
  (projectile-git-autofetch-mode)
  :diminish (projectile-git-autofetch-mode . "↓"))

(use-package counsel-projectile
  :general
  ("*" 'counsel-projectile-rg)
  :init
  (let ((inhibit-message t))
    (counsel-projectile-on))
  :config
  (setq counsel-projectile-rg-initial-input '(ivy-thing-at-point))
  (define-key projectile-mode-map [remap projectile-ag] #'counsel-projectile-rg))

(use-package paren
  :init (show-paren-mode)
  :config (setq show-paren-when-point-inside-paren nil
		show-paren-when-point-in-periphery t))

(use-package restart-emacs)


(use-package anaconda-mode
  :diminish anaconda-mode
  :init
  (add-hook 'python-mode-hook #'anaconda-mode)
  (add-hook 'python-mode-hook #'anaconda-eldoc-mode))

(use-package company-anaconda
  :defer t
  :init
  (defun my-setup-py-company ()
    (make-local-variable 'company-backends)
    (setq-local company-idle-delay 0.1)
    (add-to-list 'company-backends 'company-anaconda))
  (add-hook 'anaconda-mode-hook #'my-setup-py-company))

(use-package pyenv-mode
  :general
  (:keymaps 'python-mode-map
	    "vv" 'my-pyenv-mode-set-local-version
	    "vs" 'pyenv-mode-set
	    "vu" 'pyenv-mode-unset)
  :config
  (defun my-pyenv-mode-set-local-version ()
    (interactive)
    (let ((root-path (locate-dominating-file default-directory ".python-version")))
      (when root-path
	(let* ((file-path (expand-file-name ".python-version" root-path))
	       (version
		(with-temp-buffer
		 (insert-file-contents-literally file-path)
		  (buffer-substring-no-properties (line-beginning-position)
						  (line-end-position)))))
	  (if (member version (pyenv-mode-versions))
	      (pyenv-mode-set version)
	    (message "pyenv: version `%s' is not installed (set by %s)"
		     version file-path))))))
  (add-hook 'python-mode-hook 'pyenv-mode))

(use-package py-isort
  :config
  (add-hook 'before-save-hook 'py-isort-before-save))

(use-package pip-requirements)

(use-package doom-one-theme
  :init
  (custom-set-variables '(doom-one-brighter-comments t))
  (load-theme 'doom-one 'no-confirm))

(use-package solaire-mode
  :init
  (add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
  ;; (add-hook 'ediff-prepare-buffer-hook #'solaire-mode)
  (add-hook 'after-revert-hook #'turn-on-solaire-mode)
  ;; (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
  (solaire-mode-swap-bg))

(use-package frame
  :defer t
  :config
  (progn
    (setq window-divider-default-places 'right-only) ;Default 'right-only
    ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=27830#20
    ;; Workaround on emacs 26+ to prevent fringe truncation. You need to use
    ;; either scroll bars or window dividers to prevent that.
    ;; I dislike the default face of `window-divider', so I customize that in my
    ;; `smyx-theme`.
    (setq window-divider-default-right-width 1) ;Default 6
    (window-divider-mode 1)))

;; ;; ;;; Files

(auto-compression-mode)

(add-hook 'prog-mode-hook (lambda () (setq-local show-trailing-whitespace t)))


(use-package diff-hl
  :init (global-diff-hl-mode)
  :config
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package dired
  :defer t
  :general
  (:prefix nil
	   "-" 'dired-up-directory)
  :config
  (setq dired-auto-revert-buffer t
        dired-dwim-target t
        dired-listing-switches "-alhFv --group-directories-first"))

(use-package dired-x
  :demand t
  :init
  (add-hook 'dired-mode-hook #'dired-omit-mode)
  :after dired
  :config
  (setq dired-omit-verbose nil))

(use-package highlight-escape-sequences
  :disabled t
  :config
  ;(add-to-list 'hes-mode-alist '(lisp-interaction-mode . hes-elisp-escape-sequence-re))
  (hes-mode))

(use-package winner
  :general (:prefix "C-w"
		    "[" 'winner-undo
		    "]" 'winner-redo)
  :init (winner-mode))

(use-package ediff
  :config
  (progn
    (setq ediff-split-window-function 'split-window-horizontally
          ediff-window-setup-function 'ediff-setup-windows-plain)
    (with-eval-after-load 'winner
      (add-hook 'ediff-after-quit-hook-internal #'winner-undo))))

;;; Editing

(setq sentence-end-double-space nil)

(setq-default fill-column 80
              indent-tabs-mode nil
              indicate-empty-lines t)

(use-package flyspell
  :diminish flyspell-mode
  :config
  (setq flyspell-issue-message-flag nil
        flyspell-issue-welcome-flag nil)
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

(use-package flyspell-correct-ivy
  :general
  (:keymaps 'flyspell-mode-map :prefix nil :states nil
            "C-;" 'flyspell-correct-previous-word-generic))

(use-package markdown-mode)

(use-package undo-tree
  :diminish undo-tree-mode)


(use-package highlight-numbers
  :defer t
  :init
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))


(use-package page-break-lines
  :diminish page-break-lines-mode
  :init (global-page-break-lines-mode))

(use-package web-mode
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2)
  :mode (("\\.html\\'" . web-mode)))

(use-package js2-mode
  :defer t
  :mode (("\\.js\\'" . js2-mode))
  :config
  (js2-mode-hide-warnings-and-errors)
  (setq-default js2-basic-offset 2))

(use-package json-mode)



;; ;; ;; (use-package re-builder
;; ;; ;;   :config
;; ;; ;;   (progn
;; ;; ;;     (add-to-list 'evil-emacs-state-modes 'reb-subexp-mode)
;; ;; ;;     (--each (list reb-mode-map reb-lisp-mode-map)
;; ;; ;;       (evil-define-key 'normal it
;; ;; ;;         "q" 'reb-quit
;; ;; ;;         "y" 'reb-copy
;; ;; ;;         "n" 'reb-next-match
;; ;; ;;         "p" 'reb-prev-match
;; ;; ;;         "S" 'reb-enter-subexp-mode
;; ;; ;;         "B" 'reb-change-target-buffer
;; ;; ;;         "C" 'reb-toggle-case
;; ;; ;;         "\C-i" 'reb-change-syntax))))

(use-package sh-script
  :config
  (setq sh-indentation 2))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package xref)

(use-package eros
  :init (eros-mode))

(use-package unfill
  :init
  (global-set-key [remap fill-paragraph] #'unfill-toggle))

(use-package executable
  :init
  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p))

(use-package ng2-mode
  :disabled t
  )

(use-package sh-script
  :config
  (setq sh-basic-offset 2))

(use-package ansible
  :disabled t)

(use-package alert
  :config
  (setq alert-default-style 'libnotify))

(use-package add-node-modules-path)

(use-package refmt
  :config
  (setq refmt-show-errors nil)
  (defun refmt-before-save ()
    (add-hook 'before-save-hook 'refmt nil t))
  (add-hook 'reason-mode-hook 'refmt-before-save))

(use-package merlin
  :general
  :config
  (setq merlin-locate-in-new-window 'never)
  (setq merlin-completion-with-doc t)
  (define-key merlin-mode-map (kbd "M-.") 'merlin-locate)
  (define-key merlin-mode-map (kbd "M-,") 'merlin-pop-stack))

(use-package reason-mode
  :config
  (require 'merlin)
  (add-hook 'reason-mode-hook 'add-node-modules-path)
  (add-hook 'reason-mode-hook 'merlin-mode 'append))


(use-package compile
  :config
  (require 'ansi-color)
  (defun colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region compilation-filter-start (point)))))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
  (setq compilation-scroll-output t))

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


(use-package less-css-mode)

(use-package prettier-js
  :init
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'json-mode-hook 'prettier-js-mode)
  (add-hook 'css-mode-hook 'prettier-js-mode)
  (add-hook 'less-css-mode-hook 'prettier-js-mode))

(use-package diredfl
  :init
  (diredfl-global-mode))

(use-package dockerfile-mode)

(progn ;     startup
  (message "Loading %s...done (%.3fs)" user-init-file
           (float-time (time-subtract (current-time)
                                      before-user-init-time)))
  (add-hook 'after-init-hook
            (lambda ()
              (message
               "Loading %s...done (%.3fs) [after-init]" user-init-file
               (float-time (time-subtract (current-time)
                                          before-user-init-time))))
            t))

(progn ;     personalize
  (let ((file (expand-file-name (concat (user-real-login-name) ".el")
                                user-emacs-directory)))
    (when (file-exists-p file)
      (load file))))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
