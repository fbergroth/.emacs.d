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
  (setq inhibit-startup-buffer-menu t)
  (setq inhibit-startup-screen t)
  (setq inhibit-startup-echo-area-message "locutus")
  (setq initial-buffer-choice t)
  (setq initial-scratch-message "")
  (setq enable-recursive-minibuffers t)
  (setq sentence-end-double-space nil)
  (setq-default fill-column 80)
  (setq-default indent-tabs-mode nil)
  (setq-default indicate-empty-lines t)
  (blink-cursor-mode 0)
  (fset 'yes-or-no-p #'y-or-n-p)
  (setq ring-bell-function 'ignore))

(setq create-lockfiles nil)

(defmacro fn! (&rest body) `(lambda () (interactive) ,body))

(progn ;    `borg'
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require  'borg)
  (borg-initialize))

(progn ;    `use-package'
  (setq use-package-always-defer t)
  (setq use-package-enable-imenu-support t)
  (setq use-package-minimum-reported-time 0)
  (setq use-package-verbose t)
  (setq use-package-compute-statistics t)
  (require  'use-package))

(use-package gcmh
  :init
  (gcmh-mode))

(use-package auto-compile
  :config
  (setq auto-compile-display-buffer nil)
  (setq auto-compile-mode-line-counter t)
  (setq auto-compile-source-recreate-deletes-dest t)
  (setq auto-compile-toggle-deletes-nonlib-dest t)
  (setq auto-compile-update-autoloads t))

(use-package no-littering
  :demand t)

(use-package custom
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file)))


(use-package general
  :demand t
  :preface
  (general-evil-setup 'short)

  (general-create-definer
   my-leader
   :keymaps 'override
   :states '(emacs normal visual motion insert)
   :non-normal-prefix "C-SPC"
   :prefix "SPC")

  (general-create-definer
   my-mode-leader
   :keymaps 'override
   :states '(emacs normal visual motion insert)
   :non-normal-prefix "C-,"
   :prefix ",")

  :config
  (my-leader
   "u"   'universal-argument
   "fi"  (fn! find-file user-init-file)
   "bd"  'kill-this-buffer
   "bs"  (fn! switch-to-buffer "*scratch*")
   "bm"  'view-echo-area-messages
   "d"   'dired-jump
   "D"   'dired-jump-other-window
   "TAB" 'mode-line-other-buffer))

(progn ;     startup
  (message "Loading early birds...done (%.3fs)"
           (float-time (time-subtract (current-time)
                                      before-user-init-time))))

;;; Long tail

(use-package autorevert
  :init
  (global-auto-revert-mode)
  :config
  (setq auto-revert-verbose nil
        global-auto-revert-non-file-buffers t))

(use-package cl-lib-highlight
  :config
  :hook ((emacs-lisp-mode . cl-lib-highlight-initialize)
         (emacs-lisp-mode . cl-lib-highlight-warn-cl-initialize)))

(use-package corfu
  :init (corfu-global-mode))

(use-package compile
  :config
  (require 'ansi-color)
  (defun colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region compilation-filter-start (point)))))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
  (setq compilation-scroll-output t))

(use-package marginalia
  :init (marginalia-mode)
  :config
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))

(use-package embark
  :general
  ("C-'" 'embark-act
   "C-h B" 'embark-bindings)
  (:keymaps '(minibuffer-local-map)
            "C-o" 'embark-export)
  :init (setq prefix-help-command #'embark-prefix-help-command))

(use-package consult
  :general
  (my-leader
    "*" (fn! consult-ripgrep nil (thing-at-point 'symbol)))
  ([remap switch-to-buffer] 'consult-buffer)

  :config
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (setq consult-project-root-function
        (fn! -some-> (project-current) (project-root))))

(use-package embark-consult
  :general
  (my-leader
   "s" embark-consult-search-map)
  :after (embark consult)
  :demand t
  :hook (embark-collect-mode . embark-consult-preview-minor-mode))

(use-package vertico
  :init
  (vertico-mode))

(use-package orderless
  :demand t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package dash
  :config (dash-enable-font-lock))

(use-package diff-hl
  :hook ((magit-pre-refresh-hook . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (dired-mode . diff-hl-dired-mode))
  :init (global-diff-hl-mode))

(use-package dired
  :init
  (setq dired-auto-revert-buffer t)
  (setq dired-dwim-target t)
  (setq dired-listing-switches "-alhFv --group-directories-first"))

(use-package default-text-scale
  :demand t
  :config
  (default-text-scale-mode))

(use-package dired-x
  :hook (dired-mode . dired-omit-mode)
  :config
  (setq dired-omit-verbose nil))

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package dockerfile-mode)

(use-package modus-vivendi-theme
  :custom
  (modus-themes-slanted-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-fringes 'subtle)
  (modus-themes-variable-pitch-headings t)
  :init
  (modus-themes-load-vivendi))

(use-package ediff
  :hook (ediff-quit . winner-undo)
  :config
  (setq ediff-diff-options "-w"
        ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package eldoc
  :config (global-eldoc-mode))

(use-package elide-head
  :init (add-hook 'prog-mode-hook #'elide-head))


(use-package prog-mode
  :config
  (defun indicate-buffer-boundaries-left ()
    (setq indicate-buffer-boundaries 'left))
  (add-hook 'prog-mode-hook #'indicate-buffer-boundaries-left))

(use-package eros
  :init (eros-mode))

(use-package evil
  :custom
  (evil-cross-lines t)
  (evil-ex-substitute-global t)
  (evil-respect-visual-line-mode t)
  (evil-symbol-word-search t)
  (evil-want-C-i-jump nil)
  (evil-want-C-u-scroll t)
  (evil-want-C-w-in-emacs-state t)
  (evil-want-keybinding nil)
  (evil-undo-system 'undo-redo)
  :init (evil-mode)
  :config

  (evil-ex-define-cmd "W" 'evil-write)
  (define-key evil-normal-state-map (kbd "M-.") nil))

(use-package anzu
  :init (global-anzu-mode))

(use-package evil-anzu
  :demand t)

(use-package evil-collection
  :after evil
  :general
  (nmap evil-collection-unimpaired-mode-map
    "M-n" 'evil-collection-unimpaired-next-error
    "M-p" 'evil-collection-unimpaired-previous-error
    "C-M-n" 'next-error
    "C-M-p" 'previous-error)
  :init (evil-collection-init))

(use-package evil-ediff
  :disabled t
  :config
  (evil-ediff-init))

(use-package evil-multiedit
  :demand t
  :config
  (setq evil-multiedit-follow-matches t)
  (evil-multiedit-default-keybinds)

  (defun my-eme-match-fn ()
    (interactive)
    (evil-multiedit-match-all)
    (iedit-restrict-function))

  (define-key evil-normal-state-map (kbd "M-r") #'my-eme-match-fn))

(use-package evil-surround
  :init
  (global-evil-surround-mode)
  (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)
  (evil-define-key 'visual evil-surround-mode-map "S" 'evil-substitute))

(use-package evil-visualstar
  :init
  (global-evil-visualstar-mode))

(use-package executable
  :init
  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p))

(use-package js
  :general
  (:keymap 'js-mode-map
            [remap js-find-symbol] 'xref-find-definitions))

(defun my/eglot-fmt-before-save ()
  (add-hook 'before-save-hook 'eglot-format-buffer nil t))
(defun my/eglot-ensure ()
  (when buffer-file-name
    (eglot-ensure)))

(use-package cc-mode
  :hook
  (c++-mode . electric-pair-mode)
  (c++-mode . yas-minor-mode))

(use-package eglot
  :general
  (my-leader
   :keymaps 'eglot-mode-map
   "=" '(eglot-format-buffer :wk "fmt"))
  :hook
  (python-mode . my/eglot-ensure)
  (python-mode . my/eglot-fmt-before-save)
  (c++-mode . my/eglot-ensure)
  (c++-mode . my/eglot-fmt-before-save)
  (elm-mode . my/eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  (eglot-workspace-configuration
   '((pyls . ((configurationSources . ["flake8"])
              (plugins (flake8 (enabled . t))))))))

(use-package flyspell
  :config
  (setq flyspell-issue-message-flag nil
        flyspell-issue-welcome-flag nil)
  (add-hook 'text-mode-hook 'flyspell-mode))

(use-package flyspell-correct
  :general
  (:keymaps 'flyspell-mode-map
            "C-;" 'flyspell-correct-previous))

(use-package git-commit
  :hook (git-commit-setup . git-commit-turn-on-flyspell))

(use-package help
  :config (temp-buffer-resize-mode))

(use-package highlight-escape-sequences
  :config
  (hes-mode))

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package hippie-exp
  :general ([remap dabbrev-expand] 'hippie-expand)
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

(use-package hl-line
  :init (global-hl-line-mode))

(use-package hl-todo
  :init
  (global-hl-todo-mode))

(use-package hydra)

(use-package image-file
  :init (auto-image-file-mode))

(progn ;    `isearch'
  (setq isearch-allow-scroll t))

(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode))
  :config
  (js2-mode-hide-warnings-and-errors)
  (setq-default js2-basic-offset 2))

(use-package json-mode)

(use-package less-css-mode)

(use-package lisp-extra-font-lock
  :disabled t
  :init (lisp-extra-font-lock-global-mode))

(use-package lisp-mode
  :preface
  (defun indent-spaces-mode ()
    (setq indent-tabs-mode nil))
  :hook ((emacs-lisp-mode . outline-minor-mode)
         (emacs-lisp-mode . reveal-mode)
         (lisp-interaction-mode . indent-spaces-mode)))

(use-package macrostep
  :general
  (my-mode-leader
   :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
   "m" 'macrostep-expand)
  :config
  (evil-set-initial-state 'macrostep-mode 'motion)
  (evil-make-overriding-map macrostep-keymap 'motion))

(use-package magit
  :general
  (my-leader
    "g" 'magit-file-dispatch)
  :custom
  (magit-define-global-key-bindings nil)
  (magit-repository-directories '(("~/code" . 1)))
  (magit-save-repository-buffers 'dontask)
  (magit-status-goto-file-position t)
  :config
  (setq magit-module-sections-nested nil)
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  (setq magit-diff-refine-hunk 'all)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append))

(use-package magit-todos
  :after magit
  :disabled t
  :custom
  (magit-todos-exclude-globs '("*\.js\.map"))
  :init (magit-todos-mode))

(use-package man
  :init (setq Man-width 80))

(use-package markdown-mode)

(use-package page-break-lines
  :init (global-page-break-lines-mode))

(use-package paren
  :init (show-paren-mode)
  :config (setq show-paren-when-point-inside-paren nil
                show-paren-when-point-in-periphery t))

(use-package pip-requirements)

(use-package prettier-js
  :commands prettier-js
  :hook ((css-mode js2-mode json-mode less-css-mode) . prettier-js-mode))

(use-package org
  :commands fb/org-present-mode
  :general
  (my-mode-leader
   :keymaps 'org-mode-map
   "p" 'fb/org-present-mode
   "w" 'unfill-toggle
   "o" (fn! org-open-at-point '(16)))
  (nmap
   :definer 'minor-mode
   :keymaps 'fb/org-present-mode
   "[" 'org-previous-link
   "]" 'org-next-link
   "RET" (fn! org-open-at-point '(16))
   "<left>" 'org-tree-slide-move-previous-tree
   "<right>" 'org-tree-slide-move-next-tree
   "<up>" 'org-tree-slide-content
   "<down>" 'org-tree-slide-display-header-toggle
   "q" 'fb/org-present-mode)
  :config
  (setq org-hide-emphasis-markers nil)
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((shell . t)))

  (define-minor-mode fb/org-present-mode
    ""
    :lighter ""
    :keymap (make-sparse-keymap)
    (if fb/org-present-mode
        (progn
          (setq org-hide-emphasis-markers t)
          (org-tree-slide-mode 1)
          (olivetti-mode 1)
          (set-window-fringes (selected-window) 0 0)
          (org-superstar-mode 1)
          (org-indent-mode 1))
      (org-tree-slide-mode -1)
      (olivetti-mode -1)
      (org-superstar-mode -1)
      (org-indent-mode -1)
      (set-window-fringes (selected-window) nil))))

(use-package org-superstar
  :after org
  :config
  (setq org-superstar-remove-leading-stars t))

(use-package org-tree-slide
  :after org
  :config
  (org-tree-slide-simple-profile))

(use-package olivetti
  :custom
  (olivetti-body-width 0.7))

(use-package pyvenv
  :hook (python-mode . my-pyvenv-activate-local)
  :preface
  (defun my-pyvenv-activate-local ()
    (interactive)
    (when-let ((root (locate-dominating-file
                      default-directory
                      (lambda (d) (file-directory-p (expand-file-name ".venv" d))))))
      (pyvenv-activate (expand-file-name ".venv" root)))))

(use-package recentf
  :demand t
  :init (recentf-mode)
  :config (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:"))

(use-package restart-emacs
  :general
  (my-leader "qr" 'restart-emacs))

(use-package savehist
  :init (savehist-mode))

(use-package saveplace
  :init (save-place-mode))

(use-package sh-script
  :config
  (setq sh-basic-offset 2))

(use-package simple
  :config
  (line-number-mode)
  (column-number-mode)
  (size-indication-mode))

(use-package smex)

(use-package all-the-icons
  :custom
  (all-the-icons-scale-factor 1.0))

(use-package doom-modeline
  :custom
  (doom-modeline-icon nil)
  (doom-modeline-height 25)
  (doom-modeline-unicode-fallback nil)
  :init (doom-modeline-mode))

(use-package tramp
  :config
  (add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
  (add-to-list 'tramp-default-proxies-alist '("localhost" nil nil))
  (add-to-list 'tramp-default-proxies-alist
               (list (regexp-quote (system-name)) nil nil)))

(use-package tree-sitter
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  :init
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode))

(use-package unfill
  :general ([remap fill-paragraph] 'unfill-toggle))

(use-package web-mode
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2)
  :mode (("\\.html\\'" . web-mode)))

(use-package which-key
  :config
  (which-key-mode))

(use-package whitespace
  :hook (prog-mode . whitespace-mode)
  :config
  (setq whitespace-style '(face tab-mark tabs trailing)))

(use-package whitespace-cleanup-mode
  :init (global-whitespace-cleanup-mode))

(use-package winner
  :general
  (nmap
   "C-w [" 'winner-undo
   "C-w ]" 'winner-redo)
  :init (winner-mode))

(use-package yaml-mode)

(use-package elm-mode
  :config
  (define-key elm-mode-map (kbd "M-.") nil))

(use-package project
  :general
  (my-leader
   "p" project-prefix-map))


(use-package vterm
  :preface
  (defun my-vterm-hook ()
    (setq-local global-hl-line-mode nil)
    (setq-local truncate-lines t))
  :hook
  (vterm-mode . my-vterm-hook))


(use-package vterm-toggle
  :general
  ("C-`" 'vterm-toggle))

(use-package esup
  :custom
  (esup-depth 0))

(use-package pack
  :load-path "~/code/pack-el"
  :general
  (nmap dired-mode-map
        "P" 'pack-dired-dwim))

(use-package rust-mode
  :init
  (setq rust-format-on-save t))

(use-package flymake-diagnostic-at-point
  :hook (flymake-mode . flymake-diagnostic-at-point-mode)
  :custom
  (flymake-diagnostic-at-point-error-prefix nil)
  :after flymake)

(defun my-fix-thingatpt-url (orig-fun &rest args)
  "Remove trailing ' from urls."
  (pcase (apply orig-fun args)
    (`(,beg . ,(and end (guard (= (char-before end) ?'))))
     (cons beg (1- end)))
    (x x)))

(advice-add 'thing-at-point-bounds-of-url-at-point :around #'my-fix-thingatpt-url)

(use-package python
  :init
  (defun fb/insert-set-trace ()
    (interactive)
    (insert "import pdb; pdb.set_trace()")
    (newline nil t))
  :general
  (imap python-mode-map
        "C-p" 'fb/insert-set-trace))

(use-package dumb-jump
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package so-long
  :demand t
  :config
  (setq so-long-max-lines 100)
  (global-so-long-mode))

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

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
