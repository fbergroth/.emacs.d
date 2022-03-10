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
  (setq ring-bell-function 'ignore))

(setq create-lockfiles nil)

(setq exec-path (append exec-path '("~/.npm-global/bin")))

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

(require 'setup)

(use-package gcmh
  :init
  (gcmh-mode))

(setup auto-compile
  (:option auto-compile-display-buffer nil
           auto-compile-mode-line-counter t
           auto-compile-source-recreate-deletes-dest t
           auto-compile-toggle-deletes-nonlib-dest t
           auto-compile-update-autoloads t))

(setup (:require no-littering))

(setup
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package general
  :disabled t
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

(setup autorevert
  (:option auto-revert-verbose nil
           global-auto-revert-non-file-buffers t)
  (global-auto-revert-mode))


(use-package corfu
  ;; TAB-and-Go customizations
  :custom
  (corfu-preselect-first nil) ;; Disable candidate preselection
  (corfu-auto t)

  ;; Use TAB for cycling, default is `corfu-complete'.
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))

  :init
  (corfu-global-mode))

(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

;; Add extensions
(use-package cape
  ;; Bind dedicated completion commands
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

(use-package kind-icon
  :after corfu
  :demand t
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-hook 'corfu-margin-formatters #'kind-icon-margin-formatter))

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
  :init (marginalia-mode))

(use-package embark
  :demand t
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings)
   :map minibuffer-local-map
   ("C-o" . embark-export))
  :config
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package multiple-cursors
  :bind
  ("C-<" . mc/mark-previous-like-this)
  ("C->" . mc/mark-next-like-this)
  ("C-+" . mc/mark-next-like-this)
  ("C-c C-<" . mc/mark-all-like-this))

(use-package goggles
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse nil)) ;; set to nil to disable pulsing

(defun fb/consult-rg-region (arg)
  (interactive "P")
  (let* ((text (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (thing-at-point 'symbol)))
         (input (format (if (xor arg (use-region-p)) "%s" "\\b%s\\b") (regexp-quote text))))
    (consult-ripgrep nil input)))

(use-package consult
  :demand t
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi))           ;; needed by consult-line to detect isearch

  :config
  (add-hook 'minibuffer-setup-hook (lambda ()
                                     (setq-local completion-in-region-function 'consult-completion-in-region)))
  ;; (setup
  ;;   (:with-hook minibuffer-setup-hook
  ;;     (:local-set completion-in-region-function 'consult-completion-in-region)))
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (project-root project)))))



;; (setup consult-dir
;;   (:with-map vertico-map
;;     (:bind "M-," consult-dir
;;            "M-j" consult-dir-jump-file)))

(use-package consult-dir
  :after vertico
  :bind
  (:map vertico-map
        ("M-," . consult-dir)
        ("M-j" . consult-dir-jump-file)))

(use-package isearch
  :custom
  (lazy-count-prefix-format "%3s/%3s ")
  (isearch-allow-scroll t)
  (isearch-lazy-count t))

(use-package affe
  :disabled t
  :after orderless
  :config
  (setq affe-regexp-function #'orderless-pattern-compiler
        affe-highlight-function #'orderless-highlight-matches)
  (setf (alist-get #'affe-grep consult-config) `(:preview-key ,(kbd "M-."))))

(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package vertico
  :init
  (vertico-mode))

(use-package vertico-directory
  ;; More convenient directory navigation commands
  :load-path "lib/vertico/extensions"
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :demand t
  :config
  (orderless-define-completion-style orderless+initialism
    (orderless-matching-styles '(orderless-initialism
                                 orderless-literal
                                 orderless-regexp)))

  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides
          '((file (styles . (partial-completion)))
            (command (styles orderless+initialism))
            (symbol (styles orderless+initialism))
            (variable (styles orderless+initialism)))))

(setq set-mark-command-repeat-pop t)

(use-package reformatter
  :demand t
  :config

  (reformatter-define python-black
    :program "black"
    :args `("-q" ,@(and buffer-file-name `("--fast" "--stdin-filename" ,buffer-file-name)) "-"))

  (reformatter-define python-isort
    :program "isort"
    :args `("-q" "--profile" "black",@(and buffer-file-name `("--filename" ,buffer-file-name)) "-"))

  :hook
  (python-mode . python-black-on-save-mode)
  (python-mode . python-isort-on-save-mode))

(use-package ibuffer-project
  :init
  (add-hook 'ibuffer-hook
          (lambda ()
            (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
            (unless (eq ibuffer-sorting-mode 'project-file-relative)
              (ibuffer-do-sort-by-project-file-relative)))))

(use-package dash
  :init (global-dash-fontify-mode))

(use-package diff-hl
  :hook ((magit-pre-refresh-hook . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (dired-mode . diff-hl-dired-mode))
  :init (global-diff-hl-mode))


(setup dired
  (:also-load dired-x)
  (:option dired-auto-revert-buffer t
           dired-dwim-target t
           dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso"
           dired-omit-verbose nil)
  (:hook dired-omit-mode))

(use-package default-text-scale
  :demand t
  :config
  (default-text-scale-mode))

;; (use-package dired-x
;;   :hook (dired-mode . dired-omit-mode)
;;   :config
;;   (setq dired-omit-verbose nil))

(setup diredfl
  (:hook-into dired-mode))

;; (use-package diredfl
;;   :hook (dired-mode . diredfl-mode))

(use-package dockerfile-mode)

(use-package modus-themes
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-fringes 'subtle)
  ;; (modus-themes-headings 'variable-pitch)
  (modus-themes-mode-line '(accented borderless))
  (modus-themes-prompts '(intense bold))
  (modus-themes-region '(bg-only accented))
  :init
  (modus-themes-load-vivendi))


(setup ediff
  (:with-hook ediff-quit-hook
    (:hook winner-undo))
  (:option ediff-diff-options "-w"
           ediff-split-window-function 'split-window-horizontally
           ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package ediff
  :hook (ediff-quit . winner-undo)
  :config
  (setq ediff-diff-options "-w"
        ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package eldoc
  :demand t
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  :config
  (global-eldoc-mode))

(use-package elide-head
  :init (add-hook 'prog-mode-hook #'elide-head))

(use-package emacs
  :bind
  ([remap list-buffers] . ibuffer)
  ([remap upcase-word] . upcase-dwim)
  ([remap downcase-word] . downcase-dwim)
  ([remap capitalize-word] . capitalize-dwim)
  ([remap just-one-space] . cycle-spacing)
  :custom
  (use-short-answers t)
  )

(use-package prog-mode
  :config
  (defun indicate-buffer-boundaries-left ()
    (setq indicate-buffer-boundaries 'left))
  (add-hook 'prog-mode-hook #'indicate-buffer-boundaries-left))

(use-package eros
  :init (eros-mode))







(use-package executable
  :init
  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p))

(use-package js
  :general
  (:keymap 'js-mode-map
            [remap js-find-symbol] 'xref-find-definitions))


(defun consult--preview-p ()
  (when-let (win (active-minibuffer-window))
    (not (eq nil (buffer-local-value
                  'consult--preview-function
                  (window-buffer win))))))

(defun my/eglot-ensure ()
  (when (and buffer-file-name
             (not (consult--preview-p)))
    (eglot-ensure)))

(use-package cc-mode
  :hook
  (c++-mode . electric-pair-mode)
  (c++-mode . yas-minor-mode))


(defun my/cleanup-gfm (string)
  (thread-last string
    (replace-regexp-in-string "[\\\\]\\([][!\"#$%&'()*+,./:;<=>?@\\^_`{|}~-]\\)" "\\1")
    (replace-regexp-in-string "\\\\$" "")
    (string-replace "&nbsp;" " ")))

(use-package repeat
  :demand t
  :config
  (repeat-mode))

(use-package eglot
  :config
  (advice-add 'eglot--format-markup :filter-return 'my/cleanup-gfm)
  (push '(python-mode "pyright-langserver" "--stdio") eglot-server-programs)
  :hook
  (python-mode . my/eglot-ensure)
  (c++-mode . my/eglot-ensure)
  (elm-mode . my/eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  (eglot-workspace-configuration
   '((pylsp . ((configurationSources . ["flake8"])
               (plugins . ((flake8 . ((enabled . t)))
                           (pydocstyle . ((enabled . t))))))))))

(use-package smartparens
  :disabled t
  :demand t
  :config
  (sp-use-smartparens-bindings)
  (smartparens-global-mode))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package avy
  :bind
  ("M-j" . avy-goto-char-timer)
;  ("M-g M-g" . avy-goto-line)
  (:map isearch-mode-map
        ("M-j" . avy-isearch))
  :custom
  (avy-timeout-seconds 0.3))

(use-package iedit
  :demand t)

(use-package iedit-rect
  :demand t)

(use-package ace-window
  :bind ("M-o" . ace-window))

;; (use-package hideshow ; built-in
;;   :commands (hs-cycle
;;              hs-global-cycle)
;;   :bind (:map prog-mode-map
;;               ("C-<tab>" . hs-cycle)
;;               ("<backtab>" . hs-global-cycle)
;;               ("C-S-<iso-lefttab>" . hs-global-cycle))
;;   :config

;;   (defun hs-cycle (&optional level)
;;     (interactive "p")
;;     (if (= level 1)
;;         (pcase last-command
;;           ('hs-cycle
;;            (hs-hide-level 1)
;;            (setq this-command 'hs-cycle-children))
;;           ('hs-cycle-children
;;            ;;TODO: Fix this case. `hs-show-block' needs to be called twice to
;;            ;;open all folds of the parent block.
;;            (save-excursion (hs-show-block))
;;            (hs-show-block)
;;            (setq this-command 'hs-cycle-subtree))
;;           ('hs-cycle-subtree
;;            (hs-hide-block))
;;           (_
;;            (if (not (hs-already-hidden-p))
;;                (hs-hide-block)
;;              (hs-hide-level 1)
;;              (setq this-command 'hs-cycle-children))))
;;       (hs-hide-level level)
;;       (setq this-command 'hs-hide-level)))

;;   (defun hs-global-cycle ()
;;     (interactive)
;;     (pcase last-command
;;       ('hs-global-cycle
;;        (save-excursion (hs-show-all))
;;        (setq this-command 'hs-global-show))
;;       (_ (hs-hide-all)))))


;; (use-package bicycle
;;   :after outline
;;   :bind (:map outline-minor-mode-map
;;               ([C-tab] . bicycle-cycle)
;;               ([S-tab] . bicycle-cycle-global)))

;; (use-package prog-mode
;;   :config
;;   (add-hook 'prog-mode-hook 'outline-minor-mode)
;;   (add-hook 'prog-mode-hook 'hs-minor-mode))

;; (define-key key-translation-map "z" (lambda (_) (read-key) (read-key) (read-key) [24 11 11]))


(defun meow-kp--read-event (prefix)
  (cl-loop
   with mods = '(control)
   with mods-str = "C-"
   for ev = (read-key (concat prefix mods-str))
   if (eq ev ?g)       do (setq mods '(control meta) mods-str "C-M-")
   else if (eq ev ?m)  do (setq mods '(meta)         mods-str "M-")
   else if (eq ev ?\s) do (setq mods nil             mods-str "")
   else return (event-convert-list (append mods (list ev)))))

(defun meow-kp (_ignore)

  (if (not (and meow-mode (member meow--current-state '(normal motion))))
      [?\s]
    (let ((meow--current-state 'keypad)
          (events [])
          (pending t)
          (prefix "KEYPAD: "))
      (while pending
          (message "!! %s" (input-pending-p))
        (setq events (vconcat events (list (meow-kp--read-event prefix)))
              prefix (format "KEYPAD: %s " (key-description events)))
        (let ((target (lookup-key global-map events)))
          (when (or (null target)
                    (integerp target)
                    (commandp target))
            (setq pending nil))))
      events)))

; (define-key key-translation-map (kbd "SPC") 'meow-kp)


(use-package delsel

  :demand tr
  :config
  (pending-delete-mode)
  )

(use-package meow
;  :disabled t
  :demand t
  ;; :custom
  ;; (meow-use-cursor-position-hack t)
  :config
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("d" . dired-jump)
   '("qr"  . restart-emacs)
   `("bs" . ,(fn! switch-to-buffer "*scratch*"))
   `("fi" . ,(fn! find-file user-init-file))
   '("j" . join-line)
 ;;  '("k" . popper-kill-latest-popup)
   '("p" . "C-x p")
   `("P" . ,project-prefix-map)
   '("s" . fb/consult-rg-region)
   '("w" . other-window)
   '("W" . window-swap-states)
   '("o" . "C-x 4")
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet)
   '("vs" . magit-status)
   '("vf" . magit-file-dispatch)
   '("vd" . magit-dispatch)
   '("vp" . magit-project-status)
   )
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   `("N" . ,(lambda () (interactive) (meow--direction-backward) (meow-search nil)))
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   ;; '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore))

  (meow-normal-define-key
   '("Md" . meow-surround-delete))


  (meow-global-mode)
  (meow-setup-indicator)
  (setq meow-goto-line-function 'consult-goto-line)
  )

(defun meow-surround-delete ()
  (interactive)
  (let* ((ch (meow-thing-prompt "Delete thing: "))
         (inner (meow--parse-inner-of-thing-char ch))
         (outer (meow--parse-bounds-of-thing-char ch)))
    (delete-region (cdr inner) (cdr outer))
    (delete-region (car outer) (car inner))


    (meow--select
     (list
      '(select . surround)

      (+ (car outer) (- (cdr inner) (car inner))))
      (car outer)
     )

    ))








(use-package popper
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "^\\*vterm.*\\*$"  vterm-mode
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints

(use-package nixpkgs-fmt
  :hook (nix-mode . nixpkgs-fmt-on-save-mode))

(use-package minions
  :demand t
  :custom
  (minions-prominent-modes '(flymake-mode envrc-mode))
  :config
  (push '(flymake-mode . nil) minions-available-modes)
  (minions-mode)
  )

(use-package flyspell
  :config
  (setq flyspell-issue-message-flag nil
        flyspell-issue-welcome-flag nil)
  (define-key flyspell-mode-map (kbd "C-.") nil)
  ;; (add-hook 'text-mode-hook 'flyspell-mode)
  )

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
  (-set-initial-state 'macrostep-mode 'motion)
  (evil-make-overriding-map macrostep-keymap 'motion))

(use-package magit
  :custom
;  (magit-define-global-key-bindings nil)
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


(use-package ob-ammonite
  :after org
  :demand t
  :config
  (setq ammonite-term-repl-auto-detect-predef-file nil))

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
  :disabled t
  :hook (python-mode . my-pyvenv-activate-local)
  :preface
  (defun my-pyvenv-activate-local ()
    (interactive)
    (when-let ((root (locate-dominating-file
                      default-directory
                      (lambda (d) (file-directory-p (expand-file-name ".venv" d))))))
      (pyvenv-activate (expand-file-name ".venv" root)))))


(use-package typescript-mode)

(define-derived-mode typescript-tsx-mode typescript-mode "tsx")
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))
(add-hook 'typescript-tsx-mode-hook 'prettier-js-mode)


(use-package evil-matchit
  :disabled t
  :init (global-evil-matchit-mode))



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
  :disabled t
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
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx))
  (global-tree-sitter-mode))

(use-package unfill
  :bind
  ([remap fill-paragraph] . unfill-toggle))

(use-package kubel
  :custom
  (kubel-use-namespace-list 'on)
  ;; (evil-define-key 'normal 'kubel-yaml-editing-mode "q" #'kill-current-buffer)
  )

(use-package kubel-evil
  :disabled t
  :demand t
  :after kubel)

(use-package web-mode
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2)
  :mode (("\\.html\\'" . web-mode)))

(use-package whitespace
  :hook (prog-mode . whitespace-mode)
  :config
  (setq whitespace-style '(face tab-mark tabs trailing)))

(use-package whitespace-cleanup-mode
  :init (global-whitespace-cleanup-mode))

(use-package winner
  :init (winner-mode))

(use-package yaml-mode)

(use-package elm-mode
  :config
  (define-key elm-mode-map (kbd "M-.") nil))

(use-package project

  )

(use-package vterm
  :preface
  (defun my-vterm-hook ()
    (setq-local global-hl-line-mode nil)
    (setq-local truncate-lines t))
  :hook
  (vterm-mode . my-vterm-hook))


;; (use-package vterm-toggle
;;   :general
;;   ("C-`" 'vterm-toggle))

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

(use-package flymake
  :init
  (defvar my-flymake-repeat-map (make-sparse-keymap))
  :bind
  (:map flymake-mode-map
        ("C-c C-p" . flymake-goto-prev-error)
        ("C-c C-n" . flymake-goto-next-error))
  (:map my-flymake-repeat-map
        ("C-p" . flymake-goto-prev-error)
        ("C-n" . flymake-goto-next-error))
  :hook
  (prog-mode . flymake-mode)
  :config
  (put 'flymake-goto-next-error 'repeat-map 'my-flymake-repeat-map)
  (put 'flymake-goto-prev-error 'repeat-map 'my-flymake-repeat-map))

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
  :config
)

(use-package dumb-jump
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package so-long
  :demand t
  :config
  (setq so-long-max-lines 100)
  (global-so-long-mode))

(use-package envrc
  :demand t
  :custom
  (envrc-none-lighter nil)
  (envrc-on-lighter '(" envrc"))
  (envrc-error-lighter '((:propertize " envrc" face envrc-mode-line-error-face)))

  :init
  (envrc-global-mode))

(use-package dtache
  :hook (after-init . dtache-setup)
  :bind (([remap async-shell-command] . dtache-shell-command)
         :map dtache-shell-mode-map
         ("C-c C-q" . dtache-detach-dwim))

  :config
  (with-eval-after-load 'embark
    (defvar embark-dtache-map (make-composed-keymap dtache-action-map embark-general-map))
    (add-to-list 'embark-keymap-alist '(dtache . embark-dtache-map))))

(use-package dtache-shell
  :hook (after-init . dtache-shell-setup)
  :bind (:map dtache-shell-mode-map
         (("<S-return>" . dtache-shell-send-input)
          ("<C-return>" . dtache-shell-attach)))
  :config
  (setq dtache-shell-history-file "~/.bash_history"))

(use-package dtache-eshell
  :hook (after-init . dtache-eshell-setup)
  :bind (:map dtache-eshell-mode-map
         (("<S-return>" . dtache-eshell-send-input)
          ("<C-return>" . dtache-eshell-attach)
          ("C-c C-q" . dtache-detach-dwim))))

(use-package dtache-compile
  :hook (after-init . dtache-compile-setup)
  :bind (([remap compile] . dtache-compile)
         ([remap recompile] . dtache-compile-recompile)
         :map dtache-compilation-mode-map
         ("C-c C-q" . dtache-detach-dwim)))

(use-package dtache-consult
  :after dtache
  :bind ([remap dtache-open-session] . dtache-consult-session))

(with-eval-after-load 'make-mode
  (defun fb/unset-tabs (orig-fun &rest args)
    (let ((indent-tabs-mode nil))
      (apply orig-fun args)))
  (advice-add 'makefile-append-backslash :around #'fb/unset-tabs))


(defun fb/test ()
  (interactive)
  (set-transient-map (let ((map (make-sparse-keymap)))
                       (define-key map "Z" 'find-file))))

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
