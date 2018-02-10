;;;; .emacs ---- Emacs personal configuration file.
;;;; Commentary:
;;;; Code:

(package-initialize)

;; BASICS
(setq user-full-name "Omer YILMAZ"
      user-mail-address "mr1yh1@yahoo.com")

(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(fset 'yes-or-no-p 'y-or-n-p)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(show-paren-mode 1)
(display-time-mode)

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(setq load-prefer-newer t)
(setq scroll-step 1)
(setq sentence-end-double-space nil)
(setq show-trailing-whitespace t)
(setq-default indent-tabs-mode nil)

(defvar my-font "DejaVu Sans Mono 16")
(set-frame-font my-font)
(add-to-list 'after-make-frame-functions
             (lambda (frame)
               (set-frame-parameter frame 'font my-font)))

;; WINDOWS
(require 'windmove)
(windmove-default-keybindings)

(require 'winner)
(winner-mode 1)

;; BACKUPS
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; HISTORY
(setq history-length t)
(setq history-delete-duplicates t)
(require 'savehist)
(setq savehist-file "~/.emacs.d/savehist")
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring
        register-alist))
(savehist-mode 1)

(require 'recentf)
(setq recentf-save-file "~/.emacs.d/.recentf_save_file")
(recentf-mode 1)

;; REGISTERS
(set-register ?e (cons 'file "~/.emacs"))
(set-register ?i (cons 'file "~/Documents/org/PROJECT_IDEAS.org"))
(set-register ?a (cons 'file "~/Documents/org/agenda.org"))

;; KEY BINDINGS
(global-set-key (kbd "<f9>") 'compile)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-/") 'hippie-expand)
(global-set-key (kbd "C-.") 'find-file-at-point)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(setq hippie-expand-try-functions-list
      '(try-expand-all-abbrevs
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-from-kill
        try-expand-dabbrev-all-buffers
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; PACKAGE BASICS
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'irony))

(require 'use-package)
(setq use-package-verbose t)
(setq use-package-always-ensure t)

(use-package auto-compile
  :config (auto-compile-on-load-mode))

(use-package dash)
(use-package diminish)

;; COLOR THEME
(use-package color-theme-modern)
(use-package zenburn-theme
  :config (load-theme 'zenburn t))
(use-package powerline
  :commands (powerline-set-selected-window)
  :config
  (powerline-default-theme))

(use-package stripe-buffer
  :hook ((dired-mode-hook stripe-listify-buffer)
         (ibuffer-mode-hook stripe-listify-buffer)))

;; HELPERS
(use-package popwin)
(use-package guide-key
  :defer nil
  :diminish guide-key-mode
  :commands (guide-key-mode)
  :config
  (setq guide-key/guide-key-sequence t)
  (setq guide-key/idle-delay 2)
  (setq guide-key/popup-window-position 'right)
  (setq guide-key/text-scale-amount -2)
  (guide-key-mode))

;; EDITING
;; editing utils
(use-package undo-tree
  :diminish undo-tree-mode
  :commands (undo-tree-visualize
             undo-tree-undo
             undo-tree-redo)
  :bind (("C-x u" . undo-tree-visualize)
         ("C-_" . undo-tree-undo)
         ("M-_" . undo-tree-redo))
  :init
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff nil)
  :config
  (global-undo-tree-mode))

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package hideshow
  :diminish t
  :commands (hs-minor-mode)
  :bind (("C-c @ C-c" . hs-toggle-hiding)))
(add-hook 'prog-mode-hook (lambda () (hs-minor-mode 1)))

(use-package edit-indirect
  :bind (("C-c >" . edit-indirect-region)))

(use-package aggressive-indent
  :config
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'slime-repl-mode)
  (global-aggressive-indent-mode 1))
(add-hook 'asm-mode-hook
          (lambda ()
            (aggressive-indent-mode -1)))

(use-package paredit
  :commands (enable-paredit-mode)
  :hook ((prog-mode-mode paredit-mode)))

(use-package multiple-cursors
  :bind
  (("C-c m m" . mc/mark-all-like-this-dwim)
   ("C-c m e" . mc/mark-more-like-this-extended)
   ("C-c m l" . mc/edit-lines)
   ("C-c m s" . mc/mark-sgml-tag-pair)
   ("C-c m d" . mc/mark-all-like-this-in-defun)))

;;; IDO settings
(use-package ido
  :demand t
  :commands (ido-everywhere ido-fallback-command ido-complete ido-select-text ido-exit-minibuffer)
  :init
  (setq ido-use-faces t)
  (setq ido-file-extensions-order '(".lisp" ".org" ".el" ".emacs"))
  (setq ido-use-filename-at-point 'guess)
  :config
  (ido-mode 1)
  (ido-everywhere 1))

(use-package ido-completing-read+)

(use-package ido-vertical-mode
  :config
  (setq ido-vertical-show-count t)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  (ido-vertical-mode 1))

(use-package flx-ido
  :init
  (flx-ido-mode 1)
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil))

(use-package ido-at-point
  :config
  (ido-at-point-mode))

(use-package smex
  :bind
  (("M-x" . smex)
   ("M-X" . smex-major-mode-commands)
   ;; This is your old M-x.
   ("C-c C-c M-x" . execute-extended-command)))

(use-package imenu-anywhere
  :bind ("C-x C-." . ido-imenu-anywhere))

(use-package ido-completing-read+
  :commands (ido-ubiquitous-mode)
  :config
  (ido-ubiquitous-mode 1))

;; WEB SEARCH
(use-package engine-mode
  :commands (defengine engine/set-keymap-prefix engine-mode engine/get-query engine/execute-search)
  ;;(setq engine/browser-function 'eww-browse-url)
  :config
  (engine-mode t)
  (engine/set-keymap-prefix (kbd "C-c s"))
  (defengine word
    "http://wordnik.com/words/%s"
    :term-transformation-hook downcase
    :keybinding "d")
  (defengine google
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s" :keybinding "g")
  (defengine stack-overflow "https://stackoverflow.com/search?q=%s" :keybinding "s")
  (defengine github-el
    "https://github.com/search?type=Code&q=extension:el+%s"
    :keybinding "e"
    :browser 'browse-url-firefox
    :docstring "Search .el files on github.com.")
  (defengine youtube "http://www.youtube.com/results?aq=f&oq=&search_query=%s" :keybinding "y")
  (defengine wikipedia "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s" :keybinding "w")
  (defengine emacs-devel
    "https://lists.gnu.org/archive/cgi-bin/namazu.cgi?query=%s&submit=Search&idxname=emacs-devel"
    :keybinding "z"
    :browser 'browse-url-firefox
    :docstring "Search posts on emacs-devel archive.")
  (defengine github "https://github.com/search?ref=simplesearch&q=%s" :keybinding "h"))

;; TEXT
;; LATEX
(use-package tex
  :mode ("\\.tex$" . latex-mode)
  :commands (TeX-global-PDF-mode reftex-plug-into-AUCTeX)
  :ensure auctex
  :init
  (setq-default TeX-auto-save t
                TeX-parse-self t
                TeX-master nil
                TeX-global-PDF-mode 1
                TeX-default-mode 'context-mode)
  :config
  (TeX-fold-mode 1)
  (reftex-mode 1)
  (reftex-plug-into-AUCTeX))

;; MARKDOWN
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; ORG-mode
(use-package org-bullets
  :commands (org-bullets-mode))

(use-package org
  :mode ("\\.org$" . org-mode)
  :init
  (setq-default org-replace-disputed-keys 1)
  (setq org-todo-keywords
        '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))
  (setq org-log-done 'time)
  (setq org-agenda-files (list "~/Documents/org/agenda.org"))
  :config
  (org-bullets-mode 1))

;; PROGRAMMING
;; TEMPLATE
(use-package yasnippet
  :diminish yasnippet
  :bind (:map yas-minor-mode-map
              ("C-c y C-s" . yas-insert-snippet))
  :config
  (setq-default yas-prompt-functions '(yas-ido-prompt)))

;;; VERSION CONTROL
(use-package magit
  :bind ("C-x g" . magit-status))

;; COMPANY
(use-package company
  :diminish company-mode
  :bind (("C-M-i" . company-manual-begin)
         :map company-active-map
         ("C-s" . company-select-next)
         ("C-r" . company-select-previous)
         ("C-d" . company-show-doc-buffer)
         ("C-e" . company-other-backend))
  :config
  (setq company-idle-delay nil) ;; only manual
  (add-to-list 'company-backends 'company-ispell)
  (global-company-mode 1))

;; FLYCHECK
(use-package flycheck
  :diminish flycheck-mode
  :commands (flycheck-add-mode flycheck-select-checker)
  :config
  (add-hook 'c++-mode-hook
            (lambda ()
              (setq flycheck-gcc-language-standard "c++11")
              (setq flycheck-clang-language-standard "c++11")))
  (flycheck-add-mode 'html-tidy 'html-mode))

;; C/CPP
(use-package cmake-mode)

(use-package ggtags
  :diminish ggtags-mode
  :config (ggtags-mode 1))

(use-package rtags
  :config
  (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
  (add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
  (add-hook 'objc-mode-hook 'rtags-start-process-unless-running)
  (setq rtags-autostart-diagnostics t)
  (rtags-diagnostics)
  (setq rtags-completions-enabled t)
  (rtags-enable-standard-keybindings))

(use-package flycheck-rtags)

(defun my-flycheck-rtags-setup ()
  (flycheck-select-checker 'rtags)
  (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
  (setq-local flycheck-check-syntax-automatically nil))

(add-hook 'c-mode-hook #'my-flycheck-rtags-setup)
(add-hook 'c++-mode-hook #'my-flycheck-rtags-setup)
(add-hook 'objc-mode-hook #'my-flycheck-rtags-setup)

(use-package company-rtags
  :config
  (push 'company-rtags company-backends))

(use-package flycheck-clang-analyzer
  :config
  (flycheck-clang-analyzer-setup))

(use-package clang-format)

(use-package modern-cpp-font-lock
  :config
  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode))

;; COMMON LISP
(use-package slime
  :commands (slime)
  :defer t
  :init
  (setq slime-net-coding-system 'utf-8-unix)
  (setq slime-lisp-implementations
        '((sbcl ("sbcl")
                :coding-system utf-8-unix
                :program-args "--dynamic-space-size 2048")
          (clisp ("clisp"))
          (ecl ("ecl"))))

  (setq common-lisp-hyperspec-root (expand-file-name "~/Documents/HyperSpec-7-0/HyperSpec/"))
  :config
  (slime-setup '(slime-fancy)))

;; ELISP
;; elp profiler, ielm repl, edebug debugger, reshank for refactoring.
(add-hook 'emacs-lisp-mode 'eldoc-mode)
(add-hook 'ielm-mode-hook 'eldoc-mode)

(use-package elisp-slime-nav
  :diminish elisp-slime-nav-mode
  :init
  (dolist (i '(emacs-lisp-mode-hook ielm-mode-hook help-mode-hook c-mode-hook))
    (add-hook i 'elisp-slime-nav-mode)))

;; R OCTAVE JULIA ETC...
(use-package ess
  :mode ("\\.r$" . R-mode)
  :config
  (setq ess-use-ido t))

;; WEB
(add-hook 'html-mode-hook 'auto-fill-mode)

(use-package lorem-ipsum
  :commands (lorem-ipsum-insert-sentences
             lorem-ipsum-insert-paragraphs
             lorem-ipsum-insert-list))

(use-package js2-mode
  :mode  (("\\.js$" . js2-mode)
          ("\\.json$" . js2-mode)))

;;; STARTUP
(add-hook 'after-init-hook
          (lambda ()
            (global-flycheck-mode)
            (recentf-open-files)))

;; IRC
(use-package rcirc
  :defer t
  :config
  (setq rcirc-default-nick "mr1yh1"
        rcirc-default-user-name "mr1yh1"
        rcirc-default-full-name "Omer"
        rcirc-server-alist '(("irc.freenode.net"
                              :nick "mr1yh1"
                              :channels ("#emacs")))
        rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY")
        ;; auth values in .private
        rcirc-debug-flag t)
  (set (make-local-variable 'scroll-conservatively) 8192)
  (rcirc-track-minor-mode 1)
  (flyspell-mode 1))

;;GNUS
(use-package gnus
  :config
  (setq gnus-select-method '(nntp "news.aioe.org"))
  (add-to-list 'gnus-secondary-select-methods '(nntp "news.gnus.org"))
  (add-to-list 'gnus-secondary-select-methods '(nnml "")))

(provide '.emacs)
;;; .emacs ends here

(setq custom-file "~/.emacs.d/.emacs-custom.el")
(load custom-file)

(put 'downcase-region 'disabled nil)
