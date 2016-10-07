;;;; .emacs ---- Emacs personal configuration file.
;;;; Commentary:
;;;; Code:

(setq load-prefer-newer t)

;; visual basics
(defvar my/font "DejaVu Sans Mono 16")
(set-frame-font my/font)
(add-to-list 'after-make-frame-functions
             (lambda (frame)
               (set-frame-parameter frame 'font my/font)))

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(column-number-mode 1)
(show-paren-mode 1)

(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(setq show-trailing-whitespace t)
(setq-default indent-tabs-mode nil)

(require 'windmove)
(windmove-default-keybindings)

(require 'winner)
(winner-mode 1)

;;;
(setq user-full-name "Omer YILMAZ"
      user-mail-address "mr1yh1@yahoo.com")

(setq source-directory "/opt/emacs-25.1/")
;;(byte-recompile-directory user-emacs-directory 0)

(load "~/.emacs.d/_private" t)
;; load new CEDET before build-in CEDET is loaded.
;; git clone http://git.code.sf.net/p/cedet/git cedet
(load-file (concat user-emacs-directory "git/cedet/cedet-devel-load.el"))
;;(load-file (concat user-emacs-directory "git/cedet/contrib/cedet-contrib-load.el"))

;; backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; history
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

;;(require 'desktop)
;;(desktop-save-mode 1)

(set-register ?e (cons 'file "~/.emacs"))
(set-register ?a (cons 'file "~/org/agenda.org"))

;; key bindings
(global-set-key (kbd "<f9>") 'compile)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-/") 'hippie-expand)
;; find file or dired at point
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(setq package-enable-at-startup nil)
(package-initialize)

;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(require 'diminish)
(setq use-package-verbose t)
(setq use-package-always-ensure t)

(use-package auto-compile
  :config (auto-compile-on-load-mode))

(use-package dash)

;; color theme
(use-package color-theme-modern)

(use-package zenburn-theme
  :config (load-theme 'zenburn t))

(use-package powerline
  :commands (powerline-set-selected-window)
  :config
  (powerline-default-theme))

;; editing utils
(use-package hideshow
  :commands (hs-minor-mode)
  :init
  (add-hook 'prog-mode-hook
            (lambda () (hs-minor-mode 1))))

(use-package paredit
  :commands (enable-paredit-mode))

(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode 'enable-paredit-mode)
(add-hook 'clojure-mode-hook 'enable-paredit-mode)

(use-package aggressive-indent
  :config
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'slime-repl-mode)
  (global-aggressive-indent-mode 1))

(use-package popwin)

(use-package guide-key
  :diminish guide-key-mode
  :commands (guide-key-mode)
  :config
  (setq guide-key/guide-key-sequence t)
  (setq guide-key/idle-delay 2)
  (setq guide-key/popup-window-position 'right)
  (setq guide-key/text-scale-amount -2)
  (guide-key-mode 1))

;; popwin won't work with ECB
;; and if there is not enough space.
(use-package pos-tip
  :defer t)
(use-package guide-key-tip
  :commands (guide-key-tip/toggle-enable))

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package edit-indirect
  :bind (("C-c >" . edit-indirect-region)))

(use-package multiple-cursors
  :bind
  (("C-c m m" . mc/mark-all-like-this-dwim)
   ("C-c m e" . mc/mark-more-like-this-extended)
   ("C-c m l" . mc/edit-lines)
   ("C-c m s" . mc/mark-sgml-tag-pair)
   ("C-c m d" . mc/mark-all-like-this-in-defun)))

(use-package undo-tree
  :defer t
  :diminish undo-tree-mode
  :commands (undo-tree-visualize
             undo-tree-undo
             undo-tree-redo)
  :bind (("C-x u" . undo-tree-visualize)
         ("C-_" . undo-tree-undo)
         ("M-_" . undo-tree-redo))
  :init
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff nil))

;; ido
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

(use-package ido-ubiquitous
  :demand t
  :commands (ido-ubiquitous-should-use-old-style-default)
  :config
  (ido-ubiquitous-mode 1))

(use-package ido-vertical-mode
  :config
  (setq ido-vertical-show-count t)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  (ido-vertical-mode 1))

(use-package flx-ido
  :demand t
  :init
  (flx-ido-mode 1)
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil))

(use-package ido-at-point
  :demand t
  :config
  (ido-at-point-mode))

(use-package smex
  :bind
  (("M-x" . smex)
   ("M-X" . smex-major-mode-commands)
   ;; This is your old M-x.
   ("C-c C-c M-x" . execute-extended-command)))

;;; ORG-MODE
(use-package org-bullets
  :commands (org-bullets-mode))

(use-package org
  :mode ("\\.org" . org-mode)
  :init
  (setq-default org-replace-disputed-keys 1)
  (setq org-todo-keywords
        '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))
  (setq org-log-done 'time)
  (setq org-agenda-files (list "~/org/agenda.org"))
  :config
  (org-bullets-mode 1))

(use-package stripe-buffer
  :init
  (add-hook 'dired-mode-hook 'stripe-listify-buffer)
  (add-hook 'ibuffer-mode-hook 'stripe-listify-buffer))

;; COMPANY
(use-package company
  :bind (("C-M-." . company-manual-begin)
         :map company-active-map
         ("C-s" . company-select-next)
         ("C-r" . company-select-previous)
         ("C-d" . company-show-doc-buffer)
         ("C-e" . company-other-backend))
  :config
  (add-to-list 'company-backends 'company-ispell)
  (setq company-idle-delay nil) ;; only manual
  (global-company-mode 1))

(use-package company-quickhelp
  :bind (:map company-active-map
              ("M-h" . company-quickhelp-manual-begin)))

(use-package imenu-anywhere
  :bind ("C-x C-." . ido-imenu-anywhere))

(use-package yasnippet
  :defer t
  :bind (:map yas-minor-mode-map
              ("C-c & C-s" . yas-insert-snippet))
  :config
  (setq-default yas-prompt-functions '(yas-ido-prompt yas-x-prompt  yas-dropdown-prompt))
  (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
  (add-to-list 'company-backends 'company-yasnippet)
  (yas-global-mode 1))

;; projects
(use-package magit
  :defer t
  :bind ("C-x g" . magit-status))

(use-package projectile
  :defer t
  :diminish projectile-mode
  :config
  (setq projectile-enable-caching t)
  (projectile-mode 1))

;; LATEX
(use-package tex
  :defer t
  :commands (TeX-global-PDF-mode reftex-plug-into-AUCTeX)
  :ensure auctex
  :config
  (setq-default TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (TeX-global-PDF-mode 1)
  (setq TeX-default-mode 'context-mode)
  (require 'reftex)
  (reftex-plug-into-AUCTeX))

(add-hook 'TeX-mode-hook
          (lambda ()
            (TeX-fold-mode 1)
            (reftex-mode 1)))


;;;; PROGRAMMING

(use-package flycheck
  :commands (flycheck-add-mode)
  :config
  (flycheck-add-mode 'html-tidy 'html-mode))

;; semantic
(use-package semantic
  :init
  ;; do not disturb minibuffer.
  (setq semantic-working-type 'dynamic)
  :commands (semantic-load-enable-excessive-code-helpers
             cedet-gnu-global-version-check
             global-semantic-idle-completions-mode
             semanticdb-enable-gnu-global-databases)
  :config
  (semantic-load-enable-excessive-code-helpers)
  (when (cedet-gnu-global-version-check t)
    (semanticdb-enable-gnu-global-databases 'c-mode)
    (semanticdb-enable-gnu-global-databases 'c++-mode))
  (global-set-key (kbd "C-c C-f")  'senator-fold-tag-toggle)
  (global-semantic-idle-completions-mode -1)
  ;; adding includes
  ;; (semantic-add-system-include "/usr/include/" 'c-mode)
  ;; (add-to-list 'auto-mode-alist (cons qt4-base-dir 'c++-mode))
  ;; (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig.h"))
  ;; (global-srecode-minor-mode 1) ;; yas ?
  )

;; emacs code browser
(use-package ecb
  :init
  (setq ecb-new-ecb-frame nil)
  (setq ecb-layout-name 'left3)
  (setq ecb-windows-height 0.20)
  (setq ecb-windows-width 0.20)
  (defvar ecb-examples-bufferinfo-buffer-name nil "otherwise it complains.")
  (setq ecb-compile-window-height nil)
  ;;(setq ecb-display-news-for-upgrade nil)
  ;;(setq ecb-display-upgraded-options nil)
  :mode (("C-c. lw" . ecb-toggle-ecb-windows))
  )

;; C/C++
(use-package company-c-headers
  :defer t
  :config
  (setq company-c-headers-path-system '("/usr/include"))
  (setq company-c-headers-path-user '(""))
  (add-to-list 'company-backends 'company-c-headers))

;;; LISP
(defun my-pretty-lambda ()
  "Make some word or string show as pretty Unicode symbols."
  (setq prettify-symbols-alist
        '(
          ("lambda" . 955) ; λ
          )))

(add-hook 'lisp-mode-hook 'my-pretty-lambda)
(global-prettify-symbols-mode 1)

;; ELISP
;; elp profiler, ielm repl, edebug debugger, reshank for refactoring.
(use-package elisp-slime-nav
  :diminish elisp-slime-nav-mode
  :init
  (dolist (i '(emacs-lisp-mode-hook ielm-mode-hook help-mode-hook))
    (add-hook i 'elisp-slime-nav-mode)))

;; COMMON LISP
(use-package slime-company :defer t)
(use-package common-lisp-snippets :defer t)

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

  (setq common-lisp-hyperspec-root (expand-file-name "~/Documents/HyperSpec/"))
  :config
  (slime-setup '(slime-fancy slime-company))
  (add-to-list 'semantic-inhibit-functions
               (lambda () (derived-mode-p 'lisp-mode 'slime-repl-mode))))

;; R OCTAVE JULIA ETC...
(use-package ess
  :defer t
  :disabled t)

;; WEB
(use-package js2-mode
  :mode  (("\\.js$" . js2-mode)
          ("\\.json$" . js2-mode)))

(use-package tern
  ;; put thins into .tern_project
  ;; {
  ;; "libs": [
  ;;          "browser",
  ;;          "jquery"
  ;;          ],
  ;; "plugins": {
  ;; "node": {}
  ;; }
  ;; }
  :defer t
  :config
  (add-hook 'js-mode-hook (lambda () (tern-mode t))))

(use-package nodejs-repl
  :defer t)

(use-package company-tern
  :defer t
  :config
  (add-hook 'js-mode-hook
            (lambda ()
              ;; file from https://gist.github.com/emallson/0eae865bc99fc9639fac
              (load-file (concat user-emacs-directory "git/nodejs-repl-eval.el"))
              (define-key js2-mode-map (kbd "C-c C-e") 'nodejs-repl-eval-dwim)
              (define-key js2-mode-map (kbd "C-c C-b") 'nodejs-repl-eval-buffer)
              (add-to-list 'company-backends 'company-tern))))

(use-package simple-httpd
  :defer t)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package lorem-ipsum
  :commands (lorem-ipsum-insert-sentences
             lorem-ipsum-insert-paragraphs
             lorem-ipsum-insert-list))

;; ;;; C/C++
;; (use-package ggtags ;; semantics is better.
;;   :defer t
;;   :bind (("M-." . ggtags-find-tag-dwim))
;;   :config
;;   (add-hook 'c-mode-common-hook
;;             (lambda ()
;;               (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
;;                 (setq-local imenu-create-index-function
;;                             #'ggtags-build-imenu-index)
;;                 (ggtags-mode 1)))))

;; C styles: gnu, linux, bsd, java etc..
(use-package cc-mode
  :defer t)

(add-hook 'c-mode-hook
          (lambda ()
            (setq c-default-style "gnu")
            (hs-minor-mode)))

(use-package gdb-mi
  :defer t
  :config
  (setq gdb-many-windows t
        gdb-show-main t))

;;; STARTUP
(add-hook 'after-init-hook
          (lambda ()
            (global-flycheck-mode)
            (recentf-open-files)))

;; buggy there
(add-hook 'asm-mode-hook
          (lambda ()
            (aggressive-indent-mode -1)))

;; IRC
(use-package rcirc
  :defer t
  :config
  (setq rcirc-default-nick "mr1yh1"
        rcirc-default-user-name "mr1yh1"
        rcirc-default-full-name "Omer"
        rcirc-server-alist '(("irc.freenode.net"
                              :nick "mr1yh1"
                              :channels ("#clojure" "#emacs")))
        rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY")
        ;; auth values in .private
        rcirc-debug-flag t)
  (set (make-local-variable 'scroll-conservatively) 8192)
  (rcirc-track-minor-mode 1)
  (flyspell-mode 1))

;; search
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

;; weather reports
(use-package sunshine
  :commands (sunshine-forecast)
  :config
  (setq sunshine-location "Istanbul"
        sunshine-units 'metric
        sunshine-show-icons t
        ;;appid is in .private
        ))

(provide '.emacs)
;;; .emacs ends here

;; (Defun ede-object-system-include-path ()
;;   "Return the system include path for the current buffer."
;;   (when ede-object
;;     (ede-system-include-path ede-object)))

;; (ede-project-directories
;;    (quote
;;     ("/home/omer/tmp/deneme/include" "/home/omer/tmp/deneme/src" "/home/omer/tmp/deneme")))

;; GNUS
;; (use-package gnus
;;   :defer t
;;   :init
;;   (setq gnus-read-newsrc-file nil)
;;   (setq gnus-save-killed-list nil)
;;   (setq gnus-check-new-newsgroups nil)
;;   (setq gnus-select-method '(nnspool ""))
;;   (setq gnus-secondary-select-methods '((nntp "news.gmane.org")
;;                                         (nnmbox "")))
;;   (setq gnus-use-cache t)
;;   (setq mail-sources
;;         '((file)
;;           (pop :server "pop.mail.yahoo.com"
;;                :port 995
;;                :user "mr1yh1@yahoo.com"
;;                :stream ssl
;;                )))
;;   (setq mail-source-delete-incoming nil)

;;   (setq message-send-mail-function 'smtpmail-send-it
;;         send-mail-function    'smtpmail-send-it
;;         smtpmail-smtp-server  "smtp.mail.yahoo.com"
;;         smtpmail-stream-type  'ssl
;;         smtpmail-smtp-service 465))

;; (use-package redshank
;;   :defer t
;;   :init
;;   (require 'slime)
;;   ;;(setq redshank-install-lisp-support nil)
;;   :config
;;   (require 'redshank-loader)
;;   (redshank-setup '(lisp-mode-hook slime-repl-mode-hook) t))

;; ;; CLOJURE
;; (use-package cider
;;   :commands (cider-jack-in cider-jack-in-clojurescript)
;;   :init
;;   (add-hook 'clojure-mode-hook 'cider-mode)
;;   (setq nrepl-hide-special-buffers t)
;;   (setq cider-show-error-buffer nil)
;;   (setq cider-repl-pop-to-buffer-on-connect nil)
;;   (setq cider-repl-wrap-history t)
;;   (setq cider-repl-history-file "~/.emacs.d/.cider-repl-history")
;;   (add-hook 'cider-mode-hook 'eldoc-mode)
;;   (add-hook 'cider-repl-mode-hook 'subword-mode))

(setq custom-file "~/.emacs.d/.emacs-custom.el")
(load custom-file)

;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

;; ;; ede
;; (use-package ede
;;   :config
;;   ;; (ede-cpp-root-project "Test"
;;   ;;                       :name "Test Project"
;;   ;;                       :file "~/work/project/CMakeLists.txt"
;;   ;;                       :include-path '("/"
;;   ;;                                       "/Common"
;;   ;;                                       "/Interfaces"
;;   ;;                                       "/Libs"
;;   ;;                                       )
;;   ;;                       :system-include-path '("~/exp/include")
;;   ;;                       :spp-table '(("isUnix" . "")
;;   ;;                                    ("BOOST_TEST_DYN_LINK" . "")))
;;   (global-ede-mode 1))
