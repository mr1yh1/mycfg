;;;; package ---- Summary
;;;; Commentary:
;;;; Code:
(setq user-full-name "Omer YILMAZ"
      user-mail-address "mr1yh1@yahoo.com")

(load "~/.emacs.d/.private" t)

;; UTF-8
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

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
        regexp-search-ring))
(savehist-mode 1)

;; basics
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(column-number-mode 1)
(show-paren-mode 1)

(require 'recentf)
(setq recentf-save-file "~/.emacs.d/.recentf_save_file")
(recentf-mode 1)

(require 'windmove)
(windmove-default-keybindings)

(require 'winner)
(winner-mode 1)

(setq show-trailing-whitespace t)
(setq-default indent-tabs-mode nil)
(setq load-prefer-newer t)

(set-register ?e (cons 'file "~/.emacs"))

;; key bindings
(global-set-key (kbd "<f9>") 'compile)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-x w") 'browse-url)
(global-set-key (kbd "M-/") 'hippie-expand)

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

;; ;; utils
;; (use-package smartparens
;;   :demand t
;;   :config
;;   (require 'smartparens-config)
;;   (smartparens-global-strict-mode))

(use-package paredit
  :commands (enable-paredit-mode))

(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'clojure-mode-hook 'enable-paredit-mode)

(use-package aggressive-indent
  :config
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  (global-aggressive-indent-mode 1))

(use-package guide-key
  :demand t
  :diminish guide-key-mode
  :init
  (setq guide-key/guide-key-sequence t)
  :config
  (guide-key-mode 1))

(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("M-=" . er/contract-region)))

(use-package winner)

(use-package undo-tree
  :demand t
  :diminish undo-tree-mode
  :init
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  :config
  (global-undo-tree-mode))

;; color theme
(use-package color-theme-modern)

(use-package zenburn-theme
  :config (load-theme 'zenburn t))

(use-package powerline
  :commands (powerline-set-selected-window)
  :config
  (powerline-default-theme))

;; ido
(use-package ido
  :commands (ido-everywhere ido-fallback-command ido-complete ido-select-text ido-exit-minibuffer)
  :demand t
  :init
  (setq ido-use-faces t)
  (setq ido-file-extensions-order '(".clj" ".lisp" ".org" ".el" ".emacs"))
  (setq ido-use-filename-at-point 'guess)
  :config
  (ido-mode 1)
  (ido-everywhere 1))


(use-package ido-ubiquitous
  :commands (ido-ubiquitous-should-use-old-style-default)
  :config
  (ido-ubiquitous-mode 1))

(use-package ido-vertical-mode
  :init
  (setq ido-vertical-show-count t)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  :config
  (ido-vertical-mode 1))

(use-package flx-ido
  :init
  (setq ido-enable-flex-matching t)
  :config
  (flx-ido-mode 1))

(use-package ido-at-point
  :demand t
  :commands (ido-at-point-mode)
  :config
  (ido-at-point-mode))

(use-package smex
  :bind
  (("M-x" . smex)
   ("M-X" . smex-major-mode-commands)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPANY
(use-package company
  :demand t
  :init
  ;;:diminish company-mode
  :bind (:map company-active-map
              ("\C-n" . company-select-next)
              ("\C-p" . company-select-previous)
              ("\C-d" . company-show-doc-buffer)
              ("M-." . company-show-location))
  :config
  (global-company-mode 1))

;; (use-package company-quickhelp
;;   :defer t
;;   :config
;;   (company-quickhelp-mode 1))

;; (use-package company-statistics)
;; (add-hook 'after-init-hook 'company-statistics-mode)

(use-package imenu-anywhere
  :bind ("C-x C-." . ido-imenu-anywhere))

(use-package yasnippet
  ;;:diminish yas-minor-mode
  :init
  (setq yas-prompt-functions '(yas-ido-prompt yas-x-prompt  yas-dropdown-prompt))
  (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
  :config
  (add-to-list 'company-backends 'company-yasnippet)
  (yas-global-mode 1))

;; projects
(use-package magit
  :bind ("C-x g" . magit-status))

(use-package projectile
  :defer t
  :diminish projectile-mode
  :init
  (setq projectile-enable-caching t)
  :config
  (projectile-global-mode))

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

;; search the web
(use-package engine-mode
  :commands (defengine engine/set-keymap-prefix engine-mode engine/get-query engine/execute-search)
  :defer t
  ;;(setq engine/browser-function 'eww-browse-url)
  :config
  (engine-mode t)
  (engine/set-keymap-prefix (kbd "C-c s"))
  (defengine google
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s" :keybinding "g")
  (defengine stack-overflow "https://stackoverflow.com/search?q=%s" :keybinding "s")
  (defengine youtube "http://www.youtube.com/results?aq=f&oq=&search_query=%s" :keybinding "y")
  (defengine wikipedia "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s" :keybinding "w")
  (defengine github "https://github.com/search?ref=simplesearch&q=%s" :keybinding "h")
  (defengine amazon "http://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords=%s" :keybinding "a"))

(use-package sunshine
  :commands (sunshine-forecast)
  :init
  (setq sunshine-location "Istanbul"
        sunshine-units 'metric
        sunshine-show-icons t
        ;;appid is in .private
        ))

;; programming

(require 'ede)
(global-ede-mode 1)

(use-package flycheck
  :commands (flycheck-add-mode)
  :config
  (flycheck-add-mode 'html-tidy 'web-mode))

;; C/C++ TODO: semantic ?
(use-package company-c-headers
  :defer t
  :init
  (setq company-c-headers-path-system '("/usr/include"))
  (setq company-c-headers-path-user '(""))
  :config
  (add-to-list 'company-backends 'company-c-headers))

;; ELISP
(use-package elisp-slime-nav
  :diminish elisp-slime-nav-mode
  :init
  (dolist (i '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook i 'elisp-slime-nav-mode)))

;; COMMON LISP
(use-package slime-company :defer t)
(use-package common-lisp-snippets :defer t)

(use-package slime
  :commands (slime)
  :defer t
  :init
  (setq slime-net-coding-system 'utf-8-unix)
  (setq inferior-lisp-program "sbcl --dynamic-space-size 4096")
  ;;(setq inferior-lisp-program "/opt/ccl/lx86cl64")
  (setq common-lisp-hyperspec-root "/home/omer/Documents/HyperSpec-7-0/HyperSpec/")
  :config
  (slime-setup '(slime-fancy slime-company)))

;; elp the profiler, ielm the repl, edebug the debugger.

;; (use-package redshank
;;   :defer t
;;   :init
;;   (require 'slime)
;;   ;;(setq redshank-install-lisp-support nil)
;;   :config
;;   (require 'redshank-loader)
;;   (redshank-setup '(lisp-mode-hook slime-repl-mode-hook) t))

;; CLOJURE
(use-package cider
  :commands (cider-jack-in cider-jack-in-clojurescript)
  :init
  (add-hook 'clojure-mode-hook 'cider-mode)
  (setq nrepl-hide-special-buffers t)
  (setq cider-show-error-buffer nil)
  (setq cider-repl-pop-to-buffer-on-connect nil)
  (setq cider-repl-wrap-history t)
  (setq cider-repl-history-file "~/.emacs.d/.cider-repl-history")
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'subword-mode))

;; WEB
;; (use-package web-mode
;;   :mode (("\\.js$" . web-mode)
;;          ("\\.html$" . web-mode))
;;   :config
;;   (setq web-mode-code-indent-offset 2
;;         web-mode-css-indent-offset 2
;;         web-mode-markup-indent-offset 2))

;; (use-package company-web :defer t
;;   :config
;;   (add-to-list 'company-backends 'company-web-html))

;; (use-package impatient-mode :defer t)
;; (use-package know-your-http-well :defer t)

;; ;; LATEX
;; (use-package company-math
;;   :defer t
;;   :config
;;   (add-to-list 'company-backends '(company-math-symbols-latex company-latex-commands)))

;; ;; R OCTAVE JULIA ETC...
;; (use-package ess
;;   :defer t
;;   ;;:disabled t
;;   )


;;(add-hook 'after-init-hook #'global-flycheck-mode)

;; (ede-project-directories
;;    (quote
;;     ("/home/tamer/tmp/deneme/include" "/home/tamer/tmp/deneme/src" "/home/tamer/tmp/deneme")))

;; C/C++ development Environment

;; (use-package ggtags
;;   :defer t
;;   :bind (("M-." . ggtags-find-tag-dwim))
;;   :config
;;   (add-hook 'c-mode-common-hook
;;             (lambda ()
;;               (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
;;                 (setq-local imenu-create-index-function
;;                             #'ggtags-build-imenu-index)
;;                 (ggtags-mode 1)))))

;; ;; hs-minor-mode for folding source code
;; (add-hook 'c-mode-common-hook 'hs-minor-mode)


;; C styles: gnu, linux, bsd, java etc..
;; “gnu”: The default style for GNU projects
;; “bsd”: What BSD developers use, aka “Allman style” after Eric Allman.
;; “linux”: What the Linux developers use for kernel development
;; “java”: The default style for java-mode (see below)
(require 'cc-mode)
(add-hook 'c-mode-hook
          (lambda ()
            (setq c-default-style "gnu")))

;; ;; setup GDB
(setq gdb-many-windows t
      gdb-show-main t)


;; (defun ede-object-system-include-path ()
;;   "Return the system include path for the current buffer."
;;   (when ede-object
;;     (ede-system-include-path ede-object)))

;; GNUS
(use-package gnus
  :init
  (setq gnus-read-newsrc-file nil)
  (setq gnus-save-killed-list nil)
  (setq gnus-check-new-newsgroups nil)
  (setq gnus-select-method '(nnspool ""))
  (setq gnus-secondary-select-methods '((nntp "news.gmane.org")
                                        (nnmbox "")))
  (setq gnus-use-cache t)
  (setq mail-sources
        '((file)
          (pop :server "pop.mail.yahoo.com"
               :port 995
               :user "mr1yh1@yahoo.com"
               :stream ssl
               )))
  (setq mail-source-delete-incoming nil)

  (setq message-send-mail-function 'smtpmail-send-it
        send-mail-function    'smtpmail-send-it
        smtpmail-smtp-server  "smtp.mail.yahoo.com"
        smtpmail-stream-type  'ssl
        smtpmail-smtp-service 465))

;;; STARTUP
(add-hook 'after-init-hook
          (lambda ()
            (global-flycheck-mode)
            (recentf-open-files)))

;; buggy there
(add-hook 'asm-mode-hook
          (lambda ()
            (aggressive-indent-mode -1)))

(provide '.emacs)
;;; .emacs ends here
