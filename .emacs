;;
(fset 'yes-or-no-p 'y-or-n-p)

;; no startup screen
(setq inhibit-startup-screen t)

;; no-tool bar
(tool-bar-mode -1)

;; no menubar
(tool-bar-mode -1)

;;
(show-paren-mode 't)

;; cua selection
(cua-selection-mode 1)
(put 'erase-buffer 'disabled nil)

;; SLIME - BEGIN

;; HyperSpec documentation
(setq browse-url-browser-function 'browse-url-firefox)
(setq common-lisp-hyperspec-root "/opt/lisp/HyperSpec/")

;; Required If you use utf-8 strings.
(set-language-environment "UTF-8")
(setq slime-net-coding-system 'utf-8-unix)

(add-to-list 'load-path "/opt/lisp/libraries/slime")

; sbcl with less memory consumption
(setq inferior-lisp-program "/usr/bin/sbcl --dynamic-space-size 300")

(require 'slime-autoloads)
(slime-setup '(slime-fancy))

(global-set-key [f12] 'slime-selector)

;; SLIME - END

;; octave file extension
(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))
