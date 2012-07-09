(defvar default-load-path load-path
  "The default system `load-path' before we modify it.")

(add-to-list 'load-path user-emacs-directory)

;; the danger of themes is no worse than any other random code you load
;; be wary of everything you download and eval
(custom-set-variables '(custom-safe-themes t))

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-refresh-contents)
(package-initialize)
(unless package-activated-list
  (mapcar
   (lambda (x)
     (ignore-errors (package-install x)))
   '(coffee-mode find-file-in-project haml-mode ido-ubiquitous
     less-css-mode magit markdown-mode minimap monokai-theme
     paredit sass-mode smex yaml-mode yasnippet starter-kit
     starter-kit-bindings starter-kit-eshell starter-kit-js
     undo-tree virtualenv)))

;; add sublime-sanity to the sart of the load-path to override the sublime
;; package if it is already installed
(unless (file-exists-p (expand-file-name "sublime-sanity"))
  (shell-command
   "git clone https://github.com/aculich/sublime-sanity.el.git ~/.emacs.d/sublime-sanity"))
(add-to-list 'load-path (expand-file-name "sublime-sanity" user-emacs-directory))
(delete-other-windows)
(load "sublime")

;; load custom-file after initializing packages
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(set-cursor-color "red")

(custom-set-faces
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "#F8F8F2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(highlight ((t (:background "grey5"))))
 '(show-paren-match ((t (:foreground "red")))))

(put 'narrow-to-region 'disabled nil)

(setq visible-bell nil)

(defun other-window-reverse (arg)
  (interactive "p")
  (other-window (- (or arg 1))))
(global-set-key (kbd "C-S-<iso-lefttab>") 'other-window-reverse)
(global-set-key (kbd "C-<tab>") 'other-window)

(global-set-key (kbd "C-z")     'undo)
(global-set-key (kbd "C-q")     'kill-region)
(global-set-key (kbd "C-w")     'backward-kill-word)
(global-set-key (kbd "C-x b")   'list-buffers)
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)

(defun switch-to-other-buffer ()
  (interactive)
  (switch-to-buffer nil))

(global-set-key (kbd "C-M-S-l") 'switch-to-other-buffer)

(define-key minibuffer-local-map
             [(control return)] 'file-cache-minibuffer-complete)
(define-key minibuffer-local-map
             [(control tab)]    'other-window)

(defun paredit-close-round-and-newline-and-open-round ()
  (interactive)
  (paredit-close-round-and-newline)
  (paredit-open-round))
(define-key emacs-lisp-mode-map (kbd "C-<return>") 'paredit-close-round-and-newline)
(define-key emacs-lisp-mode-map (kbd "C-M-9")      'paredit-close-round-and-newline-and-open-round)

(defun isearch-yank-sexp ()
  "Pull next sexp from buffer into search string."
  (interactive)
  (isearch-yank-internal (lambda () (forward-sexp 1) (point))))
(define-key isearch-mode-map (kbd "M-W")       'isearch-yank-sexp)
(define-key isearch-mode-map (kbd "C-M-w")     'isearch-yank-sexp)
(define-key isearch-mode-map (kbd "C-o")       'isearch-occur)

(autoload 'magit-status "magit" nil t)
(global-set-key (kbd "C-c C-v") 'magit-status)

(eval-after-load "dired+"
  (setq diredp-font-lock-keywords-1 nil))

(global-set-key (kbd "C-c a"  ) 'org-agenda)
(global-set-key (kbd "C-c C-b") 'org-iswitchb)
(global-set-key (kbd "C-c C-l") 'org-store-link)
(global-set-key (kbd "C-c C-m") 'org-capture)
(global-set-key (kbd "C-c m"  ) 'org-capture)

(eval-after-load "org"
  '(progn
     (define-key org-mode-map (kbd "C-S-<iso-lefttab>") 'other-window-reverse)
     (define-key org-mode-map (kbd "C-<tab>") 'other-window)))

(require 'jknav)
(jknav-initialize)
