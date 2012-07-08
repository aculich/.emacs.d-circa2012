(defvar default-load-path load-path
  "The default system `load-path' before we modify it.")

(add-to-list 'load-path user-emacs-directory)

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; load custom-file after initializing packages
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(set-cursor-color "red")

(custom-set-faces
 '(highlight ((t (:background "grey5"))))
 '(show-paren-match ((t (:foreground "red")))))

(put 'narrow-to-region 'disabled nil)

(setq visible-bell nil)

(global-set-key (kbd "C-<tab>") 'other-window)
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

(define-key emacs-lisp-mode-map (kbd "C-<return>") 'paredit-close-round-and-newline)

(defun isearch-yank-sexp ()
  "Pull next sexp from buffer into search string."
  (interactive)
  (isearch-yank-internal (lambda () (forward-sexp 1) (point))))
(define-key isearch-mode-map (kbd "M-W")       'isearch-yank-sexp)
(define-key isearch-mode-map (kbd "C-M-w")     'isearch-yank-sexp)
(define-key isearch-mode-map (kbd "C-o")       'isearch-occur)

(autoload 'magit-status "magit" nil t)
(global-set-key (kbd "C-c C-v") 'magit-status)

(require 'jknav)
(jknav-initialize)

