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
  (mapcar (lambda (x) (ignore-errors (package-install x)))
          '(adaptive-wrap all android-mode apache-mode ascii browse-kill-ring c-eldoc cmake-mode coffee-mode color-file-completion crontab-mode csv-mode desktop dircmp dired+ dired-details+ dired-details dired-single doc-mode dpaste drupal-mode emacsd-tile find-file-in-git-repo gist git-auto-commit-mode git-commit grep-a-lot grep-o-matic handlebars-mode heroku html-script-src htmlize http-twiddle httpcode ibuffer-vc ido-yes-or-no idomenu igrep image-dired+ ioccur ipython isearch+ jenkins-watch keyfreq keywiz magit-gh-pulls gh magithub markdown-mode furl minimap mustache-mode pastebin pep8 php-mode pivotal-tracker powershell puppet-mode sass-mode haml-mode starter-kit starter-kit-bindings starter-kit-eshell starter-kit-js starter-kit-lisp elisp-slime-nav starter-kit-ruby paredit undo-tree virtualenv wget wgrep yaml-mode yasnippet yasnippet-bundle)))

;; add this to the sart of the load-path to override things if the user
;; already has the sublime packages installed
(add-to-list 'load-path (expand-file-name "sublime-sanity" user-emacs-directory))
(load "sublime")

;; load custom-file after initializing packages
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(set-cursor-color "red")

(custom-set-faces
 '(highlight ((t (:background "grey5"))))
 '(show-paren-match ((t (:foreground "red")))))

(put 'narrow-to-region 'disabled nil)

(setq visible-bell nil)

(defun other-window-reverse (arg)
  (interactive "P")
  (other-window (* -1 (or arg 1))))
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
