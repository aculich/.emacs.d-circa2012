(defvar default-load-path load-path
  "The default system `load-path' before we modify it.")

(add-to-list 'load-path user-emacs-directory)

;; the danger of themes is no worse than any other random code you load
;; be wary of everything you download and eval
(setq custom-safe-themes t)

(defalias 'really-kill-buffer-1 'kill-buffer)
(defun really-kill-buffer ()
  (interactive)
  (really-kill-buffer-1 nil))
(global-set-key (kbd "C-c C-k") 'really-kill-buffer)

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-refresh-contents)
(package-initialize)
(unless package-activated-list
  (mapcar
   (lambda (x)
     (ignore-errors (package-install x)))
   '(browse-kill-ring coffee-mode csv-mode desktop dircmp dired+
     dired-details dired-details+ dired-single
     find-file-in-project furl grep-a-lot haml-mode
     ido-ubiquitous igrep image-dired+ ioccur ipython isearch+
     less-css-mode magit markdown-mode minimap monokai-theme
     mustache-mode paredit pivotal-tracker sass-mode smex
     starter-kit starter-kit-bindings starter-kit-eshell
     starter-kit-js undo-tree virtualenv wget yaml-mode
     yasnippet yasnippet-bundle)))

;; bootstrap sublime-sanity and snippets from github repos. Add to the start
;; of the load-path to override the sublime package if it is already installed
(defvar bootstrap-repos
  '(("https://github.com/aculich/sublime-sanity.el.git" "sublime-sanity" t)
    ("https://github.com/aculich/snippets.git" "snippets" nil))
  "`bootstrap-repos' is a list of lists of the form (REPO LOCALNAME LOADPATH).
If non-nil then LOADPATH can be 'append or anything to prepend.")
(defun bootstrap-repos ()
  (mapcar (lambda (arg)
            (multiple-value-bind (repo localname loadpath) arg
              (let ((dir (expand-file-name localname user-emacs-directory)))

                (unless (file-exists-p dir)
                  (shell-command (mapconcat 'identity (list "git clone" repo dir) " ")))
                (when loadpath
                  (add-to-list 'load-path dir
                               (cond ((eq loadpath 'append) t)
                                     (t t)))))))
          bootstrap-repos))
(bootstrap-repos)
(delete-other-windows)
(load "sublime")

;; turn menu-bar-mode back on by default since oddly both starter-kit and
;; sublime turn menus off, but that is a bad default for people new to emacs
(menu-bar-mode 1)

;; load custom-file after initializing packages
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)
;; FIX reload custom-file after initialization is complete so that
;; user's customizations will override things set by other packages
;; just in case those other packages are (wrongly) setting
;; customization values. This is just a short-term fix. A better
;; long-term fix is to prompt the user about which values should be
;; overriden by the package vs the user.
(add-hook 'after-init-hook '(lambda () (load custom-file t)) t)

(set-face-attribute 'default nil :background "black" :foreground "#F8F8F2" :height 120)
(set-face-attribute 'highlight nil :background "grey5")
(set-face-attribute 'show-paren-match nil :foreground "red" :background "black")
(set-face-attribute 'cursor nil :background "red" :foreground "black")

(put 'narrow-to-region 'disabled nil)

(setq visible-bell nil)
(blink-cursor-mode -1)


(require 'yasnippet)
(let ((dir (expand-file-name "snippets" user-emacs-directory)))
  (when dir
    (add-to-list 'yas/root-directory dir t)
    (yas/reload-all)))

(defun other-window-reverse (arg)
  (interactive "p")
  (other-window (- (or arg 1))))
(global-set-key (kbd "C-S-<iso-lefttab>") 'other-window-reverse)
(global-set-key (kbd "C-<tab>")           'other-window)

(global-set-key (kbd "C-z")     'undo)
(global-set-key (kbd "C-q")     'kill-region)
(global-set-key (kbd "C-w")     'backward-kill-word)
(global-set-key (kbd "C-x b")   'list-buffers)
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)

(defun switch-to-other-buffer ()
  (interactive)
  (switch-to-buffer nil))

(global-set-key (kbd "C-M-S-l") 'switch-to-other-buffer)

(defalias 'list-buffers 'ibuffer)

(define-key help-map "\C-w" 'find-function-on-key)
(define-key help-map "\C-f" 'find-function-at-point)

(defvar customize-apropos-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-a" 'customize-apropos)
    (define-key map "\C-f" 'customize-apropos-faces)
    (define-key map "\C-g" 'customize-apropos-groups)
    (define-key map "\C-o" 'customize-apropos-options)
    map)
  "Keymap for characters following the Help Customize Apropos key.")

(defvar customize-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c" 'customize)
    (define-key map "\C-a" customize-apropos-map) ;; prefix key
    (define-key map "\C-b" 'customize-browse)
    (define-key map "\C-f" 'customize-face)
    (define-key map "\C-g" 'customize-group)
    (define-key map "\C-m" 'customize-mode)
    (define-key map "\C-n" 'customize-changed)
    (define-key map "\C-o" 'customize-option)
    (define-key map "\C-r" 'customize-rogue)
    (define-key map "\C-s" 'customize-saved)
    (define-key map "\C-t" 'customize-themes)
    (define-key map "\C-u" 'customize-unsaved)
    (define-key map "\C-v" 'customize-variable)
    (define-key map "\C-z" 'customize-customized)
    map)
  "Keymap for characters following the Help Customize key.")
(global-set-key (kbd "C-h C-c") customize-map)

(define-key minibuffer-local-map
             [(meta tab)]       'file-cache-minibuffer-complete)
(define-key minibuffer-local-map
             [(control return)] 'ido-select-text)
(define-key minibuffer-local-map
             [(control tab)]    'other-window)

(autoload 'paredit-close-round-and-newline "paredit")
(defun paredit-close-round-and-newline-and-open-round ()
  (interactive)
  (let ((blink-matching-paren nil))
    (paredit-close-round-and-newline)
    (paredit-open-round)))
(define-key emacs-lisp-mode-map (kbd "C-<return>") 'paredit-close-round-and-newline)
(define-key emacs-lisp-mode-map (kbd "C-M-9")      'paredit-close-round-and-newline-and-open-round)

(defun mark-sexp-contents (&optional arg allow-extend)
  (interactive "P\np")
  (mark-sexp arg allow-extend)
  (down-list)
  (exchange-point-and-mark)
  (backward-char)
  (exchange-point-and-mark))
(define-key emacs-lisp-mode-map (kbd "C-A-SPC")    'mark-sexp-contents)

(setq emacs-lisp-mode-hook '(turn-on-eldoc-mode imenu-add-menubar-index checkdoc-minor-mode))

(defun isearch-yank-sexp ()
  "Pull next sexp from buffer into search string."
  (interactive)
  (isearch-yank-internal (lambda () (forward-sexp 1) (point))))
(define-key isearch-mode-map (kbd "M-W")       'isearch-yank-sexp)
(define-key isearch-mode-map (kbd "C-M-w")     'isearch-yank-sexp)
(define-key isearch-mode-map (kbd "C-o")       'isearch-occur)

(autoload 'magit-status "magit" nil t)
(global-set-key (kbd "C-c C-v") 'magit-status)

(autoload 'find-dired "find-dired")
(defun dired-do-find-dired (args)
  "Run `find' and go into Dired mode on a buffer of the output.
The command run (after changing into DIR) is essentially

    find . \\( ARGS \\) -ls

except that the car of the variable `find-ls-option' specifies what to
use in place of \"-ls\" as the final argument."
  (interactive (list
                (read-string "Run find (with args): " find-args
                             '(find-args-history . 1))))
  (find-dired default-directory args))

(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map (kbd "f") 'dired-do-find-dired)))

(eval-after-load "dired+"
  '(progn
     (setq diredp-font-lock-keywords-1 nil)))

(global-set-key (kbd "C-c a"  ) 'org-agenda)
(global-set-key (kbd "C-c C-b") 'org-iswitchb)
(global-set-key (kbd "C-c C-l") 'org-store-link)
(global-set-key (kbd "C-c C-m") 'org-capture)
(global-set-key (kbd "C-c m"  ) 'org-capture)

(eval-after-load "org"
  '(progn
     (define-key org-mode-map (kbd "C-S-<iso-lefttab>") 'other-window-reverse)
     (define-key org-mode-map (kbd "C-<tab>") 'other-window)))

;; enable M-y to show kill-ring
(browse-kill-ring-default-keybindings)

(require 'jknav)
(jknav-initialize)

;; ---------------------------------------------------------------------

;; Ring navigation:
;; M-g ]         Go to next search results buffer, restore its current search context
;; M-g [         Ditto, but selects previous buffer.
;;               Navigation is cyclic.
;;
;; Stack navigation:
;; M-g -         Pop to previous search results buffer (kills top search results buffer)
;; M-g _         Clear the search results stack (kills all grep-a-lot buffers!)
;;
;; Other:
;; M-g =         Restore buffer and position where current search started

(require 'grep-a-lot)
(grep-a-lot-setup-keys)
(grep-a-lot-advise igrep)

(defadvice kill-find (around really-kill-buffer activate)
  (if (get-buffer-process (current-buffer))
      ad-do-it
    (really-kill-buffer)))

(require 'winner+)

(eval-after-load "minimap"
  '(progn
     (setq minimap-window-location 'right)))
