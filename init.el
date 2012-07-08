(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(set-cursor-color "red")

(custom-set-faces
 '(highlight ((t (:background "grey5")))))

(put 'narrow-to-region 'disabled nil)

(setq visible-bell nil)

(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-q")     'kill-region)
(global-set-key (kbd "C-w")     'backward-kill-word)
(global-set-key (kbd "C-x b")   'list-buffers)
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)

(eval-after-load "package"
  '(add-to-list 'package-archives
                '("marmalade" . "http://marmalade-repo.org/packages/") t))

(autoload 'magit-status "magit" nil t)
(global-set-key (kbd "C-c C-v") 'magit-status)

(defun jknav-uninstall-keys ()
  (local-unset-key (kbd "j"))
  (local-unset-key (kbd "k"))
  (message "Uninstalled j/k navigation keys"))

(defun jknav-search-key-bindings (key &optional match)
  (car (remove nil
               (mapcar (lambda (x)
                         (let ((binding
                                (key-binding (read-kbd-macro x))))
                           (when (string-match
                                  (or match
                                      "next\\|prev\\|begin\\|end\\|start\\|finish")
                                  (symbol-name binding))
                             binding)))
                       (list key
                             (format "M-%s" key)
                             (format "C-%s" key))))))

(defun jknav-install-keys (&optional force)
  (if (or force buffer-read-only)
      (let* ((match (if buffer-read-only
                        "self-insert-command\\|undefined"
                      "undefined"))
             (j (jknav-search-key-bindings "j" match))
             (k (jknav-search-key-bindings "k" match))
             (next (jknav-search-key-bindings "n"))
             (prev (jknav-search-key-bindings "p")))

        ;; FIXME: should be this:
        ;;     (when (and j k next prev)
        ;; to avoid clashing with existing keys
        (when (and next prev)
          (local-set-key (kbd "j") next)
          (local-set-key (kbd "k") prev)
          (message "Installed j/k navigation keys")))
    (jknav-uninstall-keys)))

(add-hook 'after-change-major-mode-hook 'jknav-install-keys t)

(defadvice toggle-read-only (after jknav-update-keys activate)
  (if buffer-read-only
      (jknav-install-keys)
    (jknav-uninstall-keys)))
