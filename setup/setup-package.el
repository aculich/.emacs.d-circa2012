(require 'package)

;; Add more package repositories
(defvar package-archives-dir (expand-file-name "archives" package-user-dir))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa"     . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(unless
    (remove 'nil (mapcar
                  (lambda (repo)
                    (file-exists-p
                     (expand-file-name (car repo) package-archives-dir)))
                  package-archives))
  (package-refresh-contents))

(defun package-upgrade (package &optional install-if-missing)
  (let* ((package (if (stringp package)
                      (intern package)
                    package))
         (desc (assoc package package-alist))
         (version (aref (cdr desc) 0)))
    (when (package-installed-p package version)
      (package-install package))))

(defun packages-install (&optional packages)
  (interactive)
  (let ((installed-list))
        (save-window-excursion
          (mapc
           (lambda (package)
             (ignore-errors
               (unless (package-installed-p package)
                 (package-install package)
                 (add-to-list 'installed-list package)
                 (message "Finished installing %s" package))))
           (or packages package-list))
          (package-initialize))
    (when installed-list
      (message "Finished installing all packages."))))

(provide 'setup-package)
