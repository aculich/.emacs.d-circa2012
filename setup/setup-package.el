(require 'cl)
(require 'package)
(require 'vc-git)

;; Add more package repositories
(defvar package-archives-dir (expand-file-name "archives" package-user-dir))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa"     . "http://melpa.milkbox.net/packages/") t)

;;;; TODO: activate packages with git submodule, especially if they're already
;;;; checked out.

(defun package-git-submodule-update (path)
  (vc-git-command (current-buffer) 0 nil "init")
  (let* ((default-directory user-emacs-directory)
         (root (vc-git-root default-directory))
         (l (with-temp-buffer
              (rename-buffer "*vc-git-update*" t)
              (vc-git-command (current-buffer) 0 nil
                              "submodule" "update" "--" path)))
         (mapcar (lambda (x)
                   (add-to-list 'load-path (expand-file-name path user-emacs-directory)))
                 l))))

(defsubst package-desc-path (desc)
  "Extract the kind of download from an archive package description vector."
  (when (>= (length desc) 5)
    (aref desc 4)))

(defun package-download-transaction (package-list)
  "Download and install all the packages in PACKAGE-LIST.
PACKAGE-LIST should be a list of package names (symbols).
This function assumes that all package requirements in
PACKAGE-LIST are satisfied, i.e. that PACKAGE-LIST is computed
using `package-compute-transaction'."
  (dolist (elt package-list)
    (let* ((desc (cdr (assq elt package-archive-contents)))
           ;; As an exception, if package is "held" in
           ;; `package-load-list', download the held version.
           (hold (cadr (assq elt package-load-list)))
           (v-string (or (and (stringp hold) hold)
                         (package-version-join (package-desc-vers desc))))
           (kind (package-desc-kind desc))
           (path (package-desc-path desc)))
      (cond
       ((eq kind 'git-submodule)
        (package-git-submodule-update path))
       ((eq kind 'tar)
        (package-download-tar elt v-string))
       ((eq kind 'single)
        (package-download-single elt v-string
                                 (package-desc-doc desc)
                                 (package-desc-reqs desc)))
       (t
        (error "Unknown package kind: %s" (symbol-name kind))))
      ;; If package A depends on package B, then A may `require' B
      ;; during byte compilation.  So we need to activate B before
      ;; unpacking A.
      (package-maybe-load-descriptor (symbol-name elt) v-string
                                     package-user-dir)
      (package-activate elt (version-to-list v-string)))))


(defun package-git-submodule-generate-package-desc (status)
  (multiple-value-bind
      (hash package path) status
    (let* ((version (list
                     (abs (string-to-number
                           (substring hash 1 10) 16))))
           (name (file-name-nondirectory package))
           (pkginfo (assq (intern-soft name)
                          package-archive-contents))
           (desc (if pkginfo
                     (aref (cdr pkginfo) 2)
                   "git-submodule")))
      (cons (intern-soft name)
            (vector version nil desc 'git-submodule
                    (or path package))))))

(defun package-git-submodule-archive-contents ()
  "Generate `archive-contents' by querying git submodule."
  (let* ((default-directory user-emacs-directory)
         (root (vc-git-root default-directory)))
    (with-temp-buffer
      (rename-buffer "*vc-git-submodule*" t)
      (vc-git-command (current-buffer) 0 nil
                      "submodule" "status"
;                      "submodule" "--quiet" "foreach" "echo $sha1 $name $path"
                       )

      ;; collect the package info from git submodule output
      (append '(1)
            (mapcar 'package-git-submodule-generate-package-desc
                    ;; split the buffer strings
                    (mapcar (lambda (x) (split-string x))
                            (split-string (buffer-string) "\n" t)))))))

(defun package-refresh-git-submodule ()
  (flet ((version-list-< (a b) t))
    (progn
      (mapc (lambda (x)
              (package--add-to-archive-contents
               x "git-submodule"))
            (cdr (package-git-submodule-archive-contents)))
      package-archive-contents)))

(defadvice package-refresh-contents
  (after git-submodule-archive-contents activate)
  (package-refresh-git-submodule))

(defun package-upgrade (package &optional install-if-missing)
  (let* ((package (if (stringp package)
                      (make-symbol package)
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


;; begin the initialization process
(package-initialize)
(unless
    (remove 'nil (mapcar
                  (lambda (repo)
                    (file-exists-p
                     (expand-file-name (car repo) package-archives-dir)))
                  package-archives))
  (package-refresh-contents))
(package-refresh-git-submodule)

(provide 'setup-package)
