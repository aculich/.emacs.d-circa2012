;;; jknav.el --- Automatically enable j/k keys for line-based navigation

;; Copyright (C) 2012 Aaron Culich

;; Author: Aaron Culich <aculich@gmail.com>
;; Maintainer: Aaron Culich <aculich@gmail.com>
;; Version: 0.0.1
;; Created: 7 Jul 2012
;; Keywords: keyboard navigation

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(defun jknav-uninstall-keys ()
  (interactive)
  (local-unset-key (kbd "j"))
  (local-unset-key (kbd "k"))
  (message "Uninstalled j/k navigation keys"))

(defvar jknav-search-key-pattern
  "next\\|prev\\|begin\\|end\\|start\\|finish\\|forward\\|backward\\|up\\|down"
  "Patterns used by `jknav-search-key-bindings' to detect what
  function defintion to bind to jknav keys.")

(defun jknav-search-key-bindings (key &optional match)
  (car (remove nil
               (mapcar (lambda (x)
                         (let ((binding
                                (key-binding (read-kbd-macro x))))
                           (when (string-match
                                  (or match jknav-search-key-pattern)
                                  (symbol-name binding))
                             binding)))
                       (cond ((listp key) key)
                             ((stringp key)
                              (list key
                                    (format "M-%s" key)
                                    (format "C-%s" key))))))))

(defun jknav-install-keys (&optional force)
  (interactive)
  (if (or force buffer-read-only)
      (let* ((match (if buffer-read-only
                        "self-insert-command\\|undefined"
                      "undefined"))
             (j (jknav-search-key-bindings "j" match))
             (k (jknav-search-key-bindings "k" match))
             (next (jknav-search-key-bindings "n"))
             (prev (jknav-search-key-bindings "p"))
             (scroll-up (jknav-search-key-bindings '("SPC" "C-v")))
             (scroll-down (jknav-search-key-bindings '("DEL" ";" "M-v"))))

        ;; FIXME: should be this:
        ;;     (when (and j k next prev)
        ;; to avoid clashing with existing keys
        (when (and next prev)
          (local-set-key (kbd "j") next)
          (local-set-key (kbd "k") prev)
          (when (and scroll-up scroll-down)
            (local-set-key (kbd " ") scroll-up)
            (local-set-key (kbd ";") scroll-down))
          (message "Installed j/k navigation keys")))
    (jknav-uninstall-keys)))

(defadvice toggle-read-only (after jknav-update-keys)
  (if buffer-read-only
      (jknav-install-keys)
    (jknav-uninstall-keys)))

;;;###autoload
(defun jknav-initialize ()
  (interactive)
  (add-hook 'after-change-major-mode-hook 'jknav-install-keys t)
  (ad-enable-advice 'toggle-read-only 'after 'jknav-update-keys))

(provide 'jknav)

;;; jknav.el ends here
