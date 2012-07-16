(require 'winner)
(winner-mode 1)

;;;###autoload
(defun winner-undo-redo (&optional arg)
  (interactive "P")
  (if arg (winner-redo)
    (winner-undo)))

(global-set-key (kbd "C-'") 'winner-undo-redo)

(provide 'winner+)
