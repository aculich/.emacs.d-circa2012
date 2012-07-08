(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(put 'narrow-to-region 'disabled nil)

(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-q")     'kill-region)
(global-set-key (kbd "C-w")     'backward-kill-word)
(global-set-key (kbd "C-x b")   'list-buffers)
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)

(eval-after-load "package"
  '(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t))
