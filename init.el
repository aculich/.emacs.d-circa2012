(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(put 'narrow-to-region 'disabled nil)

(eval-after-load "package"
  '(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t))
