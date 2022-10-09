(require 'package)
(setq package-archives
      '(
	("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")

        ;; ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ;; ("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/stable-melpa/")
        ))
(setq package-quickstart t)
(add-hook 'kill-emacs-hook 'package-quickstart-refresh)

(defmacro yx/require-package (package)
  "Only install the package if it is not already installed."
  `(progn
     (unless (package-installed-p ,package) (package-install ,package))
     (package-activate ,package)))

;; end 
(provide 'init-elpa)
