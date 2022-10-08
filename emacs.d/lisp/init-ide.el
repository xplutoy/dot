;; -*- coding: utf-8; lexical-binding: t; -*-
(yx-require-package 'eglot)
(add-hook 'python-mode-hook 'eglot-ensure)

(yx-require-package 'yasnippet-snippets)
(yx-require-package 'yasnippet)
(add-hook 'prog-mode-hook 'yas-minor-mode)
(with-eval-after-load 'yasnippet
  ;; unbind <TAB> completion
  (define-key yas-minor-mode-map [(tab)]        nil)
  (define-key yas-minor-mode-map (kbd "TAB")    nil)
  (define-key yas-minor-mode-map (kbd "<tab>")  nil)
  ;; bind 
  (define-key yas-minor-mode-map (kbd "S-<tab>") 'yas-expand)
  )

(yx-require-package 'vterm)
(with-eval-after-load 'vterm
  (define-key vterm-mode-map (kbd "C-q") #'vterm-send-next-key)
  (setq vterm-kill-buffer-on-exit t)

  ;; sovle counsel-yank-pop doesn't work issus
  ;; @see https://github.com/akermu/emacs-libvterm#counsel-yank-pop-doesnt-work
  ;; BUG 仍然不工作
  (defun vterm-counsel-yank-pop-action (orig-fun &rest args)
    (if (equal major-mode 'vterm-mode)
	(let ((inhibit-read-only t)
              (yank-undo-function (lambda (_start _end) (vterm-undo))))
          (cl-letf (((symbol-function 'insert-for-yank)
		     (lambda (str) (vterm-send-string str t))))
            (apply orig-fun args)))
      (apply orig-fun args)))
  (advice-add 'counsel-yank-pop-action :around #'vterm-counsel-yank-pop-action)
  
  )

(provide 'init-ide)
