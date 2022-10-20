;; -*- coding: utf-8; lexical-binding: t; -*-

;; minibuffer enhance
;; vertico ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(yx-require-package 'vertico)
(add-hook 'after-init-hook 'vertico-mode)

(with-eval-after-load 'vertico
  (setq vertico-cycle t)
  (setq vertico-scroll-margin 1)
  (setq vertico-resize t)

  ;; vertico-directory
  (define-key vertico-map "\r" #'vertico-directory-enter)
  (define-key vertico-map "\d" #'vertico-directory-delete-char)
  (define-key vertico-map "\M-\d" #'vertico-directory-delete-word)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

  ;; verico-mouse
  (vertico-mouse-mode)

  ;; vertico-quick
  (define-key vertico-map "\M-q" #'vertico-quick-insert)
  (define-key vertico-map "\C-q" #'vertico-quick-exit)

  )

;; orderless ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(yx-require-package 'orderless)
(with-eval-after-load 'vertico
  (setq completion-styles '(basic orderless)
	    completion-category-overrides '((file (styles . (basic partial-completion orderless)))
                                        (imenu (styles . (basic substring orderless)))
                                        (kill-ring (styles . (basic substring orderless)))
                                        (project-file (styles . (basic substring partial-completion orderless)))))
  )

;; embark ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(yx-require-package 'embark)
(with-eval-after-load 'vertico
  (global-set-key (kbd "C-'") 'embark-act)
  (global-set-key (kbd "C-;") 'embark-dwim)
  (global-set-key (kbd "C-h B") 'embark-bindings) ;;; alternative for `describe-bindings'

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  ;; which-key style indicator
  (defun embark-which-key-indicator ()
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
	    (which-key--show-keymap
	     (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
	     (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
	     nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators
	    '(embark-which-key-indicator
	      embark-highlight-indicator
	      embark-isearch-highlight-indicator))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator)


  (with-eval-after-load 'embark
    (define-key embark-symbol-map "D" 'sdcv-search-pointer+))
    ;; (embark-define-keymap embark-symbol-map ("D" sdcv-search-pointer+)))  dont work ?? maybe something wrong
  )

(yx-require-package 'embark-consult)
(with-eval-after-load 'consult
  (with-eval-after-load 'embark
    (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)))

;; consult ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(yx-require-package 'consult)
(with-eval-after-load 'vertico
  (global-set-key (kbd "C-x b") 'consult-buffer)
  (global-set-key (kbd "C-x 4 b") 'consult-buffer-other-window)
  (global-set-key (kbd "C-x 5 b") 'consult-buffer-other-frame)
  (global-set-key (kbd "C-x r b") 'consult-bookmark)
  (global-set-key (kbd "C-x p b") 'consult-project-buffer)
  (global-set-key (kbd "C-x C-r") 'consult-recent-file)
  ;; Custom M-# bindings for fast register access
  (global-set-key (kbd "M-#") 'consult-register-load)
  (global-set-key (kbd "M-'") 'consult-register-store)
  (global-set-key (kbd "C-M-#") 'consult-register)
  ;; Other custom bindings
  (global-set-key (kbd "M-y") 'consult-yank-pop)
  (global-set-key (kbd "C-h a") 'consult-apropos)
  ;; M-g bindings (goto-map)
  (global-set-key (kbd "M-g e") 'consult-compile-error)
  (global-set-key (kbd "M-g f") 'consult-flymake)
  (global-set-key (kbd "M-g g") 'consult-goto-line)
  (global-set-key (kbd "M-g M-g") 'consult-goto-line)
  (global-set-key (kbd "M-g o") 'consult-outline)
  (global-set-key (kbd "M-g m") 'consult-mark)
  (global-set-key (kbd "M-g k") 'consult-global-mark)
  (global-set-key (kbd "M-g i") 'consult-imenu)
  (global-set-key (kbd "M-g I") 'consult-imenu-multi)
  ;; M-s bindings (search-map)
  (global-set-key (kbd "M-s d") 'consult-find)
  (global-set-key (kbd "M-s D") 'consult-locate)
  (global-set-key (kbd "M-s g") 'consult-grep)
  (global-set-key (kbd "M-s G") 'consult-git-grep)
  (global-set-key (kbd "M-s r") 'consult-ripgrep)
  (global-set-key (kbd "M-s l") 'consult-line)
  (global-set-key (kbd "M-s L") 'consult-line-multi)
  (global-set-key (kbd "M-s o") 'consult-multi-occur)
  (global-set-key (kbd "M-s k") 'consult-keep-lines)
  (global-set-key (kbd "M-s u") 'consult-focus-lines)
  (global-set-key (kbd "M-s e") 'consult-isearch-history)
  ;; Isearch integration
  (define-key isearch-mode-map (kbd "M-e") 'consult-isearch-history)
  (define-key isearch-mode-map (kbd "M-s e") 'consult-isearch-history)
  (define-key isearch-mode-map (kbd "M-s l") 'consult-line)
  (define-key isearch-mode-map (kbd "M-s L") 'consult-line-multi)
  ;; Minibuffer history
  (define-key minibuffer-local-map (kbd "M-s") 'consult-history)
  (define-key isearch-mode-map (kbd "M-r") 'consult-history)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)

  (advice-add #'register-preview :override #'consult-register-window)

  (setq register-preview-delay 0.5
	register-preview-function #'consult-register-format)

  (add-hook 'completion-list-mode 'consult-preview-at-point-mode)

  (with-eval-after-load 'consult
    (consult-customize
     consult-ripgrep consult-git-grep consult-grep
     consult-bookmark consult-recent-file
     :preview-key (kbd "C-SPC"))
    (consult-customize
     consult-theme
     :preview-key (list (kbd "C-SPC") :debounce 0.5 'any))
    )
  )


;; consult-dir ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(yx-require-package 'consult-dir)
(with-eval-after-load 'consult
  (define-key global-map (kbd "C-x C-d") #'consult-dir)
  (define-key vertico-map (kbd "C-x C-d") #'consult-dir)
  (define-key vertico-map (kbd "C-x C-j") #'consult-dir-jump-file)
  )

;; consult-yac
(yx-require-package 'consult-yasnippet)
(with-eval-after-load 'consult
  (define-key global-map (kbd "M-s y") #'consult-yasnippet)
  )

;; corfu ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(yx-require-package 'corfu)
(defun yx-corfu-setup ()
  (setq corfu-auto t)  ;; set befor global-corfu-mode
  (setq corfu-cycle t)
  (setq corfu-quit-at-boundary nil)
  (setq corfu-quit-no-match 'separator)
  (setq corfu-auto-delay 0.1)
  (setq corfu-auto-prefix 1)
  (setq corfu-echo-documentation nil)
  (add-hook 'eshell-mode-hook #'(lambda ()
                                  (setq-local corfu-auto nil)
                                  (corfu-mode)))
  (add-hook 'minibuffer-setup-hook #' (lambda ()
                                        (unless (bound-and-true-p vertico--input)
                                          (corfu-mode 1))))

  (with-eval-after-load 'corfu
    (define-key corfu-map (kbd "SPC") #'corfu-insert-separator)
    ;; corfu-quick
    (define-key corfu-map "\M-q" #'corfu-quick-complete)
    (define-key corfu-map "\C-q" #'corfu-quick-insert))
  (global-corfu-mode 1))
(add-hook 'after-init-hook #'yx-corfu-setup)

;; corfu-doc
(yx-require-package 'corfu-doc)
(add-hook 'corfu-mode-hook #'corfu-doc-mode)
(with-eval-after-load 'corfu-doc
  (setq corfu-doc-auto nil)
  (define-key corfu-map (kbd "M-p") #'corfu-doc-scroll-down) ;; corfu-next
  (define-key corfu-map (kbd "M-n") #'corfu-doc-scroll-up)  ;; corfu-previous
  (define-key corfu-map (kbd "M-d") #'corfu-doc-toggle)
  )

;; kind-icon
(yx-require-package 'kind-icon)
(with-eval-after-load 'corfu
  ;; (require 'kind-icon)
  (setq kind-icon-default-face 'corfu-default)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  )

;; cape
(yx-require-package 'cape)
(with-eval-after-load 'corfu
  (setq cape-dabbrev-min-length 3)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-ispell)
  ;; Silence the pcomplete capf, no errors or messages!
  ;; Important for corfu
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)
  )

(provide 'init-completion)
