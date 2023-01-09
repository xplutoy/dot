;; -*- coding: utf-8; lexical-binding: t; -*-

;; minibuffer enhance
;; vertico ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package vertico
  :demand t
  :init
  (setq vertico-cycle t
        vertico-scroll-margin 1
        vertico-resize t)
  :config
  (vertico-mode 1)
  (vertico-mouse-mode 1)
  (vertico-indexed-mode 1)
  :bind (:map vertico-map
              ("M-q" . vertico-quick-insert) ;;vertico-quick
              ("C-q" . vertico-quick-exit)
              ("RET" . vertico-directory-enter) ;;vertico-directory
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  )

;; orderless ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package orderless
  :init
  (setq completion-styles '(basic orderless)
        completion-category-defaults nil
	completion-category-overrides '((file (styles . (basic partial-completion orderless)))
                                        (imenu (styles . (basic substring orderless)))
                                        (kill-ring (styles . (basic substring orderless)))
                                        (project-file (styles . (basic substring partial-completion orderless)))))
  )

;; embark ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package embark
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  (setq embark-help-key "?")
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

  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  :bind (("C-." . embark-act)
         ("C-," . embark-dwim)
         ("C-h B" . embark-bindings)
         :map embark-symbol-map
         ("D" . sdcv-search-pointer+))
  )

(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; consult ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package consult
  :init
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)
  (setq register-preview-delay 0.5
	register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)

  :config
  (setq consult-narrow-key "<")
  ;; (setq consult-preview-key "C-SPC")
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))

  :bind (("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ("C-x C-r" . consult-recent-file)
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ("M-y" . consult-yank-pop)
         ("C-h a" . consult-apropos)
         ("M-g e" . consult-compile-error) ;; M-g bindings (goto-map)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ("M-s f" . consult-find)  ;; M-s bindings (search-map)
         ("M-s d" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgre)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ("M-s e" . consult-isearch-histor)
         :map isearch-mode-map
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  )

;; consult-*** ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))
(use-package consult-project-extra
  :bind
  (("C-c p f" . consult-project-extra-find)
   ("C-c p o" . consult-project-extra-find-other-window)))
(use-package consult-yasnippet
  :bind (("M-s y" . consult-yasnippet))
  )
(use-package consult-eglot
  :bind (("M-s s" . consult-eglot-symbols))
  )

;; corfu ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package corfu
  :demand t
  :init
  (setq corfu-auto t  ;; set befor global-corfu-mode
        corfu-cycle t
        corfu-quit-at-boundary nil
        corfu-quit-no-match 'separator
        corfu-auto-delay 0
        corfu-auto-prefix 1
        corfu-preselect 'prompt
        corfu-echo-documentation nil)
  :config
  ;; (global-corfu-mode 1)
  ;; corfu plugin
  (corfu-popupinfo-mode 1)
  (corfu-echo-mode 1)
  (corfu-indexed-mode 1)
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ("S-TAB" . corfu-previous)
              ("SPC" . corfu-insert-separator)
              ("M-q" . corfu-quick-complete)
              ("C-q" . corfu-quick-insert))
  :hook ((emacs-lisp-mode prog-mode) . corfu-mode)
  )

(use-package corfu-terminal
  :unless (display-graphic-p)
  :after corfu
  :config
  (corfu-terminal-mode +1))

;; kind-icon
(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; cape
(use-package cape
  :init
  (setq cape-dabbrev-min-length 2)
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
