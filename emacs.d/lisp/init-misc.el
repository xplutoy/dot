;; -*- coding: utf-8; lexical-binding: t; -*-
;; general
(use-package general
  :demand t
  :config
  (general-evil-setup t)

  ;; use leaderkey `,' for programing and `SPC' for other
  (general-create-definer yx-comma-leader-def
    :prefix ","
    :states '(normal visual emacs))

  (yx-comma-leader-def
   "mf" 'mark-defun
   "bf" 'beginning-of-defun
   "bu" 'backward-up-list
   "ef" 'end-of-defun
   "eb" 'eval-buffer
   "ee" 'eval-expression
   "hf" 'find-function

   "fe" 'flyspell-goto-next-error
   "fa" 'flyspell-auto-correct-word

   "ar" 'align-regexp
   "xv" 'vc-next-action
   "vg" 'vc-annotate
   "vm" 'vc-msg-show
   )

  (general-create-definer yx-space-leader-def
    :prefix "SPC"
    :states '(normal visual emacs))

  (yx-space-leader-def
   "ff" 'find-file
   "fo" 'find-file-other-window
   "dj" 'dired-jump
   "bk" 'kill-buffer-and-window

   "x0" 'delete-window
   "x1" 'delete-other-windows
   "x2" 'split-window-vertically
   "x3" 'split-window-horizontally
   "xa" 'split-window-vertically
   "xd" 'split-window-horizontally
   "s0" 'delete-window
   "s1" 'delete-other-windows
   "s2" 'split-window-vertically
   "s3" 'split-window-horizontally
   "sq" 'delete-window
   "sa" 'split-window-vertically
   "sd" 'split-window-horizontally
   "oo" 'delete-other-windows

   "tff" 'toggle-frame-fullscreen
   "tfm" 'toggle-frame-maximized
   "mh" 'mark-whole-buffer
   "sc" 'shell-command
   "jj" 'scroll-other-window
   "kk" 'scroll-other-window-up
   "hml" 'yx-hide-mode-line
   "te" 'eshell/yx-eshell-toggle
   ))

;; exec-path-from-shell
(use-package exec-path-from-shell
  :if ON-MAC
  :demand t
  :config
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

;; diminish
(use-package diminish
  :config
  (diminish 'eldoc-mode)
  (diminish 'abbrev-mode)
  (diminish 'subword-mode)
  (diminish 'superword-mode)
  (with-eval-after-load 'hideshow
    (diminish 'hs-minor-mode))
  )

;;helpful
(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

;; ibuffer-vc
(use-package ibuffer-vc
  :init
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

(use-package which-key
  :diminish
  :init
  (setq which-key-idle-delay 1.0
        which-key-idle-secondary-delay 0.05)
  :config
  (which-key-setup-side-window-right-bottom)
  ;; (setq which-key-popup-type 'minibuffer)
  (which-key-mode 1)
  )

;; buffer-move
(use-package buffer-move
  :bind (("<C-S-up>" . buf-move-up)
         ("<C-S-down>" . buf-move-down)
         ("<C-S-left>" . buf-move-left)
         ("<C-S-right>" . buf-move-right)))

;; ace-window
(use-package ace-window
  :init
  (setq aw-background t
        aw-scope 'frame
        aw-dispatch-always t
        ;; aw-ignored-buffers
        aw-minibuffer-flag nil)
  :bind ("M-o" . ace-window)
  :general
  (yx-space-leader-def    "aw" 'ace-swap-window
                          "af" 'ace-maximize-window
                          "xo" 'ace-window))

;; avy
(use-package avy
  :init
  (setq avy-style 'at
        avy-timeout-seconds 0.8)
  :bind (("M-g w" . avy-goto-word-1)
         ("M-g c" . avy-goto-char-timer))
  )

;; restart-emacs
(use-package restart-emacs)

;; sdcv @https://github.com/manateelazycat/sdcv
(use-package posframe)  ;; sdcv dep
(use-package sdcv
  :ensure nil
  :demand t
  :load-path "nonelpa/sdcv"
  :init
  (setq sdcv-dictionary-simple-list (list "朗道英汉字典5.0")
        sdcv-dictionary-complete-list (list "朗道英汉字典5.0")
        sdcv-dictionary-data-dir "/Users/yx/.config/stardict/dic")
  :general
  (yx-space-leader-def
   "ds" 'sdcv-search-input
   "dt" 'sdcv-search-pointer+
   )
  )

;; emacs-rime
(use-package rime
  :when ON-MAC
  :init
  (setq rime-translate-keybindings '("C-f" "C-b" "C-n" "C-p" "C-g" "C-v" "M-v" "<delete>")
        default-input-method "rime"
        rime-librime-root "~/.emacs.d/librime/dist"
        rime-user-data-dir "/Users/yx/Library/Rime"
        rime-show-candidate 'posframe
        rime-cursor "˰"
        rime-inline-ascii-trigger 'shift-l
        rime-inline-ascii-holder ?x
        rime-posframe-properties  (list :background-color "#333333"
                                        :foreground-color "#dcdccc"
                                        :internal-border-width 3)
        rime-disable-predicates '(;; rime-predicate-after-alphabet-char-p
                                  rime-predicate-current-uppercase-letter-p
                                  ;; rime-predicate-current-input-punctuation-p
                                  ;; rime-predicate-prog-in-code-p
                                  ;; rime-predicate-in-code-string-p
                                  ;; rime-predicate-punctuation-after-space-cc-p
                                  ;; rime-predicate-space-after-cc-p
                                  )
        ))

;; key freq
(use-package keyfreq
  :init
  (setq keyfreq-excluded-commands '(self-insert-command
                                    forward-char
                                    backward-char
                                    previous-line
                                    next-line))
  :custom
  (keyfreq-file "~/.emacs.d/keyfreq")
  (keyfreq-file-lock "~/.emacs.d/keyfreq.lock")
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  )

;; crux
(use-package crux
  :config (progn (crux-with-region-or-buffer indent-region)
                 (crux-reopen-as-root-mode))
  :bind (("C-c o" . crux-open-with)
         ("S-<return>" . crux-smart-open-line)
         ("C-S-<return>" . smart-open-line-above)
         ("C-c k" . crux-kill-other-buffers)
         ("C-c z" . crux-indent-defun)
         ("C-c i" . crux-ispell-word-then-abbrev)
         ("C-c r" . crux-rename-file-and-buffer)
         ("C-c E" . crux-sudo-edit)
         ("C-c D" . crux-delete-file-and-buffer)
         ([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([remap kill-whole-line] . crux-kill-whole-line) ;; C-S-<backspace>
         )
  )

;; easy-kill
(use-package easy-kill
  :demand t
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark))
  )

;; all-the-icons
(use-package all-the-icons
  :if (display-graphic-p))


(provide 'init-misc)
