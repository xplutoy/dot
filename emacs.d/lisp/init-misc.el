;; -*- coding: utf-8; lexical-binding: t; -*-
;; exec-path-from-shell
(use-package exec-path-from-shell
  :if (or (memq window-system '(mac ns)) (daemonp))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; ibuffer-vc
(use-package ibuffer-vc
  :init
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

(use-package which-key
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
  )

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
  :load-path "nonelpa/sdcv"
  :init
  (setq sdcv-dictionary-simple-list (list "朗道英汉字典5.0")
        sdcv-dictionary-complete-list (list "朗道英汉字典5.0")
        sdcv-dictionary-data-dir "/Users/yx/.config/stardict/dic")
  :bind (("M-s d" . sdcv-search-pointer+)
         ;; ("M-s S" . sdcv-search-input)
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


(provide 'init-misc)
