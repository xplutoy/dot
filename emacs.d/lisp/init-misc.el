;; -*- coding: utf-8; lexical-binding: t; -*-
;; ui theme
;; 1, monokai-theme
;; 2, ef-themes
;; (yx-require-package 'gruvbox-theme)
;; (load-theme 'gruvbox-light-medium t)
(yx-require-package 'ef-themes)
(setq ef-themes-to-toggle '(ef-duo-light ef-winter)) ;; use ef-themes-toggle to switch
(load-theme 'ef-duo-light :no-confirm)

(yx-require-package 'ibuffer-vc)
(add-hook 'ibuffer-hook
          (lambda ()
            (ibuffer-vc-set-filter-groups-by-vc-root)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))))

;; @ https://lists.gnu.org/archive/html/bug-gnu-emacs/2012-07/msg01208.html
(let ((gls "/usr/local/bin/gls"))
  (if (file-exists-p gls) (setq insert-directory-program gls)))

(yx-require-package 'which-key)
(add-hook 'after-init-hook
          #'(lambda ()
              (setq which-key-idle-delay 1.5) ;; whick-key文挡上说必须在load之前设置
              (setq which-key-idle-secondary-delay 0.05)
              (setq which-key-popup-type 'minibuffer)
              (which-key-mode 1)
              ))

;; buffer-move
(yx-require-package 'buffer-move)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;; restart-emacs
(yx-require-package 'restart-emacs)

(yx-require-package 'posframe)  ;; sdcv dep
;; sdcv @https://github.com/manateelazycat/sdcv
(yx-run-with-idle-timer 2 #'(lambda ()
                              (add-to-list 'load-path (concat user-emacs-directory "nonelpa/sdcv"))
                              (require 'sdcv)
                              (setq sdcv-dictionary-simple-list (list "朗道英汉字典5.0")
                                    sdcv-dictionary-complete-list (list "朗道英汉字典5.0")
                                    sdcv-dictionary-data-dir "/Users/yx/.config/stardict/dic") ; set local sdcv dict dir
                              (global-set-key (kbd "M-s s") 'sdcv-search-pointer+)
                              ))

(when ON-MAC
  ;; emacs-rime
  (yx-require-package 'rime)
  (setq rime-translate-keybindings
  '("C-f" "C-b" "C-n" "C-p" "C-g" "C-v" "M-v" "<delete>"))
  (setq default-input-method "rime"
        rime-librime-root "~/.emacs.d/librime/dist"
        rime-user-data-dir "/Users/yx/Library/Rime"
        rime-show-candidate 'posframe
        rime-cursor "˰")
  (setq rime-inline-ascii-trigger 'shift-l)
  (setq rime-inline-ascii-holder ?x)
  (setq rime-posframe-properties
        (list :background-color "#333333"
              :foreground-color "#dcdccc"
              :internal-border-width 3))
  (setq rime-disable-predicates
        '(;; rime-predicate-after-alphabet-char-p
          rime-predicate-current-uppercase-letter-p
          ;; rime-predicate-current-input-punctuation-p
          ;; rime-predicate-prog-in-code-p
          ;; rime-predicate-in-code-string-p
          ;; rime-predicate-punctuation-after-space-cc-p
          ;; rime-predicate-space-after-cc-p
          ))
  )

;; cal-china-x
(yx-require-package 'cal-china-x)
(yx-run-with-idle-timer 2 #'(lambda ()
                              (require 'cal-china-x)
                              (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
                              (setq calendar-holidays (append cal-china-x-important-holidays
                                                              cal-china-x-general-holidays
                                                              holiday-general-holidays
                                                              holiday-christian-holidays))))


(provide 'init-misc)
