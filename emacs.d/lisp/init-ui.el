;;; -*- lexical-binding: t no-byte-compile: t -*-
;; ui
(when (display-graphic-p)
  (fringe-mode 4)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))
(menu-bar-mode -1)

;;; @see https://github.com/seagle0128/.emacs.d/**/custom-example.el
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun yx-setup-fonts ()
  "Setup fonts."
  (when (display-graphic-p)
    ;; Set default font
    (cl-loop for font in '("Inconsolata" "Source Code Pro" "Fira Code" "Menlo" "Monaco")
             when (font-installed-p font)
             return (set-face-attribute 'default nil
                                        :family font
                                        :height 160))

    (cl-loop for font in '("Inconsolata" "Source Code Pro" "Fira Code" "Menlo" "Monaco")
             when (font-installed-p font)
             return (set-face-attribute 'fixed-pitch nil
                                        :family font
                                        :height 1.0))

    (cl-loop for font in '("Source Serif Pro")
             when (font-installed-p font)
             return (set-face-attribute 'variable-pitch nil
                                        :family font
                                        :height 1.0))

    (cl-loop for font in '("Latin Modern Mono")
             when (font-installed-p font)
             return (set-face-attribute 'fixed-pitch-serif nil
                                        :family font
                                        :height 1.0))

    ;; Specify font for all unicode characters
    (cl-loop for font in '("Segoe UI Symbol" "Symbola" "Symbol")
             when (font-installed-p font)
             return (set-fontset-font t 'symbol (font-spec :family font)))

    ;; Emoji
    (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji" "Segoe UI Emoji")
             when (font-installed-p font)
             return (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend))

    ;; Specify font for Chinese characters
    (cl-loop for font in '("LXGW WenKai Mono" "PingFang SC" "STFangsong")
             when (font-installed-p font)
             return (progn
                      (setq face-font-rescale-alist `((,font . 1.05))) ;; 1.05 magic number
                      (set-fontset-font t '(#x4e00 . #x9fff) (font-spec :family font))))))

(yx-setup-fonts)
(add-hook 'window-setup-hook #'yx-setup-fonts)
(add-hook 'server-after-make-frame-hook #'yx-setup-fonts)

;; theme
;; 1, monokai-theme
;; 2, ef-themes
(yx-require-package 'ef-themes)
(setq ef-themes-mixed-fonts t
      ef-themes-variable-pitch-ui t)
(setq ef-themes-headings
      '((0 . (light variable-pitch 1.5))
        (1 . (light variable-pitch 1.3))
        (2 . (variable-pitch 1.1))
        (t . (variable-pitch))))
(setq ef-themes-to-toggle '(ef-duo-light ef-winter)) ;; use ef-themes-toggle to switch
;;no need mode line.
(defun yx-mode-line-setup ()
  (interactive)
  (let ((bg (face-attribute 'mode-line :background))
        (ibg (face-attribute 'mode-line-inactive :background)))
    (set-face-attribute 'mode-line nil
                        :foreground bg
                        :height 0.1
                        :box nil)
    (set-face-attribute 'mode-line-inactive nil
                        :foreground ibg
                        :height 0.1
                        :box nil
                        :inherit 'unspecified)
    )
  )
(add-hook 'ef-themes-toggle #'yx-mode-line-setup)
(add-hook 'ef-themes-post-load-hook #'yx-mode-line-setup)
(mapc #'disable-theme custom-enabled-themes)
(ef-themes-select 'ef-summer)

;; end init-ui ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-ui)
