;; -*- lexical-binding: t no-byte-compile: t -*-

;; @see https://github.com/seagle0128/.emacs.d/**/custom-example.el
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
                                        :family font :height 160))
    (cl-loop for font in '("Inconsolata" "Source Code Pro" "Fira Code" "Menlo" "Monaco")
             when (font-installed-p font)
             return (set-face-attribute 'fixed-pitch nil
                                        :family font :height 1.0))
    (cl-loop for font in '("Source Serif Pro")
             when (font-installed-p font)
             return (set-face-attribute 'variable-pitch nil
                                        :family font :height 1.0))
    (cl-loop for font in '("Latin Modern Mono")
             when (font-installed-p font)
             return (set-face-attribute 'fixed-pitch-serif nil
                                        :family font :height 1.0))
    (cl-loop for font in '("Segoe UI Symbol" "Symbola" "Symbol")
             when (font-installed-p font)
             return (set-fontset-font t 'symbol (font-spec :family font)))
    (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji" "Segoe UI Emoji")
             when (font-installed-p font)
             return (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend))
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
(use-package ef-themes
  :init
  (setq ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui t
        ef-themes-disable-other-themes t
        ef-themes-region '(intense no-extend neutral)
        ef-themes-headings '((0 . (light variable-pitch 1.5))
                             (1 . (light variable-pitch 1.3))
                             (2 . (variable-pitch 1.1))
                             (t . (variable-pitch))))
  (mapc #'disable-theme custom-enabled-themes)
  (setq ef-themes-to-toggle '(ef-duo-light ef-dark))

  :config
  (ef-themes-select 'ef-dark)

  ;; yx-hide-mode-line
  (defun yx-hide-mode-line ()
    (interactive)
    (ef-themes-with-colors
      (custom-set-faces
       `(mode-line ((,c :foreground ,bg-mode-line :height 0.1)))
       `(mode-line-inactive ((,c :foreground ,bg-alt :height 0.1)))))
    )

  ;; hl-todo-keyword-faces
  (defun yx-ef-themes-hl-todo-faces ()
    "Configure `hl-todo-keyword-faces' with Ef themes colors.
The exact color values are taken from the active Ef theme."
    (ef-themes-with-colors
      (setq hl-todo-keyword-faces
            `(("HOLD" . ,yellow)
              ("TODO" . ,red)
              ("NEXT" . ,blue)
              ("DONT" . ,yellow-warmer)
              ("FAIL" . ,red-warmer)
              ("BUG" . ,red-warmer)
              ("DONE" . ,green)
              ("NOTE" . ,blue-warmer)
              ("HACK" . ,cyan)
              ("FIXME" . ,red-warmer)))))
  (yx-ef-themes-hl-todo-faces)
  (add-hook 'ef-themes-post-load-hook #'yx-ef-themes-hl-todo-faces)
  )

;; mode line

;; end init-ui ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-ui)
