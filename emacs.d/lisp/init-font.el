;;; -*- lexical-binding: t no-byte-compile: t -*-
;;; @see https://github.com/seagle0128/.emacs.d/**/custom-example.el

;; Fonts
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun yx/setup-fonts ()
  "Setup fonts."
  (when (display-graphic-p)
    ;; Set default font
    (cl-loop for font in '("Inconsolata" "Source Code Pro" "Fira Code" "Menlo" "Monaco")
             when (font-installed-p font)
             return (set-face-attribute 'default nil
                                        :family font
                                        :height (cond (ON-MAC 150)
                                                      (ON-WINDOWS 110)
                                                      (t 100))))

    ;; Specify font for all unicode characters
    (cl-loop for font in '("Segoe UI Symbol" "Symbola" "Symbol")
             when (font-installed-p font)
             return (set-fontset-font t 'symbol (font-spec :family font)))

    ;; Emoji
    (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji" "Segoe UI Emoji")
             when (font-installed-p font)
             return (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend))

    ;; Specify font for Chinese characters
    (cl-loop for font in '("LXGW WenKai" "PingFang SC" "STFangsong")
             when (font-installed-p font)
             return (progn
                      (setq face-font-rescale-alist `((,font . 1.0)))
                      (set-fontset-font t '(#x4e00 . #x9fff) (font-spec :family font))))))

(yx/setup-fonts)
(add-hook 'window-setup-hook #'yx/setup-fonts)
(add-hook 'server-after-make-frame-hook #'yx/setup-fonts)

(provide 'init-font)


