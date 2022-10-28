;; -*- coding: utf-8; lexical-binding: t; -*-
;; eshell
(add-to-list 'display-buffer-alist
             '("\\*e?shell\\*" display-buffer-in-direction
               (direction . bottom)
               (window . root)
               (window-height . 0.45)))

(defun yx-eshell-toggle ()
  "eshell toggle"
  (interactive)
  (if (equal major-mode 'eshell-mode)
      (quit-window)
    (eshell))
  )
(global-set-key (kbd "C-,") #'yx-eshell-toggle)

(setq eshell-prefer-lisp-functions t)
;; @see https://github.com/manateelazycat/aweshell
(with-eval-after-load 'eshell
  (defun yx-eshell-clear-buffer ()
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))
  (add-hook 'eshell-mode-hook #'(lambda () ;; @see https://github.com/noctuid/general.el/issues/80
                                  (add-to-list 'eshell-visual-commands "ssh")
                                  (define-key eshell-mode-map (kbd "C-l") #'yx-eshell-clear-buffer )))
  )

;; eshell prompt
;; @see https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org
(defun curr-dir-git-branch-string (pwd)
  "Returns current git branch as a string, or the empty string if
PWD is not in a git repo (or the git command is not found)."
  (interactive)
  (when (and (not (file-remote-p pwd))
             (eshell-search-path "git")
             (locate-dominating-file pwd ".git"))
    (let* ((git-url (shell-command-to-string "git config --get remote.origin.url"))
           (git-repo (file-name-base (string-trim git-url)))
           (git-output (shell-command-to-string (concat "git rev-parse --abbrev-ref HEAD")))
           (git-branch (string-trim git-output))
           (git-icon  "\xe0a0")
           (git-icon2 (propertize "\xf020" 'face `(:family "octicons"))))
      (concat git-repo " " git-icon2 " " git-branch))))

(defun pwd-replace-home (pwd)
  "Replace home in PWD with tilde (~) character."
  (interactive)
  (let* ((home (expand-file-name (getenv "HOME")))
         (home-len (length home)))
    (if (and
         (>= (length pwd) home-len)
         (equal home (substring pwd 0 home-len)))
        (concat "~" (substring pwd home-len))
      pwd)))

(defun pwd-shorten-dirs (pwd)
  "Shorten all directory names in PWD except the last two."
  (let ((p-lst (split-string pwd "/")))
    (if (> (length p-lst) 2)
        (concat
         (mapconcat (lambda (elm) (if (zerop (length elm)) ""
                               (substring elm 0 1)))
                    (butlast p-lst 2)
                    "/")
         "/"
         (mapconcat (lambda (elm) elm)
                    (last p-lst 2)
                    "/"))
      pwd)))  ;; Otherwise, we just return the PWD

(defun split-directory-prompt (directory)
  (if (string-match-p ".*/.*" directory)
      (list (file-name-directory directory) (file-name-base directory))
    (list "" directory)))

(defun eshell/eshell-local-prompt-function ()
  "A prompt for eshell that works locally (in that is assumes
that it could run certain commands) in order to make a prettier,
more-helpful local prompt."
  (interactive)
  (let* ((pwd        (eshell/pwd))
         (directory (split-directory-prompt
                     (pwd-shorten-dirs
                      (pwd-replace-home pwd))))
         (parent (car directory))
         (name   (cadr directory))
         (branch (curr-dir-git-branch-string pwd))
         
         (dark-env (eq 'dark (frame-parameter nil 'background-mode)))
         (for-bars                 `(:weight bold))
         (for-parent  (if dark-env `(:foreground "dark orange") `(:foreground "blue")))
         (for-dir     (if dark-env `(:foreground "orange" :weight bold)
                        `(:foreground "blue" :weight bold)))
         (for-git                  `(:foreground "green")))

    (concat
     (propertize "⟣─ "    'face for-bars)
     (propertize parent   'face for-parent)
     (propertize name     'face for-dir)
     (when branch
       (concat (propertize " ── "    'face for-bars)
               (propertize branch   'face for-git)))
     (propertize "\n"     'face for-bars)
     (propertize (if (= (user-uid) 0) " #" " $") 'face `(:weight ultra-bold))
     ;; (propertize " └→" 'face (if (= (user-uid) 0) `(:weight ultra-bold :foreground "red") `(:weight ultra-bold)))
     (propertize " "    'face `(:weight bold)))))

(setq eshell-highlight-prompt nil)
(setq-default eshell-prompt-function #'eshell/eshell-local-prompt-function)


(provide 'init-eshell)
