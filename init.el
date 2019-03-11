;; Copyright (c) 2014-2019 Kevin Peng and contributors
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;; 1. Redistributions of source code must retain the above copyright notice,
;;    this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;    this list of conditions and the following disclaimer in the documentation
;;    and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

(require 'cl-lib)
(require 'package)

(nconc package-archives '(("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(setq package-selected-packages
  '(;;multi-term
    smart-tabs-mode
    ;;fill-column-indicator
    undo-tree
    delight
    puppet-mode
    multi-web-mode
    rust-mode))

(unless (cl-every #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (dolist (p package-selected-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))
(require 'multi-term)
(require 'fill-column-indicator)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.
Ignores CHAR at point."
  'interactive)

;; multi-term keybindings
(setq term-unbind-key-list '("C-x" "M-x" "C-b"))
(setq term-bind-key-alist
      '(("C-b c" . multi-term)
        ("C-b p" . multi-term-prev)
        ("C-b n" . multi-term-next)
        ("C-b [" . term-line-mode)
        ("C-b C-b" . term-send-raw)
        ("C-c C-v" . scroll-up-command)
        ("C-c M-v" . scroll-down-command)
        ("C-c C-x" . term-send-raw)
        ("C-c M-x" . term-send-raw-meta)))

;; Misc variables
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(setq column-number-mode t)
(setq dired-dwim-target t)
(setq-default indent-tabs-mode nil)
(setq-default scroll-preserve-screen-position t)
(setq-default buffer-file-coding-system 'utf-8-unix)

;; Misc keybindings
(defun backward-whitespace (arg)
  "Like `forward-whitespace', but moves backward."
  (interactive "^p")
  (forward-whitespace (* -1 arg)))
(global-set-key (kbd "M-Z") 'zap-up-to-char)
(global-set-key (kbd "M-F") 'forward-whitespace)
(global-set-key (kbd "M-B") 'backward-whitespace)

;; Highlight the active window when using inferior terminals that don't support
;; grayscale
(defun my-set-highlight-active-window (frame)
  (unless (display-grayscale-p (frame-terminal frame))
    (set-face-foreground 'mode-line "green" frame)
    (set-face-foreground 'mode-line-inactive "white" frame)))
(add-hook 'after-make-frame-functions 'my-set-highlight-active-window)
(add-hook 'window-setup-hook
          (lambda () (my-set-highlight-active-window (selected-frame))))

;; C/C++ customization
(setq c-default-style "stroustrup")
;; Enable smart tabs in c-mode and c++-mode when the file/directory local
;; variable `use-smart-tabs` is t
(defvar-local use-smart-tabs nil)
(add-hook 'hack-local-variables-hook
          (lambda ()
            (if (and (memq major-mode '(c-mode c++-mode))
                     use-smart-tabs)
                (progn
                  (smart-tabs-mode-enable)
                  (smart-tabs-advice c-indent-line c-basic-offset)
                  (smart-tabs-advice c-indent-region c-basic-offset)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((use-smart-tabs . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Make something for scrolling the screen
(global-set-key (kbd "s-v") 'scroll-up-line)
(global-set-key (kbd "M-<down>") 'scroll-up-line)
(global-set-key (kbd "ESC <down>") 'scroll-up-line)
(global-set-key (kbd "s-M-v") 'scroll-down-line)
(global-set-key (kbd "M-<up>") 'scroll-down-line)
(global-set-key (kbd "ESC <up>") 'scroll-down-line)

;; Get a list of recent files
(recentf-mode 1)
(global-set-key (kbd "C-x M-r") 'recentf-open-files)

;; Use ibuffer as my buffer list
(substitute-key-definition 'list-buffers 'ibuffer global-map)
(setq ibuffer-expert t)
(add-hook 'ibuffer-mode-hook 'ibuffer-auto-mode)

;; Don't display splash screen if we already have another file open
(dolist (arg (cdr command-line-args) nil)
  (unless (string-prefix-p "-" arg)
    (setq inhibit-startup-screen t)))

;; Define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

;; Use ido-mode for buffer completion
(ido-mode 'buffers)
(setq ido-enable-flex-matching t)

;; Use undo-tree everywhere, and don't have it pollute the mode line
(delight 'undo-tree-mode nil 'undo-tree)
(global-undo-tree-mode)

;; Use multi-web-mode
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags
      '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
        (js-mode  "<script[^>]*>" "</script>")
        (css-mode "<style[^>]*>" "</style>")))
(setq mweb-filename-extensions
      '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)

;; I type M-x make too often, so I gave up trying to fight it
(defalias 'make 'compile)

(if (file-readable-p "~/.emacs.d/site.el")
    (load "~/.emacs.d/site.el"))
