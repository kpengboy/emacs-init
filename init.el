;; Copyright (c) 2014-2017 Kevin Peng
;; All rights reserved.
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

(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))
(require 'multi-term)
(require 'smart-tabs-mode)
(require 'fill-column-indicator)
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.
Ignores CHAR at point."
  'interactive)

;; multi-term keybindings
(setq term-unbind-key-list '("C-x" "M-x"))
(setq term-bind-key-alist
      `(("C-c c" . multi-term)
        ("C-c p" . multi-term-prev)
        ("C-c n" . multi-term-next)
        ,@(if (fboundp 'scroll-up-command)
              '(("C-c C-v" . scroll-up-command)
                ("C-c M-v" . scroll-down-command))
            '(("C-c C-v" . scroll-up)
              ("C-c M-v" . scroll-down)))
        ("C-c C-x" . term-send-raw)
        ("C-c M-x" . term-send-raw-meta)))

;; Misc.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(setq column-number-mode t)
(setq dired-dwim-target t)
(setq-default indent-tabs-mode nil)
(setq-default scroll-preserve-screen-position t)
(setq-default buffer-file-coding-system 'utf-8-unix)

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
(smart-tabs-insinuate 'c 'c++)
(setq c-default-style "k&r")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((smart-tabs-mode)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Make something for scrolling the screen
(unless (fboundp 'scroll-up-line)
  (defun scroll-up-line (&optional args)
    (interactive "P")
    (scroll-up (or args 1)))
  (defun scroll-down-line (&optional args)
    (interactive "P")
    (scroll-down (or args 1))))
(global-set-key (kbd "s-v") 'scroll-up-line)
(global-set-key (kbd "M-<down>") 'scroll-up-line)
(global-set-key (kbd "s-M-v") 'scroll-down-line)
(global-set-key (kbd "M-<up>") 'scroll-down-line)

;; Get a list of recent files
(recentf-mode 1)
(global-set-key (kbd "C-x M-r") 'recentf-open-files)

;; Make keybinding for zap-up-to-char
(global-set-key (kbd "M-Z") 'zap-up-to-char)

;; Use ibuffer as my buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)
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

(if (file-readable-p "~/.emacs.d/site.el")
    (load "~/.emacs.d/site.el"))
