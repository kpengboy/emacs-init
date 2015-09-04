;; Copyright (c) 2014-2015 Kevin Peng
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

; multi-term keybindings
(setq term-unbind-key-list '())
(setq term-bind-key-alist
      `(("C-c c" . multi-term)
	("C-c p" . multi-term-prev)
	("C-c n" . multi-term-next)
	,@(if (fboundp 'scroll-up-command)
	      '(("C-c C-v" . scroll-up-command)
		("C-c M-v" . scroll-down-command))
	    '(("C-c C-v" . scroll-up)
	      ("C-c M-v" . scroll-down)))
	("C-c M-x" . execute-extended-command)))

; Misc.
(put 'upcase-region 'disabled nil)
(setq column-number-mode t)
(setq-default indent-tabs-mode nil)

; Highlight the current buffer unless in graphical mode (where the current
; buffer is already highlighted)
(unless (display-graphic-p)
  (set-face-foreground 'mode-line "green")
  (set-face-foreground 'mode-line-inactive "white"))

; C/C++ customization
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
