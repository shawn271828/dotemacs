;; init-highlight.el --- Initialize highlight configurations.	-*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Highlight configurations.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; Highlight the current line
(use-package hl-line
  :ensure nil
  :init (add-hook 'after-init-hook #'global-hl-line-mode))

;; Highlight symbols
(use-package symbol-overlay
  :diminish symbol-overlay-mode
  :bind (("M-i" . symbol-overlay-put)
         :map symbol-overlay-map
         ;; ("M-n" . symbol-overlay-switch-forward)
         ;; ("M-p" . symbol-overlay-switch-backward)
         ("M-q" . symbol-overlay-remove-all))
  :init (add-hook 'prog-mode-hook #'symbol-overlay-mode))

;; Highlight matching paren
(use-package paren
  :ensure nil
  :init (add-hook 'after-init-hook #'show-paren-mode)
  :config
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-when-point-in-periphery t)
  (setq show-paren-style 'parenthesis)
  (face-spec-set 'show-paren-match
                 '((t :foreground nil
                      :background nil
                      :underline t))
                 nil))

;; Highlight indentions
(use-package indent-guide
  :diminish indent-guide-mode
  :bind (("<f12>" . indent-guide-global-mode))
  :config
  (setq indent-guide-char ">")
  (setq indent-guide-recursive t))

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Rainbow mode
(use-package rainbow-mode)

;; Visualize TAB, (HARD) SPACE, NEWLINE
(use-package whitespace
  :ensure nil
  :diminish whitespace-mode
  ;; :bind (("M-<f12>" . whitespace-mode))
  :config
  (setq whitespace-line-column fill-column)
  (setq whitespace-action '(report-on-bogus))
  (setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab)))

(use-package vimish-fold
  :init (add-hook 'after-init-hook #'vimish-fold-global-mode))

(provide 'init-highlight)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-highlight.el ends here
