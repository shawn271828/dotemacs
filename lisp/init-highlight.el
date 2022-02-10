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

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Rainbow mode
(use-package rainbow-mode)

;; Visualize TAB, (HARD) SPACE, NEWLINE
(use-package whitespace
  :ensure nil
  :diminish whitespace-mode
  :bind (("<f12>" . whitespace-mode))
  :config
  (setq whitespace-line-column fill-column))

(provide 'init-highlight)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-highlight.el ends here
