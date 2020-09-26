;; init-ui.el --- Initialize ui configurations.	-*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             UI configurations.
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

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

;; Title
(setq frame-title-format
      '("XEmacs"  ": "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
(setq icon-title-format frame-title-format)

;; Maximize frame
(add-hook 'after-init-hook #'toggle-frame-maximized)

;; Menu/Tool/Scroll bars
(unless sys/mac-x-p (menu-bar-mode -1))
(and (bound-and-true-p tool-bar-mode) (tool-bar-mode -1))
(and (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(and (bound-and-true-p horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

;; Margin size for flycheck
(setq-default right-margin-width 2)

;; (use-package nyan-mode
;;  :init (add-hook 'after-init-hook #'nyan-mode)
;;  :config
;;  (setq nyan-bar-length 32))

(when (boundp 'ns-pop-up-frames)
  (setq ns-pop-up-frames nil))

;; Don't use GTK+ tooltip
(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))

;; Transparency
;; (use-package seethru
;;   :bind (("C-M-8" . (lambda () (interactive) (seethru-relative -2)))
;;          ("C-M-9" . (lambda () (interactive) (seethru-relative 2)))
;;          ("C-M-0" . (lambda () (interactive) (seethru 100)))))

;; Show native line numbers if possible (Emacs 25 nlinum/linum is full of hack and bugs)
(when (fboundp 'display-line-numbers-mode)
  (add-hook 'after-init-hook #'global-display-line-numbers-mode))

;; Prettify symbols
(when (boundp 'global-prettify-symbols-mode)
  (add-hook 'after-init-hook #'global-prettify-symbols-mode))

;; Cursor beacon (only work well on Emacs 26 for macos)
;; (when (>= emacs-major-version 26)
;;  (use-package beacon
;;    :diminish beacon
;;    :init (add-hook 'after-init-hook #'beacon-mode)
;;    :config
;;    (setq-default beacon-lighter "")
;;    (setq-default beacon-size 10)
;;    (setq-default beacon-color (face-attribute 'cursor :background))))

;; Fill column indication
(use-package fill-column-indicator
  :bind ("H-<f12>" . fci-mode)
  :config
  (setq fci-rule-width 2))

;; Disable bold face globally
(defun replace-bold-with-normal (args)
  (mapcar (lambda (x) (if (eq x 'bold) 'normal x)) args))
(advice-add 'set-face-attribute :filter-args #'replace-bold-with-normal)

;; Dimmer
(use-package dimmer
  :demand
  :init
  (setq dimmer-exclusion-regexp "^\*helm.*\\|^ \*Minibuf-.*\\|^ \*Echo.*")
  (add-hook 'after-init-hook 'dimmer-mode))

;; Mode line
;; (use-package doom-modeline
;;   :ensure t
;;   :init (doom-modeline-mode 1))

(provide 'init-ui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui.el ends here
