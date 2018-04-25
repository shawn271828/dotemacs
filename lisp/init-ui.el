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

;; Start maximised (cross-platf)
(use-package maxframe
  :init (add-hook 'window-setup-hook #'maximize-frame t))

;; Title
(setq frame-title-format
      '("GNU Emacs " emacs-version "@" user-login-name " : "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
(setq icon-title-format frame-title-format)

;; Menu/Tool/Scroll bars
(unless sys/mac-x-p (menu-bar-mode -1))
(and (bound-and-true-p tool-bar-mode) (tool-bar-mode -1))
(and (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(and (bound-and-true-p horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

;; Nyan cat
;; (use-package nyan-mode
;;   :init (add-hook 'after-init-hook #'nyan-mode))

;; Modeline
(use-package spaceline-config
  :ensure spaceline
  :diminish eldoc-mode
  :init (add-hook 'after-init-hook 'spaceline-emacs-theme)
  :config
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-modified)
  (setq powerline-default-separator 'box)
  (setq powerline-image-apple-rgb sys/mac-x-p))

;; Don't open a file in a new frame
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

;; Show native line numbers if possible
(when (fboundp 'display-line-numbers-mode)
  (use-package display-line-numbers
    :ensure nil
    :init (add-hook 'prog-mode-hook #'display-line-numbers-mode)))

;; Prettify symbols
(when (boundp 'global-prettify-symbols-mode)
  (add-hook 'after-init-hook #'global-prettify-symbols-mode))

;; Cursor beacon
(use-package beacon
  :diminish beacon
  :init (add-hook 'after-init-hook #'beacon-mode)
  :config
  (setq-default beacon-lighter "")
  (setq-default beacon-size 20)
  (setq-default beacon-color "IndianRed1"))

;; Fill column indication
(use-package fill-column-indicator
  :bind ("<f12>" . fci-mode)
  :config
  (setq fci-rule-width 2)
  (setq fci-rule-color "green"))

;; Fonts
(use-package cnfonts
  :init (add-hook 'after-init-hook #'cnfonts-enable)
  :bind (("C-M--" . cnfonts-decrease-fontsize)
         ("C-M-=" . cnfonts-increase-fontsize))
  :config
  (setq cnfonts-keep-frame-size nil)
  (add-hook 'window-setup-hook
            (lambda ()
              (setq cnfonts-keep-frame-size t)))
  ;; `cnfonts' has issue on Emacs 26
  (balance-windows)

  (setq cnfonts-use-cache t)
  (setq cnfonts-profiles
        '("program1" "program2" "program3" "org-mode" "read-book"))
  (setq cnfonts--profiles-steps '(("program1" . 4)
                                  ("program2" . 5)
                                  ("program3" . 3)
                                  ("org-mode" . 6)
                                  ("read-book" . 8))))

;; Dimmer
(use-package dimmer
  :init
  ;; Do not dim helm and minibuffer
  (setq dimmer-exclusion-regexp "^\*helm.*\\|^ \*Minibuf-.*")
  (add-hook 'after-init-hook 'dimmer-mode))

(provide 'init-ui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui.el ends here
