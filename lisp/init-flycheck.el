;; init-flycheck.el --- Initialize flycheck configurations.	-*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Flycheck configurations.
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

(use-package flycheck
  :ensure hydra
  :init (add-hook 'prog-mode-hook #'global-flycheck-mode)
  :bind ("C-c f" . hydra-flycheck/body)
  :config
  (setq flycheck-indication-mode 'left-fringe)
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (defhydra hydra-flycheck ()
    "Flycheck"
    ("n" flycheck-next-error "next error")
    ("p" flycheck-previous-error "previous error")))

;; Display Flycheck errors in GUI tooltips
(use-package flycheck-pos-tip
  :init (flycheck-pos-tip-mode 1)
  :config (setq flycheck-pos-tip-timeout 15))

;; Colorful Flycheck mode line
(use-package flycheck-color-mode-line
  :init (add-hook 'flycheck-mode-hook #'flycheck-color-mode-line-mode))

(provide 'init-flycheck)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-flycheck.el ends here
