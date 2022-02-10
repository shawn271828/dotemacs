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

(eval-when-compile
  (require 'init-const))

(use-package flycheck
  :ensure t
  :commands (flycheck-mode flycheck-list-errors flycheck-buffer)
  :init (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (setq flycheck-indication-mode 'right-margin)
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

;; Display Flycheck errors in GUI tooltips (only when Emacs 26+)
;; (when (>= emacs-major-version 26)
;;   (use-package flycheck-pos-tip
;;     :init (flycheck-pos-tip-mode 1)
;;     :config
;;     (setq flycheck-pos-tip-timeout 10)
;;     (setq flycheck-display-errors-delay 0.5)))

;; Colorful Flycheck mode line
;; (use-package flycheck-color-mode-line
;;   :init (add-hook 'flycheck-mode-hook #'flycheck-color-mode-line-mode))

(provide 'init-flycheck)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-flycheck.el ends here
