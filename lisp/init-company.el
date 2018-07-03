;; init-company.el --- Initialize company configurations.	-*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Company configurations.
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

(use-package company
  :diminish company-mode
  :init (add-hook 'after-init-hook #'global-company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection)
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         :map company-search-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next))
  :config
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map [return] nil)
  (setq company-tooltip-limit 10
        company-show-numbers t
        company-idle-delay 0.1
        company-echo-delay 0
        company-minimum-prefix-length 2
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-tooltip-align-annotations t)

  ;; Popup documentation for completion candidates (only when Emacs 26+)
  (when (>= emacs-major-version 26)
    (use-package company-quickhelp
      :bind (:map company-active-map
                  ("M-h" . company-quickhelp-manual-begin))
      :init (company-quickhelp-mode 1)
      :config
      (use-package pos-tip :demand)
      ;; Essential hack for company-quickhelp working on Emacs 26.1rc
      (pos-tip-show "")
      (setq company-quickhelp-delay 0.8)))

  ;; Support yas in commpany
  ;; Note: Must be the last to involve all backends
  (defvar company-enable-yas t
    "Enable yasnippet for all backends.")

  (defun company-backend-with-yas (backend)
    (if (or (not company-enable-yas)
            (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (setq company-backends (mapcar #'company-backend-with-yas company-backends)))

(provide 'init-company)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-company.el ends here
