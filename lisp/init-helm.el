;; init-helm.el --- Initialize helm configurations.	-*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Helm configurations.
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
  (require 'init-custom))

(use-package helm
  :diminish helm-mode
  :init (helm-mode 1)
  :bind (("C-h a" . helm-apropos)
         ("C-x C-f" . helm-find-files)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("M-j" . helm-imenu)
         ("M-s o" . helm-occur)
         ("C-x r m" . helm-filtered-bookmarks)
         ("C-x b" . my-switch-to-buffer)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("C-i" . helm-execute-persistent-action)
         ("C-z" . helm-select-action)
         :map helm-find-files-map
         ("C-s" . helm-ff-run-grep-ag)
         :map isearch-mode-map
         ("M-o" . helm-occur-from-isearch))
  :config
  ;; General settings
  (require 'helm-config)
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  (helm-autoresize-mode 1)
  (setq helm-candidate-number-limit           100
        helm-autoresize-min-height            35
        helm-autoresize-max-height            0
        helm-split-window-inside-p            t
        helm-move-to-line-cycle-in-source     nil
        helm-scroll-amount                    10
        helm-echo-input-in-header-line        t
        helm-display-header-line              nil
        helm-M-x-fuzzy-match                  nil
        helm-buffers-fuzzy-matching           nil
        helm-recentf-fuzzy-match              nil
        helm-apropos-fuzzy-match              nil)

  (defun my-switch-to-buffer (arg)
        "Call helm-mini or helm-buffers-list."
        (interactive "P")
        (if arg
            (helm-mini)
          (helm-buffers-list)))

  ;; Beautify candidate separator
  (with-eval-after-load 'page-break-lines
    (setq helm-candidate-separator "\f")
    (push 'helm-major-mode page-break-lines-modes))

  ;; Eshell
  (add-hook 'eshell-mode-hook
            (lambda ()
              (eshell-cmpl-initialize)
              (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
              (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)))
  (add-hook 'eshell-mode-hook
            (lambda ()
              (define-key eshell-mode-map
                (kbd "M-p")
                'helm-eshell-history)))

  ;; Various shell completion
  (use-package pcomplete-extension)

  ;; Minibuffer hiding
  (add-hook 'helm-minibuffer-set-up-hook #'helm-hide-minibuffer-maybe)

  ;; Helm-ag
  (use-package helm-ag
    :demand)

  ;; Helm xref
  (use-package helm-xref
    :demand)

  ;; Projectile integration
  (use-package helm-projectile
    :demand
    :config
    (helm-projectile-on)
    (with-eval-after-load 'projectile
      (setq projectile-completion-system 'helm)))

  ;; Helm-descbinds
  (use-package helm-descbinds
    :bind (("C-h b" . helm-descbinds)))

  ;; Wgrep to enable modification of occur
  (use-package wgrep
    :demand))

(provide 'init-helm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-helm.el ends here
