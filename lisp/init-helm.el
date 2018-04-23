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

(use-package helm
  :diminish helm-mode
  :ensure helm-descbinds
  :init (helm-mode 1)
  :bind (("C-x b" . helm-mini)
         ("C-h b" . helm-descbinds)
         ("C-h a" . helm-apropos)
         ("C-x C-f" . helm-find-files)
         ("M-x" . helm-M-x)
         :map helm-command-map
         ("M-o" . helm-occur)
         :map isearch-mode-map
         ("M-o" . helm-occur-from-isearch))
  :config
  (require 'helm-config)
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  (helm-autoresize-mode t)
  (setq helm-autoresize-min-height            30
        helm-autoresize-max-height            0
        helm-split-window-inside-p            t
        helm-move-to-line-cycle-in-source     t
        helm-scroll-amount                    8
        helm-echo-input-in-header-line        t
        helm-M-x-fuzzy-match                  nil
        helm-buffers-fuzzy-matching           nil
        helm-recentf-fuzzy-match              nil)

  ;; minibuf hiding
  (defun spacemacs//helm-hide-minibuffer-maybe ()
    "Hide minibuffer in Helm session if we use the header line as input field."
    (when (with-helm-buffer helm-echo-input-in-header-line)
      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
        (overlay-put ov 'window (selected-window))
        (overlay-put ov 'face
                     (let ((bg-color (face-background 'default nil)))
                       `(:background ,bg-color :foreground ,bg-color)))
        (setq-local cursor-type nil))))
  (add-hook 'helm-minibuffer-set-up-hook #'spacemacs//helm-hide-minibuffer-maybe)

  ;; projectile integration
  (use-package helm-projectile
    :defer 1
    :config
    (use-package helm-ag)
    (helm-projectile-on)
    (with-eval-after-load 'projectile
      (setq projectile-completion-system 'helm)))

  ;; use wgrep from github
  (require 'wgrep-helm))

(provide 'init-helm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-helm.el ends here
