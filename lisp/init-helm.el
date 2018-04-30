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
  :init (helm-mode 1)
  :bind (("C-x b" . helm-mini)
         ("C-h a" . helm-apropos)
         ("<f1> a" . helm-apropos)
         ("C-x C-f" . helm-find-files)
         ("M-x" . helm-M-x)
         ([remap occur] . helm-occur)
         :map helm-find-files-map
         ("C-s" . helm-ff-run-grep-ag)
         :map isearch-mode-map
         ("M-o" . helm-occur-from-isearch))
  :config
  ;; General settings
  (require 'helm-config)
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  (setq helm-candidate-number-limit           100
        helm-autoresize-min-height            25
        helm-autoresize-max-height            0
        helm-split-window-inside-p            t
        helm-move-to-line-cycle-in-source     nil
        helm-scroll-amount                    10
        helm-echo-input-in-header-line        t
        helm-M-x-fuzzy-match                  nil
        helm-buffers-fuzzy-matching           t
        helm-recentf-fuzzy-match              t
        helm-apropos-fuzzy-match              nil)
  (helm-autoresize-mode 1)

  (frame-list)
  ;; Display some helm sessions in a separate frame
  ;; More details on `https://github.com/emacs-helm/helm/wiki/frame'
  (setq helm-actions-inherit-frame-settings t)
  (setq helm-display-buffer-reuse-frame t) ;Emacs 26+ regression
  (setq helm-display-function #'helm-display-buffer-in-own-frame)

  ;; Raise gc threshold during minibuffer mode (including helm)
  (defun my-minibuffer-setup-hook ()
    (setq gc-cons-threshold (* 128 1024 1024)))

  (defun my-minibuffer-exit-hook ()
    (setq gc-cons-threshold 800000))
  
  (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
  (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

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
  ;; (defun spacemacs//helm-hide-minibuffer-maybe ()
  ;;   "Hide minibuffer in Helm session if we use the header line as input field."
  ;;   (when (with-helm-buffer helm-echo-input-in-header-line)
  ;;     (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
  ;;       (overlay-put ov 'window (selected-window))
  ;;       (overlay-put ov 'face
  ;;                    (let ((bg-color (face-background 'default nil)))
  ;;                      `(:background ,bg-color :foreground ,bg-color)))
  ;;       (setq-local cursor-type nil))))
  ;; (add-hook 'helm-minibuffer-set-up-hook #'spacemacs//helm-hide-minibuffer-maybe)

  ;; Helm-ag
  (use-package helm-ag
    :demand
    :bind (("M-s M-o" . helm-do-ag-this-file)
           ("M-s s" . helm-do-ag-buffers)
           ("M-s M-s" . helm-do-ag-buffers))
    :init
    ;; For consistency, replace 'helm-grep-ag with 'helm-do-ag
    (fset 'helm-grep-ag 'helm-do-ag))
  
  ;; Projectile integration
  (use-package helm-projectile
    :demand
    :config
    (helm-projectile-on)
    (with-eval-after-load 'projectile
      (setq projectile-completion-system 'helm)))

  ;; Helm-descbinds
  (use-package helm-descbinds
    :bind (("C-h b" . helm-descbinds)
           ("<f1> b" . helm-descbinds)))
  
  ;; Use wgrep from github to enable wgrep from helm-occur
  (require 'wgrep-helm))

(provide 'init-helm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-helm.el ends here
