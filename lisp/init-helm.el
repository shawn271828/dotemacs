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
  :bind (("C-x b" . helm-mini)
         ("C-h a" . helm-apropos)
         ("<f1> a" . helm-apropos)
         ("C-x C-f" . helm-find-files)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("M-j" . helm-imenu)
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
        helm-autoresize-min-height            25
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

  ;; Beautify candidate separator
  (with-eval-after-load 'page-break-lines
    (setq helm-candidate-separator "\f")
    (push 'helm-major-mode page-break-lines-modes))

  (when my-helm-in-frame
    ;; Display some helm sessions in a separate frame
    ;; More details on `https://github.com/emacs-helm/helm/wiki/frame'
    (setq helm-actions-inherit-frame-settings t)
    (setq helm-display-buffer-reuse-frame t)
    (setq helm-display-buffer-width 72)
    (setq helm-display-buffer-height 20)
    ;; (setq helm-display-function #'helm-default-display-buffer)
    (setq helm-display-function #'helm-display-buffer-in-own-frame))

  (defun my-make-commands-in-frame (commands-list)
    "Make command in COMMANDS-LIST show in separate frame."
    (when my-helm-in-frame
      (dolist (command commands-list)
        (add-to-list 'helm-commands-using-frame command))))

  (my-make-commands-in-frame '(completion-at-point
                               helm-occur
                               helm-occur-from-isearch
                               helm-M-x
                               helm-apropos))

  ;; Raise gc threshold during minibuffer mode (including helm)
  (defun my-minibuffer-setup-hook ()
    (setq gc-cons-threshold (* 20 1024 1024)))

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
    :bind (("M-s o" . helm-do-ag-this-file)
           ("M-s s" . helm-do-ag)
           ("M-s a" . helm-do-ag-buffers)
           ("M-s M-o" . helm-occur))
    :config
    (fset 'helm-grep-ag 'helm-do-ag)
    ;; (setq helm-ag-base-command "rg --vimgrep --no-heading --smart-case")
    (my-make-commands-in-frame '(helm-do-ag
                                 helm-do-ag-buffers
                                 helm-do-ag-this-file
                                 helm-ag
                                 helm-ag-buffers)))

  ;; Company integration
  ;; (use-package helm-company
  ;;   :demand
  ;;   :config
  ;;   (with-eval-after-load 'company
  ;;     (define-key company-mode-map (kbd "H-/") 'helm-company)
  ;;     (my-make-commands-in-frame '(helm-company))
  ;;     ;; Workaround strange company complete pop up after helm-company cancel
  ;;     (defun my-hack-to-helm-company ()
  ;;       (company-cancel))
  ;;     (advice-add 'helm-company :after 'my-hack-to-helm-company)))

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
  (use-package wgrep
    :load-path "site-lisp/wgrep"
    :demand))

(provide 'init-helm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-helm.el ends here
