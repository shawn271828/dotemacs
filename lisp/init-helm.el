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
         :map helm-command-map
         ;; use occur/moccur if not large files
         ("1" . helm-do-ag-this-file)
         ("2" . helm-do-ag-buffers)
         :map helm-find-files-map
         ("C-s" . helm-ff-run-grep-ag)
         :map isearch-mode-map
         ("M-o" . helm-occur-from-isearch))
  :config
  (require 'helm-config)
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  (helm-autoresize-mode t)
  (setq helm-autoresize-min-height            25
        helm-autoresize-max-height            0
        helm-split-window-inside-p            t
        helm-move-to-line-cycle-in-source     nil
        helm-scroll-amount                    10
        helm-echo-input-in-header-line        t
        helm-M-x-fuzzy-match                  t
        helm-buffers-fuzzy-matching           nil
        helm-recentf-fuzzy-match              nil
        helm-apropos-fuzzy-match              t)
  (with-eval-after-load 'spaceline-config
    (spaceline-helm-mode))

  ;; minibuffer hiding
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

  ;; helm-ag
  (use-package helm-ag
    :demand
    :init
    ;; For consistency, replace 'helm-grep-ag with 'helm-do-ag
    (fset 'helm-grep-ag 'helm-do-ag))
  
  ;; projectile integration
  (use-package helm-projectile
    :demand
    :config
    (helm-projectile-on)
    (with-eval-after-load 'projectile
      (setq projectile-completion-system 'helm)))

  ;; helm-descbinds
  (use-package helm-descbinds
    :bind (("C-h b" . helm-descbinds)
           ("<f1> b" . helm-descbinds)))
  
  ;; Use wgrep from github
  ;; This is for occur/moccur buffer only
  (require 'wgrep-helm))

(provide 'init-helm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-helm.el ends here
