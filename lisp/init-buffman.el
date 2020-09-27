;; init-ibuffer.el --- Initialize buffer configurations.	-*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             buffer configurations.
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

;; Tabs
(use-package centaur-tabs
  :demand
  :bind (("C-c C-<left>" . centaur-tabs-backward)
         ("C-c C-<right>" . centaur-tabs-forward)
         ("C-c C-<up>" . centaur-tabs-forward-group)
         ("C-c C-<down>" . centaur-tabs-backward-group)
         ("C-c b" . helm-centaur-tabs-switch-group))
  :config
  ;; The original function uses macro when helm is not present.
  (defun helm-centaur-tabs-switch-group ()
    (interactive)
    (when (featurep 'helm)
      (require 'helm)
      (helm :sources (helm-build-sync-source "Centaur-Tabs Group"
				  :candidates #'centaur-tabs-get-groups
				  :action '(("Switch to group" . centaur-tabs-switch-group))))))

  (setq centaur-tabs-style "bar"
        centaur-tabs-set-icons nil
        centaur-tabs-set-modified-marker nil
        centaur-tabs-show-navigation-buttons nil
        centaur-tabs-set-close-button nil
        centaur-tabs-set-bar 'under)

  (centaur-tabs-headline-match)
  (centaur-tabs-mode t)

  (defun centaur-tabs-hide-tab (x)
  "Do no to show buffer X in tabs."
  (let ((name (format "%s" x)))
    (or
     ;; Current window is not dedicated window.
     (window-dedicated-p (selected-window))

     ;; Buffer name not match below blacklist.
     (string-prefix-p "*epc" name)
     (string-prefix-p "*helm" name)
     (string-prefix-p "*Helm" name)
     (string-prefix-p "*Compile-Log*" name)
     (string-prefix-p "*lsp" name)
     (string-prefix-p "*company" name)
     (string-prefix-p "*Flycheck" name)
     (string-prefix-p "*tramp" name)
     (string-prefix-p " *Mini" name)
     (string-prefix-p "*help" name)
     (string-prefix-p "*straight" name)
     (string-prefix-p " *temp" name)
     (string-prefix-p "*Help" name)

     (string-prefix-p "magit" name)

     ;; Is not magit buffer.
     (and (string-prefix-p "magit" name)
	  (not (file-name-extension name)))
     )))

  (centaur-tabs-group-by-projectile-project))

(use-package bufler
  :init (add-hook 'after-init-hook #'bufler-mode)
  :bind ("C-x C-b" . bufler))

(provide 'init-buffman)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-buffer.el ends here
