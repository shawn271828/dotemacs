;; init-window.el --- Initialize window configurations.	-*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Window configurations.
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

;; Directional window-selection routines
;; (use-package windmove
;;   :ensure nil
;;   :init (add-hook 'after-init-hook #'windmove-default-keybindings))

;; Restore old window configurations
(use-package winner
  :ensure nil
  :init
  (setq winner-boring-buffers
        '("*Completions*"
          "*Compile-Log*"
          "info"
          "*inferior-lisp*"
          "*Fuzzy Completions*"
          "*Apropos*"
          "*Help*"
          "*cvs*"
          "*Buffer List*"
          "*Ibuffer*"
          "*esh command on file*"
          "*Kill Ring*"))
  (add-hook 'after-init-hook #'winner-mode))

;; Quickly switch windows
;; (use-package switch-window
;;   :bind (("M-o" . switch-window))
;;   :config
;;   (require 'switch-window-mvborder)
;;   (define-key switch-window-extra-map (kbd "k") 'switch-window-mvborder-up)
;;   (define-key switch-window-extra-map (kbd "j") 'switch-window-mvborder-down)
;;   (define-key switch-window-extra-map (kbd "h") 'switch-window-mvborder-left)
;;   (define-key switch-window-extra-map (kbd "l") 'switch-window-mvborder-right)
;;   (setq switch-window-timeout nil)
;;   (setq switch-window-shortcut-style 'quail))

(use-package ace-window
  :defer 1
  :bind ("M-o" . ace-window)
  :config
  (setq aw-dispatch-always nil)
  (set-face-attribute 'aw-leading-char-face nil
                      :height 2.0 :foreground "Green"))

;; Easy window config switching
;; (use-package eyebrowse
;;   :init
;;   (progn
;;     (setq eyebrowse-keymap-prefix (kbd "C-c w"))
;;     (add-hook 'after-init-hook #'eyebrowse-mode)))

;; Golden ratio
;; (use-package golden-ratio
;;   :diminish golden-ratio-mode
;;   :init (add-hook 'after-init-hook #'golden-ratio-mode)
;;   :config
;;   (add-to-list 'golden-ratio-extra-commands 'ace-window))

;; Transpose window
(use-package transpose-frame
  :bind (("C-c t t" . transpose-frame)  ;swap x and y axis
         ("C-c t f" . flip-frame)       ;flip vertically
         ("C-c t F" . flop-frame)       ;flip horizontally
         ("C-c t r" . rotate-frame-clockwise)
         ("C-c t R" . rotate-frame-anticlockwise)))

(provide 'init-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-window.el ends here
