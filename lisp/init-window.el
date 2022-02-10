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

;; Windows management
(use-package winner)

;; Ace window
(use-package ace-window
  :demand
  :init (add-hook 'after-init-hook #'winner-mode)
  :bind ("M-o" . ace-window)
  :config
  ;; Setup ace-window
  (setq aw-dispatch-always nil)
  (set-face-attribute 'aw-leading-char-face nil
                      :height 2.0 :foreground "Green")
  ;; Winner settings
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
          "*Kill Ring*")))

;; Winum
(use-package winum
  :ensure t
  :bind (("M-0" . winum-select-window-0-or-10)
         ("M-1" . winum-select-window-1)
         ("M-2" . winum-select-window-2)
         ("M-3" . winum-select-window-3)
         ("M-4" . winum-select-window-4))
  :init (add-hook 'after-init-hook #'winum-mode))

;; Windmove
(use-package windmove
  :ensure t
  :init (windmove-default-keybindings))

;; Ibuffer
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

;; HACK to try splitting horizontally if possible
;; Nowadays display is much wider so make use of it
(defun my-split-window-sensibly (&optional window)
  (let ((window (or window (selected-window))))
    (or
     (and (window-splittable-p window t)
	  ;; Split window horizontally.
	  (with-selected-window window
	    (split-window-right)))
     (and (window-splittable-p window)
	  ;; Split window vertically.
	  (with-selected-window window
	    (split-window-below)))
     (and
      ;; If WINDOW is the only usable window on its frame (it is
      ;; the only one or, not being the only one, all the other
      ;; ones are dedicated) and is not the minibuffer window, try
      ;; to split it vertically disregarding the value of
      ;; `split-height-threshold'.
      (let ((frame (window-frame window)))
        (or
         (eq window (frame-root-window frame))
         (catch 'done
           (walk-window-tree (lambda (w)
                               (unless (or (eq w window)
                                           (window-dedicated-p w))
                                 (throw 'done nil)))
                             frame nil 'nomini)
           t)))
      (not (window-minibuffer-p window))
      (let ((split-height-threshold 0))
	(when (window-splittable-p window)
	  (with-selected-window window
	    (split-window-below))))))))
(advice-add #'split-window-sensibly :override #'my-split-window-sensibly)

(provide 'init-window)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-window.el ends here
