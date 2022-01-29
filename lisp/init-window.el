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

;; Layout config
(use-package eyebrowse
  :hook (after-init . eyebrowse-mode)
  :config
  (setq eyebrowse-new-workspace t
        eyebrowse-keymap-prefix (kbd "C-c \"")))

;; Windows management
(use-package winner)

;; Ace window
(use-package ace-window
  :demand
  :init (add-hook 'after-init-hook #'winner-mode)
  :bind (("M-O" . hydra-layout-manager/body)
         ("M-o" . ace-window))
  :config
  (use-package windmove :demand)
  (use-package transpose-frame :demand)
  (use-package hydra)
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
          "*Kill Ring*"))

  ;; Layout hydra
  (defhydra hydra-layout-manager ()
    "Layout Manager"
    ("<left>" hydra-move-splitter-left)
    ("<down>" hydra-move-splitter-down)
    ("<up>" hydra-move-splitter-up)
    ("<right>" hydra-move-splitter-right)
    ("t" transpose-frame)
    ("f" flop-frame)
    ("F" flip-frame)
    ("r" rotate-frame-clockwise)
    ("R" rotate-frame-anticlockwise)
    ("o" ace-window)
    ("q" nil "quit"))

  ;; splitter helper functions
  (defun hydra-move-splitter-left (arg)
    "Move window splitter left."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (shrink-window-horizontally arg)
      (enlarge-window-horizontally arg)))

  (defun hydra-move-splitter-right (arg)
    "Move window splitter right."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (enlarge-window-horizontally arg)
      (shrink-window-horizontally arg)))

  (defun hydra-move-splitter-up (arg)
    "Move window splitter up."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (enlarge-window arg)
      (shrink-window arg)))

  (defun hydra-move-splitter-down (arg)
    "Move window splitter down."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (shrink-window arg)
      (enlarge-window arg))))

;; winum
(use-package winum
  :ensure t
  :bind (("M-0" . winum-select-window-0-or-10)
         ("M-1" . winum-select-window-1)
         ("M-2" . winum-select-window-2)
         ("M-3" . winum-select-window-3)
         ("M-4" . winum-select-window-4))
  :init (add-hook 'after-init-hook #'winum-mode))

;; Golden Ratio
;; (use-package golden-ratio
;;   :init (add-hook 'after-init-hook #'golden-ratio-mode)
;;   :config
;;   (with-eval-after-load 'ace-window
;;     (add-to-list 'golden-ratio-extra-commands 'ace-window)))

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
