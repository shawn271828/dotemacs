;; init-restore.el --- Initialize restore configurations.	-*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Restore configurations.
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

;; (defun try-load-undo-tree-history (file-name)
;;   (let ((name (undo-tree-make-history-save-file-name file-name)))
;;     (when (file-exists-p name)
;;       (undo-tree-load-history name t))))

;; (defun undo-tree-load-history-advice (&rest args)
;;   (progn
;;     (switch-to-buffer (nth 2 args))
;;     (try-load-undo-tree-history (nth 1 args))
;;     ))

;; (defun restore-current-buffer-undo ()
;;   (try-load-undo-tree-history (buffer-file-name)))

;; Save and restore status
(use-package desktop
  :ensure nil
  :init (desktop-save-mode -1)
  :config
  ;; (add-hook 'desktop-after-read-hook (lambda () (run-with-timer 2 nil #'restore-current-buffer-undo)))
  (setq desktop-restore-in-current-display nil)
  ;; Don't save/restore frames in tty
  (unless (display-graphic-p)
    (setq desktop-restore-frames nil)))
;; (with-eval-after-load 'undo-tree
;;   (advice-add 'desktop-create-buffer :after #'undo-tree-load-history-advice)))

(provide 'init-restore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-restore.el ends here
