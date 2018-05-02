;; init-edit.el --- Initialize edit configurations.	-*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Edit configurations.
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
  (require 'init-const))

;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)

;; Misc
(fset 'yes-or-no-p 'y-or-n-p)
(blink-cursor-mode -1)
(global-set-key (kbd "M-SPC") 'set-mark-command)

(setq-default  auto-save-default nil                    ; Disable auto save
               delete-by-moving-to-trash t              ; Deleting files go to OS's trash folder
               set-mark-command-repeat-pop t            ; Repeating C-SPC after popping mark pops it again
               delete-old-versions -1                   ; Don't remove old versions
               version-control t                        ; Backup as many versions as it could
               vc-make-backup-files t                   ; Even files under Git will be backuped
               inhibit-startup-screen t
               ;; visible-bell t
               track-eol t                              ; Keep cursor at end of lines. Require line-move-visual is nil.
               line-move-visual nil
               ;; blink-cursor-interval 0.4
               bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
               buffers-menu-max-size 30
               case-fold-search t
               require-final-newline t
               ediff-split-window-function 'split-window-horizontally
               ediff-window-setup-function 'ediff-setup-windows-plain
               indent-tabs-mode nil
               tab-width 4
               mouse-yank-at-point t
               save-interprogram-paste-before-kill t
               scroll-preserve-screen-position 'always
               set-mark-command-repeat-pop t
               tooltip-delay 1.5
               truncate-lines nil
               truncate-partial-width-windows nil
               visible-bell nil
               echo-keystrokes 1
               fill-column 79)

(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(delete-selection-mode t)

;; Place all backup and auto save files in one dir
;; and create it if not exists.
(unless (file-exists-p my-backup-dir)
  (make-directory my-backup-dir))

(setq backup-directory-alist `(("." . ,my-backup-dir)))
(setq auto-save-file-name-transforms  `(("." ,my-backup-dir) t))

;; Show path if names are the same
(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator " • ")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))

;; Newline behavior
(global-set-key (kbd "RET") 'newline-and-indent)

;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure nil
  :diminish auto-revert-mode
  :init (add-hook 'after-init-hook #'global-auto-revert-mode)
  :config
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil))

;; Jump to things in Emacs tree-style
;; (use-package avy
;;   :bind (("C-'" . avy-goto-char-timer)
;;          ([remap goto-line] . avy-goto-line))
;;   :init (add-hook 'after-init-hook #'avy-setup-default)
;;   :config
;;   (setq avy-background t)
;;   (setq avy-style 'at-full)
;;   (setq avy-all-windows nil)
;;   (setq avy-timeout-seconds 0.5))

;; Show number of matches in mode-line while searching
(use-package anzu
  :diminish anzu-mode
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :init (add-hook 'after-init-hook #'global-anzu-mode)
  :config
  (setq anzu-replace-to-string-separator
        (if (char-displayable-p ?→) " → " " -> ")))

;; Move to the beginning/end of line or code
(use-package mwim
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)))

;; Treat undo history as a tree
(use-package undo-tree
  :diminish undo-tree-mode
  :init (add-hook 'after-init-hook #'global-undo-tree-mode)
  :config
  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist  `((".*" . ,my-backup-dir)))
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t))

;; Handling capitalized subwords in a nomenclature
(use-package subword
  :ensure nil
  :diminish subword-mode
  :init
  (add-hook 'prog-mode-hook #'subword-mode)
  (add-hook 'minibuffer-setup-hook #'subword-mode))

;; Page break lines
(use-package page-break-lines
  :diminish page-break-lines-mode
  :init (add-hook 'after-init-hook 'global-page-break-lines-mode))

;; Make pairs balance
(use-package elec-pair
  :ensure nil
  :init (add-hook 'after-init-hook #'electric-pair-mode))

(use-package paredit
  :demand
  :init
  (add-hook 'prog-mode-hook #'paredit-everywhere-mode)
  (add-hook 'css-mode #'paredit-everywhere-mode)
  :config
  (defvar paredit-everywhere-mode-map
    (let ((m (make-sparse-keymap)))
      ;; (define-key m (kbd "DEL") 'paredit-backward-delete)
      ;; (define-key m (kbd "C-d") 'paredit-forward-delete)
      (define-key m (kbd "C-<left>") 'paredit-forward-barf-sexp)
      (define-key m (kbd "C-<right>") 'paredit-forward-slurp-sexp)
      (define-key m (kbd "M-(") 'paredit-wrap-round)
      (define-key m (kbd "M-[") 'paredit-wrap-square)
      (define-key m (kbd "M-{") 'paredit-wrap-curly)
      (define-key m (kbd "M-)") 'paredit-close-round)
      (define-key m (kbd "M-]") 'paredit-close-square)
      (define-key m (kbd "M-}") 'paredit-close-curly)
      (define-key m (kbd "M-\"") 'paredit-meta-doublequote)
      (define-key m (kbd "M-S") 'paredit-split-sexp)
      (define-key m (kbd "M-J") 'paredit-join-sexps)
      (define-key m (kbd "C-<up>") 'paredit-splice-sexp)
      m)
    "Keymap for `paredit-everywhere-mode'.")

  ;; Define minor mode
  (define-minor-mode paredit-everywhere-mode
    "A cut-down version of paredit which can be used in non-lisp buffers."
    nil
    ""
    paredit-everywhere-mode-map)

  (defun turn-off-paredit-everywhere-mode ()
    "Disable `paredit-everywhere-mode'."
    (paredit-everywhere-mode 0))

  ;; Disable paredit-everywhere when full paredit is enabled
  (add-hook 'paredit-mode-hook #'turn-off-paredit-everywhere-mode))

;; Expand region
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Multiple cursors
(use-package multiple-cursors
  :init (use-package hydra)
  :bind ("C-c m" . hydra-multiple-cursors/body)
  :config
  (defhydra hydra-multiple-cursors (:hint nil)
    "
^Mark^            ^Unmark^        ^Others^
-----------------------------------------------------^
_a_: all          _N_: forward    _s_: sort region
_A_: regexp       _P_: backward   _S_: reverse region
_n_: forward      _M-n_: skip     _i_: insert number
_p_: backward     _M-p_: skip     _I_: insert string
_r_: rectangle
"
    ("a" mc/mark-all-like-this :exit t)
    ("A" mc/mark-all-in-region-regexp :exit t)
    ("l" mc/edit-lines :exit t)
    ("n" mc/mark-next-like-this)
    ("N" mc/skip-to-next-like-this)
    ("M-n" mc/unmark-next-like-this)
    ("p" mc/mark-previous-like-this)
    ("P" mc/skip-to-previous-like-this)
    ("M-p" mc/unmark-previous-like-this)
    ("r" set-rectangular-region-anchor :exit t)
    ("s" mc/sort-regions :exit t)
    ("S" mc/reverse-regions :exit t)
    ("i" mc/insert-numbers :exit t)
    ("I" mc/insert-letters :exit t)
    ("<mouse-1>" mc/add-cursor-on-click)
    ("<down-mouse-1>" ignore)
    ("<drag-mouse-1>" ignore)
    ("q" nil "quit"))

  ;; MC customizations
  (setq mc/cmds-to-run-for-all
        '(mwim-beginning-of-code-or-line
          mwim-end-of-code-or-line
          hungry-delete-backward
          hungry-delete-forward
          keyboard-escape-quit))

  (setq mc/cmds-to-run-once
        '(counsel-M-x
          hydra--digit-argument
          hydra-multiple-cursors/body
          hydra-multiple-cursors/mc/edit-lines
          hydra-multiple-cursors/mc/edit-lines-and-exit
          hydra-multiple-cursors/mc/insert-letters
          hydra-multiple-cursors/mc/insert-numbers
          hydra-multiple-cursors/mc/insert-numbers-and-exit
          hydra-multiple-cursors/mc/mark-all-dwim
          hydra-multiple-cursors/mc/mark-all-dwim-and-exit
          hydra-multiple-cursors/mc/mark-all-like-this-and-exit
          hydra-multiple-cursors/mc/mark-next-like-this
          hydra-multiple-cursors/mc/mark-previous-like-this
          hydra-multiple-cursors/mc/skip-to-next-like-this
          hydra-multiple-cursors/mc/skip-to-previous-like-this
          hydra-multiple-cursors/mc/unmark-next-like-this
          hydra-multiple-cursors/mc/unmark-previous-like-this
          hydra-multiple-cursors/nil
          hydra-multiple-cursors/set-rectangular-region-anchor
          swiper-mc)))

;; Comment
(use-package comment-dwim-2
  :bind (([remap comment-dwim] . #'comment-dwim-2)))

;; Hungry deletion
(use-package hungry-delete
  :diminish hungry-delete-mode
  :init (add-hook 'after-init-hook #'global-hungry-delete-mode)
  :config (setq-default hungry-delete-chars-to-skip " \t\f\v"))

;; Minor mode to aggressively keep your code always indented
(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :init
  (add-hook 'after-init-hook #'global-aggressive-indent-mode)

  ;; FIXME: Disable in big files due to the performance issues
  ;; https://github.com/Malabarba/aggressive-indent-mode/issues/73
  (add-hook 'find-file-hook
            (lambda ()
              (if (> (buffer-size) (* 3000 80))
                  (aggressive-indent-mode -1))))
  :config
  ;; Disable in some modes
  (dolist (mode '(asm-mode web-mode html-mode css-mode robot-mode python-mode))
    (push mode aggressive-indent-excluded-modes))

  ;; Be slightly less aggressive in C/C++/C#/Java/Go/Swift (where semicolon `;' matters)
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (or (derived-mode-p 'c-mode)
             (derived-mode-p 'c++-mode)
             (derived-mode-p 'csharp-mode)
             (derived-mode-p 'java-mode)
             (derived-mode-p 'go-mode)
             (derived-mode-p 'swift-mode))
         (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                             (thing-at-point 'line))))))

;; Better than zap-up-to-char
(use-package zop-to-char
  :bind (("M-z" . zop-up-to-char)
         ("M-Z" . zop-to-char)))

;; Move text and region
(use-package move-text
  :bind
  (([(meta up)] . move-text-up)
   ([(meta down)] . move-text-down)))

;; Auto save buffer
(use-package super-save
  :diminish super-save-mode
  :init (add-hook 'after-init-hook #'super-save-mode))

;; Bookmark plus from github
(use-package bookmark+
  :load-path "site-lisp/bookmark-plus"
  :config
  (let ((bmkp-dir (expand-file-name ".bmkp" user-emacs-directory)))
    (unless (file-exists-p bmkp-dir)
      (mkdir bmkp-dir))
    (setq bmkp-bmenu-commands-file (expand-file-name "emacs-bmk-bmenu-commands.el" bmkp-dir))
    (setq bmkp-bmenu-state-file (expand-file-name "emacs-bmk-bmenu-state" bmkp-dir))))

;; Avy
(use-package avy
  :ensure t
  :bind (([remap goto-line] . avy-goto-line)
         :map isearch-mode-map
         ("C-j" . avy-isearch)))

(provide 'init-edit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
