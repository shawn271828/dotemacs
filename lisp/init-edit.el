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

;; Encoding syste
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)

;; Editor settings
(setq-default auto-save-default nil)
(setq-default delete-by-moving-to-trash t)
(setq-default set-mark-command-repeat-pop t)
(setq-default delete-old-versions -1)
(setq-default version-control t)
(setq-default vc-make-backup-files t)
(setq-default inhibit-startup-screen t)
(setq-default track-eol t)
(setq-default line-move-visual nil)
(setq-default buffers-menu-max-size 30)
(setq-default case-fold-search t)
(setq-default require-final-newline t)
(setq-default indent-tabs-mode nil)
(setq-default mouse-yank-at-point t)
(setq-default save-interprogram-paste-before-kill t)
(setq-default scroll-preserve-screen-position 'always)
(setq-default set-mark-command-repeat-pop t)
(setq-default tooltip-delay 1.5)
(setq-default truncate-lines nil)
(setq-default visible-bell nil)
(setq-default echo-keystrokes 0.02)
(setq-default fill-column 80)
(setq-default large-file-warning-threshold 100000000)
(setq-default show-trailing-whitespace nil)
(setq-default window-combination-resize t)
(setq-default scroll-conservatively 100)
(setq-default sentence-end-double-space nil)
(setq-default read-file-name-completion-ignore-case t)
(setq-default read-buffer-completion-ignore-case t)
(setq-default tab-always-indent 'complete)

;; Misc settings
(fset 'yes-or-no-p 'y-or-n-p)
(blink-cursor-mode -1)
(global-set-key (kbd "M-SPC") 'set-mark-command)
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(delete-selection-mode t)
(add-hook 'prog-mode-hook #'(lambda () (setq show-trailing-whitespace t)))

;; ido mode enable
(ido-mode 1)

;; Place all backup and auto save files in one dir
;; and create it if not exists.
(unless (file-exists-p my-backup-dir)
  (make-directory my-backup-dir))

(setq backup-directory-alist `(("." . ,my-backup-dir)))
(setq auto-save-file-name-transforms  `(("." ,my-backup-dir) t))

;; Desktop save mode
;; (desktop-save-mode t)
;; (defvar desktop-restore-eager 5)
;; (defvar desktop-save t)

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

(use-package beginend
  :ensure t
  :config
  (beginend-global-mode))

;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure nil
  :diminish auto-revert-mode
  :init (add-hook 'after-init-hook #'global-auto-revert-mode)
  :config
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil))

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
  ;; (setq undo-tree-history-directory-alist  `((".*" . ,my-backup-dir)))
  ;; (setq undo-tree-auto-save-history t)
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
(use-package smartparens
  :diminish
  :init (add-hook 'after-init-hook #'smartparens-global-strict-mode)
  :bind (("C-<right>" . sp-forward-slurp-sexp)
         ("C-<left>" . sp-forward-barf-sexp)
         ("C-M-<right>" . sp-backward-barf-sexp)
         ("C-M-<left>" . sp-backward-slurp-sexp)
         ("C-<up>" . sp-splice-sexp)
         ("C-<down>" . sp-split-sexp)
         ("C-<return>" . sp-rewrap-sexp)
         :map smartparens-strict-mode-map
         ([remap kill-region] . nil)
         ([remap delete-region] . nil))
  :config
  (require 'smartparens-config))

;; Expand region
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Easy kill
(use-package easy-kill
  :ensure t
  :bind (([remap kill-ring-save] . #'easy-kill)
         ([remap mark-sexp] . #'easy-mark)))

;; Multiple cursors
(use-package multiple-cursors
  :init (use-package hydra)
  :bind ("C-c M-d" . hydra-multiple-cursors/body)
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
_l_: lines
_m_: smart
"
    ("a" mc/mark-all-like-this :exit t)
    ("A" mc/mark-all-in-region-regexp :exit t)
    ("l" mc/edit-lines :exit t)
    ("m" mc/mark-all-dwim)
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
    ("q" nil "quit")))

;; Comment
(use-package comment-dwim-2
  :bind (([remap comment-dwim] . #'comment-dwim-2)))

;; Hungry deletion
(use-package hungry-delete
  :diminish hungry-delete-mode
  :init (add-hook 'after-init-hook #'global-hungry-delete-mode)
  :config (setq-default hungry-delete-chars-to-skip " \t\f\v"))

;; Better than zap-up-to-char
(use-package zop-to-char
  :bind (("M-z" . zop-up-to-char)
         ("M-Z" . zop-to-char)))

;; Move text and region
(use-package move-text
  :bind (("M-<up>" . move-text-up)
         ("M-<down>" . move-text-down)))

;; Auto save buffer
(use-package super-save
  :diminish super-save-mode
  :init (add-hook 'after-init-hook #'super-save-mode))

;; Jump to things in Emacs tree-style
(use-package avy
  :demand
  :bind (("C-:" . avy-goto-char-timer)
         :map isearch-mode-map
         ("C-j" . avy-isearch))
  :config
  (setq  avy-case-fold-search nil
         avy-all-windows      nil
         avy-background       t))

;; Other key chords (should already be enabled in init-package.el)
(use-package key-chord
  :chords (("jj" . mode-line-other-buffer)))

;; Goto last change
(use-package goto-chg
  :commands goto-last-change
  :bind ("C-c ," . goto-last-change))

;; Useful commands
(use-package crux
  :demand
  :bind (("C-x 4 t" . crux-transpose-windows)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-c d" . crux-duplicate-current-line-or-region)
         ("C-c r" . crux-rename-file-and-buffer)))

(provide 'init-edit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
