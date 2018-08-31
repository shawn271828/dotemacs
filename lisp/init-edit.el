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
	           c-basic-offset 4
               python-guess-indent nil
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
         ("C-<up>" . sp-splice-sexp)
         ("C-<down>" . sp-split-sexp)
         ("C-<return>" . sp-rewrap-sexp))
  :config
  (require 'smartparens-config)
  (sp-pair "(" ")" :wrap "M-(")
  (sp-pair "[" "]" :wrap "M-[")
  (sp-pair "{" "}" :wrap "M-{")

  (add-to-list 'sp-no-reindent-after-kill-modes 'yaml-mode)

  (defun sp-web-mode-is-code-context (id action context)
      (and (eq action 'insert)
           (not (or (get-text-property (point) 'part-side)
                    (get-text-property (point) 'block-side)))))
  (sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context))
  
  (defun shawn-enter-and-indent-sexp (&rest _ignored)
    "Insert an extra newline after point, and reindent."
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))

  (dolist (mode '(c-mode c++-mode css-mode objc-mode java-mode
                         js2-mode json-mode
                         python-mode sh-mode web-mode))
    (sp-local-pair mode "{" nil :post-handlers
                   '((shawn-enter-and-indent-sexp "RET")
                     (shawn-enter-and-indent-sexp "<return>"))))

  (dolist (mode '(js2-mode json-mode python-mode web-mode))
    (sp-local-pair mode "[" nil :post-handlers
                   '((shawn-enter-and-indent-sexp "RET")
                     (shawn-enter-and-indent-sexp "<return>"))))

  (dolist (mode '(python-mode))
    (sp-local-pair mode "(" nil :post-handlers
                   '((shawn-enter-and-indent-sexp "RET")
                     (shawn-enter-and-indent-sexp "<return>")))
    (sp-local-pair mode "\"\"\"" "\"\"\"" :post-handlers
                   '((shawn-enter-and-indent-sexp "RET")
                     (shawn-enter-and-indent-sexp "<return>")))))

;; Expand region
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Multiple cursors
(use-package multiple-cursors
  :init (use-package hydra)
  :bind ("C-c M" . hydra-multiple-cursors/body)
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
    ("<mouse-1>" mc/add-cursor-on-click)
    ("<down-mouse-1>" ignore)
    ("<drag-mouse-1>" ignore)
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
  :bind
  (([(meta up)] . move-text-up)
   ([(meta down)] . move-text-down)))

;; Auto save buffer
(use-package super-save
  :diminish super-save-mode
  :init (add-hook 'after-init-hook #'super-save-mode))

;; Jump to things in Emacs tree-style
(use-package avy
  :demand
  :bind (("C-'" . avy-goto-word-1)
         ([remap goto-line] . avy-goto-line)
         :map isearch-mode-map
         ("C-j" . avy-isearch))
  :config
  (setq  avy-case-fold-search nil
         avy-all-windows      nil
         avy-background       nil)

  (defun my-avy-action-copy-line (pt)
    "Copy to end of line starting on PT."
    (save-excursion
      (let (str)
        (goto-char pt)
        (end-of-line)
        (setq str (buffer-substring pt (point)))
        (kill-new str)
        (message "Copied: %s" str)))
    (let ((dat (ring-ref avy-ring 0)))
      (select-frame-set-input-focus
       (window-frame (cdr dat)))
      (select-window (cdr dat))
      (goto-char (car dat))))

  (defun my-avy-action-yank-line (pt)
    "Yank end of line starting at PT at the current point."
    (my-avy-action-copy-line pt)
    (yank)
    t)

  (add-to-list 'avy-dispatch-alist '(?N . my-avy-action-copy-line))
  (add-to-list 'avy-dispatch-alist '(?Y . my-avy-action-yank-line))

  ;; Install avy with modifier
  (defun my-make-universal-key-handler (func)
    "Make a univeral key handler with FUNC."
    (lambda ()
      (interactive)
      (let* ((event     last-input-event)
             (modifiers (event-modifiers event))
             (basic     (event-basic-type event))
             (char      (if (memq 'shift modifiers)
                            (upcase  basic)
                          basic)))
        (apply func (list char)))))

  (defun my-install-avy-with-modifier (modifier avy-func)
    (let ((handler (my-make-universal-key-handler avy-func)))
      (cl-loop for key-char from 33 to 126
               do (let ((key (format "%s%s" modifier (string key-char))))
                    (global-set-key (kbd key) handler)))))

  (my-install-avy-with-modifier "H-" #'avy-goto-word-1))

;; Other key chords (should already be enabled in init-package.el)
(use-package key-chord
  :chords (("JJ" . mode-line-other-buffer)))

(use-package goto-chg
  :commands goto-last-change
  ;; complementary to
  ;; C-x r m / C-x r l
  ;; and C-<space> C-<space> / C-u C-<space>
  :bind (("H-<left>" . goto-last-change)
         ("H-<right>" . goto-last-change-reverse)))

(use-package vimish-fold
  :init (add-hook 'after-init-hook #'vimish-fold-global-mode))

(use-package neotree
  :bind (("<f8>" . neotree-project-dir))
  :config
  (setq neo-window-width 40)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (with-eval-after-load 'projectile
    (setq projectile-switch-project-action 'neotree-projectile-action)
    (defun neotree-project-dir ()
      "Open NeoTree using the git root."
      (interactive)
      (let ((project-dir (projectile-project-root))
            (file-name (buffer-file-name)))
        (neotree-toggle)
        (if project-dir
            (if (neo-global--window-exists-p)
                (progn
                  (neotree-dir project-dir)
                  (neotree-find file-name)))
          (message "Could not find git project root."))))))

(use-package all-the-icons)

(provide 'init-edit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
