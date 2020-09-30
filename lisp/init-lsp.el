;; init-lsp.el --- Initialize lsp configurations.	-*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             LSP configurations.
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

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((rust-mode . lsp-deferred))
  :bind
  (:map lsp-mode-map
        ("C-c s i" . lsp-describe-thing-at-point)
        ("C-c s c" . lsp-find-references)
        ("C-c s d" . xref-find-definitions-other-window))
  :init
  (setq lsp-keymap-prefix "C-c s"
        lsp-keep-workspace-alive nil
        lsp-print-performance t
        lsp-modeline-code-actions-enable nil
        lsp-enable-symbol-highlighting t
        lsp-lens-enable t
        lsp-headerline-breadcrumb-enable nil
        lsp-diagnostics-provider :flycheck
        lsp-modeline-diagnostics-enable t
        lsp-signature-auto-activate nil
        lsp-signature-render-documentation nil
        lsp-completion-provider :capf
        lsp-completion-show-detail t
        lsp-completion-show-kind t
        lsp-auto-guess-root nil
        lsp-file-watch-threshold 2000)
  :config
  (use-package company-lsp
    :ensure t
    :commands company-lsp
    :config
    (shawn/local-push-company-backend 'company-lsp))
  (use-package lsp-ui
    :commands lsp-ui-mode
    :bind (()
           :map lsp-ui-mode-map
           ("C-c s !" . #'lsp-ui-flycheck-list)
           ("M-j" . #'lsp-ui-imenu)
           ([remap xref-find-definitions] . #'lsp-ui-peek-find-definitions)
           ([remap xref-find-references] . #'lsp-ui-peek-find-references)
           :map lsp-ui-flycheck-list-mode-map
           ("RET" . #'lsp-ui-flycheck-list--visit)
           ("TAB" . #'lsp-ui-flycheck-list--view))
    :init
    (setq lsp-ui-doc-enable nil
          lsp-ui-doc-use-webkit nil
          lsp-ui-doc-delay 0.2
          lsp-ui-doc-include-signature t
          lsp-ui-doc-position 'at-point
          lsp-ui-doc-border (face-foreground 'default)
          lsp-eldoc-enable-hover t

          lsp-ui-imenu-enable t
          lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                                ,(face-foreground 'font-lock-string-face)
                                ,(face-foreground 'font-lock-constant-face)
                                ,(face-foreground 'font-lock-variable-name-face))

          lsp-ui-sideline-enable nil
          lsp-ui-sideline-show-hover nil
          lsp-ui-sideline-show-diagnostics nil
          lsp-ui-sideline-ignore-duplicate t
          lsp-ui-sideline-show-code-actions nil

          lsp-ui-flycheck-list-position 'right

          lsp-ui-peek-show-directory t
          lsp-ui-peek-list-width 50)
    :config
    (add-to-list 'lsp-ui-doc-frame-parameters '(right-fringe . 8))

    ;; `C-g'to close doc
    (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)

    ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
    ;; @see https://github.com/emacs-lsp/lsp-ui/issues/243
    (defun my-lsp-ui-imenu-hide-mode-line ()
      "Hide the mode-line in lsp-ui-imenu."
      (setq mode-line-format nil))
    (advice-add #'lsp-ui-imenu :after #'my-lsp-ui-imenu-hide-mode-line)

    ;; WORKAROUND lsp-ui-peek under terminal refs count number overflow
    ;; reserve one more space from right-fringe
    (defun my-lsp-ui-peek--show (xrefs)
      "Create a window to list references/defintions.
XREFS is a list of references/definitions."
      (setq lsp-ui-peek--win-start (window-start)
            lsp-ui-peek--selection 0
            lsp-ui-peek--offset 0
            lsp-ui-peek--size-list 0
            lsp-ui-peek--list nil)
      (when (eq (logand lsp-ui-peek-peek-height 1) 1)
        (setq lsp-ui-peek-peek-height (1+ lsp-ui-peek-peek-height)))
      (when (< (- (line-number-at-pos (window-end)) (line-number-at-pos))
               (+ lsp-ui-peek-peek-height 3))
        (recenter 15))
      (setq xrefs (--sort (string< (plist-get it :file) (plist-get other :file)) xrefs))
      (--each xrefs
        (-let* (((&plist :file filename :xrefs xrefs :count count) it)
                (len-str (number-to-string count)))
          (setq lsp-ui-peek--size-list (+ lsp-ui-peek--size-list count))
          (push (concat (propertize (if lsp-ui-peek-show-directory
                                        (lsp-ui--workspace-path filename)
                                      (file-name-nondirectory filename))
                                    'face 'lsp-ui-peek-filename
                                    'file filename
                                    'xrefs xrefs)
                        ;; (propertize " " 'display `(space :align-to (- right-fringe ,(1+ (length len-str)))))
                        (propertize " " 'display `(space :align-to (- right-fringe ,(1+ (1+ (length len-str))))))
                        (propertize len-str 'face 'lsp-ui-peek-filename))
                lsp-ui-peek--list)))
      (setq lsp-ui-peek--list (nreverse lsp-ui-peek--list))
      (lsp-ui-peek--expand xrefs)
      (lsp-ui-peek--peek))
    (advice-add #'lsp-ui-peek--show :override #'my-lsp-ui-peek--show)))

(provide 'init-lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
