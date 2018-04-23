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
  :defer 1
  :config
  ;; imenu support
  (require 'lsp-imenu)
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)

  ;; ui integration
  (use-package lsp-ui
    :defer 1
    :init (add-hook 'lsp-mode-hook 'lsp-ui-mode)
    :config
    (defun sync-peek-face ()
      (set-face-attribute 'lsp-ui-peek-selection nil :background (face-attribute 'highlight :background) :foreground (face-attribute 'default :foreground))
      (set-face-attribute 'lsp-ui-peek-filename nil :foreground (face-attribute 'font-lock-constant-face :foreground))
      (set-face-attribute 'lsp-ui-peek-highlight nil :background (face-attribute 'highlight :background) :foreground (face-attribute 'highlight :foreground) :distant-foreground (face-attribute 'highlight :foreground))
      (set-face-attribute 'lsp-ui-peek-header nil :background (face-attribute 'highlight :background) :foreground (face-attribute 'default :foreground)))
    (sync-peek-face)
    (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
    (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)) 

  ;; company integration
  (use-package company-lsp
    :defer 1
    (shawn/local-push-company-backend 'company-lsp))

  ;; Python
  ;; caveat: lsp-symbol-highlight currently not supported by pyls
  (use-package python
    ;; How to configure lsp-mode for Python:
    ;; 1. Need to install pyls using: 'pip install 'python-language-server[all]'.
    ;; 2. pyls honors VIRTUAL_ENV and project root path.
    ;;    See `https://github.com/palantir/python-language-server/issues/126' and `workspace.py'.
    ;; 3. See `https://github.com/emacs-lsp/lsp-mode/issues/167' if need to send settings to pyls.
    :config
    (setq python-indent-offset 4)
    (lsp-define-stdio-client lsp-python-mode
                             "python"
                             (lsp-make-traverser (lambda (dir)
                                                   (directory-files
                                                    dir
                                                    nil
                                                    "environment\\.yml")))
                             '("/Users/ts/anaconda3/bin/pyls"))
    (with-eval-after-load 'conda
      (when (fboundp 'conda--switch-buffer-auto-activate)
        (advice-add 'lsp-python-mode-enable :before #'conda--switch-buffer-auto-activate)))
    (add-hook 'python-mode-hook #'lsp-python-mode-enable)))

(provide 'init-lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
