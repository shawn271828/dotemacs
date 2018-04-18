;; init-lsp.el --- Initialize highlight configurations.	-*- lexical-binding: t -*-
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
  :demand
  :config
  (require 'lsp-imenu)
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
  ;; Python
  ;; How to configure lsp-mode for Python:
  ;; 1. Need to install pyls using: 'pip install 'python-language-server[all]'.
  ;; 2. pyls honors VIRTUAL_ENV and project root path.
  ;;    See `https://github.com/palantir/python-language-server/issues/126' and `workspace.py'.
  ;; 3. See `https://github.com/emacs-lsp/lsp-mode/issues/167' if need to send settings to pyls.
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
  (add-hook 'python-mode-hook #'lsp-python-mode-enable))

(provide 'init-lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
