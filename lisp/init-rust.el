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

(use-package rust-mode
  :hook (rust-mode . lsp)
  :after lsp-mode
  :bind(:map lsp-mode-map
             ("C-c s m" . lsp-rust-analyzer-expand-macro)
             ("C-c s h" . lsp-rust-analyzer-inlay-hints-mode))
  :config
  (setq lsp-rust-server 'rust-analyzer)
  (setq lsp-rust-analyzer-inlay-hints-mode t)
  (setq lsp-rust-analyzer-display-chaining-hints t)
  (setq lsp-rust-analyzer-display-parameter-hints t)
  (setq lsp-rust-analyzer-server-display-inlay-hints t))

(use-package cargo
  :after rust-mode
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :after rust-mode flycheck
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(provide 'init-rust)
