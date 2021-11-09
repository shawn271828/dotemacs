;; init-lsp.el --- Initialize rust mode configurations.	-*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             rust configurations.
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
  :after lsp-mode
  :bind (:map rust-mode-map
              ("C-c s l" . lsp-rust-analyzer-inlay-hints-mode))
  :config
  (setq lsp-rust-server 'rust-analyzer)
  (setq lsp-rust-analyzer-inlay-hints-mode nil)
  (setq lsp-rust-analyzer-display-chaining-hints t)
  (setq lsp-rust-analyzer-display-parameter-hints t)
  (setq lsp-rust-analyzer-server-display-inlay-hints nil)

  (defun my-get-linked-projects ()
    "Get linked projects if exists."
    (let* ((project-root-suggestion (or (lsp--suggest-project-root) default-directory))
           (linked-projects-file-name (concat project-root-suggestion ".linked-projects")))
      (if (file-exists-p linked-projects-file-name)
          (vconcat (with-temp-buffer
                     (insert-file-contents linked-projects-file-name)
                     (split-string (buffer-string) "\n" t)))
        (vector))))

  (advice-add #'lsp-rust-analyzer--make-init-options
                    :around
                    '(lambda (origin-fun &rest args)
                       (cons :linkedProjects
                             (cons (my-get-linked-projects) (apply origin-fun args))))))
(use-package cargo
  :after rust-mode
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :after rust-mode flycheck
  :init (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(provide 'init-rust)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-rust.el ends here
