;; init-ccls.el --- Initialize clike configurations.	-*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             C/C++ language configurations.
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

(use-package ccls
  :after lsp-mode
  :bind
  (:map c-mode-map
        ("C-c s l" . hydra-ccls-reference/body))
  (:map c++-mode-map
        ("C-c s l" . hydra-ccls-reference/body))
  :config
  (defhydra hydra-ccls-reference ()
    ("&" ccls/references-address :exit t)
    ("#" ccls/references-macro :exit t)
    ("v" ccls/references-not-call :exit t)
    ("r" ccls/references-read :exit t)
    ("w" ccls/references-write :exit t))

  (setq lsp-enable-file-watchers nil)

  (defun ccls/callee ()
    (interactive)
    (lsp-ui-peek-find-custom "$ccls/call" '(:callee t)))

  (defun ccls/caller
    (interactive)
    (lsp-ui-peek-find-custom "$ccls/call"))

  (defun ccls/vars (kind)
    (lsp-ui-peek-find-custom "$ccls/vars" `(:kind ,kind)))

  (defun ccls/base (levels)
    (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels ,levels)))

  (defun ccls/derived (levels)
    (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels ,levels :derived t)))

  (defun ccls/member (kind)
    (lsp-ui-peek-find-custom "$ccls/member" `(:kind ,kind)))

  ;; The meaning of :role corresponds to https://github.com/maskray/ccls/blob/master/src/symbol.h

  ;; References w/ Role::Address bit (e.g. variables explicitly being taken addresses)
  (defun ccls/references-address ()
    (interactive)
    (lsp-ui-peek-find-custom "textDocument/references"
                             (plist-put (lsp--text-document-position-params) :role 128)))

  ;; References w/ Role::Dynamic bit (macro expansions)
  (defun ccls/references-macro ()
    (interactive)
    (lsp-ui-peek-find-custom "textDocument/references"
                             (plist-put (lsp--text-document-position-params) :role 64)))

  ;; References w/o Role::Call bit (e.g. where functions are taken addresses)
  (defun ccls/references-not-call ()
    (interactive)
    (lsp-ui-peek-find-custom "textDocument/references"
                             (plist-put (lsp--text-document-position-params) :excludeRole 32)))

  ;; References w/ Role::Read
  (defun ccls/references-read ()
    (interactive)
    (lsp-ui-peek-find-custom "textDocument/references"
                             (plist-put (lsp--text-document-position-params) :role 8)))

  ;; References w/ Role::Write
  (defun ccls/references-write ()
    (interactive)
    (lsp-ui-peek-find-custom "textDocument/references"
                             (plist-put (lsp--text-document-position-params) :role 16)))

  )

(provide 'init-ccls)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ccls.el ends here
