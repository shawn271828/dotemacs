;;; init.el --- user init configuration.	-*- lexical-binding: t no-byte-compile: t; -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
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

(package-initialize)

(when (version< emacs-version "24.4")
  (error "This requires Emacs 24.4 and above!"))

;; Accept safe local variables
(setq enable-local-variables :safe)

;; Optimize loading performance
(setq gc-cons-threshold (* 128 1024 1024))
(setq read-process-output-max (* 1024 1024))

;; Prefers the newest version of a file
(setq load-prefer-newer t)

;; Load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Constants
(require 'init-const)

;; Customization
(require 'init-custom)

;; Packages
(require 'init-package)

;; Extra keymap under terminal
(require 'init-keymap)

;; Various settings
(require 'init-basic)
(require 'init-utils)
(require 'init-ui)
(require 'init-theme)
(require 'init-edit)
(require 'init-ibuffer)
(require 'init-highlight)
(require 'init-window)
(require 'init-helm)
(require 'init-projectile)
(require 'init-hippie-expand)
(require 'init-flycheck)
(require 'init-company)
(require 'init-yasnippet)
(require 'init-vcs)
(require 'init-lsp)
(require 'init-prog)
(require 'init-rust)
(require 'init-clike)
(require 'init-markdown)
(require 'init-treemacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
