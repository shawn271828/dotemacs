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

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(when (version< emacs-version "24.4")
  (error "This requires Emacs 24.4 and above!"))

;; Optimize loading performance
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore defalut values after init"
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold (* 64 1024 1024))))

;; Prefers the newest version of a file
(setq load-prefer-newer t)

;; Load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

;; Constants
(require 'init-const)

;; Customization
(require 'init-custom)

;; Packages
(require 'init-package)

;; Basic packages and settings
(require 'init-basic)
(require 'init-utils)

;; Visual and editing
(require 'init-ui)
(require 'init-edit)
(require 'init-highlight)
(require 'init-window)
(require 'init-helm)
(require 'init-flycheck)
(require 'init-projectile)
(require 'init-hippie-expand)
;; (require 'init-company)
;; (require 'init-python)
(require 'init-conda)

;; Restore
(require 'init-restore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
