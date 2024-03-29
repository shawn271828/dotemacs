;; init-custom.el --- Initialize custom configurations.	-*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Custom configurations.
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
;; along with this program; see the custom-file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defgroup my nil
  "Personal Emacs configurations."
  :group 'extensions)

(defcustom my-full-name "User Name"
  "Set user full name."
  :type 'string)

(defcustom my-mail-address "user@mail.com"
  "Set user email address."
  :type 'string)

(defcustom my-proxy "127.0.0.1:1087"
  "Set network proxy."
  :type 'string)

(defcustom my-package-archives 'tuna
  "Set package archives from which to fetch."
  :type '(choice
          (const :tag "Melpa" melpa)
          (const :tag "Emacs-China" emacs-china)
          (const :tag "Tuna" tuna)))

(defcustom persp-auto-save ".persp-saved"
  "Perspective auto-save file name."
  :type 'string)

(defcustom my-preferred-theme 'doom-one
  "Theme."
  :type 'symbol)

(defcustom org-directory "~/org"
  "Org file diretory."
  :type 'string)

(defcustom my-themes-pool '(doom-one-light doome-on)
  "Themes pool."
  :type 'list)

;; For Emacs devel
;; e.g. release is 24.5 or 25.1, while devel build is 26.0.90
(when (= emacs-minor-version 0)
  (setq package-user-dir (locate-user-emacs-file "elpa-devel"))
  (setq desktop-base-file-name ".emacs-devel.desktop")
  (setq desktop-base-lock-name ".emacs-devel.desktop.lock"))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file)
    (load custom-file))

(provide 'init-custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-custom.el ends here
