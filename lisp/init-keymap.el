;; init-ui.el --- Initialize ui configurations.	-*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             UI configurations.
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

(define-key input-decode-map "\e[27;4;60~" (kbd "M-<"))
(define-key input-decode-map "\e[27;4;62~" (kbd "M->"))
(define-key input-decode-map "\e[27;4;37~" (kbd "M-%"))
(define-key input-decode-map "\e[27;8;37~" (kbd "C-M-%"))
(define-key input-decode-map "\e[27;4;58~" (kbd "M-:"))
(define-key input-decode-map "\e[27;4;63~" (kbd "M-?"))
(define-key input-decode-map "\e[27;3;47~" (kbd "M-/"))
(define-key input-decode-map "\e[27;7;47~" (kbd "C-M-/"))
(define-key input-decode-map "\e[27;6;83~" (kbd "C-S-s"))

(provide 'init-keymap)
