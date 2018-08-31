;; init-web.el --- Initialize web configurations.	-*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Web configurations.
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

;; CSS mode
(use-package css-mode
  :ensure nil
  :init (setq css-indent-offset 2))

;; SCSS mode
(use-package scss-mode
  :init
  ;; Disable complilation on save
  (setq scss-compile-at-save nil))

;; New `less-cs-mde' in Emacs26
(unless (fboundp 'less-css-mode)
  (use-package less-css-mode))

;; CSS eldoc
(use-package css-eldoc
  :commands turn-on-css-eldoc
  :init
  (dolist (hook '(css-mode-hook scss-mode-hook less-css-mode-hook))
    (add-hook hook #'turn-on-css-eldoc)))

;; JSON mode
(use-package json-mode)

;; Improved JavaScript editing mode
(use-package js2-mode
  :mode "\\.js$"
  :interpreter "node"
  :init
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  (add-hook 'js2-mode-hook #'js2-highlight-unused-variables-mode)
  :config
  (setq js2-strict-missing-semi-warning nil)
  (setq js2-basic-offset 2)
  (use-package js2-refactor
    :diminish js2-refactor-mode
    :init (add-hook 'js2-mode-hook #'js2-refactor-mode)
    :config (js2r-add-keybindings-with-prefix "C-c C-m")))

;; Run Mocha or Jasmine tests
(use-package mocha
  :config (use-package mocha-snippets))

;; Major mode for editing web templates
(use-package web-mode
  :mode "\\.\\(vue\\|phtml\\|php|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\)$"
  :config
  ;; Leave auto-paring for smartparens
  (setq web-mode-enable-auto-pairing nil)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)

  ;; Complete for web,html,emmet,jade,slim modes
  (with-eval-after-load 'company
    (use-package company-web
      :init
      (cl-pushnew (company-backend-with-yas 'company-web-html) company-backends)
      (cl-pushnew (company-backend-with-yas 'company-css) company-backends))))

;; Format HTML, CSS and JavaScript/JSON by js-beautify
(use-package web-beautify
  :init
  (with-eval-after-load 'js2-mode
    (bind-key "C-c C-b" 'web-beautify-js js2-mode-map))
  (with-eval-after-load 'json-mode
    (bind-key "C-c C-b" 'web-beautify-js json-mode-map))
  (with-eval-after-load 'sgml-mode
    (bind-key "C-c C-b" 'web-beautify-html html-mode-map))
  (with-eval-after-load 'css-mode
    (bind-key "C-c C-b" 'web-beautify-css css-mode-map))
  (with-eval-after-load 'web-mode
    (bind-key "C-c C-b" 'web-beautify-html web-mode-map))
  
  :config
  ;; Set indent size to 2
  (setq web-beautify-args '("-s" "2" "-f" "-")))

;; Emmet
(use-package emmet-mode
  :init
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode)
  :config
  ;; (setq emmet-move-cursor-between-quotes t)
  
  )

(provide 'init-web)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-web.el ends here
