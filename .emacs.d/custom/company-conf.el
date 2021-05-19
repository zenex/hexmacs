;; ─┐ ┬┌─┐┌┐┌┌─┐┌┐ ┬ ┬┌┬┐┌─┐ ─┐ ┬┬ ┬┌─┐
;; ┌┴┬┘├┤ ││││ │├┴┐└┬┘ │ ├┤  ┌┴┬┘└┬┘┌─┘
;; ┴ └─└─┘┘└┘└─┘└─┘ ┴  ┴ └─┘o┴ └─ ┴ └─┘
;; Author:  SENEX @ XENOBYTE.XYZ
;; License: MIT License
;; Website: https://xenobyte.xyz/projects/?nav=hexmacs

;; COMPANY MODE SETTINGS !!! LOAD ORDER MATTERS !!!
;; ------------------------------------------------------------------------------------------------------------------------------------ ;;
;; NOTE ABOUT COMPANY MODES: THE ORDER IN WHICH YOU LOAD THE MODES MATTERS, EXPERIMENT IF SOMETHING GOES WRONG WITH M-X COMPANY-DIAG    ;;
;; ------------------------------------------------------------------------------------------------------------------------------------ ;;

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(setq company-show-numbers t
      company-minimum-prefix-length 2
      company-tooltip-limit 12
      company-idle-delay 0.0)
(setq company-tooltip-align-annotations t)
(setq company-auto-complete nil)

(with-eval-after-load 'company
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "SPC") nil)
  (define-key company-active-map (kbd "<space>") nil)
  (define-key company-active-map (kbd "M-<tab>") #'company-complete-selection))

;; Don't auto complete numbers
(setq company-dabbrev-char-regexp "[A-z:-]")

(global-set-key (kbd "C-;") 'company-complete)

;; CAPF
(add-to-list 'company-backends '(company-capf company-dabbrev))
;; (add-to-list 'company-backends '(company-capf))

;;CTAGS
(require 'company-ctags)
(with-eval-after-load 'company
  (company-ctags-auto-setup))

;;HTML
(require 'company-web-html)
(add-to-list 'company-backends 'company-web-html)

;;PHP
;; PHP auto completion depends on php-extras generated library
;; M-x load-library RET php-extras-gen-eldoc RET
;; M-x php-extras-generate-eldoc RET
;; For local auto complete add a .ac-php-conf.json file to the project root
;; then run ac-php-remake-tags to generate a template, then configure it
(add-hook 'php-mode-hook
          '(lambda ()
             (require 'company-php)
             (company-mode t)
             (set (make-local-variable 'company-backends)
                   '((company-ac-php-backend company-capf company-dabbrev-code) company-files))))


;;emoji
;; (require 'company-emoji)
;; (add-to-list 'company-backends 'company-emoji)

;;LUA
(require 'company-lua)
(add-to-list 'company-backends 'company-lua)

;;JS
;; company-tern got axed, using LSP now
;; language servers must be manually installed https://emacs-lsp.github.io/lsp-mode/page/lsp-typescript-javascript/
;; npm i -g bash-language-server
;; npm i -g typescript-language-server; npm i -g typescript
;; npm i -g javascript-typescript-langserver

;; (require 'company-lsp)
;; (add-to-list 'company-backends 'company-lsp)
;; (setq company-lsp-cache-candidates 'auto
;;         company-lsp-async t
;;         company-lsp-enable-snippet nil
;;         company-lsp-enable-recompletion t)

;; DEPRECATED - WILL BE REMOVED SOON
;;(require 'company-tern)
;;(add-to-list 'company-backends 'company-tern)
;;(add-hook 'js2-mode-hook (lambda ()
;;                           (tern-mode)
;;                          (company-mode)))
;; Disable completion keybindings, as we use xref-js2 instead
;;(define-key tern-mode-keymap (kbd "M-.") nil)
;;(define-key tern-mode-keymap (kbd "M-,") nil)


;;PYTHON
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)
;; (add-to-list 'company-backends 'company-jedi)
;; (add-to-list 'company-backends 'company-anaconda)

;;HTML
;; (require 'company-web-html)                          ; load company mode html backend
;; (add-to-list 'company-backends 'company-web-html)

;;C / C++ headers
(require 'company-irony)
(require 'company-irony-c-headers)
;; (require 'company-c-headers)
;; (add-to-list 'company-backends '(company-irony-c-headers company-irony))
(add-to-list 'company-backends '(company-irony company-irony-c-headers))

;; If you installed this package from without MELPA, you may need
(require 'company-arduino)
;; Configuration for irony.el
;; Add arduino's include options to irony-mode's variable.
(add-hook 'irony-mode-hook 'company-arduino-turn-on)
;; Configuration for company-c-headers.el
;; The `company-arduino-append-include-dirs' function appends
;; Arduino's include directories to the default directories
;; if `default-directory' is inside `company-arduino-home'. Otherwise
;; just returns the default directories.
;; Please change the default include directories accordingly.
(defun my-company-c-headers-get-system-path ()
  "Return the system include path for the current buffer."
  (let ((default '("/usr/include/" "/usr/local/include/")))
    (company-arduino-append-include-dirs default t)))
(setq company-c-headers-path-system 'my-company-c-headers-get-system-path)

;; Activate irony-mode on arduino-mode
;; (add-hook 'arduino-mode-hook 'irony-mode)

;; GLSL completion
;; requires glslang https://github.com/KhronosGroup/glslang
;; (require'company-glsl)
;; (add-to-list 'company-backends 'company-glsl)

(provide 'company-conf)
