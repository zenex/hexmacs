;; ─┐ ┬┌─┐┌┐┌┌─┐┌┐ ┬ ┬┌┬┐┌─┐ ─┐ ┬┬ ┬┌─┐
;; ┌┴┬┘├┤ ││││ │├┴┐└┬┘ │ ├┤  ┌┴┬┘└┬┘┌─┘
;; ┴ └─└─┘┘└┘└─┘└─┘ ┴  ┴ └─┘o┴ └─ ┴ └─┘
;; Author:  SENEX @ XENOBYTE.XYZ
;; License: MIT License
;; Website: https://xenobyte.xyz/projects/?nav=hexmacs

;; WEB DEVELOPMENT SETTINGS

;; HTML ,CSS AND XML --CONFIGS--
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))


(setq web-mode-enable-auto-closing t)

(define-key web-mode-map (kbd "C-c /") 'web-mode-element-close-and-indent)

(setq web-mode-auto-close-style 1)
(setq web-mode-tag-auto-close-style 1)

(require 'zencoding-mode)
(add-hook 'web-mode-hook 'zencoding-mode)




;;PHP --CONFIGS--
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))

;; ;; ac-php
;;   (add-hook 'php-mode-hook
;;             '(lambda ()
;;                (auto-complete-mode t)
;;                (require 'ac-php)
;;                (setq ac-sources  '(ac-source-php ) )
;;                (yas-global-mode 1)

;;                ;; (ac-php-core-eldoc-setup ) ;; BROKEN!
;;                (define-key php-mode-map  (kbd "C-]") 'ac-php-find-symbol-at-point)   ;goto define
;;                (define-key php-mode-map  (kbd "C-t") 'ac-php-location-stack-back   ) ;go back
;;                ))



;;Restclient REST API tester
(require 'restclient)

;;JS - JAVASCRIPT --CONFIGS--

;; NOTE: that lsp may clash with js2-mode when loading a file, if you're experiencing issues just manually
;; re-enable it with M-x js2-mode
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;; Better imenu
(require 'js2-refactor)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

(require 'xref-js2)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;;tern (must be manually installed)
;; npm install -g tern
;; THE TERN PACKAGE LOOKS FOR THE "tern" EXECUTABLE IN /usr/local/bin/
;; REQUIRES A .tern-project file present
;; example .tern-project
;; {
;;   "libs": [
;;     "jquery"
;;   ],
;;   "loadEagerly": [
;;     "./**/*.js"
;;   ],
;;   "dontLoad": [
;;     "./bower_components/"
;;   ],
;;   "plugins": {
;;     "requirejs": {
;;       "baseURL": "./"
;;     }
;;   }
;; }

;; Required in some systems due to loading inconsistencies!
(add-to-list 'exec-path "~/.node_modules/bin/")

(require 'tern)
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))

;; Indium, requires manual install
;; npm install -g indium
;; https://indium.readthedocs.io/en/latest/setup.html
;; https://emacs.cafe/emacs/javascript/indium/2018/08/14/indium.html
;; EMACS 27+ NOTE: 
;; Indium's dependencies are outdated and may fail to properly download when first installing.
;; If you get "json-process-client-*" errors simply comment out the indium package and relaunch
;; if the installation finished successfully you can then uncomment indium and its respective loader
;; function
;; EMACS 27+ NOTE:
;; Deoending on your system emacs may be fail to properly locate then required node pacakges.
;; Make sure GLOBALLY install then following packages:
;; npm install -g semver, node-fetch source-map, p-queue, chrome-remote-interface
;; And make sure to have the root modules at $HOME/.node_modules/bin/tern
(require 'json-process-client) ;; Usually note required but as of 11/Oct/2020 Indium fails without it
(require 'indium)
(require 'lsp-mode)
(require 'lsp-ui)

(add-hook 'js2-mode-hook #'lsp-deferred)
(add-hook 'css-mode-hook #'lsp-deferred)
(add-hook 'web-mode-hook #'lsp-deferred)
(add-hook 'php-mode-hook #'lsp-deferred)

(setq lsp-auto-configure t
      lsp-auto-guess-root t
      ;; don't set flymake or lsp-ui so the default linter doesn't get trampled
      lsp-diagnostic-package :none)

(setq lsp-ui-doc-enable t
      lsp-ui-doc-use-childframe t
      lsp-ui-doc-position 'top
      lsp-ui-doc-include-signature t
      lsp-ui-sideline-enable nil
      lsp-ui-flycheck-enable t
      lsp-ui-flycheck-list-position 'right
      lsp-ui-flycheck-live-reporting t
      lsp-ui-peek-enable t
      lsp-ui-peek-list-width 60
      lsp-ui-peek-peek-height 25)

(add-hook 'lsp-mode-hook 'lsp-ui-mode)

;; (setq lsp-ui-sideline-enable t
;;       ;; disable flycheck setup so default linter isn't trampled
;;       lsp-ui-flycheck-enable nil
;;       lsp-ui-sideline-show-symbol nil
;;       lsp-ui-sideline-show-hover nil
;;       lsp-ui-sideline-show-code-actions nil
;;       lsp-ui-peek-enable nil
;;       lsp-ui-imenu-enable nil
;;       lsp-ui-doc-enable nil)

(provide 'web-conf)
