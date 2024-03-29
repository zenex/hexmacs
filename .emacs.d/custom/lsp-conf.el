;; ─┐ ┬┌─┐┌┐┌┌─┐┌┐ ┬ ┬┌┬┐┌─┐ ─┐ ┬┬ ┬┌─┐
;; ┌┴┬┘├┤ ││││ │├┴┐└┬┘ │ ├┤  ┌┴┬┘└┬┘┌─┘
;; ┴ └─└─┘┘└┘└─┘└─┘ ┴  ┴ └─┘o┴ └─ ┴ └─┘
;; Author:  SENEX @ XENOBYTE.XYZ
;; License: MIT License
;; Website: https://xenobyte.xyz/projects/?nav=hexmacs

(require 'lsp-mode)

(add-hook 'js2-mode-hook #'lsp-deferred)
(add-hook 'css-mode-hook #'lsp-deferred)
(add-hook 'web-mode-hook #'lsp-deferred)
(add-hook 'php-mode-hook #'lsp-deferred)
(add-hook 'python-mode-hook #'lsp-deferred)

(setq lsp-auto-configure t
      lsp-auto-guess-root t
      ;; don't set flymake or lsp-ui so the default linter doesn't get trampled
      lsp-diagnostic-package :none)

;; Disable the annoying top bar
(setq lsp-signature-auto-activate nil)
(setq lsp-headerline-breadcrumb-enable nil)
(setq lsp-ui-sideline-enable nil)
(setq lsp-ui-sideline-show-code-actions nil)
(setq lsp-ui-sideline-enable nil)
(setq lsp-lens-enable nil)
(setq lsp-ui-doc-show-with-cursor nil)

(setq lsp-completion-provider t)
(setq lsp-completion-show-detail t)
(setq lsp-completion-show-kind t)
(setq lsp-signature-auto-activate t)
(setq lsp-eldoc-enable-hover t)

(setq lsp-enable-file-swatchers nil)

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

(setq lsp-clients-clangd-args
          '("-j=2"
            "--background-index"
            "--clang-tidy"
            "--completion-style=bundled"
            "--pch-storage=memory"
            "--header-insertion=never"
            "--header-insertion-decorators=0"))


;; (setq lsp-completion-provider :none)

;; (setq lsp-ui-sideline-enable t
;;       ;; disable flycheck setup so default linter isn't trampled
;;       lsp-ui-flycheck-enable nil
;;       lsp-ui-sideline-show-symbol nil
;;       lsp-ui-sideline-show-hover nil
;;       lsp-ui-sideline-show-code-actions nil
;;       lsp-ui-peek-enable nil
;;       lsp-ui-imenu-enable nil
;;       lsp-ui-doc-enable nil)



;;LSP --CONFIGS-- END

(provide 'lsp-conf)
