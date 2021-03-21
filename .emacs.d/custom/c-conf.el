;; ─┐ ┬┌─┐┌┐┌┌─┐┌┐ ┬ ┬┌┬┐┌─┐ ─┐ ┬┬ ┬┌─┐
;; ┌┴┬┘├┤ ││││ │├┴┐└┬┘ │ ├┤  ┌┴┬┘└┬┘┌─┘
;; ┴ └─└─┘┘└┘└─┘└─┘ ┴  ┴ └─┘o┴ └─ ┴ └─┘
;; Author:  SENEX @ XENOBYTE.XYZ
;; License: MIT License
;; Website: https://xenobyte.xyz/projects/?nav=hexmacs

;; C/C++ CONFIGURATION

;; Available C style:
;; “gnu”: The default style for GNU projects
;; “k&r”: What Kernighan and Ritchie, the authors of C used in their book
;; “bsd”: What BSD developers use, aka “Allman style” after Eric Allman.
;; “whitesmith”: Popularized by the examples that came with Whitesmiths C, an early commercial C compiler.
;; “stroustrup”: What Stroustrup, the author of C++ used in his book
;; “ellemtel”: Popular C++ coding standards as defined by “Programming in C++, Rules and Recommendations,” Erik Nyquist and Mats Henricson, Ellemtel
;; “linux”: What the Linux developers use for kernel development
;; “python”: What Python developers use for extension modules
;; “java”: The default style for java-mode (see below)
;; “user”: When you want to define your own style
(setq c-default-style "linux" ; set style to "linux"
      c-basic-offset 4)

;; (require 'lsp-mode)
;; (require 'lsp-ui)
;; (add-hook 'c-mode-hook 'lsp)
;; (add-hook 'c++-mode-hook 'lsp)

;; (setq lsp-clients-clangd-args
;;           '("-j=2"
;;             "--background-index"
;;             "--clang-tidy"
;;             "--completion-style=bundled"
;;             "--pch-storage=memory"
;;             "--header-insertion=never"
;;             "--header-insertion-decorators=0"))

;; (setq gc-cons-threshold (* 100 1024 1024)
;;       read-process-output-max (* 1024 1024)
;;       treemacs-space-between-root-nodes nil
;;       company-idle-delay 0.0
;;       company-minimum-prefix-length 2
;;       lsp-idle-delay 0.1)  ;; clangd is fast
;; (setq lsp-clients-clangd-args
;;       '("--header-insertion=never"))


;;Irony
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; Use compilation database first, clang_complete as fallback.
(setq-default irony-cdb-compilation-databases '(irony-cdb-libclang
                                                irony-cdb-clang-complete))
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; For local auto cmplete make sure to use BEAR to
;; generate an appropriate compile_commands.json
;; cd to the project folder and run
;; make clean
;; make bear


;;CEDET
(require 'cc-mode)
(require 'semantic)

(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)

(semantic-mode 1)
;;(global-semantic-idle-summary-mode 1)
;; Enable case-insensitive searching:
(set-default 'semantic-case-fold t)

(semantic-add-system-include "/usr/include/boost" 'c++-mode)
;; (semantic-add-system-include "~/linux/kernel")
;; (semantic-add-system-include "~/linux/include")
(semantic-add-system-include "~/Qt/5.15.1/gcc_64/include/" 'c++-mode)
(semantic-add-system-include "/usr/include/c++/10.2.0" 'c++-mode)

;;show function name on top of buffer
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)

;;Code folding
(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'c++-mode-common-hook   'hs-minor-mode)

;;GDB
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

;;Function args (show arguments near cursor)
(require 'function-args)
(fa-config-default)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(set-default 'semantic-case-fold t)
(require 'semantic/bovine/c)
(add-to-list 'semantic-lex-c-preprocessor-symbol-file
             "/usr/lib/gcc/x86_64-pc-linux-gnu/10.2.0/include/stddef.h")

;; Mostly use it with C++ and SkeletonGL, might as well add it here
;; (require 'glsl-mode)
;; (autoload 'glsl-mode "glsl-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
;; (add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
;; (add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
;; (add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))

;; Syntax highlighting support for "Modern C++"
(require 'modern-cpp-font-lock)
(add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)

;;C/C++ --CONFIGS-- END

(provide 'c-conf)
