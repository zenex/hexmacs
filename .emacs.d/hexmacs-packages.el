;; ─┐ ┬┌─┐┌┐┌┌─┐┌┐ ┬ ┬┌┬┐┌─┐ ─┐ ┬┬ ┬┌─┐
;; ┌┴┬┘├┤ ││││ │├┴┐└┬┘ │ ├┤  ┌┴┬┘└┬┘┌─┘
;; ┴ └─└─┘┘└┘└─┘└─┘ ┴  ┴ └─┘o┴ └─ ┴ └─┘
;; Author:  SENEX @ XENOBYTE.XYZ
;; License: MIT License
;; Website: https://xenobyte.xyz/projects/?nav=hexmacs

;; This file loads the required-packages list and makes sure every included package is
;; downloaded, validated and kept to date on startup, I recommend you only enable the
;; themes once to download them and then comment them again, no point in verifying them
;; each time you start emacs.
;; If the package can't be downloaded an error will be thrown.

(require 'cl)
(require 'package)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(custom-set-variables
 '(gnutls-algorithm-priority "normal:-vers-tls1.3"))



;; added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Update the path, many plugins depend on already installed executables
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
;; Load the following files
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/custom/")

;; START PACKAGE LIST
(defvar required-packages
  '(
    ;;Helm
    helm
    helm-gtags
    helm-themes
    helm-tramp
    helm-lsp
    swiper-helm
    ;;IVY
    ivy
    ivy-hydra
    counsel
    swiper
    ;;Company
    company
    ;; Auto-complete
    auto-complete
    ;; company-c-headers
    company-php
    ;;company-tern ;; pulled from melpa and gtihub lol
    company-web
    company-irony
    company-irony-c-headers
    company-lua
    company-anaconda
    company-jedi
    company-arduino
    company-glsl
    ;; company-lsp ;; no longer supported in favor of company-capf
    company-ctags ;; TESTING
    ;;Flycheck
    flycheck
    flycheck-irony
    flycheck-tip
    ;;Yasnippet
    yasnippet
    yasnippet-snippets
    ;;C++
    irony ;; irony-install-server
    function-args
    glsl-mode ;; technically its own language but w/e
    modern-cpp-font-lock
    ;;PHP
    php-mode
    ac-php-core
    ac-php
    ;;php-extras
    ;;HTML CSS
    web-mode
    ;;JS
    ;;json-process-client
    lsp-mode
    lsp-ui
    lsp-treemacs
    js2-mode
    js2-refactor
    xref-js2
    tern ;; (MUST BE MANUALLY INSTALLED AND ADDED)
    indium
    ;;LUA
    lua-mode
    ;;PYTHON
    elpy
    py-autopep8
    anaconda-mode ;; pip(3) install jedi
    ;;ASM
    iasm-mode
    nasm-mode
    x86-lookup
    ;;Common Packages
    all-the-icons
    exec-path-from-shell
    counsel-etags
    ws-butler
    smartparens
    undo-tree
    volatile-highlights
    duplicate-thing
    clean-aindent-mode
    ibuffer
    recentf-ext
    ztree
    neotree
    vlf
    diff-hl
    highlight-numbers
    highlight-symbol
    rainbow-mode
    rainbow-delimiters
    clean-aindent-mode
    flyspell
    zencoding-mode
    drag-stuff
    restclient
    expand-region
    symon
    mode-icons
    move-text
    nlinum
    browse-kill-ring
    majapahit-theme
    powerline
    org-journal
    dumb-jump
    epa
    yaml-mode
    cl-lib
    goto-chg
    auctex
    markdown-mode
    latex-preview-pane
    popwin
    popup
    rebox2
    ggtags
    highlight-parentheses
    pdf-tools
    dap-mode
    ;;eglot
    ;; ;;THEMES -- ONLY ENABLE ONCE TO DOWNLOAD THEM THEN SET WITH M-X CUSTOMIZE-THEMES
    base16-theme
    abyss-theme
    afternoon-theme
    ahungry-theme
    alect-themes
    ample-theme
    ample-zen-theme
    anti-zenburn-theme
    apropospriate-theme
    arjen-grey-theme
    atom-dark-theme
    ;; atom-one-dark-theme
    ;; autumn-light-theme
    ;; badger-theme
    ;; badwolf-theme
    ;; base16-theme
    ;; basic-theme
    ;; birds-of-paradise-plus-theme
    ;; bliss-theme
    ;; borland-blue-theme
    ;; boron-theme
    ;; bubbleberry-theme
    ;; busybee-theme
    ;; calmer-forest-theme
    ;; caroline-theme
    ;; cherry-blossom-theme
    ;; clues-theme
    ;; colonoscopy-theme
    ;; cyberpunk-theme
    ;; cycle-themes
    ;; dakrone-theme
    ;; darcula-theme
    ;; darkburn-theme
    ;; dark-krystal-theme
    ;; dark-mint-theme
    ;; darkmine-theme
    ;; darktooth-theme
    ;; distinguished-theme
    ;; django-theme
    dracula-theme
    ;; eclipse-theme
    ;; eink-theme
    ;; espresso-theme
    ;; faff-theme
    ;; farmhouse-theme
    ;; firecode-theme
    ;; flatland-theme
    ;; flatland-black-theme
    ;; flatui-theme
    ;; foggy-night-theme
    ;; gandalf-theme
    ;; gotham-theme
    ;; grandshell-theme
    ;; green-phosphor-theme
    ;; greymatters-theme
    ;; gruber-darker-theme
    ;; gruvbox-theme
    ;; hamburg-theme
    ;; hc-zenburn-theme
    ;; hemisu-theme
    ;; heroku-theme
    ;; hydandata-light-theme
    ;; idea-darkula-theme
    ;; inkpot-theme
    ;; iodine-theme
    ;; ir-black-theme
    ;; jazz-theme
    ;; jbeans-theme
    ;; kooten-theme
    ;; lavender-theme
    ;; lenlen-theme
    ;; leuven-theme
    ;; light-soap-theme
    ;; lush-theme
    ;; majapahit-theme
    ;; material-theme
    ;; mbo70s-theme
    ;; mellow-theme
    ;; minimal-theme
    ;; moe-theme
    ;; molokai-theme
    ;; monochrome-theme
    ;; monokai-theme
    ;; mustang-theme
    ;; mustard-theme
    ;; naquadah-theme
    ;; noctilux-theme
    ;; obsidian-theme
    ;; occidental-theme
    ;; oldlace-theme
    ;; org-beautify-theme
    ;; organic-green-theme
    ;; paper-theme
    ;; pastelmac-theme
    ;; pastels-on-dark-theme
    ;; peacock-theme
    ;; per-buffer-theme
    ;; phoenix-dark-mono-theme
    ;; phoenix-dark-pink-theme
    ;; plan9-theme
    ;; planet-theme
    ;; professional-theme
    ;; purple-haze-theme
    ;; quasi-monochrome-theme
    ;; railscasts-theme
    ;; rand-theme
    ;; reverse-theme
    ;; reykjavik-theme
    ;; seoul256-theme
    ;; seti-theme
    ;; silkworm-theme
    ;; slime-theme
    ;; smyx-theme
    ;; soft-charcoal-theme
    ;; soft-morning-theme
    ;; soft-stone-theme
    ;; soothe-theme
    ;; spacegray-theme
    ;; spacemacs-theme
    ;; subatomic-theme
    ;; subatomic256-theme
    ;; sublime-themes
    ;; sunny-day-theme
    ;; suscolors-theme
    ;; tango-2-theme
    ;; tango-plus-theme
    ;; tangotango-theme
    ;; tao-theme
    ;; termbright-theme
    ;; tommyh-theme
    ;; tronesque-theme
    ;; twilight-anti-bright-theme
    ;; twilight-bright-theme
    ;; twilight-theme
    ;; ubuntu-theme
    ;; ujelly-theme
    ;; underwater-theme
    ;; waher-theme
    ;; warm-night-theme
    ;; white-sand-theme
    ;; xresources-theme
    ;; zen-and-art-theme
    ;; zenburn-theme
    ;; zerodark-theme

    ) "List of packages that must be verified and installed during launch.")

                                        ; method to check if all packages are installed
(defun packages-installed-p ()
  (loop for p in required-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

                                        ; if not all packages are installed, check one by one and install the missing ones.
(unless (packages-installed-p)
                                        ; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
                                        ; install the missing packages
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;; END PACKAGE LIST
