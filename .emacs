;; ─┐ ┬┌─┐┌┐┌┌─┐┌┐ ┬ ┬┌┬┐┌─┐ ─┐ ┬┬ ┬┌─┐
;; ┌┴┬┘├┤ ││││ │├┴┐└┬┘ │ ├┤  ┌┴┬┘└┬┘┌─┘
;; ┴ └─└─┘┘└┘└─┘└─┘ ┴  ┴ └─┘o┴ └─ ┴ └─┘
;; Author:  SENEX @ XENOBYTE.XYZ
;; License: MIT License
;; Website: https://xenobyte.xyz/projects/?nav=hexmacs

;; Include all your packages in the list to have them automatically downloaded, validated
;; and updated
(load "~/.emacs.d/hexmacs-packages.el")

;; MODULE LOADING
;; Keep everything relevant in the same file. This greatly
;; aids in debugging and keeping track of what you actually
;; use.
;; Only enable helm or ivy
(require 'helm-conf)
;; (require 'ivy-conf)
(require 'flycheck-conf)
(require 'company-conf)
(require 'yasnippet-conf)
(require 'asm-conf)
(require 'web-conf)
(require 'lua-conf)
(require 'python-conf)
(require 'c-conf)
(require 'keys-conf)
(require 'interface-conf)
(require 'general-conf)

;; Might clash with emacs config face settings
(setq inhibit-x-resources 't)

;; User details
(setq user-full-name "SENEX")
(setq user-mail-address "xenobyte.xyz@protonmail.com")

;; Warning level
;; Careful, if you completely disable them you may end up making an otherwise simple debugging
;; process a complete nightmare
(setq warning-minimum-level :emergency)

;; Show a buffer with debug info on error
;; (setq debug-on-error t)

;; Backup files
(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
;; Or disable them (at your own risk!)
;; (setq make-backup-files nil)

;; Automatically recompile everything that may need it
;; KEEPING THIS ENABLED SLOWS DOWN INITIALIZATION
(byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

;;END CUSTOM EMACS CONFIGURATION
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
 [default default default italic underline success warning error])
 '(beacon-color "#F8BBD0")
 '(company-quickhelp-color-background "#b0b0b0")
 '(company-quickhelp-color-foreground "#232333")
 '(custom-enabled-themes '(base16-atelier-lakeside))
 '(custom-safe-themes
 '("45a8b89e995faa5c69aa79920acff5d7cb14978fbf140cdd53621b09d782edcf" "69e7e7069edb56f9ed08c28ccf0db7af8f30134cab6415d5cf38ec5967348a3c" "41eb3fe4c6b80c7ad156a8c52e9dd6093e8856c7bbf2b92cc3a4108ceb385087" "732ccca2e9170bcfd4ee5070159923f0c811e52b019106b1fc5eaa043dff4030" "fc7fd2530b82a722ceb5b211f9e732d15ad41d5306c011253a0ba43aaf93dccc" "0961d780bd14561c505986166d167606239af3e2c3117265c9377e9b8204bf96" "3e34e9bf818cf6301fcabae2005bba8e61b1caba97d95509c8da78cff5f2ec8e" "b67b2279fa90e4098aa126d8356931c7a76921001ddff0a8d4a0541080dee5f6" "87d46d0ad89557c616d04bef34afd191234992c4eb955ff3c60c6aa3afc2e5cc" "fede08d0f23fc0612a8354e0cf800c9ecae47ec8f32c5f29da841fe090dfc450" "0c3b1358ea01895e56d1c0193f72559449462e5952bded28c81a8e09b53f103f" "aded4ec996e438a5e002439d58f09610b330bbc18f580c83ebaba026bbef6c82" "60e09d2e58343186a59d9ed52a9b13d822a174b33f20bdc1d4abb86e6b17f45b" "02940c38e51991e8ee8ac69537341149d56e9c88d57f2c357eeb1744daad1953" "074f60822c8a332b1500092daae2fe048e43a11072c70b6f0d249931bdbe55dc" "99c86852decaeb0c6f51ce8bd46e4906a4f28ab4c5b201bdc3fdf85b24f88518" "3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "cea3ec09c821b7eaf235882e6555c3ffa2fd23de92459751e18f26ad035d2142" "24714e2cb4a9d6ec1335de295966906474fdb668429549416ed8636196cb1441" "a61109d38200252de49997a49d84045c726fa8d0f4dd637fce0b8affaa5c8620" "1d079355c721b517fdc9891f0fda927fe3f87288f2e6cc3b8566655a64ca5453" "c614d2423075491e6b7f38a4b7ea1c68f31764b9b815e35c9741e9490119efc0" "760ce657e710a77bcf6df51d97e51aae2ee7db1fba21bbad07aab0fa0f42f834" "34ed3e2fa4a1cb2ce7400c7f1a6c8f12931d8021435bad841fdc1192bd1cc7da" "b3bcf1b12ef2a7606c7697d71b934ca0bdd495d52f901e73ce008c4c9825a3aa" "5b7c31eb904d50c470ce264318f41b3bbc85545e4359e6b7d48ee88a892b1915" "fe36e4da2ca97d9d706e569024caa996f8368044a8253dc645782e01cd68d884" "70f5a47eb08fe7a4ccb88e2550d377ce085fedce81cf30c56e3077f95a2909f2" "c3e6b52caa77cb09c049d3c973798bc64b5c43cc437d449eacf35b3e776bf85c" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" "33ea268218b70aa106ba51a85fe976bfae9cf6931b18ceaf57159c558bbcd1e6" "c7eb06356fd16a1f552cfc40d900fe7326ae17ae7578f0ef5ba1edd4fdd09e58" "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "bf798e9e8ff00d4bf2512597f36e5a135ce48e477ce88a0764cfb5d8104e8163" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6" "5e3fc08bcadce4c6785fc49be686a4a82a356db569f55d411258984e952f194a" "a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "ab04c00a7e48ad784b52f34aa6bfa1e80d0c3fcacc50e1189af3651013eb0d58" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9" "bf10bd6d21928bf87bc3032b498c62cb9d48c54c06d217c8b00bef8090e539f7" "57e3f215bef8784157991c4957965aa31bac935aca011b29d7d8e113a652b693" "3d4df186126c347e002c8366d32016948068d2e9198c496093a96775cc3b3eaa" "7bef2d39bac784626f1635bd83693fae091f04ccac6b362e0405abf16a32230c" "3de3f36a398d2c8a4796360bfce1fa515292e9f76b655bb9a377289a6a80a132" "a85e40c7d2df4a5e993742929dfd903899b66a667547f740872797198778d7b5" "8be07a2c1b3a7300860c7a65c0ad148be6d127671be04d3d2120f1ac541ac103" "d2bd16a8bcf295dce0b70e1d2b5c17bb34cb28224a86ee770d56e6c22a565013" "bc4c89a7b91cfbd3e28b2a8e9e6750079a985237b960384f158515d32c7f0490" "dcdd1471fde79899ae47152d090e3551b889edf4b46f00df36d653adc2bf550d" "3a5f04a517096b08b08ef39db6d12bd55c04ed3d43b344cf8bd855bde6d3a1ae" "5a21604c4b1f2df98e67cda2347b8f42dc9ce471a48164fcb8d3d52c3a0d10be" "4f77827c989554f290a8f98a123ea020550864fa43776ca219d9cc76f7c42a94" "446cc97923e30dec43f10573ac085e384975d8a0c55159464ea6ef001f4a16ba" default))
 '(diary-entry-marker 'font-lock-variable-name-face)
 '(emms-mode-line-icon-color "#358d8d")
 '(emms-mode-line-icon-image-cache
 '(image :type xpm :ascent center :data "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c #358d8d\",
\"# c None s None\",
/* pixels */
\"###...####\",
\"###.#...##\",
\"###.###...\",
\"###.#####.\",
\"###.#####.\",
\"#...#####.\",
\"....#####.\",
\"#..######.\",
\"#######...\",
\"######....\",
\"#######..#\" };"))
 '(evil-emacs-state-cursor '("#D50000" hbar))
 '(evil-insert-state-cursor '("#D50000" bar))
 '(evil-normal-state-cursor '("#F57F17" box))
 '(evil-visual-state-cursor '("#66BB6A" box))
 '(fci-rule-color "#c7c7c7")
 '(gnus-logo-colors '("#0d7b72" "#adadad") t)
 '(gnus-mode-line-image-cache
 '(image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c #358d8d\",
\"# c None s None\",
/* pixels */
\"##################\",
\"######..##..######\",
\"#####........#####\",
\"#.##.##..##...####\",
\"#...####.###...##.\",
\"#..###.######.....\",
\"#####.########...#\",
\"###########.######\",
\"####.###.#..######\",
\"######..###.######\",
\"###....####.######\",
\"###..######.######\",
\"###########.######\" };") t)
 '(helm-gtags-auto-update t)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-path-style 'relative)
 '(highlight-indent-guides-auto-enabled nil)
 '(highlight-parentheses-colors '("#B9F" "#B8D" "#B7B" "#B69" "#B57" "#B45" "#B33" "#B11"))
 '(highlight-symbol-colors
 '("#F57F17" "#66BB6A" "#0097A7" "#42A5F5" "#7E57C2" "#D84315"))
 '(highlight-symbol-foreground-color "#546E7A")
 '(highlight-tail-colors '(("#F8BBD0" . 0) ("#FAFAFA" . 100)))
 '(nrepl-message-colors
 '("#336c6c" "#205070" "#0f2050" "#806080" "#401440" "#6c1f1c" "#6b400c" "#23733c"))
 '(package-selected-packages
 '(dracula-theme atom-dark-theme arjen-grey-theme apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes ahungry-theme afternoon-theme abyss-theme base16-theme ggtags rebox2 popwin latex-preview-pane auctex goto-chg yaml-mode dumb-jump org-journal powerline majapahit-theme browse-kill-ring nlinum move-text mode-icons symon expand-region restclient drag-stuff zencoding-mode rainbow-delimiters rainbow-mode highlight-symbol highlight-numbers diff-hl vlf neotree ztree recentf-ext clean-aindent-mode duplicate-thing volatile-highlights undo-tree smartparens ws-butler counsel-etags x86-lookup nasm-mode iasm-mode py-autopep8 elpy indium tern xref-js2 js2-refactor js2-mode lsp-ui web-mode php-extras ac-php function-args yasnippet-snippets yasnippet flycheck-tip flycheck-irony flycheck company-ctags company-lsp company-glsl company-arduino company-jedi company-anaconda company-lua company-irony-c-headers company-irony company-web company-php auto-complete company counsel ivy-hydra swiper-helm helm-tramp helm-themes helm-gtags helm))
 '(pdf-view-midnight-colors '("#232333" . "#c7c7c7"))
 '(pos-tip-background-color "#303030")
 '(pos-tip-foreground-color "#78909C")
 '(red "#ffffff")
 '(tabbar-background-color "#ffffffffffff")
 '(vc-annotate-background "#d4d4d4")
 '(vc-annotate-color-map
 '((20 . "#437c7c")
   (40 . "#336c6c")
   (60 . "#205070")
   (80 . "#2f4070")
   (100 . "#1f3060")
   (120 . "#0f2050")
   (140 . "#a080a0")
   (160 . "#806080")
   (180 . "#704d70")
   (200 . "#603a60")
   (220 . "#502750")
   (240 . "#401440")
   (260 . "#6c1f1c")
   (280 . "#935f5c")
   (300 . "#834744")
   (320 . "#732f2c")
   (340 . "#6b400c")
   (360 . "#23733c")))
 '(vc-annotate-very-old-color "#23733c")
 '(xterm-color-names
 ["#303030" "#D66F84" "#D79887" "#D49A8A" "#94B1A3" "#A8938C" "#989584" "#BAB2A9"])
 '(xterm-color-names-bright
 ["#3A3A3A" "#E47386" "#CC816B" "#769188" "#7D6F6A" "#9C8772" "#BAB2A9"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
