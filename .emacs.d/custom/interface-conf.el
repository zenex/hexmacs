;; ─┐ ┬┌─┐┌┐┌┌─┐┌┐ ┬ ┬┌┬┐┌─┐ ─┐ ┬┬ ┬┌─┐
;; ┌┴┬┘├┤ ││││ │├┴┐└┬┘ │ ├┤  ┌┴┬┘└┬┘┌─┘
;; ┴ └─└─┘┘└┘└─┘└─┘ ┴  ┴ └─┘o┴ └─ ┴ └─┘
;; Author:  SENEX @ XENOBYTE.XYZ
;; License: MIT License
;; Website: https://xenobyte.xyz/projects/?nav=hexmacs

;; APPEARANCE SETTINGS


;;Remove clutter
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(setq-default blink-cursor-mode 0)
(setq-default cursor-type 'bar)
(setq-default set-cursor-color "#00ff00")
;; (setq default-frame-alist '((cursor-type . 'bar)))
;; (setq default-frame-alist '((blink-cursor-mode . 1)))
;; (setq default-frame-alist '((set-cursor-color . "#00ff00")))

;;Blinking cursor
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(size-indication-mode t)

;; ;;Custom cursor shapea
;; (defun cursor-shape-hook ()
;;     (if (equal (thing-at-point 'line) "\n") (setq cursor-type 'bar)
;;        (setq cursor-type 'box)))

;; (add-hook 'post-command-hook 'cursor-shape-hook)


;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
;; taken from prelude-ui.el
(setq frame-title-format
      '("" invocation-name " - " (:eval (if (buffer-file-name)
                                            (abbreviate-file-name (buffer-file-name))
                                          "%b"))))

;;disable line highlight (looks like shit with certain themes)
;;(setq-local global-hl-line-mode nil)
(setq global-hl-line-mode -1)
(setq hl-line-mode -1)


;; orgmode tags
(setq org-todo-keywords
      '((sequence "TODO" "|" "DONE")
        (sequence "REPORT" "BUG" "KNOWN_CAUSE" "|" "FIXED")
        (sequence "|" "CANCELED")))

;; Font configuration
;; TEWI FONT
(add-to-list 'default-frame-alist '(font . "tewi-9" ))
(set-face-attribute 'default t :font "tewi-9" )
;; Ac437 ATI 8x8 FONT
;; (add-to-list 'default-frame-alist '(font . "Ac437 ATI 8x8-6" ))
;; (set-face-attribute 'default t :font "Ac437 ATI 8x8-6" )

;; OHSNAP FONT
;; (add-to-list 'default-frame-alist '(font . "misc ohsnapu-9:bold" ))
;; (set-face-attribute 'default t :font "misc ohsnapu-9:bold" )
;; (add-to-list 'default-frame-alist '(font . "misc ohsnapu-9" ))
;; (set-face-attribute 'default t :font "misc ohsnapu-9" )

;;Theme (Overrides them one selected include M-x customize-themes)
;;(load-theme 'majapahit-dark t)

;;Shift select
(setq shift-select-mode t)

;; Open files on startup
;; THIS BREAKS EMACS AS A DAEMON SINCE THE LACK OF WINDOW SPACE
;; THROWS AN ERROR, COMMENT OUT IF YOU WANT TO RUN AN EMACS SERVER!
;; (defun startup-layout ()
;;   (interactive)
;;   (delete-other-windows)
;;   (split-window-horizontally) ;; -> |
;;   (next-multiframe-window)
;;   (find-file "~/.Xresources")
;;   (split-window-vertically) ;;  -> --
;;   (next-multiframe-window)
;;   (find-file "~/.zshrc")
;;   (split-window-horizontally) ;; -> |
;;   (next-multiframe-window)
;;   (find-file "~/.emacs")
;;   (split-window-vertically) ;;  -> --
;;   (next-multiframe-window)
;;   (find-file "~/.config/awesome/rc.lua")
;;   (next-multiframe-window)
;;   (dired "~")
;;   )
;; ;; execute the layout
;; (startup-layout)

;;hide the startup message
(setq inhibit-startup-message t) ;; hide the startup message


;;auto refresh
(global-auto-revert-mode t)

;;Open as root---------------------------------
;;Ask if I want to open file as root (and use tramp-sudo to give
;;permission)
(defun th-rename-tramp-buffer ()
  (when (file-remote-p (buffer-file-name))
    (rename-buffer
     (format "%s:%s"
             (file-remote-p (buffer-file-name) 'method)
             (buffer-name)))))

(add-hook 'find-file-hook
          'th-rename-tramp-buffer)

(defadvice find-file (around th-find-file activate)
  "Open FILENAME using tramp's sudo method if it's read-only."
  (if (and (not (file-writable-p (ad-get-arg 0)))
           (y-or-n-p (concat "File "
                             (ad-get-arg 0)
                             " is read-only.  Open it as root? ")))
      (th-find-file-sudo (ad-get-arg 0))
    ad-do-it))

(defun th-find-file-sudo (file)
  "Opens FILE with root privileges."
  (interactive "F")
  (set-buffer (find-file (concat "/sudo::" file))))

;; size in bytes
(setq large-file-warning-threshold 100000000)

;; also save your regexp search queries
(setq savehist-additional-variables '(search ring regexp-search-ring)
      savehist-autosave-interval 60     ; save every minute
      )

;; column numbers
(column-number-mode 1)

;; winner mode
(winner-mode 1)

;; avoid start screen
(setq
 inhibit-startup-screen t
 )

;; kill ring
(setq global-mark-ring-max 5000         ; increase mark ring to contains 5000 entries
      mark-ring-max 5000                ; increase kill ring to contains 5000 entries
      mode-require-final-newline t      ; add a newline to end of file
      )


;; default to 4 visible spaces to display a tab
(setq-default tab-width 4)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(setq-default indent-tabs-mode nil)
(delete-selection-mode)
(global-set-key (kbd "RET") 'newline-and-indent)

;; .el modes (to edit config files)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; Start garbage collection every 100MB to improve Emacs performance
(setq gc-cons-threshold 100000000)

;; saveplace remembers your location in a file when saving files
(require 'saveplace)
(setq-default save-place t)
;; use (toggle-save-place-globally 1) in Emacs above 24.4
(toggle-save-place-globally 1)

;; update any change made on file to the current buffer
(global-auto-revert-mode)

;; whenever you create useless whitespace, the whitespace is highlighted
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)


;; hippie-expand is a better version of dabbrev-expand.
;; While dabbrev-expand searches for words you already types, in current;; buffers and other buffers, hippie-expand includes more sources,
;; such as filenames, klll ring...
(global-set-key (kbd "M-/") 'hippie-expand) ;; replace dabbrev-expand
(setq
 hippie-expand-try-functions-list
 '(try-expand-dabbrev ;; Try to expand word "dynamically", searching the current buffer.
   try-expand-dabbrev-all-buffers ;; Try to expand word "dynamically", searching all other buffers.
   try-expand-dabbrev-from-kill ;; Try to expand word "dynamically", searching the kill ring.
   try-complete-file-name-partially ;; Try to complete text as a file name, as many characters as unique.
   try-complete-file-name ;; Try to complete text as a file name.
   try-expand-all-abbrevs ;; Try to expand word before point according to all abbrev tables.
   try-expand-list ;; Try to complete the current line to an entire line in the buffer.
   try-expand-line ;; Try to complete the current line to an entire line in the buffer.
   try-complete-lisp-symbol-partially ;; Try to complete as an Emacs Lisp symbol, as many characters as unique.
   try-complete-lisp-symbol) ;; Try to complete word as an Emacs Lisp symbol.
 )


;; go-to-address-mode
(add-hook 'prog-mode-hook 'goto-address-mode)
(add-hook 'text-mode-hook 'goto-address-mode)

(provide 'interface-conf)
