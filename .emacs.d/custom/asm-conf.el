;; ─┐ ┬┌─┐┌┐┌┌─┐┌┐ ┬ ┬┌┬┐┌─┐ ─┐ ┬┬ ┬┌─┐
;; ┌┴┬┘├┤ ││││ │├┴┐└┬┘ │ ├┤  ┌┴┬┘└┬┘┌─┘
;; ┴ └─└─┘┘└┘└─┘└─┘ ┴  ┴ └─┘o┴ └─ ┴ └─┘
;; Author:  SENEX @ XENOBYTE.XYZ
;; License: MIT License
;; Website: https://xenobyte.xyz/projects/?nav=hexmacs

;; ASM RELATED SETTINGS

(require 'iasm-mode)
(global-set-key (kbd "C-c C-d") 'iasm-disasm)
(global-set-key (kbd "C-c C-l") 'iasm-ldd)
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "C-c d") 'iasm-goto-disasm-buffer)
            (local-set-key (kbd "C-c l") 'iasm-disasm-link-buffer)))

;; x86-lookup starts here
;; THIS REQUIRES THE X86 SPEC PDF TO BE MANUALLY DOWNLOADED, GOOGLE IT
;; https://github.com/skeeto/x86-lookup
(global-set-key (kbd "C-h x") #'x86-lookup)
(require 'cl-lib)
(require 'doc-view)

(defgroup x86-lookup ()
  "Options for x86 instruction set lookup."
  :group 'extensions)

 ;; Must be manually downloaded
 ;; https://software.intel.com/content/www/us/en/develop/download/intel-64-and-ia-32-architectures-sdm-combined-volumes-1-2a-2b-2c-2d-3a-3b-3c-3d-and-4.html
 
 (defcustom x86-lookup-pdf "~/.emacs.d/x86-lookup.pdf"
  "/home/alexhg/.emacs.d/x86-lookup.pdf"
  :group 'x86-lookup
  :type '(choice (const nil)
                 (file :must-match t)))

;; Must be manually downloaded
;; pip3 install pdftotext
(defcustom x86-lookup-pdftotext-program "pdftotext"
  "Path to pdftotext, part of Popper."
  :group 'x86-lookup
  :type 'string)

(defcustom x86-lookup-browse-pdf-function #'x86-lookup-browse-pdf-any
  "A function that launches a PDF viewer at a specific page.
This function accepts two arguments: filename and page number."
  :group 'x86-lookup
  :type '(choice (function-item :tag "First suitable PDF reader" :value
                                x86-lookup-browse-pdf-any)
                 (function-item :tag "Evince" :value
                                x86-lookup-browse-pdf-evince)
                 (function-item :tag "Xpdf" :value
                                x86-lookup-browse-pdf-xpdf)
                 (function-item :tag "Okular" :value
                                x86-lookup-browse-pdf-okular)
                 (function-item :tag "gv" :value
                                x86-lookup-browse-pdf-gv)
                 (function-item :tag "zathura" :value
                                x86-lookup-browse-pdf-zathura)
                 (function-item :tag "MuPDF" :value
                                x86-lookup-browse-pdf-mupdf)
                 (function-item :tag "browse-url"
                                :value x86-lookup-browse-pdf-browser)
                 (function :tag "Your own function")))

(defcustom x86-lookup-cache-directory
  (let ((base (or (getenv "XDG_CACHE_HOME")
                  (getenv "LocalAppData")
                  "~/.cache")))
    (expand-file-name "x86-lookup" base))
  "Directory where the PDF mnemonic index with be cached."
  :type 'string)

(defvar x86-lookup-index nil
  "Alist mapping instructions to page numbers.")

(defvar x86-lookup--expansions
  '(("^PREFETCH\\(h\\)$"
     "" "nta" "t0" "t1" "t2")
    ("^J\\(cc\\)$"
     "a" "ae" "b" "be" "c" "cxz" "e" "ecxz" "g" "ge" "l" "le" "na" "nae" "nb"
     "nbe" "nc" "ne" "ng" "nge" "nl" "nle" "no" "np" "ns" "nz" "o" "p" "pe"
     "po" "rcxz" "s" "z")
    ("^SET\\(cc\\)$"
     "a" "ae" "b" "be" "c" "e" "g" "ge" "l" "le" "na" "nae" "nb" "nbe" "nc"
     "ne" "ng" "nge" "nl" "nle" "no" "np" "ns" "nz" "o" "p" "pe" "po" "s" "z")
    ("^CMOV\\(cc\\)$"
     "a" "ae" "b" "be" "c" "e" "g" "ge" "l" "le" "na" "nae" "nb" "nbe" "nc"
     "ne" "ng" "nge" "nl" "nle" "no" "np" "ns" "nz" "o" "p" "pe" "po" "s" "z")
    ("^FCMOV\\(cc\\)$"
     "b" "e" "be" "u" "nb" "ne" "nbe" "nu")
    ("^LOOP\\(cc\\)$"
     "e" "ne")
    ("^VBROADCAST\\(\\)$"
     "" "ss" "sd" "f128")
    ("^VMASKMOV\\(\\)$"
     "" "ps" "pd")
    ("^VPBROADCAST\\(\\)$"
     "" "b" "w" "d" "q" "I128")
    ("^VPMASKMOV\\(\\)$"
     "" "d" "q")
    ("\\(\\)" ; fallback "match"
     ""))
  "How to expand mnemonics into multiple mnemonics.")

(defun x86-lookup--expand (names page)
  "Expand string of PDF-sourced mnemonics into user-friendly mnemonics."
  (let ((case-fold-search nil)
        (rev-string-match-p (lambda (s re) (string-match re s))))
    (save-match-data
      (cl-loop for mnemonic-raw in (split-string names " */ *")
               ;; Collapse "int 3" and "int n" into "int"
               for mnemonic = (replace-regexp-in-string " .+$" "" mnemonic-raw)
               for (_ . tails) = (cl-assoc mnemonic x86-lookup--expansions
                                           :test rev-string-match-p)
               nconc (cl-loop for tail in tails
                              for rep = (replace-match tail nil nil mnemonic 1)
                              collect (cons (downcase rep) page))))))

(cl-defun x86-lookup-create-index (&optional (pdf x86-lookup-pdf))
  "Create an index alist from PDF mapping mnemonics to page numbers.
This function requires the pdftotext command line program."
  (let ((mnemonic (concat "INSTRUCTION SET REFERENCE, [A-Z]-[A-Z]\n\n"
                          "\\([[:alnum:]/ ]+\\)[- ]?—"))
        (case-fold-search nil))
    (with-temp-buffer
      (call-process x86-lookup-pdftotext-program nil t nil
                    (file-truename pdf) "-")
      (setf (point) (point-min))
      (cl-loop for page upfrom 1
               while (< (point) (point-max))
               when (looking-at mnemonic)
               nconc (x86-lookup--expand (match-string 1) page) into index
               do (forward-page)
               finally (cl-return
                        (cl-remove-duplicates
                         index :key #'car :test #'string= :from-end t))))))

(defun x86-lookup--index-file (pdf)
  "Return index filename from PDF filename."
  (concat (sha1 pdf) "_v3"))

(defun x86-lookup--save-index (pdf index)
  "Save INDEX for PDF in `x86-lookup-cache-directory'."
  (let* ((index-file (x86-lookup--index-file pdf))
         (cache-path (expand-file-name index-file x86-lookup-cache-directory)))
    (mkdir x86-lookup-cache-directory t)
    (with-temp-file cache-path
      (prin1 index (current-buffer)))
    index))

(defun x86-lookup--load-index (pdf)
  "Return index PDF from `x86-lookup-cache-directory'."
  (let* ((index-file (x86-lookup--index-file pdf))
         (cache-path (expand-file-name index-file x86-lookup-cache-directory)))
    (when (file-exists-p cache-path)
      (with-temp-buffer
        (insert-file-contents cache-path)
        (setf (point) (point-min))
        (ignore-errors (read (current-buffer)))))))

(defun x86-lookup-ensure-index ()
  "Ensure the PDF index has been created, returning the index."
  (when (null x86-lookup-index)
    (cond
     ((null x86-lookup-pdf)
      (error "No PDF available. Set `x86-lookup-pdf'."))
     ((not (file-exists-p x86-lookup-pdf))
      (error "PDF not found. Check `x86-lookup-pdf'."))
     ((setf x86-lookup-index (x86-lookup--load-index x86-lookup-pdf))
      x86-lookup-index)
     ((progn
        (message "Generating mnemonic index ...")
        (setf x86-lookup-index (x86-lookup-create-index))
        (x86-lookup--save-index x86-lookup-pdf x86-lookup-index)))))
  x86-lookup-index)

(defun x86-lookup-browse-pdf (pdf page)
  "Launch a PDF viewer using `x86-lookup-browse-pdf-function'."
  (funcall x86-lookup-browse-pdf-function pdf page))

;;;###autoload
(defun x86-lookup (mnemonic)
  "Jump to the PDF documentation for MNEMONIC.
Defaults to the mnemonic under point."
  (interactive
   (progn
     (x86-lookup-ensure-index)
     (let* ((mnemonics (mapcar #'car x86-lookup-index))
            (thing (thing-at-point 'word))
            (mnemonic (if (member thing mnemonics) thing nil))
            (prompt (if mnemonic
                        (format "Mnemonic (default %s): " mnemonic)
                      "Mnemonic: ")))
       (list
        (completing-read prompt mnemonics nil t nil nil mnemonic)))))
  (let ((page (cdr (assoc mnemonic x86-lookup-index))))
    (x86-lookup-browse-pdf (file-truename x86-lookup-pdf) page)))

;; PDF viewers:

(defun x86-lookup-browse-pdf-pdf-tools (pdf page)
  "View PDF at PAGE using Emacs' `pdf-view-mode' and `display-buffer'."
  (require 'pdf-tools)
  (prog1 t
    (with-selected-window (display-buffer (find-file-noselect pdf :nowarn))
      (with-no-warnings
        (pdf-view-goto-page page)))))

(defun x86-lookup-browse-pdf-doc-view (pdf page)
  "View PDF at PAGE using Emacs' `doc-view-mode' and `display-buffer'."
  (prog1 t
    (unless (doc-view-mode-p 'pdf)
      (error "doc-view not available for PDF"))
    (with-selected-window (display-buffer (find-file-noselect pdf :nowarn))
      (doc-view-goto-page page))))

(defun x86-lookup-browse-pdf-xpdf (pdf page)
  "View PDF at PAGE using xpdf."
  (start-process "xpdf" nil "xpdf" "--" pdf (format "%d" page)))

(defun x86-lookup-browse-pdf-evince (pdf page)
  "View PDF at PAGE using Evince."
  (start-process "evince" nil "evince" "-p" (format "%d" page) "--" pdf))

(defun x86-lookup-browse-pdf-okular (pdf page)
  "View PDF at PAGE file using Okular."
  (start-process "okular" nil "okular" "-p" (format "%d" page) "--" pdf))

(defun x86-lookup-browse-pdf-gv (pdf page)
  "View PDF at PAGE using gv."
  (start-process "gv" nil "gv" "-nocenter" (format "-page=%d" page) "--" pdf))

(defun x86-lookup-browse-pdf-zathura (pdf page)
  "View PDF at PAGE using zathura."
  (start-process "zathura" nil "zathura" "-P" (format "%d" page) "--" pdf))

(defun x86-lookup-browse-pdf-mupdf (pdf page)
  "View PDF at PAGE using MuPDF."
  ;; MuPDF doesn't have a consistent name across platforms.
  ;; Furthermore, Debian ships with a broken "mupdf" wrapper shell
  ;; script and must be avoided. Here we use `executable-find' to
  ;; avoid calling it as mupdf-x11 on non-X11 platforms.
  (let ((exe (or (executable-find "mupdf-x11") "mupdf")))
    (start-process "mupdf" nil exe "--" pdf (format "%d" page))))

(defun x86-lookup-browse-pdf-browser (pdf page)
  "Visit PDF using `browse-url' with a fragment for the PAGE."
  (browse-url (format "file://%s#%d" pdf page)))

(defun x86-lookup-browse-pdf-any (pdf page)
  "Try visiting PDF using the first viewer found."
  (or (ignore-errors (x86-lookup-browse-pdf-pdf-tools pdf page))
      (ignore-errors (x86-lookup-browse-pdf-doc-view pdf page))
      (ignore-errors (x86-lookup-browse-pdf-evince pdf page))
      (ignore-errors (x86-lookup-browse-pdf-xpdf pdf page))
      (ignore-errors (x86-lookup-browse-pdf-okular pdf page))
      (ignore-errors (x86-lookup-browse-pdf-gv pdf page))
      (ignore-errors (x86-lookup-browse-pdf-zathura pdf page))
      (ignore-errors (x86-lookup-browse-pdf-mupdf pdf page))
      (ignore-errors (x86-lookup-browse-pdf-browser pdf page))
      (error "Could not find a PDF viewer.")))

(provide 'x86-lookup)

;;; x86-lookup.el ends here

;;NASM-mode
(add-to-list 'auto-mode-alist '("\\.nasm\\'" . nasm-mode))

(provide 'asm-conf)
