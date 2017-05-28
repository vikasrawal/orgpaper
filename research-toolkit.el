(require 'ox-latex)
(require 'ox-beamer)
(require 'ox-odt)
(require 'ox-html)
(require 'ox-publish)
(require 'org-ref)

;; optional but very useful libraries in org-ref
(require 'doi-utils)
                                        ;  (require 'jmax-bibtex)
                                        ;  (require 'pubmed)
                                        ;  (require 'arxiv)
                                        ;  (require 'sci-id)
                                        ;  (setq reftex-default-bibliography '("/Users/vikas/ssercloud/bibliobase/bibliobase.bib"))
(setq org-ref-default-citation-link "citep")
(setq org-ref-insert-cite-key "C-c )")
(global-set-key [f10] 'org-ref-open-bibtex-notes)
(global-set-key [f11] 'org-ref-open-bibtex-pdf)
(global-set-key [f12] 'org-ref-open-in-browser)

(setq org-confirm-babel-evaluate nil)
(setq org-startup-indented t)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-export-with-smart-quotes t)
(require 'ess-site)
'(org-cycle-include-plain-lists t)
'(org-hide-leading-stars t)
'(org-alphabetical-lists t)
'(org-koma-letter-prefer-subject t)
'(org-latex-default-table-environment "tabularx")
'(ebib-bibtex-dialect (quote biblatex))

;; for getting toprule and bottomrule

(setq org-latex-caption-above '(image table special-block))

;; org capture and refile setup

(define-key global-map "\C-cr" 'org-remember)
(define-key global-map "\C-cc" 'org-capture)
(global-set-key (kbd "<f12>") 'org-agenda)
(setq org-agenda-include-diary t)

(setq org-refile-targets (quote ((nil :maxlevel . 2)
                                 (org-agenda-files :maxlevel . 2))))
                                        ; Stop using paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)
(setq org-refile-allow-creating-parent-nodes (quote confirm)) ; Allow refile to create parent tasks with confirmation
(add-to-list 'exec-path "/usr/local/bin/")


;; comment/uncomment the two lines below to turn on/off auto fill and turn off/on visual line mode
(add-hook 'org-mode-hook 'turn-off-auto-fill)
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)

;; unfill-paragraph command to revert line breaks introduced by auto-fill-mode
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))
(define-key global-map "\M-Q" 'unfill-paragraph)


;; spell-check in org mode buffers

(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-buffer)

(setq flyspell-issue-message-flag nil)

(add-hook 'org-mode-hook (lambda () (setq ispell-parser 'tex)))
(defun flyspell-ignore-tex ()
  (interactive)
  (set (make-variable-buffer-local 'ispell-parser) 'tex))
(add-hook 'org-mode-hook 'flyspell-ignore-tex)

(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

(org-add-link-type
 "color" nil
 (lambda (path desc format)p
  (cond
   ((eq format 'html)
    (format "<span style=\"color:%s;\">%s</span>" path desc))
   ((eq format 'latex)
    (format "{\\color{%s}%s}" path desc)))))

(org-add-link-type
 "hl" nil
 (lambda (path desc format)
  (cond
   ((eq format 'html)
    (format "<font style=\"background-color:%s;\">%s</font>" path desc))
   ((eq format 'latex)
    (format "\\colorbox{%s}{%s}" path desc))))) ;; require \usepackage{color}

(org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (R . t)
     (org . t)
     (ditaa . t)
     (latex . t)
     (dot . t)
     (emacs-lisp . t)
     (ledger . t)
     (gnuplot . t)
     (screen . nil)
     (sh . t)
     (sql . nil)
     (sqlite . t)))

(add-to-list 'org-latex-classes
             '("beamer"
               "\\documentclass[presentation]{beamer}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

(add-to-list 'org-beamer-environments-extra
             '("onlyenv" "O" "\\begin{onlyenv}%a" "\\end{onlyenv}"))
(add-to-list 'org-beamer-environments-extra
             '("textpos" "X" "\\begin{textblock}{10}(3,3) \\visible %a {" "} \\end{textblock}"))
(add-to-list 'org-beamer-environments-extra
             '("textpos1" "w" "\\begin{textblock}{%h}(3,3) \\visible %a {" "} \\end{textblock}"))

;; set value of the variable org-latex-pdf-process

; (setq org-latex-pdf-process
;   '("pdflatex -interaction nonstopmode -output-directory %o %f" "biber %b" "pdflatex -interaction nonstopmode -output-directory %o %f" "pdflatex -interaction nonstopmode -output-directory %o %f"))
;(setq org-latex-pdf-process
;    (list "latexmk -xelatex -pdf -f %s"))
(setq org-latex-pdf-process
   '("xelatex -interaction nonstopmode -output-directory %o %f" "biber %b" "xelatex -interaction nonstopmode -output-directory %o %f" "xelatex -interaction nonstopmode -output-directory %o %f"))

;; various functions useful for bibliography management

(defun my-bibliography-selector-hook (backend)
    (case backend
      (latex
       (when (save-excursion
               (re-search-forward "^[ \t]*\\bibliography\\(?:style\\)?{" nil t))
         (while (re-search-forward "^[ \t]*#+BIBLIOGRAPHY:.*$" nil t)
           (when (eq (org-element-type (save-match-data (org-element-at-point)))
                     'keyword)
             (replace-match "")))))
      (html
       (when (save-excursion
               (re-search-forward "^[ \t]*#+BIBLIOGRAPHY:.*$" nil t))
         (while (re-search-forward "^[ \t]*\\bibliography\\(?:style\\)?{.*$" nil t)
           (replace-match ""))))))

(add-hook 'org-export-before-parsing-hook 'my-bibliography-selector-hook)

(require 'reftex)

(add-to-list 'load-path "~/.emacs.d/elpa/bibretrieve-20170417.620/bibretrieve")
(byte-recompile-directory "~/.emacs.d/elpa/bibretrieve-20170417.620" 0)
(load "bibretrieve")
(setq bibretrieve-backends '(("citebase" . 10) ("mrl" . 10) ("arxiv" . 5) ("zbm" . 5)))

(defun bibretrieve-scholar-create-url (author title)

  (let ((tempfile (make-temp-file "scholar" nil ".bib")))

    (call-process-shell-command "~/bin/gscholar/gscholar/gscholar.py --all" nil nil nil
                (if (> (length author) 0) (concat "\"" author "\""))
                (if (> (length title) 0)  (concat "\"" title "\""))
                (concat " > " tempfile))
    (concat "file://" tempfile)
))

(defun bibretrieve-scholar ()
  (interactive)
  (setq mm-url-use-external t)
  (setq bibretrieve-backends '(("scholar" . 5)))
  (bibretrieve)
  (setq mm-url-use-external nil)
)

(defun bibretrieve-amazon-create-url (author title)
  (concat "http://lead.to/amazon/en/?key="(mm-url-form-encode-xwfu title) "&si=ble&op=bt&bn=&so=sa&ht=us"))

(defun bibretrieve-amazon ()
  (interactive)
  (setq mm-url-use-external t)
  (setq mm-url-program "w3m")
  (setq mm-url-arguments (list "-dump"))
  (setq bibretrieve-backends '(("amazon" . 5)))
  (bibretrieve)
  (setq mm-url-use-external nil)
)

;; some other useful functions

(defun org-export-multicolumn-filter (row backend info)
  (cond
   ((org-export-derived-backend-p backend 'latex)
    (org-export-multicolumn-filter-latex row backend info))
   ((org-export-derived-backend-p backend 'html)
    (org-export-multicolumn-filter-html row backend info))))

(defun org-export-multicolumn-filter-latex (row backend info)
  (while (string-match
          "\\(<\\([0-9]+\\)col\\([lrc]\\)?>[[:blank:]]*\\([^&]+\\)\\)" row)
    (let ((columns (string-to-number (match-string 2 row)))
          (start (match-end 0))
          (contents (replace-regexp-in-string
                     "\\\\" "\\\\\\\\"
                     (replace-regexp-in-string "[[:blank:]]*$" ""
                                               (match-string 4 row))))
          (algn (or (match-string 3 row) "l")))
      (setq row (replace-match
                 (format "\\\\multicolumn{%d}{%s}{%s}" columns algn contents)
                 nil nil row 1))
      (while (and (> columns 1) (string-match "&" row start))
        (setq row (replace-match "" nil nil row))
        (decf columns))))
  row)

(defun org-export-multicolumn-filter-html (row backend info)
  (while (string-match "class=\".*\" *>&lt;\\([0-9]+\\)col\\([lrc]\\)?&gt;" row)
    (let ((columns (string-to-number (match-string 1 row)))
          (start (match-end 0))
          (algn (case (intern (or (match-string 2 row) "l"))
                  (c "center")
                  (r "right")
                  (l "left"))))
      (setq row (replace-match
                 (format " class=\"%s\" colspan=\"%s\">" algn columns)
                 nil nil row))
      (while (and (> columns 1)
                  (string-match "<th .*>&#xa0;</th>" row start))
        (setq row (replace-match "" nil nil row))
        (decf columns))))
  row)

(add-to-list 'org-export-filter-table-row-functions
             'org-export-multicolumn-filter)


(defun org-word-count (beg end
                           &optional count-latex-macro-args?
                           count-footnotes?)
  "Report the number of words in the Org mode buffer or selected region.
Ignores:
- comments
- tables
- source code blocks (#+BEGIN_SRC ... #+END_SRC, and inline blocks)
- hyperlinks (but does count words in hyperlink descriptions)
- tags, priorities, and TODO keywords in headers
- sections tagged as 'not for export'.

The text of footnote definitions is ignored, unless the optional argument
COUNT-FOOTNOTES? is non-nil.

If the optional argument COUNT-LATEX-MACRO-ARGS? is non-nil, the word count
includes LaTeX macro arguments (the material between {curly braces}).
Otherwise, and by default, every LaTeX macro counts as 1 word regardless
of its arguments."
  (interactive "r")
  (unless mark-active
    (setf beg (point-min)
          end (point-max)))
  (let ((wc 0)
        (latex-macro-regexp "\\\\[A-Za-z]+\\(\\[[^]]*\\]\\|\\){\\([^}]*\\)}"))
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (cond
         ;; Ignore comments.
         ((or (org-in-commented-line) (org-at-table-p))
          nil)
         ;; Ignore hyperlinks. But if link has a description, count
         ;; the words within the description.
         ((looking-at org-bracket-link-analytic-regexp)
          (when (match-string-no-properties 5)
            (let ((desc (match-string-no-properties 5)))
              (save-match-data
                (incf wc (length (remove "" (org-split-string
                                             desc "\\W")))))))
          (goto-char (match-end 0)))
         ((looking-at org-any-link-re)
          (goto-char (match-end 0)))
         ;; Ignore source code blocks.
         ((org-in-regexps-block-p "^#\\+BEGIN_SRC\\W" "^#\\+END_SRC\\W")
          nil)
         ;; Ignore inline source blocks, counting them as 1 word.
         ((save-excursion
            (backward-char)
            (looking-at org-babel-inline-src-block-regexp))
          (goto-char (match-end 0))
          (setf wc (+ 2 wc)))
         ;; Count latex macros as 1 word, ignoring their arguments.
         ((save-excursion
            (backward-char)
            (looking-at latex-macro-regexp))
          (goto-char (if count-latex-macro-args?
                         (match-beginning 2)
                       (match-end 0)))
          (setf wc (+ 2 wc)))
         ;; Ignore footnotes.
         ((and (not count-footnotes?)
               (or (org-footnote-at-definition-p)
                   (org-footnote-at-reference-p)))
          nil)
         (t
          (let ((contexts (org-context)))
            (cond
             ;; Ignore tags and TODO keywords, etc.
             ((or (assoc :todo-keyword contexts)
                  (assoc :priority contexts)
                  (assoc :keyword contexts)
                  (assoc :checkbox contexts))
              nil)
             ;; Ignore sections marked with tags that are
             ;; excluded from export.
             ((assoc :tags contexts)
              (if (intersection (org-get-tags-at) org-export-exclude-tags
                                :test 'equal)
                  (org-forward-same-level 1)
                nil))
             (t
              (incf wc))))))
        (re-search-forward "\\w+\\W*")))
    (message (format "%d words in %s." wc
                     (if mark-active "region" "buffer")))))

(defvar jmax-lower-case-words
  '("a" "aboard" "about" "above" "absent" "across"
    "after" "against" "along" "alongside" "amid"
    "amidst" "among" "amongst" "an" "and" "around"
    "as" "as" "aslant" "astride" "at" "athwart"
    "atop" "barring" "before" "behind" "below"
    "beneath" "beside" "besides" "between" "beyond"
    "but" "by" "despite" "down" "during" "except"
    "failing" "following" "for" "for" "from" "in"
    "inside" "into" "like" "mid" "minus" "near"
    "next" "nor" "notwithstanding" "of" "off" "on"
    "onto" "opposite" "or" "out" "outside" "over" "past"
    "per" "plus" "regarding" "round" "save" "since" "so"
    "than" "the" "through" "throughout" "till" "times"
    "to" "toward" "towards" "under" "underneath" "unlike"
    "until" "up" "upon" "via" "vs." "when" "with" "within"
    "without" "worth" "yet")
  "List of words to keep lowercase")

(defun jmax-title-case-article (&optional key start end)
  "Convert a bibtex entry article title to title-case. The
arguments are optional, and are only there so you can use this
function with `bibtex-map-entries' to change all the title
entries in articles."
  (interactive)
  (bibtex-beginning-of-entry)

  (let* ((title (bibtex-autokey-get-field "title"))
         (words (split-string title))
         (lower-case-words '("a" "aboard" "about" "above" "absent" "across"
                             "after" "against" "along" "alongside" "amid"
                             "amidst" "among" "amongst" "an" "and" "around"
                             "as" "as" "aslant" "astride" "at" "athwart"
                             "atop" "barring" "before" "behind" "below"
                             "beneath" "beside" "besides" "between" "beyond"
                             "but" "by" "despite" "down" "during" "except"
                             "failing" "following" "for" "for" "from" "in"
                             "inside" "into" "like" "mid" "minus" "near"
                             "next" "nor" "notwithstanding" "of" "off" "on"
                             "onto" "opposite" "or" "out" "outside" "over" "past"
                             "per" "plus" "regarding" "round" "save" "since" "so"
                             "than" "the" "through" "throughout" "till" "times"
                             "to" "toward" "towards" "under" "underneath" "unlike"
                             "until" "up" "upon" "via" "vs." "when" "with" "within"
                             "without" "worth" "yet")))
    (when
        (string= "article" (downcase (cdr (assoc "=type=" (bibtex-parse-entry)))))
      (setq words (mapcar
                   (lambda (word)
                     (if (or
                          ;; match words containing {} or \ which are probably
                          ;; LaTeX or protected words
                          (string-match "\\$\\|{\\|}\\|\\\\" word)
                          ;; these words should not be capitalized, unless they
                          ;; are the first word
                          (-contains? lower-case-words (s-downcase word)))
                         word
                       (s-capitalize word)))
                   words))

      ;; Check if first word should be capitalized
      (when (-contains? jmax-lower-case-words (car words))
        (setf (car words) (s-capitalize (car words))))

      ;; this is defined in doi-utils
      (bibtex-set-field
       "title"
       (mapconcat 'identity words " "))
      (bibtex-fill-entry))))

(org-add-link-type
 "comment"
 (lambda (linkstring)
   (let ((elm (org-element-context))
         (use-dialog-box nil))
     (when (y-or-n-p "Delete comment? ")
       (setf (buffer-substring
              (org-element-property :begin elm)
              (org-element-property :end elm))
             (cond
              ((org-element-property :contents-begin elm)
               (buffer-substring
                (org-element-property :contents-begin elm)
                (org-element-property :contents-end elm)))
              (t
               ""))))))
 (lambda (keyword desc format)
   (cond
    ((eq format 'html)
     (format "<font color=\"red\"><abbr title=\"%s\" color=\"red\">COMMENT</abbr></font> %s" keyword (or desc "")))
    ((eq format 'latex)
     (format "\\todo{%s}{%s}" keyword (or desc ""))))))

(defun add-comment (begin end)
  (interactive "r")
  (if (region-active-p)
      (let ((selected-text (buffer-substring begin end)))
        (setf (buffer-substring begin end)
              (format "[[comment:%s][%s]]"
                      (read-input "Comment: ") selected-text)))
  (insert (format  "[[comment:%s]]" (read-input "Comment: ")))))


