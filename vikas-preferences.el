;(setq server-socket-dir (format "/tmp/emacs%d" (user-uid)))
(server-start)
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))

(turn-on-eldoc-mode)
(prefer-coding-system 'utf-8)
(savehist-mode 1)
(setq visible-bell t)
(setq column-number-mode t)
(setq-default indicate-empty-lines t)
(setq-default show-trailing-whitespace t)
(setq ns-function-modifier 'hyper)  ; make Fn key do Hyper
(setq mac-option-modifier 'super) ; make opt key do Super;
(fset 'yes-or-no-p 'y-or-n-p)

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

;(require 'auto-complete-config)
;(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;(ac-config-default)

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

(setq browse-url-browser-function 'eww-browse-url)

;; helm
;; helm-flyspell
(define-key flyspell-mode-map (kbd "C-;") 'helm-flyspell-correct)
;; helm-flx
(helm-flx-mode +1)
(setq helm-flx-for-helm-find-files t ;; t by default
      helm-flx-for-helm-locate t) ;; nil by default
;; swiper
(global-set-key (kbd "C-s") 'swiper)

;; remove prelude-whitespace
(setq prelude-whitespace nil)
(add-hook 'prog-mode-hook 'whitespace-mode)

;; language/dictionary
(setenv "DICTIONARY" "en_GB-ise-w_accents")

# for getting toprule and bottomrule

(setq org-latex-caption-above '(image table special-block))

'(org-koma-letter-prefer-subject t)
'(org-latex-default-table-environment "tabularx")
'(ebib-bibtex-dialect (quote biblatex))
