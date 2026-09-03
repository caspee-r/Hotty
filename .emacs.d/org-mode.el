;; Org-Mode

(defun caspeer/org-mode-setup ()
	(org-indent-mode)
	(variable-pitch-mode 1)
	(visual-line-mode 1)
	(setq
	 ;; Edit settings
	 org-auto-align-tags t
	 org-tags-column 0
	 org-tag-alist
	 '(
	   ("@computer" . ?c)
	   ("@home" . ?h)
	   ("@outside" . ?o)
	   )
	 org-catch-invisible-edits 'show-and-error
	 org-special-ctrl-a/e t
	 org-insert-heading-respect-content t
	 org-use-speed-commands t
	 org-return-follows-link t
	 org-outline-path-complete-in-steps nil
	 org-src-fontify-natively t
	 org-src-tab-acts-natively t
	 org-footnote-auto-adjust t
	 org-log-done 'time
	 org-log-into-drawer t
	 org-latex-image-default-scale "1"

	 ;; Org styling, hide markup etc.
	 org-hide-emphasis-markers t
	 org-ellipsis " ▾"
	 org-pretty-entities t
	 ;; Agenda styling
	 org-agenda-tags-column 0
	 org-agenda-block-separator ?─
	 org-agenda-start-with-log-mode t
	 org-agenda-time-grid '((daily today require-timed)
							(800 1000 1200 1400 1600 1800 2000)
							" ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
	 org-agenda-current-time-string
	 "󰩓 now ─────────────────────────────────────────────────"
	 org-agenda-custom-commands
	 '(
	   ("g" "G T D"
		((agenda ""
				 ((org-agenda-span 'day)
				  (org-agenda-skip-function
				   '(org-agenda-skip-entry-if 'deadline))
				  (org-deadline-warning-days 0)))
		 (todo "NEXT"
			   (
				(org-agenda-files (list "~/org/inbox.org" "~/org/projects.org"))
				(org-agenda-todo-list-sublevels t)
				(org-agenda-show-outline-path t)
				(org-agenda-skip-function
				 '(org-agenda-skip-entry-if 'deadline))
				(org-agenda-prefix-format "	 %i %-12:c [%e] %s ")
				(org-agenda-overriding-header "\nActionable Tasks\n")))
		 (agenda nil
				 ((org-agenda-entry-types '(:deadline))
				  (org-agenda-format-date "")
				  (org-deadline-warning-days 7)
				  (org-agenda-skip-function
				   '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
				  (org-agenda-overriding-header "\nDeadlines\n")))
		 (todo "TODO"
			   (
				(org-agenda-files (list "~/org/inbox.org"))
				(org-agenda-prefix-format "	 %?-12t% s")
				(org-agenda-overriding-header "\nInbox\n")))
		 (tags "CLOSED>=\"<today>\""
			   ((org-agenda-overriding-header "\nCompleted today\n")))
		 (tags "calendar"
			   ((org-agenda-overriding-header "\nCalendar\n")))
		 ))
	   )
	 )

	(modify-all-frames-parameters
	 '((right-divider-width . 40)
	   (internal-border-width . 40)))
	(dolist (face '(window-divider
					window-divider-first-pixel
					window-divider-last-pixel))
		(face-spec-reset-face face)
		(set-face-foreground face (face-attribute 'default :background)))
	(set-face-background 'fringe (face-attribute 'default :background))

	)

(defun caspeer/org-font-setup ()

    ;; Set faces for heading levels
    (dolist (face '((org-level-1 . 1.2)
					(org-level-2 . 1.1)
					(org-level-3 . 1.05)
					(org-level-4 . 1.0)
					(org-level-5 . 1.1)
					(org-level-6 . 1.1)
					(org-level-7 . 1.1)
					(org-level-8 . 1.1)))
		(set-face-attribute (car face) nil :font "ETBembo" :weight 'regular :height (cdr face)))

    ;; (set-face-attribute 'org-block nil    :foreground "red" :inherit 'fixed-pitch)
    ;; (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
    ;; (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
    ;; (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
    ;; (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
    ;; (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    ;; (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    ;; (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    ;; (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
										;(set-face-attribute 'line-number nil :inherit 'fixed-pitch)
										;(set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch)
	)



(use-package org-modern
    :ensure t
	:config
	(setq org-modern-star 'replace)
	)

(use-package olivetti-mode
    :hook (org-mode . olivetti-mode)
	:config
	(setq
	 olivetti-body-width 100
	 )
	)

(defvar org-electric-pairs
  '((?/ . ?/)
    (?= . ?=))
  "Electric pairs for org-mode.")
(defun org-add-electric-pairs ()
	"set up local electric pairs for org buffers"
  (setq-local electric-pair-pairs
              (append org-electric-pairs electric-pair-pairs)))
(add-hook 'org-mode-hook #'org-add-electric-pairs)

(defun caspeer/org-capture-inbox ()
	(interactive)
	(org-capture nil "i")
	)

(defun caspeer/video-script-file ()
	(expand-file-name
	 (concat
      (read-string "Script:  ")
      ".org")
	 "~/org/scripts/"))

(defun caspeer/writing-script-file ()
	(expand-file-name
	 (concat
      (read-string "Script:  ")
      ".org")
	 "~/org/writing/"))

(use-package org
	:commands (org-capture org-agenda)
	:init
	(add-hook 'org-mode-hook #'caspeer/org-mode-setup)
	(add-hook 'org-mode-hook #'caspeer/org-font-setup)
	(add-hook 'org-mode-hook #'org-modern-mode)
	(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
	(add-hook 'org-mode-hook #'flyspell-mode)
	(add-hook 'org-capture-mode-hook 'delete-other-windows)

	:bind (("C-c l" . org-store-link)
		   ("C-c c" . org-capture)
		   ("C-c i" . 'caspeer/org-capture-inbox )
		   ("C-M-|" . indent-rigidly)
		   ("C-c !" . org-timestamp)
		   )
	:config
	(global-set-key "\C-cb" 'org-switchb)
	(add-to-list 'org-modules 'org-habit)
	(fringe-mode nil )
	(setq org-capture-templates
		  `(
			("i" "Todo [INBOX] "
			 entry (file "~/org/inbox.org")
			 "* TODO %i%?"
			 )
			("a" "Appointment / Meeting	 "
			 entry (file "~/org/calendar.org")
			 "* TODO Meeting %? %U"
			 )
			("s" "Script Idea"
			 entry (file "~/org/scripts/inbox.org")
			 "* TODO %?"
			 )
			("v" "Video Script" plain
			 (file caspeer/video-script-file)
			 "
#+TITLE:
#+FILETAGS: :youtube:script:
:PROPERTIES:
:ID: %(org-id-new)
:CREATED: %U
:SOURCE: %(when (org-id-get) (format \"id:%s\" (org-id-get)))
:END:

* Hook

%?

* Outline

* Script

* Assets Needed

* CTA
")
			("A" "Article [INBOX] "
			 entry	(file "~/org/writing/inbox.org")
			 "* TODO %i%?"
			 )
			("w" "Writing"
			 entry	(file caspeer/writing-script-file)
			 "
#+title:
#+date: %U
#+desription:
#+category:
#+tags:
%?
"
			 )
			))
	(setq org-todo-keywords
		  '((sequence "TODO(t)" "NEXT(n)" "Waiting(w@/!)" "|" "DONE(d!)"  )
			))
	(setq org-refile-use-outline-path 'file)
	(setq org-refile-targets '(
							   ("~/org/someday.org" :level . 1)
							   ("~/org/projects.org" :maxlevel . 2)
							   ("~/org/calendar.org" :level . 1)
							   ))

	(setq org-agenda-files (list "inbox.org" "calendar.org" "projects.org" "someday.org"))
	;; Configure custom agenda views
	(global-set-key "\C-ca" 'org-agenda)
	;; ORG PUBLISH
	(setq org-publish-project-alist '(
									  (
									   "Articles"
									   :base-directory "~/org/writing/"
									   :publishing-directory "~/org/writing/html/"
									   :publishing-function org-html-publish-to-html
									   :with-toc nil
									   :exclude "\\(inbox\\.org\\|META\\)"
									   )
									  (
									   "Astro Articles"
									   :base-directory "~/org/writing/"
									   :publishing-directory "~/caspee-r.github.io/src/content/articles/"
									   :publishing-function caspeer/org-gfm-publish-to-mdx
									   :with-toc t
									   :exclude "\\(inbox\\.org\\|META\\)"
									   )
									  ("Astro Static"
									   :base-directory "~/org/static/"
									   :base-extension "css\\|js\\|png\\|jpg\\|jpeg\\|gif\\|svg\\|webp\\|pdf"
									   :publishing-directory "~/caspee-r.github.io/public/"
									   :publishing-function org-publish-attachment
									   :recursive t)
									  ("Astro"
									   :components ("Astro Articles" "Astro Static"))
									  ))
	)
;; Save Org buffers after refiling!
(add-hook 'org-after-refile-insert-hook #'org-save-all-org-buffers)

(require 'ox)
(require 'ox-gfm)
(require 'json)
(require 'subr-x)
(use-package ox-gfm
	:after ox
	:config
	(add-to-list
	 'org-export-filter-final-output-functions
	 #'caspeer/ox-gfm-frontmatter)
	)

(defun caspeer/gfm-link (link desc info)
	(let ((path (org-element-property :path link)))
		(when (and (equal (org-element-property :type link) "file")
				   (string-prefix-p "static/" path))
			(org-element-put-property
			 link
			 :path (concat "/" path))))
	;; MDX repurposes <...> entirely for JSX, so GFM's bare
	;; autolink fallback (<https://...>) for description-less
	;; links breaks the MDX parser — it reads "https" as a tag
	;; name, ":" as a JSX namespace separator, then chokes on
	;; the "/" that follows. Force a description so org-md-link
	;; always emits the safe [text](url) form instead. This
	;; also covers links inside footnotes/sidenotes, since those
	;; get exported through this same translator.
	(let ((description
		   (if (org-string-nw-p desc)
				   desc
			   (org-element-property :raw-link link))))
		(org-md-link link description info)))

;; ------------------------------------------------------------
;; MDX <Sidenote> import line.
;;
;; A relative path rather than an alias (e.g. "@/components/..."),
;; on purpose — it works regardless of whether tsconfig.json has
;; a `@` -> `src/` paths alias configured, which is one less
;; thing to get wrong. This assumes articles are published to
;; src/content/articles/*.mdx (two levels below src/) and the
;; component lives at src/components/Sidenote.astro; adjust the
;; path below if either of those ever changes.
;; ------------------------------------------------------------
(defvar caspeer/mdx-sidenote-import
	"import Sidenote from '../../components/Sidenote.astro';\n"
	"Import statement injected at the top of every exported .mdx file.")

;; ------------------------------------------------------------
;; Footnote references -> <Sidenote> components.
;;
;; `org-export-get-footnote-definition' resolves BOTH inline
;; definitions ([fn::text], [fn:label:text]) and references to
;; a definition written elsewhere in the file ([fn:label] on its
;; own, defined later via "[fn:label] text ..."), so this one
;; function handles every footnote form Org supports.
;;
;; Note: if the same label is referenced more than once, this
;; re-renders the full note text at every occurrence rather than
;; a "jump back to note N" link — there's no footnotes section
;; left at the bottom to jump to, since every note is now
;; inlined as its own <Sidenote>.
;; ------------------------------------------------------------
(defun caspeer/gfm-footnote-reference (footnote-reference contents info)
	"Transcode FOOTNOTE-REFERENCE into an MDX <Sidenote> component.
CONTENTS is unused — the footnote's own content is pulled via
`org-export-get-footnote-definition' instead, since that's the
only thing that works uniformly for both inline and standalone
footnote definitions. INFO is the export communication channel."
	(let* ((definition
			(org-export-get-footnote-definition footnote-reference info))
		   (rendered
			(org-trim (org-export-data definition info)))
		   ;; Collapse to a single line: a footnote that expands
		   ;; to multiple markdown blocks (blank-line-separated
		   ;; paragraphs, lists, etc.) would otherwise break out
		   ;; of the inline <Sidenote>...</Sidenote> tag and
		   ;; confuse the MDX parser.
		   (flattened
			(replace-regexp-in-string "[ \t\n\r]+" " " rendered)))
		(format "<Sidenote>%s</Sidenote>" flattened)))

;; ------------------------------------------------------------
;; Suppress the default footnotes-at-the-bottom section.
;;
;; ox-gfm (via ox-md) normally appends a
;;   [^1]: definition text
;; block per footnote at the end of the document. Since every
;; footnote is now inlined as a <Sidenote> at its point of use,
;; that trailing section is no longer needed.
;; ------------------------------------------------------------
(defun caspeer/gfm-footnote-section (_info)
	"Return an empty string instead of the default footnotes list."
	"")

(org-export-define-derived-backend 'caspeer/gfm 'gfm
	:translate-alist '((link . caspeer/gfm-link)
					   (footnote-reference . caspeer/gfm-footnote-reference)
					   (footnote-section . caspeer/gfm-footnote-section)))
;; ------------------------------------------------------------
;; Org keyword helper
;; ------------------------------------------------------------

(defun caspeer/org-keyword-in-buffer (buffer keyword)
	(when (buffer-live-p buffer)
		(with-current-buffer buffer
			(let ((value
				   (cadr (assoc keyword
								(org-collect-keywords (list keyword))))))
				(when value
					(string-trim value))))))


;; ------------------------------------------------------------
;; Convert a heading to a URL-friendly slug.
;; ------------------------------------------------------------

(defun caspeer/org-slugify (title)
	"Turn TITLE into a URL-friendly slug."
	(let ((slug (downcase title)))

		;; Remove Org emphasis markers.
		(setq slug
			  (replace-regexp-in-string
			   "[_*~+=]" ""
			   slug))

		;; Remove punctuation.
		(setq slug
			  (replace-regexp-in-string
			   "[^[:alnum:][:space:]-]"
			   ""
			   slug))

		;; Spaces -> hyphens.
		(setq slug
			  (replace-regexp-in-string
			   "[[:space:]]+"
			   "-"
			   slug))

		;; Collapse multiple hyphens.
		(setq slug
			  (replace-regexp-in-string
			   "-+"
			   "-"
			   slug))

		;; Remove leading/trailing hyphens.
		(string-trim slug "-+" "-+")))


;; ------------------------------------------------------------
;; Extract headings from the Org document.
;; ------------------------------------------------------------

(defun caspeer/org-toc-headings ()
	"Return the headings in the current Org buffer.

Each element is:

  (LEVEL TITLE SLUG)"
	(let ((tree (org-element-parse-buffer))
          (used-slugs (make-hash-table :test #'equal))
          result)

		(org-element-map tree 'headline
			(lambda (headline)

				(let* ((level (org-element-property :level headline))
					   (title (org-element-property :raw-value headline))
					   (base-slug (caspeer/org-slugify title))
					   (count (gethash base-slug used-slugs 0))
					   (slug (if (= count 0)
									 base-slug
								 (format "%s-%d"
										 base-slug
										 count))))

					(puthash base-slug (1+ count) used-slugs)

					(push
					 (list level title slug)
					 result))))

		(nreverse result)))


;; ------------------------------------------------------------
;; Escape a string for YAML.
;;
;; JSON strings are valid YAML double-quoted strings.
;; ------------------------------------------------------------

(defun caspeer/yaml-string (string)
	"Return STRING encoded as a YAML-safe string."
	(json-encode-string (or string "")))


;; ------------------------------------------------------------
;; Generate nested TOC YAML.
;; ------------------------------------------------------------

(defun caspeer/org-toc-tree ()
	"Build a nested TOC tree from the current Org buffer."

	(let ((headings (caspeer/org-toc-headings))
          (root '())
          (stack nil))

		(dolist (heading headings)

			(let* ((level (nth 0 heading))
				   (title (nth 1 heading))
				   (slug (nth 2 heading))
				   (node
					(list
					 :title title
					 :id slug
					 :children nil)))

				;; Find the parent.
				(while (and stack
							(>= (plist-get (car stack) :level)
								level))
					(pop stack))

				(if stack
						(let ((parent (car stack)))
							(push node
								  (plist-get parent :children)))

					;; Top-level heading.
					(push node root))

				;; This heading becomes a possible parent.
				(push
				 (list
				  :level level
				  :node node)
				 stack)))

		;; Reverse children recursively.
		(cl-labels
				((reverse-tree (nodes)
					 (mapcar
					  (lambda (node)
						  (plist-put
						   node
						   :children
						   (reverse-tree
							(reverse
							 (plist-get node :children)))))
					  (reverse nodes))))

			(reverse-tree root))))


(defun caspeer/org-toc-yaml ()
	"Generate nested YAML TOC from the Org heading tree."

	(require 'cl-lib)

	(cl-labels
			((render-node (node indent)

				 (let ((title (plist-get node :title))
					   (id (plist-get node :id))
					   (children (plist-get node :children))
					   (spaces (make-string indent ?\s)))

					 (concat
					  (format "%s- title: %s\n"
							  spaces
							  (caspeer/yaml-string title))

					  (format "%s  id: %s\n"
							  spaces
							  (caspeer/yaml-string id))

					  (if children
							  (concat
							   (format "%s  children:\n" spaces)
							   (mapconcat
								(lambda (child)
									(render-node child (+ indent 4)))
								children
								""))
						  "")))))

		(concat
		 "toc:\n"
		 (mapconcat
		  (lambda (node)
			  (render-node node 2))
		  (caspeer/org-toc-tree)
		  ""))))


;; ------------------------------------------------------------
;; Frontmatter
;; ------------------------------------------------------------

(defun caspeer/ox-gfm-frontmatter (output backend info)
	"Add YAML frontmatter, including the generated TOC, to GFM output."
	(if (not (org-export-derived-backend-p backend 'gfm))
			output

		(let* ((buffer
				(get-buffer (plist-get info :input-buffer)))

			   ;; Title from Org export INFO.
			   (title
				(substring-no-properties
				 (org-export-data
				  (plist-get info :title)
				  info)))

			   ;; Metadata from the Org buffer.
			   (description
				(caspeer/org-keyword-in-buffer
				 buffer "DESCRIPTION"))

			   (date-raw
				(caspeer/org-keyword-in-buffer
				 buffer "DATE"))

			   (category
				(caspeer/org-keyword-in-buffer
				 buffer "CATEGORY"))

			   (tags-raw
				(caspeer/org-keyword-in-buffer
				 buffer "TAGS"))

			   ;; Convert Org timestamp:
			   ;; <2026-08-18 Tue>
			   ;; -> 2026-08-18
			   (date
				(when date-raw
					(format-time-string
					 "%Y-%m-%d"
					 (org-time-string-to-time date-raw))))

			   ;; Convert "#+TAGS: 8086 interrupts hardware"
			   ;; into a list.
			   (tags
				(when tags-raw
					(split-string tags-raw "[ \t]+" t))))

			(concat
			 "---\n"

			 ;; Basic metadata
			 (format "title: %s\n"
					 (json-encode-string title))

			 (format "description: %s\n"
					 (json-encode-string
					  (or description "")))

			 (format "date: %s\n"
					 (or date ""))

			 (format "category: %s\n"
					 (json-encode-string
					  (or category "")))

			 ;; Tags
			 "tags:\n"
			 (if tags
					 (mapconcat
					  (lambda (tag)
						  (format "  - %s"
								  (json-encode-string tag)))
					  tags
					  "\n")
				 "  []")

			 "\n"

			 ;; Generated TOC
			 (caspeer/org-toc-yaml)

			 "\n---\n\n"

			 caspeer/mdx-sidenote-import

			 "\n"

			 output))))



;; ------------------------------------------------------------
;; Publishing function
;; ------------------------------------------------------------

(defun caspeer/org-gfm-publish-to-mdx
		(plist filename pub-dir)
	"Publish an Org file as MDX for Astro, with <Sidenote> footnotes."

	(org-publish-org-to
	 'caspeer/gfm
	 filename
	 ".mdx"
	 plist
	 pub-dir))
