;; Time-stamp: <2018-01-05 21:10:39 kmodi>
;; https://web.archive.org/web/20160815085513/http://orgmode.org/worg/sources/emacs.el
;; (setq custom-file "~/.emacs-custom.el")
;; (load custom-file)

(defconst orgmode-org-base "~/temp/orgmode.org/"
  "Path to Orgmode.org base directory.")

(defconst orgweb-base (concat orgmode-org-base "orgweb/")
  "Path to Orgweb source directory.")

(defconst orgweb-htmlroot (concat temporary-file-directory (getenv "USER") "/orgmode.org/")
  "Path where the Org site should be exported.")

(defconst worg-org-mode-lisp-path "~/e/elisp/org-mode/lisp/"
  "Path to Org Lisp files.")

(defconst worg-org-mode-contrib-lisp-path "~/e/elisp/org-mode/contrib/lisp/"
  "Path to Org Contrib Lisp files.")

(defconst worg-other-lisp-path "/home/kmodi/e/misc/worg/elisp/"
  "Path to other Lisp files (like `htmlize') necessary to build Worg.")

(defconst worg-base "~/e/misc/worg/"
  "Path to Worg source directory.")

(defconst worg-htmlroot (concat orgweb-htmlroot "worg/")
  "Path where the Worg site should be exported.")

(add-to-list 'load-path worg-org-mode-lisp-path)
(add-to-list 'load-path worg-org-mode-contrib-lisp-path)
(add-to-list 'load-path worg-other-lisp-path)

(show-paren-mode 1)
(menu-bar-mode 0)
(set-face-foreground 'font-lock-keyword-face "DeepSkyBlue1")
(set-face-foreground 'font-lock-string-face "Goldenrod")

(require 'org)
(require 'org-id)
(require 'htmlize)
;; to have things work correctly in batch-mode
(require 'font-lock)
(require 'cc-mode)
(require 'ox-org)
(c-after-font-lock-init)

(setq make-backup-files nil
      vc-handled-backends nil)

(setq org-export-default-language "en"
      org-export-html-extension "html"
      org-export-with-timestamps nil
      org-export-with-section-numbers nil
      org-export-with-tags 'not-in-toc
      org-export-skip-text-before-1st-heading nil
      org-export-with-sub-superscripts '{}
      org-export-with-LaTeX-fragments t
      org-export-with-archived-trees nil
      org-export-highlight-first-table-line t
      org-export-latex-listings-w-names nil
      org-html-head-include-default-style nil
      org-html-head ""
      org-export-htmlize-output-type 'css
      org-startup-folded nil
      org-export-allow-BIND t
      org-publish-list-skipped-files t
      org-publish-use-timestamps-flag t
      org-export-babel-evaluate nil
      org-confirm-babel-evaluate nil)

(eval-after-load "org-html"
  '(setq org-html-scripts
	 (concat org-html-scripts "\n"
		 "<script type=\"text/javascript\">
    function rpl(expr,a,b) {
      var i=0
      while (i!=-1) {
         i=expr.indexOf(a,i);
         if (i>=0) {
            expr=expr.substring(0,i)+b+expr.substring(i+a.length);
            i+=b.length;
         }
      }
      return expr
    }

    function show_org_source(){
       document.location.href = rpl(document.location.href,\"html\",\"org.html\");
    }
</script>
")))

;; re-export everything regardless of whether or not it's been modified
;; (setq org-publish-use-timestamps-flag nil)

(setq worg-base-directory worg-base)
(setq worg-base-style-directory (concat worg-base "style/"))
(setq worg-base-code-directory (concat worg-base "code/"))
(setq worg-base-color-themes-directory (concat worg-base "color-themes/"))
(setq worg-base-images-directory (concat worg-base "images/"))
(setq worg-publish-directory worg-htmlroot)
(setq worg-publish-style-directory (concat worg-htmlroot "style/"))

(defun set-org-publish-project-alist ()
  "Set publishing projects for Orgweb and Worg."
  (interactive)
  (setq org-publish-project-alist
	`(("worg" :components ("worg-org-faq" "worg-pages" "worg-code" ;; "worg-color-themes"
			       "worg-images" "worg-sources" "worg-extra" "worg-bibtex"))
	  ("orgweb" :components ("orgwebpages" "orgweb-extra"))
	  ("worg-org-faq"
	   :base-directory ,worg-base-directory
	   :base-extension "dummy"
	   :include ("org-faq.org")
	   :html-extension "html"
	   :publishing-directory ,worg-publish-directory
	   :publishing-function (org-html-publish-to-html)
	   :section-numbers nil
	   :table-of-contents nil
	   :html-head "<link rel=\"stylesheet\" title=\"Standard\" href=\"/worg/style/worg.css\" type=\"text/css\" />
<link rel=\"alternate stylesheet\" title=\"Zenburn\" href=\"/worg/style/worg-zenburn.css\" type=\"text/css\" />
<link rel=\"alternate stylesheet\" title=\"Classic\" href=\"/worg/style/worg-classic.css\" type=\"text/css\" />
<link rel=\"SHORTCUT ICON\" href=\"/org-mode-unicorn.ico\" type=\"image/x-icon\" />
<link rel=\"icon\" href=\"/org-mode-unicorn.ico\" type=\"image/ico\" />"
	   :recursive t
	   :html-preamble ,(with-temp-buffer (insert-file-contents (expand-file-name "preamble.html" worg-base)) (buffer-string))
	   :html-postamble "<div id=\"show_source\"><input type=\"button\" value=\"Show Org source\" onClick='show_org_source()'></div><div id=\"license\"><p>Documentation from the https://orgmode.org/worg/ website (either in its HTML format or in its Org format) is licensed under the <a href=\"http://www.gnu.org/copyleft/fdl.html\">GNU Free Documentation License version 1.3</a> or later.  The code examples and css stylesheets are licensed under the <a href=\"http://www.gnu.org/licenses/gpl.html\">GNU General Public License v3</a> or later.</p></div>")
	  ("worg-pages"
	   :base-directory ,worg-base-directory
	   :base-extension "org"
	   :exclude "FIXME"
	   :makeindex t
	   :auto-sitemap nil
	   :sitemap-ignore-case t
	   :html-extension "html"
	   :publishing-directory ,worg-publish-directory
	   :publishing-function (org-html-publish-to-html org-org-publish-to-org)
	   :htmlized-source t
	   :section-numbers nil
	   :table-of-contents nil
	   :html-head "<link rel=\"stylesheet\" title=\"Standard\" href=\"/worg/style/worg.css\" type=\"text/css\" />
<link rel=\"alternate stylesheet\" title=\"Zenburn\" href=\"/worg/style/worg-zenburn.css\" type=\"text/css\" />
<link rel=\"alternate stylesheet\" title=\"Classic\" href=\"/worg/style/worg-classic.css\" type=\"text/css\" />
<link rel=\"SHORTCUT ICON\" href=\"/org-mode-unicorn.ico\" type=\"image/x-icon\" />
<link rel=\"icon\" href=\"/org-mode-unicorn.ico\" type=\"image/ico\" />"
	   :recursive t
	   :html-preamble ,(with-temp-buffer (insert-file-contents (expand-file-name "preamble.html" worg-base)) (buffer-string))
	   :html-postamble "<div id=\"show_source\"><input type=\"button\" value=\"Show Org source\" onClick='show_org_source()'></div><div id=\"license\"><p>Documentation from the https://orgmode.org/worg/ website (either in its HTML format or in its Org format) is licensed under the <a href=\"http://www.gnu.org/copyleft/fdl.html\">GNU Free Documentation License version 1.3</a> or later.  The code examples and css stylesheets are licensed under the <a href=\"http://www.gnu.org/licenses/gpl.html\">GNU General Public License v3</a> or later.</p></div>")
	  ("worg-code"
	   :base-directory ,worg-base-code-directory
	   :base-extension "html\\|css\\|png\\|js\\|bz2\\|el\\|sty\\|awk\\|pl"
	   :html-extension "html"
	   :publishing-directory ,(concat worg-htmlroot "code/")
	   :recursive t
	   :publishing-function org-publish-attachment)
	  ("worg-sources"
	   :base-directory ,worg-base-directory
	   :base-extension "org"
	   :publishing-directory ,(concat worg-htmlroot "sources/")
	   :recursive t
	   :publishing-function org-publish-attachment)
	  ("worg-images"
	   :base-directory ,worg-base-directory
	   :base-extension "png\\|jpg\\|gif\\|pdf\\|csv\\|css\\|tex"
	   :publishing-directory ,worg-publish-directory
	   :recursive t
	   :publishing-function org-publish-attachment)
	  ("worg-extra"
	   :base-directory ,worg-base-style-directory
	   :base-extension "css"
	   :publishing-directory ,worg-publish-style-directory
	   :publishing-function org-publish-attachment)
	  ("worg-bibtex"
	   :base-directory ,(concat worg-base "org-contrib/bibtex/")
	   :base-extension "bib"
	   :publishing-directory ,(concat worg-htmlroot "org-contrib/bibtex/")
	   :recursive nil
	   :publishing-function org-publish-attachment)
	  ("orgwebpages"
	   :base-directory ,orgweb-base
	   :base-extension "org"
	   :html-extension "html"
	   :publishing-directory ,orgweb-htmlroot
	   :publishing-function (org-html-publish-to-html)
	   :auto-sitemap nil
	   :section-numbers nil
	   :table-of-contents t
	   :html-head "<link rel=\"SHORTCUT ICON\" href=\"https://orgmode.org/org-mode-unicorn.ico\" type=\"image/x-icon\" />
<link rel=\"icon\" href=\"https://orgmode.org//org-mode-unicorn.ico\" type=\"image/ico\" />
<link rel=\"publisher\" href=\"https://plus.google.com/102778904320752967064\" />"
	   :html-preamble ,(with-temp-buffer (insert-file-contents (expand-file-name "preamble.html" worg-base)) (buffer-string))
	   :html-postamble nil
	   :exclude "DS_Store"
	   :recursive t)
	  ("orgweb-extra"
	   :base-directory ,orgweb-base
	   :base-extension "css\\|html\\|png\\|jpg\\|js"
	   :publishing-directory ,orgweb-htmlroot
	   :publishing-function org-publish-attachment
	   :recursive t))))

(set-org-publish-project-alist)

(defun worg-fix-symbol-table ()
  (when (string-match "org-symbols\\.html" buffer-file-name)
    (goto-char (point-min))
    (while (re-search-forward "<td>&amp;\\([^<;]+;\\)" nil t)
      (replace-match (concat "<td>&" (match-string 1)) t t))))

(defun publish-worg nil
  "Publish Worg."
  (interactive)
  (add-hook 'org-publish-after-export-hook 'worg-fix-symbol-table)
  (let ((org-format-latex-signal-error nil)
	(worg-base-directory worg-base)
	(worg-base-code-directory (concat worg-base "code/"))
	(worg-base-color-themes-directory (concat worg-base "color-themes/"))
	(worg-base-images-directory (concat worg-base "images/"))
	(worg-publish-directory worg-htmlroot))
    (set-org-publish-project-alist)
    (message "Emacs %s" emacs-version)
    (org-version)
    (org-publish-project "worg")))

(defun publish-orgweb nil
  "Publish Org web pages."
  (interactive)
  (add-hook 'org-publish-after-export-hook 'worg-fix-symbol-table)
  (let ((org-format-latex-signal-error nil)
	(org-export-with-sub-superscripts nil))
    (set-org-publish-project-alist)
    (org-publish-project "orgweb")))

(defun parse-org-quotes ()
  "Create org-quotes.js from org-quotes.org."
  (interactive)
  (load (concat worg-base "code/elisp/worg-fortune.el"))
  (worg-write-fortune-file
   (concat worg-base "org-quotes.org")
   (concat orgmode-org-base "org-quotes.js")
   120
   "r_text[%d] = \"%s\";" "\n"
   'worg-fortune-insert-javascript-pre
   'worg-fortune-insert-javascript-post))
