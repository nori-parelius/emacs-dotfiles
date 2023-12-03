(let ((deft-dir '"~/Documents/TheNotes/")
      (agenda-files '("~/Documents/TheNotes/20230228174603-stream.org" "~/Documents/TheNotes/output"))
      (roam-dir '"~/Documents/TheNotes/")
      (bib-file '"/home/nori/Documents/TheNotes/biblio.bib"))
;; Enables basic packaging support
(require 'package)

;; MELPA
;; Adds the Melpa archive to the list of available repositories
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/")
	     '("gnu" . "https://elpa.gnu.org/packages/")
	     )

;; Initializes the package infrastructure
(package-initialize)

;; USE-PACKAGE
;; If we don't have the use-package package, we need to refresh contents and install it. The rest will be installed with it
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
;; MAGIT
(use-package magit
  :ensure t)
;; deft needs deft-dir
;; DEFT
(use-package deft
	     :ensure t
	     :custom
	     ((deft-directory deft-dir)
	      (deft-extensions '("org" "txt" "tex" "md"))
	      (deft-recursive t)
	      )
	     :config
	     (global-set-key [f8] 'deft)
	     )

(defun nori/deft-parse-title (file contents)
    "Parse the given FILE and CONTENTS and determine the title.
  If `deft-use-filename-as-title' is nil, the title is taken to
  be the first non-empty line of the FILE.  Else the base name of the FILE is
  used as title."
      (let ((begin (string-match "^#\\+[tT][iI][tT][lL][eE]: .*$" contents)))
	(if begin
	    (string-trim (substring contents begin (match-end 0)) "#\\+[tT][iI][tT][lL][eE]: *" "[\n\t ]+")
	  (deft-base-filename file))))
  
(advice-add 'deft-parse-title :override #'nori/deft-parse-title)
  
(setq deft-strip-summary-regexp
      (concat "\\("
	      "[\n\t]" ;; blank
	      "\\|^#\\+[[:alpha:]_]+:.*$" ;; org-mode metadata
	      "\\|^:PROPERTIES:\n\\(.+\n\\)+:END:\n"
	      "\\)"))
;; org-agenda needs agenda-files
;; ORG AGENDA
;;(setq org-agenda-files (list "~/Documents/TheNotes/20230228174603-stream.org"
;;			     "~/Documents/TheNotes/output"))
(setq org-agenda-files agenda-files)
(setq org-log-done 'time)
(global-set-key (kbd "C-c a t") 'org-todo-list)
;; ORG-BABEL
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (python . t)
   (shell . t)
   (emacs-lisp . t)))


;; to syntax highlight code in babel and to remove the "Do you want to execute?" question
(setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-src-preserve-indentation t
      )
(setq python-indent-guess-indent-offset nil)
(setq python-indent-offset 4)
;; VERTICO
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :config
(setq enable-recursive-minibuffers t))
;; ORDERLESS
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))
;; MARGINALIA
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))
;; EMBARK
(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act))         ;; pick some comfortable binding
  )
;; org-roam needs roam-dir
;; ORG-ROAM
(use-package org-roam
	     :ensure t
	     :custom
	     (org-roam-directory roam-dir)
	     (org-roam-completion-everywhere t)
	     (org-roam-capture-templates
	      '(("d" "default" plain
		 "%?"
		 :target
		 (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
		:unnarrowed t)
		("z" "zettel" plain
		"%?"
		:target
		(file+head "zettel/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
		:unnarrowed t)
	       	("o" "output" plain
		"%?"
		:target
		(file+head "output/%<%Y%m%d%H%M%S>-O-${slug}.org" "#+title: O-${title}\n#+filetags: :output")
		:unnarrowed t)
  	        ("i" "input" plain
		"%?"
		:target
		(file+head "input/%<%Y%m%d%H%M%S>-I-${slug}.org" "#+title: I-${title}\n#+filetags: :input")
		:unnarrowed t)
	        ("r" "reference" plain
		"%?"
		:target
		(file+head "%(expand-file-name \"input\" org-roam-directory)/%<%Y%m%d%H%M%S>-I-${citekey}.org" "#+title: I-${citekey}\n#+filetags: :input")
		:unnarrowed t)
		)
	      )
	     :bind (("C-c n l" . org-roam-buffer-toggle)
		    ("C-c n f" . org-roam-node-find)
		    ("C-c n i" . org-roam-node-insert)
		    ("C-M-i" . completion-at-point))
	     :config
	     (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:20}" 'face 'org-tag)))
	     (org-roam-db-autosync-mode t)

	     )
;; citar needs bib-file
;; CITAR
(use-package citar
  :ensure t
  :custom
  (citar-bibliography '(bib-file))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  :bind
  (:map org-mode-map :package org ("C-c b" . #'org-cite-insert))
  )
;; CITAR-ORG-ROAM
(use-package citar-org-roam
  :ensure t
  :after citar org-roam
  :no-require t
  :config
  (setq citar-org-roam-capture-template-key "r")
  )
(use-package citar-embark
  :ensure t
  :after citar embark
  :no-require
  :config (citar-embark-mode))
;;;; ORG-ROAM-UI
;;(use-package org-roam-ui
;;  :ensure t
;;  :after org-roam)
;;(require 'org-roam-ui)
;;;; OX-HUGO
;;(use-package ox-hugo
;;  :ensure t
;;  :pin melpa
;;  :after ox
;;  )
;;
;; THEME
(require 'modus-themes)

(load-theme 'modus-operandi t)

(define-key global-map (kbd "<f5>") #'modus-themes-toggle)
;; EXWM

;; Disable menu-bar, tool-bar and scroll-bar to increase the usable space.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; Also shrink fringes to 1 pixel.
(fringe-mode 1)

;; Turn on `display-time-mode' if you don't use an external bar.
(setq display-time-default-load-average nil)
(display-time-mode t)

;;;; Below are configurations for EXWM.

;; Load EXWM.
(require 'exwm)

;; System tray
(require 'exwm-systemtray)
(exwm-systemtray-enable)
;;(setq exwm-systemtray-height 30)

;; Set the initial number of workspaces (they can also be created later).
(setq exwm-workspace-number 4)

;; All buffers created in EXWM mode are named "*EXWM*". You may want to
;; change it in `exwm-update-class-hook' and `exwm-update-title-hook', which
;; are run when a new X window class name or title is available.  Here's
;; some advice on this topic:
;; + Always use `exwm-workspace-rename-buffer` to avoid naming conflict.
;; + For applications with multiple windows (e.g. GIMP), the class names of
;    all windows are probably the same.  Using window titles for them makes
;;   more sense.
;; In the following example, we use class names for all windows except for
;; Java applications and GIMP.
(add-hook 'exwm-update-class-hook
          (lambda ()
            (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-class-name))))
(add-hook 'exwm-update-title-hook
          (lambda ()
            (when (or (not exwm-instance-name)
                      (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                      (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-title))))

;; Global keybindings can be defined with `exwm-input-global-keys'.
;; Here are a few examples:
(setq exwm-input-global-keys
      `(
        ;; Bind "s-r" to exit char-mode and fullscreen mode.
        (,(kbd "C-c R") . exwm-reset)
	;; Bind "C-c C-k" to enter char-mode
	(,(kbd "C-c C-k") . exwm-input-release-keyboard)
        ;; Bind "s-&" to launch applications ('M-&' also works if the output
        ;; buffer does not bother you).
        (,(kbd "C-c &") . (lambda (command)
		     (interactive (list (read-shell-command "$ ")))
		     (start-process-shell-command command nil command)))
	))

;; The following example demonstrates how to use simulation keys to mimic
;; the behavior of Emacs.  The value of `exwm-input-simulation-keys` is a
;; list of cons cells (SRC . DEST), where SRC is the key sequence you press
;; and DEST is what EXWM actually sends to application.  Note that both SRC
;; and DEST should be key sequences (vector or string).
(setq exwm-input-simulation-keys
      '(
        ;; movement
        ([?\C-b] . [left])
        ([?\M-b] . [C-left])
        ([?\C-f] . [right])
        ([?\M-f] . [C-right])
        ([?\C-p] . [up])
        ([?\C-n] . [down])
        ([?\C-a] . [home])
        ([?\C-e] . [end])
        ([?\M-v] . [prior])
        ([?\C-v] . [next])
        ([?\C-d] . [delete])
        ([?\C-k] . [S-end delete])
        ;; cut/paste.
        ([?\C-w] . [?\C-x])
        ([?\M-w] . [?\C-c])
        ([?\C-y] . [?\C-v])
        ;; search
        ([?\C-s] . [?\C-f])))

;; Do not forget to enable EXWM. It will start by itself when things are
;; ready.  You can put it _anywhere_ in your configuration.
(exwm-enable)


(start-process-shell-command "cbatticon" nil "cbatticon")

;; ===================================
;; Basic Customization
;; ===================================
;; Start citar-org-roam
(citar-org-roam-mode t)

;; Set org-image width to nil, so it can be set manually
(setq org-image-actual-width nil)

;; Increase size of LaTeX fragment previews
(plist-put org-format-latex-options :scale 2)

;; Enable word wrap
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; Set visible bell instead of sound
(setq visible-bell 1)

;; Auto save buffer if idled for 2 seconds.
(setq auto-save-timeout 2)
(auto-save-visited-mode +1)

;; Watch and reload the file changed on the disk.
(global-auto-revert-mode +1)
(setq auto-revert-remote-files t)

;; Delete the selected text first before editing.
(delete-selection-mode +1)

;; Disable splash screen
(setq inhibit-startup-message t)

;; Smooth Scrolling
(setq scroll-conservatively 10000
      scroll-step 1)

;; MIXED-PITCH
(use-package mixed-pitch
  :ensure t
  :hook
  (text-mode . mixed-pitch-mode)
  ;;:config
  ;;(set-face-attribute 'default nil :font "DejaVu Sans Mono" :height 130)
  ;;(set-face-attribute 'fixed-pitch nil :font "DejaVu Sans Mono")
  ;;(set-face-attribute 'variable-pitch nil :font "DejaVu Sans")
  )

;; User-Defined init.el ends here
)
