;; Enables basic packaging support
(require 'package)
(unless package-archive-contents
  (package-refresh-contents))


;; MELPA
;; Adds the Melpa archive to the list of available repositories
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))


;; Initializes the package infrastructure
(package-initialize)

;; USE-PACKAGE
;; If we don't have the use-package package, we need to refresh contents and install it. The rest will be installed with it
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; ENV
;; Set locale and encoding for better subprocess compatibility
(setenv "LC_TIME" "C")      ; English day/month/month names
(setenv "LC_MESSAGES" "C")  ; English messages from Git/Grep
(setenv "LANG" "C")
(set-language-environment "English")
(prefer-coding-system 'utf-8)

;; NO LITTERING
(use-package no-littering
  :ensure t)

;; PUT ~ elsewhere
;; https://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files/151946#151946
(setq version-control t     ;; Use version numbers for backups.
      kept-new-versions 10  ;; Number of newest versions to keep.
      kept-old-versions 0   ;; Number of oldest versions to keep.
      delete-old-versions t ;; Don't ask to delete excess backup versions.
      backup-by-copying t)  ;; Copy all files, don't rename them.
(setq vc-make-backup-files t) ;; Backup versioned files

;; Default and per-save backups go here:
(setq backup-directory-alist '(("" . "~/.emacs.d/backup/per-save")))
(defun nori-force-backup-of-buffer ()
  ;; Make a special "per session" backup at the first save of each
  ;; emacs session.
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist '(("" . "~/.emacs.d/backup/per-session")))
          (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))

(add-hook 'before-save-hook  'nori-force-backup-of-buffer)
(setq org-capture-templates
      '(("i" "Inbox Entry" entry
	 (file+headline "~/Documents/Notes/0000--entry-point.org" "Inbox")
	 "** %^{Note}\n:CREATED: %U")))      
(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode)
  :bind
  (("C-c n n" . denote)
   ("C-c n r" . denote-rename-file)
   ("C-c n l" . denote-link)
   ("C-c n b" . denote-backlinks)
   ("C-c n d" . denote-dired)
   ("C-c n g" . denote-grep)
   ("C-c n o" . denote-open-or-create)
   ("C-c n c" . denote-link-after-creating)
   )
  :config
  (setq denote-directory (expand-file-name "~/Documents/Notes"))
  (setq denote-known-keywords '("source" "project" "archived" "zk" "attach"))
  (setq denote-excluded-directories-regexp '("OldZK" ".git" ".stfolder" ".obsidian" "BuJo"))
  ;; When creating a note from another, automatically link to it
  ;; (setq denote-link-after-creating t)
  ;; Automatically rename Denote buffers when opening them so that
  ;; instead of their long file name they have, for example, a literal
  ;; "[D]" followed by the file's title.  Read the doc string of
  ;; `denote-rename-buffer-format' for how to modify this.
  ;;(denote-rename-buffer-mode 1)
  )

(use-package denote-org
  :ensure t)
;; MAGIT
(use-package magit
  :ensure t
)
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
;; OX-HUGO
(use-package ox-hugo
  :after (org)
  :ensure t
  :pin melpa
  :after ox
  )
(use-package languagetool
  :ensure t
  :defer t
  :commands (languagetool-check
             languagetool-clear-suggestions
             languagetool-correct-at-point
             languagetool-correct-buffer
             languagetool-set-language
             languagetool-server-mode
             languagetool-server-start
             languagetool-server-stop)
  :config
  (setq languagetool-java-arguments '("-Dfile.encoding=UTF-8")
        languagetool-console-command "~/.languagetool/languagetool-commandline.jar"
        languagetool-server-command "~/.languagetool/languagetool-server.jar"))
;; THEME
(require 'ef-themes)

(load-theme 'ef-autumn :no-confirm)

(setq ef-themes-to-toggle '(ef-autumn ef-cyprus))

(define-key global-map (kbd "<f5>") #'ef-themes-toggle)

;; ===================================
;; Basic Customization
;; ===================================

;; Allow for manual resizing of images in org.
;; Set org-image width to nil, so it can be set manually
(setq org-image-actual-width nil)

;; Enable word wrap
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; Set visible bell instead of sound
(setq visible-bell 1)

;; Auto save buffer if idled for 2 seconds.
;; https://whhone.com/emacs-config/#taking-note-with-org-roam.
(setq auto-save-timeout 2)
(auto-save-visited-mode +1)

;; Watch and reload the file changed on the disk.
(global-auto-revert-mode +1)
(setq auto-revert-remote-files t)

;; Delete the selected text first before editing.
;; https://whhone.com/emacs-config/#taking-note-with-org-roam.
(delete-selection-mode +1)

;; Disable splash screen
(setq inhibit-startup-message t)

;; Smooth Scrolling
;; https://www.emacswiki.org/emacs/SmoothScrolling.
(setq scroll-conservatively 10000
      scroll-step 1)

(use-package markdown-mode
  :ensure t
  )

(use-package darkroom
  :ensure t
  )

;; Line numbers in terminal
(when (display-graphic-p)
  (global-display-line-numbers-mode))

;; MIXED-PITCH
(use-package mixed-pitch
  :ensure t
  :hook
  (text-mode . mixed-pitch-mode)
  :config
  (set-face-attribute 'default nil :font "DejaVu Sans Mono" :height 130)
  (set-face-attribute 'fixed-pitch nil :font "DejaVu Sans Mono")
  (set-face-attribute 'variable-pitch nil :font "Liberation Sans")
  )



(defun nori-autopull ()
  "Run my git autopull Bash script."
  (interactive)
  (shell-command "bash /home/nori/.emacs.d/autopull.sh"))

(defun nori-autopush ()
  "Run my git autocommitpush Bash script."
  (interactive)
  (shell-command "bash /home/nori/.emacs.d/autocommitpush.sh"))

(defun nori-insert-org-list-of-page-links (dir)
  "Insert org-mode list items linking to all .jpg files in DIR.
Assumes filenames are of the form pages_<desc>.jpg and uses <dirname>_<desc> as link text."
  (interactive "DSelect directory: ")
  (let ((dir-name (file-name-nondirectory (directory-file-name dir)))
	(files (directory-files dir nil "\\.jpg$"))) ;; nil is for not FULL path
    (dolist (file files)
      (let* ((desc (string-remove-suffix ".jpg" (string-remove-prefix "pages_" file)))
	     (rel-path (file-relative-name (expand-file-name file dir)
					   default-directory)))
	(insert (format "*** [[./%s][%s_%s]]\n" rel-path dir-name desc))))))


;; Enable line numbers globally
;;(global-linum-mode t) deprecated since Emacs 29 https://emacs.stackexchange.com/questions/78369/what-to-use-instead-of-linum-mode-in-emacs-29
(global-display-line-numbers-mode t)

(add-hook 'kill-emacs-hook #'nori-autopush) ;; to run it on exit
(nori-autopull)
;; User-Defined init.el ends here
