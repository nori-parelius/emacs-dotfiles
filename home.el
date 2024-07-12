(let ((roam-dir '"~/Documents/TheNotes/")
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

;; Set org-image width to nil, so it can be set manually
(setq org-image-actual-width nil)

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
(defun nori-magit-pull-if-no-unstaged-changes (directory)
  "Perform a `git pull` in the specified DIRECTORY if there are no unstaged changes."
  (interactive "DDirectory: ") ;;interactively asks for directory and offers autocomplete
  (magit--with-safe-default-directory directory ;;temporarily change dir
    (message "Checking if %s is a git repository" directory)
    (if (not (magit-git-repo-p directory))
        (message "Not a git repository: %s" directory)
      (let ((has-diff (magit-git-string "diff" "--exit-code"))) ;;save exit code of running git diff into has-diff
        (message "has-diff: %s" has-diff)
        (if has-diff
            (message "There are unstaged changes in %s. Please commit or stash them before pulling." directory)
          (progn ;;to execute multiple expressions and return the last
            (magit-git-string-ng "pull")
            (message "Pulled %s successfully." directory)))))))

(defun nori-magit-pull-directories (directories)
  "Perform a 'git pull' in each directory in directories if there are no unstaged changes."
  (interactive)
  (dolist (directory directories)
    (nori-magit-pull-if-no-unstaged-changes directory)))

(defun nori-magit-pull-my-dirs ()
  "Perform a 'git pull' on a list of my directories."
  (interactive)
  (let ((directories '("~/Documents/writing"
		       "~/Documents/Notes"
		       "~/Documents/noriparelius"
		       "~/Documents/CompNotes"
		       "~/.emacs.d/")))
    (nori-magit-pull-directories directories)))
   

;; Enable line numbers globally
;;(global-linum-mode t) deprecated since Emacs 29 https://emacs.stackexchange.com/questions/78369/what-to-use-instead-of-linum-mode-in-emacs-29
(global-display-line-numbers-mode t)

(nori-magit-pull-my-dirs)
;; User-Defined init.el ends here
)
