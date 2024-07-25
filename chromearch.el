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
        (,(kbd "C-c y") . (lambda (command)
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

(defun nori-none ()
  ;;Nothing
  )

(defun nori-wait-for-magit-processes ()
  "Wait for all active Magit processes to complete."
  (while (seq-some (lambda (proc)
                     (and (process-live-p proc)
                          (string-match-p "magit" (process-name proc))))
                   (process-list))
    (sleep-for 1)))

(defun nori-wait-for-magit-processes2 ()
  "Wait for all active Magit processes to complete."
  (let ((running t))
    (while running
      (setq running nil)
      (dolist (proc (process-list))
        (when (and (process-live-p proc)
                   (string-match-p "magit" (process-name proc)))
          (setq running t)))
      (sleep-for 1))))

(defun nori-close-all-magit-buffers ()
  "Close all Magit buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (when (and (buffer-live-p buffer)
               (with-current-buffer buffer
                 (derived-mode-p 'magit-mode)))
      (message "Killing Magit buffer: %s" (buffer-name buffer))
      (kill-buffer buffer))))

(defun nori-close-all-magit-processes-and-buffers ()
  "Wait for all Magit processes to complete and then close all Magit buffers."
  (nori-wait-for-magit-processes)
  (nori-close-all-magit-buffers))


(defun nori-magit-pull-if-no-unstaged-changes (directory)
  "Perform a git pull in the specified DIRECTORY if there are no unstaged changes."
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
    (progn
      (nori-magit-pull-directories directories)
      (nori-close-all-magit-processes-and-buffers))))


(defun nori-magit-push-with-date (directory)
  "Perform a git commit with the day's date on a specified directory and push it upstream."
  (interactive "DDirectory: ")
  (progn
    (magit--with-safe-default-directory directory
      (message "Checking if %s is a git repo" directory)
      (if (not (magit-git-repo-p directory))
	  (message "Not a git repository: %s" directory)
	(progn
	  ;; Update magit
          (magit-refresh)  ;; Refresh the status to make sure we catch everything
          ;; Stage all changes
          (magit-stage-modified 'all)  ;; Stages all modified files, including deleted. With all flag also stages new files.
          ;; Commit
          (if (magit-anything-staged-p)
              (let ((commit-message (current-time-string)))
                (magit-commit-create `("-m" ,commit-message))))
          ;; Push
	  (magit-push-current-to-upstream nil)
	  (magit-refresh)
	  ))
      )))
        


(defun nori-magit-push-directories (directories)
  "Perform a 'git commit' and 'git push' in each directory in directories if there are unstaged changes."
  (interactive)
  (dolist (directory directories)
    (nori-magit-push-with-date directory)))

(defun nori-magit-push-my-dirs ()
  "Perform a 'git commit' and 'git push' on a list of my directories."
  (interactive)
  (let ((directories '("~/Documents/Notes"
		       "~/Documents/writing"
		       "~/Documents/noriparelius"
		       "~/Documents/CompNotes")))
    (nori-magit-push-directories directories)
    (nori-close-all-magit-processes-and-buffers)))


(add-to-list 'magit-no-confirm 'stage-all-changes) ;; not to be asked to stage all changes, so I can have the next hook
(add-hook 'kill-emacs-hook #'nori-magit-push-my-dirs) ;; to run it on exit
 

(nori-magit-pull-my-dirs)
;; User-Defined init.el ends here
)
