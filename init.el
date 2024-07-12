(cond
 ((equal system-name "myarch")
  (load "~/.emacs.d/chromearch.el"))
 ((equal system-name "nori-MS-7982")
  (load "~/.emacs.d/home.el"))
 ((equal system-name "MU386U")
  (load "C:/Users/ELPAR/AppData/Roaming/.emacs.d/work.el")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(mixed-pitch vertico ox-hugo orderless marginalia magit languagetool embark ef-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
