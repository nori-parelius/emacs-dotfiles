  (cond
   ((equal system-name "NoriPCdebian")
    (load "~/.emacs.d/home.el"))
   ((equal system-name "debchrome")
    (load "~/.emacs.d/chrome.el"))
   ((equal system-type 'windows-nt)
    (load "C:/Users/ELPAR/AppData/Roaming/.emacs.d/work.el")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
