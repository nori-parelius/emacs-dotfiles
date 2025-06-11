  (cond
   ((equal system-name "NoriPCdebian")
    (load "~/.emacs.d/home.el"))
   ((equal system-name "debchrome")
    (load "~/.emacs.d/chrome.el"))
   ((equal system-type 'windows-nt)
    (load "C:/Users/ELPAR/AppData/Roaming/.emacs.d/work.el"))
   (t (load "~/.emacs.d/server.el"))
   )
