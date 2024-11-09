(cond
 ((equal system-name "myarch")
  (load "~/.emacs.d/chromearch.el"))
 ((equal system-name "NoriPCdebian")
  (load "~/.emacs.d/home.el"))
 ((equal system-name "MU386U")
  (load "C:/Users/ELPAR/AppData/Roaming/.emacs.d/work.el")))
