  (cond
   ((equal system-name "myarch")
    (load "~/.emacs.d/chromearch.el"))
   ((equal system-name "nori-MS-7982")
    (load "~/.emacs.d/home.el")))
