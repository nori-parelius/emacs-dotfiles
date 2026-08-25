;; -*- lexical-binding: t; -*-
  (cond
   ((equal system-name "NoriPC")
    (load "~/.emacs.d/home.el"))
   ;; emacs server inside the container has weird names
   (t (load "~/.emacs.d/server.el"))
   )
