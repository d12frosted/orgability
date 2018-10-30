(require 'cl)
(require 'el-mock)
(require 'undercover)
(require 'subr-x)

(setq user-home-directory (concat (getenv "HOME") "/"))

(undercover "*.el")

(add-to-list 'load-path ".")
(load "orgability.el")
