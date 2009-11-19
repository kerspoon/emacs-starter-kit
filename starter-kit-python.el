;;; starter-kit-python.el --- make python auto-complete
;;
;; Part of the Emacs Starter Kit
 
;; Initialize Python
(require 'python)
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
 
;; Initialize Pymacs
;; (add-to-list 'load-path "~/.emacs.d/elpa-to-submit/Pymacs-0.23")

;; (require 'pymacs)
;; (autoload 'pymacs-apply "pymacs")
;; (autoload 'pymacs-call "pymacs")
;; (autoload 'pymacs-eval "pymacs" nil t)
;; (autoload 'pymacs-exec "pymacs" nil t)
;; (autoload 'pymacs-load "pymacs" nil t)
 
;; Initialize Rope
;; (pymacs-load "ropemacs" "rope-")
;; (setq ropemacs-enable-autoimport t)

;; Initialize autocomplete
(require 'auto-complete-python)
;; (ac-ropemacs-init)
 
(add-hook 'python-mode-hook '(lambda ()
                    (auto-complete-mode t)))

(provide 'starter-kit-python)
;;; starter-kit-python.el ends here

