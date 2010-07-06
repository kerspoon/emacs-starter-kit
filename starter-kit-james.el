;;; starter-kit-james.el --- kerspoon
;;
;; Part of the Emacs Starter Kit


;;; ---------------------------------------------------------------------------

;; http://stackoverflow.com/questions/92971/how-do-i-set-the-size-of-emacs-window
;; http://www.gnu.org/software/emacs/windows/old/faq4.html

;; (set-frame-width (selected-frame) 80)
;; (set-frame-height (selected-frame) 60))
;; (set-frame-position (selected-frame) 10 30)

(defun set-frame-size-according-to-resolution ()
  ;; use 170 char wide window for largeish displays
  ;; and smaller 85 column windows for smaller displays
  ;; pick whatever numbers make sense for you
        
  ;; for the height, subtract a bit
  ;; from the screen height (for panels, menubars and
  ;; whatnot), then divide by the height of a char to
  ;; get the height we want
  (interactive)
  (if window-system
      (let ((vtop  '(top .  1))
            (vleft '(left . 1))
            (vwidth (if (> (x-display-pixel-width) 1280)
                        (cons 'width 170)
                      (cons 'width 85)))
            (vheight (cons
                      'height
                      (/ (- (x-display-pixel-height) 80)
                         (frame-char-height)))))
        (setq default-frame-alist (list vtop vleft vwidth vheight)))))

(set-frame-size-according-to-resolution)

(defun size-80-full ()
  (interactive)
  (progn
    (set-frame-width (selected-frame) 85)
    (set-frame-height (selected-frame) (/ (- (x-display-pixel-height) 80)
                         (frame-char-height)))
    (set-frame-position (selected-frame) 0 0)))
  
(defun size-160-full ()
  (interactive)
  (progn
    (set-frame-width (selected-frame) 170)
    (set-frame-height (selected-frame) (/ (- (x-display-pixel-height) 80)
                         (frame-char-height)))
    (set-frame-position (selected-frame) 0 0)))

(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

;;; ---------------------------------------------------------------------------

;; Misc
(transient-mark-mode 1) ;; highlight region 
(cua-mode t) ;; various awesomeness
(global-visual-line-mode 1) ;; decent line/word wrapping
(column-number-mode t) ;; I want to know what column I am on
(setq mouse-drag-copy-region nil) ;; I don't want Emacs to copy the region
(delete-selection-mode 1) ; turn on text selection highlighting and
                                        ; make typing override
                                        ; selected text (Note:
                                        ; when delete-selection-mode
                                        ; is on, then
                                        ; transient-mark-mode is
                                        ; automatically on
                                        ; too.)

;; all auto-save go in one place 
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))

;; Line numbering
(require 'linum)
(setq linum-format "%d ")
(global-linum-mode 1)

;; Quick browser searching
(require 'browse-apropos-url)

;; Movement
(defun move-line-toggle ()
  "toggle between start and end of line"
  (interactive)
  (if (= (line-beginning-position) (point))
      (end-of-line)
    (beginning-of-line)))

(defun move-buffer-toggle ()
  "toggle between start and end of buffer"
  (interactive)
  (if (= 1 (point))
      (end-of-buffer)
    (beginning-of-buffer)))

;;; ---------------------------------------------------------------------------

;; Snippets
;; (require 'yasnippet)
;; (yas/initialize)
;; (yas/load-directory "/usr/share/emacs/site-lisp/yasnippet/snippets")

;; Python refactoring
;; (require 'pymacs)

;; Python code checking via flymake
;; (setq pycodechecker "pyflakes")
;; (when (load "flymake" t)
;;   (defun flymake-pycodecheck-init ()
;;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                        'flymake-create-temp-inplace))
;;            (local-file (file-relative-name
;;                         temp-file
;;                         (file-name-directory buffer-file-name))))
;;       (list pycodechecker (list local-file))))
;;   (add-to-list 'flymake-allowed-file-name-masks 
;;                '("\\.py\\'" flymake-pycodecheck-init)))
;; (add-hook 'python-mode-hook (lambda () (flymake-mode 1)))

;; Complete as you type
;; (require 'auto-complete)
;; (require 'auto-complete-config)
;; (global-auto-complete-mode t)
;; (ac-python-keywords-initialize)
;; (ac-c++-keywords-initialize)


;; Python Lookup
;; (require 'pylookup)
;; (setq pylookup-dir "~/.emacs.d/elpa-to-submit/pylookup")
;; (setq pylookup-program (concat pylookup-dir "/pylookup.py"))
;; (setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))

;;; --------------------------------------------------------------------------

;; Single line/word cursor movement
(global-set-key (kbd "M-j") 'backward-word)
(global-set-key (kbd "M-l") 'forward-word)
(global-set-key (kbd "M-i") 'previous-line)
(global-set-key (kbd "M-k") 'next-line)

;; Move by char
(global-set-key (kbd "M-J") 'backward-char) ; para-edit join sexp 
(global-set-key (kbd "M-L") 'forward-char) ; was (prefix)

;; Move by paragraph
(global-set-key (kbd "M-u") 'backward-paragraph)
(global-set-key (kbd "M-o") 'forward-paragraph)

;; Move to beginning/ending of line
(global-set-key (kbd "M-h") 'move-line-toggle)
(global-set-key (kbd "M-H") 'move-line-toggle)

;; Move by screen (page up/down)
(global-set-key (kbd "M-I") 'scroll-down)
(global-set-key (kbd "M-K") 'scroll-up)

;; Move to beginning/ending of file
(global-set-key (kbd "M-J") 'move-buffer-toggle)
(global-set-key (kbd "M-L") 'move-buffer-toggle)

;; isearch
(global-set-key (kbd "M-;") 'isearch-forward)
(global-set-key (kbd "M-:") 'isearch-backward)

(global-set-key (kbd "M-p") 'recenter)

(defun kill-arrows ()
  (progn
    (global-set-key (kbd "<down>") nil)
    (global-set-key (kbd "<left>") nil)
    (global-set-key (kbd "<up>") nil)
    (global-set-key (kbd "<right>") nil)
    (global-set-key (kbd "<insert>") nil)
    (global-set-key (kbd "<home>") nil)
    (global-set-key (kbd "<next>") nil)
    (global-set-key (kbd "<delete>") nil)
    (global-set-key (kbd "<end>") nil)
    (global-set-key (kbd "<prior>") nil)))

;;; --------------------------------------------------------------------------

;; (global-set-key "\C-ch" 'pylookup-lookup)

(global-set-key [f5]  'browse-apropos-url)
(global-set-key [f6]  'comment-or-uncomment-region)
(global-set-key [f7]  'quick-calc)
(global-set-key [f8]  'eshell)

(global-set-key [f11] 'fullscreen)

(global-set-key [C-tab] 'other-window)

;;; --------------------------------------------------------------------------

(provide 'starter-kit-james)
;;; starter-kit-james.el ends here
