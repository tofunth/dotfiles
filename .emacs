;;----------------------------------------------------------------
;; Package manager
;;----------------------------------------------------------------
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;;----------------------------------------------------------------
;; Theme and font
;;----------------------------------------------------------------
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(custom-enabled-themes (quote (leuven)))
;;  '(package-selected-packages
;;    (quote
;;     (borland-blue-theme evil auctex auto-complete-auctex python-mode fill-column-indicator edit-server auto-complete)))
;;  '(tool-bar-mode nil))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )

(load-theme 'monokai t)

(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-11"))
(set-face-attribute 'default nil :font "DejaVu Sans Mono-11")
;; (set-face-attribute 'default nil :height 90)

;;----------------------------------------------------------------
;; Windows and frames
;;----------------------------------------------------------------
;; don't show menu bar
(menu-bar-mode nil)

;;----------------------------------------------------------------
;; User interface
;;----------------------------------------------------------------
;; display line number
(global-linum-mode t)
;; show current line and column numbers in status bar
(line-number-mode t)
(column-number-mode t)

;; column indicator
(require 'fill-column-indicator)
(define-globalized-minor-mode
  global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode t)

;; show corresponding brackets
(show-paren-mode 1)
;; (setq show-paren-delay 0)

;;----------------------------------------------------------------
;; Some behaviours
;;----------------------------------------------------------------
;; Interatively do things mode: buffer looks better
(require 'ido)
(ido-mode t)

;; auto-complete mode
; (require 'auto-complete)
; (require 'auto-complete-config)
; (global-auto-complete-mode t)

;; always use spaces, not tabs
(setq indent-tabs-mode nil)

;; ignore case when searching
(setq case-fold-search t)

;; require final new lines in files, when saved
(setq require-final-newline t)

;; scroll half-page
(global-set-key (kbd "C-v") 'View-scroll-half-page-forward)
(global-set-key (kbd "M-v") 'View-scroll-half-page-backward)

;;----------------------------------------------------------------
;; python environment setups
;;----------------------------------------------------------------
(require 'python-mode)
;; use IPython
(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "IPython")
;; use the wx backend, for both mayavi and matplotlib
(setq py-python-command-args
  '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
(setq py-force-py-shell-name-p t)

;; switch to the interpreter after executing code
(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p t)
;; don't split windows
(setq py-split-windows-on-execute-p nil)
;; try to automagically figure out indentation
(setq py-smart-indentation t)

;;----------------------------------------------------------------
;; Org-mode settings
;;----------------------------------------------------------------
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;;----------------------------------------------------------------
;; Emacs sessions
;;----------------------------------------------------------------
;; (desktop-save-mode 1)

;;----------------------------------------------------------------
;; Smooth Scrolling
;;----------------------------------------------------------------
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1))) ;; three lines at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;;----------------------------------------------------------------
;; No GNU emacs message
;;----------------------------------------------------------------
(setq inhibit-startup-screen t)
(put 'dired-find-alternate-file 'disabled nil)

;;----------------------------------------------------------------
;; edit in emacs
;;----------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/elpa")
(require 'edit-server)
(edit-server-start)

;;----------------------------------------------------------------
;; AUC latex
;;----------------------------------------------------------------
;; auto-complete
(load "auctex.el" nil t t)
; (load "preview-latex.el" nil t t)
(require 'auto-complete-auctex)
(setq TeX-PDF-mode t)

;;----------------------------------------------------------------
;; Evil mode 
;;----------------------------------------------------------------
(require 'evil)

;;----------------------------------------------------------------
;; Helm
;;----------------------------------------------------------------
(require 'helm-config)
(helm-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
v '(custom-safe-themes
   (quote
    ("5dd70fe6b64f3278d5b9ad3ff8f709b5e15cd153b0377d840c5281c352e8ccce" default)))
 '(package-selected-packages
   (quote
    (helm slime monokai-theme markdown-mode python-mode fill-column-indicator evil edit-server borland-blue-theme auto-complete-auctex auctex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
