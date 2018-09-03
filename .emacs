;; CUSTOMIZE SOME BUILTIN VARIABLES

;; auto revert mode
(global-auto-revert-mode 1)

;; disable start up page
(setq inhibit-startup-screen t)

;; start emacs & new frame in fullscreen
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; set fonts
(add-to-list 'default-frame-alist '(font . "Monospace-10"))
(set-face-attribute 'default t :font "Monospace-10")

;; use spaces, not tabs
(setq-default indent-tabs-mode nil)

;; set tab width to 4
(setq-default tab-width 4)

;; require saving files ending with new line
(setq-default require-final-newline t)


;; display line number
(global-linum-mode t)

;; save mode
(desktop-save-mode 1)

;; highlight current line
(global-hl-line-mode 1)

;; display current line & column number in the status bar
(line-number-mode t)
(column-number-mode t)

;; highlight columns after 80
(require 'whitespace)
(setq-default whitespace-style '(face lines-tail))
(global-whitespace-mode t)

;; preserve point on scrolling
(setq scroll-preserve-screen-position t)

;; scrolloff by 5 lines
(setq scroll-margin 5)

;; one solution for smooth scrolling on https://www.emacswiki.org/emacs/SmoothScrolling
(setq scroll-step 1
      scroll-conservatively 10000)

;; set html indentation to 2 spaces
(add-hook 'html-mode-hook
        (lambda ()
          ;; Default indentation is usually 2 spaces
          (set (make-local-variable 'sgml-basic-offset) 2)))

;; EXTERNAL PACKAGES
(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))


(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (modify-syntax-entry ?_ "w")
  (setq x-select-enable-clipboard nil)
)

(setq-default evil-vsplit-window-right t)
(setq-default evil-split-window-below t)

(use-package evil-matchit
  :ensure t
  :config
  (global-evil-matchit-mode 1)
)

(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "C-c c g") 'counsel-git)
  (global-set-key (kbd "C-c c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c c C-r") 'ivy-resume)
  )

(use-package swiper
  :ensure t
  )


(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  (setq ivy-use-virtual-buffers t)
 )


(use-package elpy
  :ensure t
  :config
  (elpy-enable)
)

(use-package seoul256-theme
  :ensure t
  :config
  (setq seoul256-background 235
        seoul256-alternate-background 238)
  (load-theme 'seoul256 t)
)


(use-package highlight-indent-guides
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))


(use-package treemacs
  :ensure t
  :config
  (global-set-key [f8] 'treemacs)
)


(use-package treemacs-evil
  :ensure t
)


(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-c g s") 'magit-status)
  (global-set-key (kbd "C-c g b") 'magit-blame)
  (global-set-key (kbd "C-c g k m") 'smerge-keep-mine)
  (global-set-key (kbd "C-c g k o") 'smerge-keep-other)
  (global-set-key (kbd "C-c g k a") 'smerge-keep-all)
)


(use-package evil-magit
  :ensure t
)


(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-indexing-method 'native)
  (setq projectile-enable-caching t)
)


(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode)
)


(use-package ag
  :ensure t
)


(use-package rg
  :ensure t
)


(use-package fzf
  :ensure t
)


(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "<return>") nil)
    (define-key company-active-map (kbd "RET") nil)
    (define-key company-active-map (kbd "M-RET") #'company-complete-selection)
    (define-key company-active-map (kbd "C-w") 'evil-delete-backward-word))
)


(use-package js2-mode
  :ensure t
  :config
  (add-hook 'js-mode-hook 'js2-minor-mode)
  (add-hook 'js2-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
)


(use-package tex
  :ensure auctex
  :config
  (setq reftex-plug-into-AUCTeX t)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
)

;(use-package solarized-theme
;  :ensure t
;  :config
;  (load-theme 'solarized-dark t)
;)

(use-package markdown-mode
  :ensure t
)

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))


(use-package go-eldoc
  :ensure t)


(use-package company-go
  :config (setq company-go-show-annotation t)
  :ensure t)


(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :config
 )


(use-package flymd
  :ensure t
  :config
  (defun my-flymd-browser-function (url)
     (let ((browse-url-browser-function 'browse-url-firefox))
       (browse-url url)))
   (setq flymd-browser-open-function 'my-flymd-browser-function)
 )


(use-package yaml-mode
  :ensure t
  :mode
  ("\\.yml\\'" . yaml-mode)
  ("\\.yml.j2\\'" . yaml-mode)
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (fzf counsel-projectile yaml-mode use-package treemacs-evil sublimity seoul256-theme rg neotree markdown-preview-mode js2-mode highlight-indent-guides go-eldoc flymd evil-tabs evil-org evil-matchit evil-magit elpy company-go auto-dim-other-buffers auctex ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
