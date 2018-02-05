;; CUSTOMIZE SOME BUILTIN VARIABLES
;; disable start up page
(setq inhibit-startup-screen t)

;; start emacs & new frame in fullscreen
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; set fonts
(add-to-list 'default-frame-alist '(font . "Ubuntu Mono-13"))
(set-face-attribute 'default t :font "Ubuntu Mono-13")

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
  (evil-mode 1))

(setq-default evil-vsplit-window-right t)
(setq-default evil-split-window-below t)


(use-package counsel
  :ensure t
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
  (add-hook 'python-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
)

(use-package seoul256-theme
  :ensure t
  :config
  (setq seoul256-background 235)
  (load-theme 'seoul256 t))


(use-package highlight-indent-guides
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))


(use-package treemacs
  :ensure t
  :config
  (global-set-key [f8] 'treemacs-toggle)
)


(use-package treemacs-evil
  :ensure t
)


(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status)
)


(use-package evil-magit
  :ensure t
)


(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
)


(use-package counsel-projectile
  :ensure t
)


(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
)


(use-package js2-mode
  :ensure t
  :config
  (add-hook 'js-mode-hook 'js2-minor-mode)
  (add-hook 'js2-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
)


(use-package sublimity
  :ensure t
  :config
  (require 'sublimity)
  (require 'sublimity-scroll)
  (sublimity-mode 1)
)


(use-package tex
  :ensure auctex
  :config
  (setq reftex-plug-into-AUCTeX t)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (use-package treemacs-evil sublimity seoul256-theme neotree js2-mode highlight-indent-guides flx evil-tabs evil-magit elpy counsel-projectile auctex anaconda-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
