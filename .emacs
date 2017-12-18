;; CUSTOMIZE SOME BUILTIN VARIABLES
;; use spaces, not tabs
(setq-default indent-tabs-mode nil)

;; set tab width to 4
(setq-default tab-width 4)

;; require saving files ending with new line
(setq-default require-final-newline t)


;; ENABLE SOME MODES BY DEFAULT
;; show corresponding brackets
(show-paren-mode 1)

;; display line number
(global-linum-mode t)

;; highlight current line
(global-hl-line-mode 1)

;; display current line & column number in the status bar
(line-number-mode t)
(column-number-mode t)

;; highlight columns after 80
(require 'whitespace)
(setq-default whitespace-style '(face lines-tail))
(global-whitespace-mode t)


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
(setq-default evil-split-window-below-t)


(use-package evil-tabs
  :ensure t
  :config
  (global-evil-tabs-mode t))


(use-package counsel
  :ensure t
  )


(use-package swiper
  :ensure t
  )


(use-package find-file-in-project
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
  (setq seoul256-background 235)
  (load-theme 'seoul256 t))


(use-package highlight-indent-guides
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))


(use-package neotree
  :ensure t
  :config
  (global-set-key [f8] 'neotree-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "R") 'neotree-change-root)
  (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-select-previous-sibling-node)
  (evil-define-key 'normal neotree-mode-map (kbd "L") 'neotree-select-next-sibling-node)
)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (elpy anaconda-mode find-file-in-project flx counsel neotree highlight-indent-guides ivy evil-tabs evil use-package seoul256-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
