;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CORE BOOTSTRAP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Vim mode
(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil)
  (setq x-select-enable-clipboard nil)
  (setq save-interprogram-paste-before-kill nil)  ; stop dd from adding to clipboard
  :config
  (evil-mode 1))

(use-package evil-escape
  :ensure t
  :config
  (setq-default evil-escape-key-sequence "jk")
  (setq-default evil-escape-delay 0.2)
  (evil-escape-mode 1)
)

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Better defaults
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(when window-system
  (scroll-bar-mode 0)                             ; Disable the scroll bar
  (tool-bar-mode 0)                               ; Disable the tool bar
  (tooltip-mode 0))                               ; Disable the tooltips

(setq-default
 ad-redefinition-action 'accept                   ; Silence warnings for redefinition
 auto-window-vscroll nil                          ; Lighten vertical scroll
 confirm-kill-emacs 'yes-or-no-p                  ; Confirm before exiting Emacs
 cursor-in-non-selected-windows nil               ; Hide the cursor in inactive windows
 delete-by-moving-to-trash t                      ; Delete files to trash
 display-time-default-load-average nil            ; Don't display load average
 display-time-format "%H:%M"                      ; Format the time string
 fill-column 80                                   ; Set width for automatic line breaks
 help-window-select t                             ; Focus new help windows when opened
 indent-tabs-mode nil                             ; Stop using tabs to indent
 inhibit-startup-screen t                         ; Disable start-up screen
 initial-scratch-message ";; Nhap..."             ; Empty the initial *scratch* buffer
 mouse-yank-at-point t                            ; Yank at point rather than pointer
 ns-use-srgb-colorspace nil                       ; Don't use sRGB colors
 recenter-positions '(5 top bottom)               ; Set re-centering positions
 scroll-conservatively most-positive-fixnum       ; Always scroll by one line
 scroll-margin 10                                 ; Add a margin when scrolling vertically
 select-enable-clipboard t                        ; Merge system's and Emacs' clipboard
 sentence-end-double-space nil                    ; End a sentence after a dot and a space
 show-trailing-whitespace nil                     ; Display trailing whitespaces
 split-height-threshold nil                       ; Disable vertical window splitting
 split-width-threshold nil                        ; Disable horizontal window splitting
 tab-width 4                                      ; Set width for tabs
 uniquify-buffer-name-style 'forward              ; Uniquify buffer names
 window-combination-resize t                      ; Resize windows proportionally
 x-stretch-cursor t                               ; Stretch cursor to the glyph width
 pop-up-frames nil
 shell-file-name "/bin/bash")
(cd "~/")                                         ; Move to the user directory
(delete-selection-mode 1)                         ; Replace region when inserting text
(display-time-mode 1)                             ; Enable time in the mode-line
(fringe-mode 0)                                   ; Disable fringes
(fset 'yes-or-no-p 'y-or-n-p)                     ; Replace yes/no prompts with y/n
(global-subword-mode 1)                           ; Iterate through CamelCase words
(menu-bar-mode 0)                                 ; Disable the menu bar
(mouse-avoidance-mode 'banish)                    ; Avoid collision of mouse with point
(put 'downcase-region 'disabled nil)              ; Enable downcase-region
(put 'upcase-region 'disabled nil)                ; Enable upcase-region
(set-default-coding-systems 'utf-8)               ; Default to utf-8 encoding

(add-hook 'focus-out-hook #'garbage-collect)      ; Snappier

(defun indent-buffer ()
  "Indents an entire buffer using the default intenting scheme."
  (interactive)
  (point-to-register 'o)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max))
  (jump-to-register 'o))

(defun format-buffer ()
  "Format the whole buffer"
  (interactive)
  (indent-buffer)
  (delete-trailing-whitespace))

;; Change text size globally
(define-globalized-minor-mode
  global-text-scale-mode
  text-scale-mode
  (lambda () (text-scale-mode 1)))

(defun global-text-scale-adjust (inc) (interactive)
       (text-scale-set 1)
       (kill-local-variable 'text-scale-mode-amount)
       (setq-default text-scale-mode-amount (+ text-scale-mode-amount inc))
       (global-text-scale-mode 1))

;; Which Key
(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode))

;; Buffer move
(use-package buffer-move
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LANGUAGE-SUPPORTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Company
(use-package company
  :ensure t
  :bind
  (:map company-active-map
        ("RET" . nil)
        ([return] . nil)
        ("TAB" . company-complete-selection)
        ([tab] . company-complete-selection)
        ("<right>" . company-complete-common))
  :hook
  (after-init . global-company-mode)
  :custom
  (company-dabbrev-downcase nil)
  (company-idle-delay .2)
  (company-minimum-prefix-length 1)
  (company-require-match nil)
  (company-tooltip-align-annotations t))

;; C/C++
(use-package cmake-ide
  :ensure t
  )

(use-package rtags
  :ensure t
  :after cmake-ide
  :config
  (cmake-ide-setup))

;; jenkins/ groovy
(use-package groovy-mode
  :ensure t)

;; docker
(use-package dockerfile-mode
  :ensure t)

;; bazel
(use-package bazel-mode
  :ensure t)

;; markdown
(use-package markdown-mode
  :ensure t)

;; markdown
(use-package terraform-mode
  :ensure t)

;; yaml
(use-package yaml-mode
  :ensure t)

;; python
(use-package elpy
  :ensure t)

;; syntax check
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VERSION CONTROL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit
  :ensure t)

(use-package evil-magit
  :after magit
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UTILITIES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Helm
(use-package helm
  :ensure t
  :init
  (setq helm-mode-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t)
  (setq helm-candidate-number-list 50))

(use-package helm-ls-git
  :ensure t)

(use-package helm-projectile
  :ensure t
  :defer nil
  :bind
  (:map helm-projectile-find-file-map
        ("<left>" . backward-char)
        ("<right>" . forward-char))
  :config
  (helm-projectile-toggle 1))

(use-package projectile
  :ensure t
  :hook
  (after-init . projectile-global-mode)
  :init
  (setq-default
   projectile-cache-file (expand-file-name ".projectile-cache" user-emacs-directory)
   projectile-known-projects-file (expand-file-name ".projectile-bookmarks" user-emacs-directory))
  :custom
  (setq projectile-completion-system 'helm)
  (setq projectile-enable-caching t))

;; Highlight whitespaces
(use-package whitespace
  :ensure nil
  :hook
  ((prog-mode . whitespace-turn-on)
   (text-mode . whitespace-turn-on))
  :custom
  (whitespace-style '(face empty indentation::space tab trailing)))

;; treemacs
(use-package treemacs
  :ensure t)

(use-package treemacs-evil
  :ensure t)

;; ripgrep
(use-package rg
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LATEX STUFFS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; auctex
(use-package tex-site
  :ensure auctex
  :defer t
  :after (tex latex)
  :config
  (setq TeX-parse-self t))

(use-package reftex
  :ensure t
  :config
  (setq reftex-plug-into-AUCTeX t))


(use-package helm-bibtex
  :ensure t
  :config
  (setq helm-bibtex-bibliography
        '("~/hieustuffs/masterthesis/write/bibs/masterthesis.bib"))
  (setq helm-bibtex-library-path
        '("~/hieustuffs/bibtex/pdfs"
          "~/hieustuffs/bibtex/papers")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KEY BINDING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Custom keybinding
(use-package general
  :ensure t
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   ;; "/"   '(counsel-rg :which-key "ripgrep") ; You'll need counsel package for this
   "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
   "SPC" '(helm-M-x :which-key "M-x")
   ;; File Navigation
   "ff"  '(helm-find-files :which-key "find files")
   "ft"  '(treemacs :which-key "treemacs")
   "fl"  '(helm-locate :which-key "locate file")
   "pff" '(helm-projectile-find-file :which-key "projectile find file")
   "pfd" '(helm-projectile-find-file-dwim :which-key "projectile find file at point")
   ;; Buffers
   "bb"  '(helm-buffers-list :which-key "buffers list")
   "bl"  '(buf-move-right :which-key "move right")
   "bh"  '(buf-move-left :which-key "move left")
   "bk"  '(buf-move-up :which-key "move up")
   "bj"  '(buf-move-down :which-key "move bottom")
   "bf"  '(format-buffer :which-key "format the whole buffer")
   ;; magit
   "gg"  '(magit-status :which-key "magit")
   "gb"  '(magit-blame :which-key "magit blame")
   "gf" '(helm-ls-git-ls :which-key "git find file")
   "gs"  '(helm-grep-do-git-grep :which-key "git grep")
   ;; latex
   "lca"  '(TeX-command-run-all :which-key "tex compile all")
   "lbt"  '(helm-bibtex :which-key "helm bibtex")
   ;; QoL
   "w="  '((lambda () (interactive)
             (global-text-scale-adjust (- text-scale-mode-amount))
             (global-text-scale-mode -1)) :which-key "set default font size")
   "w+"  '((lambda () (interactive) (global-text-scale-adjust 1))
           :which-key "increase font size")
   "w-"  '((lambda () (interactive) (global-text-scale-adjust -1))
           :which-key "decrease font size")
   ;; ripgrep
   "rr"  '(rg :which-key "rg")
   "rp"  '(rg-project :which-key "rg project")
   "rd"  '(rg-dwim :which-key "rg at point")
   "rl"  '(rg-list-searches :which-key "rg searches list")
   "rs"  '(rg-save-search :which-key "rg save search")
   "rS"  '(rg-save-search-as-name :which-key "rg save search as name")
   "rt"  '(rg-literal :which-key "rg non-regex")
   ;; Others
   "at"  '(ansi-term :which-key "open terminal")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom in a seperate file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default custom-file (expand-file-name ".custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
