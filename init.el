;;; init.el --- Initialization file for Emacs
;;; Commentary: Emacs Startup File --- initialization for Emacs

;;; init.el --- Initialization file for Emacs

(setq user-full-name "Hieu Nguyen"
      user-mail-address "tofunth@gmail.com")


;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

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
(eval-when-compile
  (require 'use-package))

(use-package material-theme
  :ensure t
  :config
  (if (display-graphic-p)
  (load-theme 'material-light t)
  )
  )

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Better defaults
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when window-system
  (scroll-bar-mode 0)                             ; Disable the scroll bar
  (tool-bar-mode 0)                               ; Disable the tool bar
  (tooltip-mode 0)                                ; Disable the tooltips
)

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
 initial-scratch-message ";; Nothing really matters..."             ; Empty the initial *scratch* buffer
 mouse-yank-at-point t                            ; Yank at point rather than pointer
 ns-use-srgb-colorspace nil                       ; Don't use sRGB colors
 recenter-positions '(5 top bottom)               ; Set re-centering positions
 scroll-conservatively most-positive-fixnum       ; Always scroll by one line
 scroll-margin 5                                 ; Add a margin when scrolling vertically
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
(mouse-avoidance-mode 'exile)                    ; Avoid collision of mouse with point
(put 'downcase-region 'disabled nil)              ; Enable downcase-region
(put 'upcase-region 'disabled nil)                ; Enable upcase-region
(set-default-coding-systems 'utf-8)               ; Default to utf-8 encoding
(show-paren-mode 1)                               ; Show matching parenthesis
(linum-mode 1)                                    ; Show line number

(add-hook 'focus-out-hook #'garbage-collect)      ; Snappier

(add-hook 'before-save-hook 'delete-trailing-whitespace) ; Automatically delete trailing whitespaces

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

;; undo-tree
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

(use-package fill-column-indicator
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook (lambda ()(fci-mode 1)))
  (add-hook 'python-mode-hook (lambda () (fci-mode 1)))
  (add-hook 'shell-mode-hook (lambda () (fci-mode 1)))
  (add-hook 'groovy-mode-hook (lambda () (fci-mode 1))))

;; Which Key
(use-package which-key
  :defer 1
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
  :defer 2
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

;; jenkins/ groovy
(use-package groovy-mode
  :defer 1
  :ensure t)

;; docker
(use-package dockerfile-mode
  :defer 1
  :ensure t)

;; bazel
(use-package bazel-mode
  :defer 1
  :ensure t)

;; markdown
(use-package markdown-mode
  :defer 1
  :ensure t)

;; markdown
(use-package terraform-mode
  :defer 1
  :ensure t)

;; yaml
(use-package yaml-mode
  :defer 1
  :ensure t)

;; python
(use-package elpy
  :defer 2
  :ensure t)

;; syntax check
(use-package flycheck
  :defer 1
  :ensure t
  :init (global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VERSION CONTROL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit
  :defer 2
  :ensure t
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UTILITIES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Helm
(use-package helm
  :ensure t
  :init
  (setq helm-mode-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t)
  (setq helm-candidate-number-list 50)
)

(use-package helm-ls-git
  :defer 2
  :ensure t)

(use-package helm-projectile
  :defer 2
  :ensure t
  :defer nil
  :bind
  (:map helm-projectile-find-file-map
        ("<left>" . backward-char)
        ("<right>" . forward-char))
  :config
  (helm-projectile-toggle 1))

(use-package projectile
  :defer 2
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
;; (use-package whitespace
;;   :ensure nil
;;   :hook
;;   ((prog-mode . whitespace-turn-on)
;;    (text-mode . whitespace-turn-on))
;;   :custom
;;   (whitespace-style '(face empty indentation::space tab trailing)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LATEX STUFFS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; auctex
(use-package tex-site
  :defer 1
  :ensure auctex
  :defer t
  :after (tex latex)
  :config
  (setq TeX-parse-self t))

(use-package reftex
  :defer 2
  :ensure t
  :config
  (setq reftex-plug-into-AUCTeX t))


(use-package helm-bibtex
  :defer 2
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

;; override default keybindings
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)

;; magit
(global-set-key (kbd "C-c g g") 'magit-status)
(global-set-key (kbd "C-c g b") 'magit-blame)
(global-set-key (kbd "C-c g f") 'helm-ls-git-ls)
(global-set-key (kbd "C-c g s") 'helm-grep-do-git-grep)

;; file navigation
 (global-set-key (kbd "C-c f l") 'helm-locate)

;; QoL
(global-set-key (kbd "C-c w =") '(lambda () (interactive)
             (global-text-scale-adjust (- text-scale-mode-amount))
             (global-text-scale-mode -1)))

(global-set-key (kbd "C-c w +")
             '(lambda () (interactive) (global-text-scale-adjust 1)))

(global-set-key (kbd "C-c w -")
                '(lambda () (interactive) (global-text-scale-adjust -1)))

;; coding
(global-set-key (kbd "C-c c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c c m") 'match-paren)
(global-set-key "%" 'match-paren)

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;; windows
(define-key global-map (kbd "C-<up>") 'windmove-up)
(define-key global-map (kbd "C-<down>") 'windmove-down)
(define-key global-map (kbd "C-<left>") 'windmove-left)
(define-key global-map (kbd "C-<right>") 'windmove-right)

(define-key global-map (kbd "C-S-<up>") 'buf-move-up)
(define-key global-map (kbd "C-S-<down>") 'buf-move-down)
(define-key global-map (kbd "C-S-<left>") 'buf-move-left)
(define-key global-map (kbd "C-S-<right>") 'buf-move-right)

(add-hook 'org-mode-hook '(lambda ()
   (local-set-key [C-S-up]    'buf-move-up)
   (local-set-key [C-S-down]  'buf-move-down)
   (local-set-key [C-S-left]  'buf-move-left)
   (local-set-key [C-S-right] 'buf-move-right)))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
 (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))
(global-set-key (kbd "M-<backspace>") 'backward-delete-word)

(defun delete-line (&optional arg)
  "Delete the rest of the current line; if no nonblanks there, delete thru newline.
With prefix argument ARG, delete that many lines from point.
Negative arguments delete lines backward.
With zero argument, delete the text before point on the current line.

When calling from a program, nil means \"no arg\",
a number counts as a prefix arg.

If `show-trailing-whitespace' is non-nil, this command will just
delete the rest of the current line, even if there are no nonblanks
there.

If option `kill-whole-line' is non-nil, then this command deletes the whole line
including its terminating newline, when used at the beginning of a line
with no argument.

If the buffer is read-only, Emacs will beep and refrain from deleting
the line."
  (interactive "P")
  (delete-region
   (point)
   (progn
     (if arg
         (forward-visible-line (prefix-numeric-value arg))
       (if (eobp)
           (signal 'end-of-buffer nil))
       (let ((end
              (save-excursion
                (end-of-visible-line) (point))))
         (if (or (save-excursion
                   ;; If trailing whitespace is visible,
                   ;; don't treat it as nothing.
                   (unless show-trailing-whitespace
                     (skip-chars-forward " \t" end))
                   (= (point) end))
                 (and kill-whole-line (bolp)))
             (forward-visible-line 1)
           (goto-char end))))
     (point))))

(global-set-key (kbd "C-k") 'delete-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom in a seperate file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default custom-file (expand-file-name ".custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
