;; Initialize package management
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
;; (add-to-list 'package-archives
;;     '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp/")

; Set where is the custom theme load path
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;;
;; General Settings
;;

; Prevent dead-acutes and so on
(require 'iso-transl)

; y or n.
(defalias 'yes-or-no-p 'y-or-n-p)

; When two buffers have the same name display buff|dir1 and buff|dir2
; instead of buff<1> buff<2>
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

; Indent everything with spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

; Auto auto-fill (pun intended) to 80 columns on tex-mode
(add-hook 'text-mode-hook '(lambda() (turn-on-auto-fill) (set-fill-column 79)))

; Automatic bracket insertion by pairs
(electric-pair-mode 1)

; Move more easily between buffers
(global-set-key (kbd "C-c j")  'windmove-left)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c i")    'windmove-up)
(global-set-key (kbd "C-c k")  'windmove-down)

; Resize buffers
(global-set-key (kbd "C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-<down>") 'shrink-window)
(global-set-key (kbd "C-<up>") 'enlarge-window)

;; Fill column indicator
(require 'fill-column-indicator)
(define-globalized-minor-mode
  global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode t)
(setq-default fill-column 79)

;; Company mode
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
;(setq company-tooltip-align-annotations t)

; Reduce the time after which the company auto completion popup opens
(setq company-idle-delay 0.2)

; Reduce the number of characters before company kicks in
(setq company-minimum-prefix-length 1)

; the tab key may be used to indent or to autocomplete
(defun indent-or-complete ()
  (interactive)
  (if (looking-at "\\_>")
      (company-complete-common)
    (indent-according-to-mode)))

(add-hook 'company-mode
    (lambda ()
     (define-key indent-or-complete-map (kdb "TAB") 'complete-or-indent)))

(setq company-minimum-prefix-length 1
    company-idle-delay 0.0) ;; default is 0.2

;; Toggle NeoTree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-smart-open t)
(eval-after-load 'neotree
    '(custom-set-variables
    '(neo-window-position (quote left))))

; do not show some types of files. Until .elc (included) are the default values
(setq neo-hidden-regexp-list '("^\\." "\\.cs\\.meta$" "\\.pyc$" "~$" "^#.*#$" "\\.elc$" "\\.lo$"))


; Rainbow delimiters everywhere
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

; Always highlight current line
(global-hl-line-mode 1)

; Disable scroll bars and other bars
(toggle-scroll-bar 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(fringe-mode 10)

; 5 line margin on scrolls, step of 1
(setq scroll-margin 5 scroll-conservatively 9999 scroll-step 1)

; Cool mode bar
(require 'powerline)
(powerline-center-theme)


; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
; To speed up flycheck enable on-the-fly only afer saving
(setq flycheck-check-syntax-automatically '(save mode-enable))
;; the default value was '(save idle-change new-line mode-enable


; Show flycheck errors on popups
(eval-after-load 'flycheck
    '(custom-set-variables
    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

; Color the mode line based on the flycheck state
(require 'flycheck-color-mode-line)
(eval-after-load "flycheck"
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

; tmp files (~) will be saved in a special directory
(setq backup-directory-alist `(("." . "~/.emacs-saves")))

;;
;; Ansi color function
;;
(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun display-ansi-colors-read-only ()
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

;;
;; Markdown mode
;;

; Set Github-flavoured markdown preview (mostly for tables)
(setq markdown-command "~/.emacs.d/flavor.rb")
; Add flyspell on markdown by default
(add-hook 'markdown-mode-hook 'flyspell-mode)


;;
;; Rust
;;

(require 'rust-mode)
(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  (lsp)
  ;(setq rust-format-on-save t)
  ) ; can also be launched with C-c C-f

(require 'lsp-mode)
(with-eval-after-load 'lsp-mode
    (setq lsp-rust-server 'rust-analyzer)
    ;(lsp-eldoc-render-all t)
    ;(lsp-idle-delay 0.6)
    ;(lsp-rust-analyzer-server-display-inlay-hints t)
    (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(with-eval-after-load 'lsp-ui
    (lsp-ui-peek-always-show t)
    (lsp-ui-sideline-show-hover t)
    (lsp-ui-doc-enable nil))

;;
;; Go
;;
(require 'lsp-mode)
(add-hook 'go-mode-hook #'lsp-deferred)

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;;
;; Customization
;;


;;
;; More customization
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#2d2d2d" :foreground "#d3d0c8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 128 :width normal :foundry "PfEd" :family "DejaVu Sans Mono")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#393939" "#f2777a" "#99cc99" "#ffcc66" "#6699cc" "#cc99cc" "#6699cc" "#e8e6df"])
 '(ansi-term-color-vector
   [unspecified "#393939" "#f2777a" "#99cc99" "#ffcc66" "#6699cc" "#cc99cc" "#6699cc" "#e8e6df"])
 '(auth-source-save-behavior nil)
 '(custom-enabled-themes '(base16-eighties-dark-seven))
 '(custom-safe-themes
   '("d05246b6b0ef9e9c58d8348840cac1d81c7df8c72f884502c2b52d99ded757ee" "54dd417837055b689d37f8d466f47dee0211894190225c50b00406b1b70d6b1b" "8514a60c65539e76b72905beb52af8b25beee8ac809e0fe9a15a574c11a12d0a" "67fdaff573b9ba142ab79cdc5b24b2b55b77cc786524efe33d3a4a7e1f82500b" default))
 '(fancy-splash-image nil nil nil "You can only see as far as you think.")
 '(fci-rule-color "#343d46")
 '(flycheck-checker-error-threshold 1000)
 '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)
 '(ido-mode 'both nil (ido))
 '(inhibit-startup-screen t)
 '(neo-window-position 'left)
 '(org-agenda-files '("~/todo.org"))
 '(package-selected-packages
   '(go-mode eglot lsp-ui lsp-mode flycheck-color-mode-line powerline flycheck company flycheck-rust rust-mode spacegray-theme vlf slime rainbow-mode rainbow-delimiters projectile paredit neotree markdown-mode hl-todo highlight-numbers highlight-indentation flymake-python-pyflakes fill-column-indicator cider base16-theme auto-complete))
 '(send-mail-function 'smtpmail-send-it)
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 25)
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#bf616a")
     (40 . "#DCA432")
     (60 . "#ebcb8b")
     (80 . "#B4EB89")
     (100 . "#89EBCA")
     (120 . "#89AAEB")
     (140 . "#C189EB")
     (160 . "#bf616a")
     (180 . "#DCA432")
     (200 . "#ebcb8b")
     (220 . "#B4EB89")
     (240 . "#89EBCA")
     (260 . "#89AAEB")
     (280 . "#C189EB")
     (300 . "#bf616a")
     (320 . "#DCA432")
     (340 . "#ebcb8b")
     (360 . "#B4EB89")))
 '(vc-annotate-very-old-color nil))
