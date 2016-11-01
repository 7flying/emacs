;; Initialize package management
(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
    '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
;(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp/")
;;
;; General Settings
;;

;; Prevent dead-acutes and so on
(require 'iso-transl)

;; Fill column indicator
(require 'fill-column-indicator)
(define-globalized-minor-mode
  global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode t)
(setq-default fill-column 80)

;; Company mode
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

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
(setq company-tooltip-align-annotations t)

;; Yasnippet
(eval-after-load 'auto-complete
  '(progn
     (require 'yasnippet)
     (yas-global-mode 1)))
(eval-after-load 'yasnippet
  '(progn
     (require 'auto-complete-config)
     (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
     (ac-config-default)
     (ac-set-trigger-key "TAB")
     (ac-set-trigger-key "<tab>")))


;; Toggle NeoTree
(require 'neotree)
(global-set-key [f8] 'neotree-tgogle)
(setq neo-smart-open t)

;; Rainbow delimiters everywhere
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

; Always highlight current line
(global-hl-line-mode 1)

; Highlight TODO keywords
(global-hl-todo-mode 1)

; Highlight numbers
(add-hook 'prog-mode-hook 'highlight-numbers-mode)

; Highlight indentation
(add-hook 'prog-mode-hook 'highlight-indentation-mode)

; y or n. 
(defalias 'yes-or-no-p 'y-or-n-p)

; Set where is the custom theme load path
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

; Disable scroll bars and other bars
(toggle-scroll-bar 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(fringe-mode 4)

; 5 line margin on scrolls, step of 1
(setq scroll-margin 5 scroll-conservatively 9999 scroll-step 1)

; wrap lines
(global-visual-line-mode)

; Auto auto-fill (pun intended) to 80 columns on tex-mode
(add-hook 'text-mode-hook '(lambda() (turn-on-auto-fill) (set-fill-column 80)))


; Cool mode bar
(require 'powerline)
(powerline-center-theme)

; Automatic bracket insertion by pairs
(electric-pair-mode 1)

; When two buffers have the same name display buff|dir1 and buff|dir2
; instead of buff<1> buff<2>
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

; Indent everything with spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

; Disable flymake
(remove-hook 'elpy-modules 'elpy-module-flymake)

; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

; TRAMP
(setq tramp-default-method "ssh")

; Show flycheck errors on popups
(eval-after-load 'flycheck
    '(custom-set-variables
    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

; Color the mode line based on the flycheck state
(require 'flycheck-color-mode-line)
(eval-after-load "flycheck"
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

; Shell
(require 'multi-term)
(setq multi-term-program "/bin/bash")


;;
;; Custom keys
;; 
(defun indent-whole-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))
(global-set-key [f2] 'indent-whole-buffer)

(defun rotate-dictionary ()
  (interactive)
  (let* ((dict ispell-current-dictionary)
         (change (if (string= dict "spanish")
                     "british" "spanish")))
    (ispell-change-dictionary change)))
(global-set-key [f9] 'rotate-dictionary)

; Move more easily between buffers 
(global-set-key (kbd "C-c j")  'windmove-left)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c i")    'windmove-up)
(global-set-key (kbd "C-c k")  'windmove-down)



;;;;
;;;; -- Programming language specific stuff --
;;;;

;;
;; Clojure
;;
(require 'clojure-mode-extra-font-locking)
(add-hook 'clojure-mode-hook #'paredit-mode)

;; Cider
;; Pop up contextual documentation
(eval-after-load "cider"
  '(define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))
;; Refresh repl after saving a file
(add-hook 'cider-mode-hook
          '(lambda () (add-hook 'after-save-hook
                                '(lambda ()
                                   (if (and (boundp 'cider-mode) cider-mode)
                                       (cider-namespace-refresh))))))
(defun cider-namespace-refresh ()
  (interactive)
  (cider-interactive-eval
   "(require 'clojure.tools.namespace.repl) (clojure.tools.namespace.repl/refresh)"))

;; Auto-complete for cider: ac-cider
(require 'ac-cider)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'cider-mode)
     (add-to-list 'ac-modes 'cider-repl-mode)))

;;
;; Common Lisp
;;

;; Slime
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-conftribs '(slime-fancy))

;;
;; Hy
;;
(add-hook 'hy-mode-hook #'paredit-mode)

;;
;; LaTeX
;;

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-PDF-mode t)

;; Spell checking
(setq ispell-program-name "aspell")
; english by default and use f9 to change it
(setq ispell-dictionary "british")

; Highlights the errors while writing
(add-hook 'latex-mode-hook 'flyspell-mode)
; Highlights the erros in the whole buffer
(add-hook 'latex-mode-hook 'flyspell-buffer)

; Reload from disk when the document is compiled :)
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

; DocView in continuous mode for easy scrolling
(setq doc-view-continuous t)

; 120 for latex
(add-hook 'latex-mode-hook
          (lambda () (set-fill-column 120)))

; Doc-view resolution
(setq doc-view-resolution 300)

;;
;; Python
;;

; General
(add-hook 'python-mode-hook 'highlight-numbers-mode)
(add-hook 'python-mode-hook 'hl-todo-mode)
(add-hook 'python-mode-hook 'rainbow-delimiters-mode)

; Anaconda: needs an inferior Python process to complete built-in definitions
(add-hook 'python-mode-hook 'run-python-once)

(defun run-python-once ()
  (remove-hook 'python-mode-hook 'run-python-once)
  (run-python))

; Anaconda for company
(eval-after-load "company" '(add-to-list 'company-backends 'company-anaconda))
(add-hook 'python-mode-hook 'anaconda-mode)


;;
;; Arff mode
;;
(require 'generic)
(define-generic-mode 'arff-mode
  (list ?%)
  (list "attribute" "relation" "end" "data")
  '(
    ("\\('.*'\\)" 1 'font-lock-string-face)
    ("^\\@\\S-*\\s-\\(\\S-*\\)" 1 'font-lock-string-face)
    ("^\\@.*\\(real\\)" 1 'font-lock-type-face)
    ("^\\@.*\\(integer\\)" 1 'font-lock-type-face)
    ("^\\@.*\\(numeric\\)" 1 'font-lock-type-face)
    ("^\\@.*\\(string\\)" 1 'font-lock-type-face)
    ("^\\@.*\\(date\\)" 1 'font-lock-type-face)
    ("^\\@.*\\({.*}\\)" 1 'font-lock-type-face)
    ("^\\({\\).*\\(}\\)$" (1 'font-lock-reference-face)
     (2 'font-lock-reference-face))
    ("\\(\\?\\)" 1 'font-lock-reference-face)
    ("\\(\\,\\)" 1 'font-lock-keyword-face)
    ("\\(-?[0-9]+?.?[0-9]+\\)" 1 'font-lock-constant-face)
    ("\\(\\@\\)" 1 'font-lock-preprocessor-face))
  (list "\.arff?")
  (list (function
     (lambda ()
         (setq font-lock-defaults
            (list 'generic-font-lock-defaults nil t ; case insensitive
                (list (cons ?* "w") (cons ?- "w"))))
         (turn-on-font-lock)))) "Mode for arff-files.")

;;
;; Markdown mode
;;

; Set Github-flavoured markdown preview (mostly for tables)
(setq markdown-command "~/.emacs.d/flavor.rb")
; Add flyspell on markdown by default
(add-hook 'markdown-mode-hook 'flyspell-mode)

;;
;; NASM mode
;;

(load "~/.emacs.d/modes/nasm-mode.el")
(require 'nasm-mode)
(add-to-list 'auto-mode-alist '("\\.\\(asm\\)$" . nasm-mode))

;;
;; C/C++ mode
;;
(require 'cc-mode)
(setq-default c-basic-offset 4 c-default-style "linux")
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

; safe args for .dir-locals.el
(put 'flycheck-clang-args 'safe-local-variable (lambda (xx) t))
(put 'flycheck-gcc-args 'safe-local-variable (lambda (xx) t))

; Use c++11
(add-hook 'c++-mode-hook
          (lambda () (setq-default flycheck-clang-language-standard "c++11"
                                   flycheck-gcc-language-standard "c++11")))

; Sometimes weird things happen with flycheck (TODO: investigate),
; uncommenting one of these may help
;(add-to-list 'flycheck-disabled-checkers 'c/c++-clang)
;(add-to-list 'flycheck-disabled-checkers 'gcc)
;(setq-default flycheck-c/c++-clang-executable "/usr/bin/clang")

; Auto-completion based on etags
(eval-after-load "company" '(add-to-list 'company-backends 'company-etags))

; company for c++ headers
(add-hook 'c++-mode-hook
         (lambda () (add-to-list 'company-c-headers-path-system
                                 "/usr/include/c++/4.9/")))
; Auto-completion for C/C++ headers
(add-to-list 'company-backends 'company-c-headers)

; Auto-completion based on semantic db info
(eval-after-load "company" '(add-to-list 'company-backend 'company-semantic))

; Show function signatures
; and enable semantic on C/C++
(defun semantic-c-and-cpp ()
    (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
    (semantic-mode 1)
    (require 'stickyfunc-enhance))

(add-hook 'c++-mode-hook #'semantic-c-and-cpp)
(add-hook 'c-mode-hook #'semantic-c-and-cpp)

;; Pebble C mode
(defun pebble-c-mode ()
  (interactive)
  (c-mode)
  (add-to-list 'company-c-headers-path-system
               "~/.pebble-sdk/SDKs/3.9.2/sdk-core/pebble/chalk/include/")
  (setq flycheck-clang-include-path
        (list (expand-file-name "~/.pebble-sdk/SDKs/3.9.2/sdk-core/pebble/chalk/include/"))))

;; Linux C Mode
(defun linux-c-mode ()
  (interactive)
  (c-mode)
  (c-set-style "K&R")
  (setq c-basic-offset 8))

(defun c-99-hook ()
  (setq flycheck-gcc-language-standard "gnu99"))

(add-hook 'c-mode-hook #'c-99-hook)

;;
;; Rust
;;

; Set path to racer binary
(setq racer-cmd "/usr/local/bin/racer")
; Set path to rust src directory
(setq racer-rust-src-path "~/sw/rust/src/")
; Load rust-mode when you open `.rs` files
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
; Setting up configurations when you load rust-mode
(add-hook 'racer-mode-hook #'company-mode)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

;;
;; R
;;

;; Use ESS
;(add-to-list 'load-path "~/.emacs.d/ESS/lisp/")
;(load "ess-site")
;(require 'ess-site)

;;
;; Matlab - Octave
;;
;(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))

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
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 105 :width normal :foundry "unknown" :family "Ubuntu Mono")))))

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
 '(custom-enabled-themes (quote (base16-eighties-dark-seven)))
 '(custom-safe-themes
   (quote
    ("67fdaff573b9ba142ab79cdc5b24b2b55b77cc786524efe33d3a4a7e1f82500b" default)))
 '(fancy-splash-image nil nil nil "You can only see as far as you think.")
 '(fci-rule-color "#343d46")
 '(ido-mode (quote both) nil (ido))
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(org-agenda-files (quote ("~/todo.org")))
 '(org-trello-files (quote ("~/org-trello/")) nil (org-trello))
 '(package-selected-packages
   (quote
    (vlf spacegray-theme slime rainbow-mode rainbow-delimiters projectile paredit nyan-mode neotree markdown-mode js2-mode hl-todo highlight-numbers highlight-indentation flymake-python-pyflakes fill-column-indicator cider base16-theme auto-complete)))
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 25)
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#bf616a")
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
     (360 . "#B4EB89"))))
 '(vc-annotate-very-old-color nil))
