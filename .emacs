;; Initialize package management
(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
;(add-to-list 'package-archives 
;  '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

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

;; Auto-complete
(require 'auto-complete-config)
(setq ac-delay 0.0)
(setq ac-quick-help-delay 0.5)
(ac-config-default)

;; Pop up contextual documentation
(eval-after-load "cider"
  '(define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))

;; Toggle NeoTree
;(add-to-list 'load-path "/some/path/neotree")
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-smart-open t)

;; Rainbow delimiters everywhere
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;
;; Clojure
;;
(require 'clojure-mode-extra-font-locking)
(add-hook 'clojure-mode-hook #'paredit-mode)
;(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode) ; now global

;; Cider
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

;;
;; Common Lisp
;;

;; Slime
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-conftribs '(slime-fancy))


;;
;; LaTeX
;;
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-PDF-mode t)

;; Spell checking
(setq ispell-program-name "aspell")
(setq ispell-dictionary "english")
;(setq ispell-dictionary "spanish")

; Highlights the errors while writing
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
; Highlights the erros in the whole buffer
(add-hook 'LaTeX-mode-hook 'flyspell-buffer)


;;
;; Python
;;

; Python mode
(setq py-install-directory "~/.emacs.d/python-mode-6.2.0")
(add-to-list 'load-path py-install-directory)
(require 'python-mode)

(eval-after-load "python-mode"
  '(progn
    ;Set the Python shell
    ;(setq-default py-shell-name "ipython")
    (setq-default py-shell-name "python")
    ;(setq-default py-which-bufname "IPython")
    (setq-default py-which-bufname "Python")

    ; Switch to interpreter buffer after executing code
    (setq py-shell-switch-buffers-on-execute-p t)
    (setq py-switch-buffers-on-execute-p t)
    (setq py-split-windows-on-execute-p nil)

    ; Smart identation
    (setq py-smart-identation t)

    ; Pymacs
    (add-to-list 'load-path "~/.emacs.d/pymacs-0.25")
    (autoload 'pymacs-apply "pymacs")
    (autoload 'pymacs-call "pymacs")
    (autoload 'pymacs-eval "pymacs" nil t)
    (autoload 'pymacs-exec "pymacs" nil t)
    (autoload 'pymacs-load "pymacs" nil t)
    (autoload 'pymacs-autoload "pymacs")

    ; Ropemacs
    (require 'pymacs)
    (pymacs-load "ropemacs" "rope-")
    
    ; flymake-python
    (when (load "flymake" t)
         (defun flymake-pyflakes-init ()
           (let* ((temp-file (flymake-init-create-temp-buffer-copy
                              'flymake-create-temp-inplace))
              (local-file (file-relative-name
                           temp-file
                           (file-name-directory buffer-file-name))))
             (list "pyflakes" (list local-file))))

         (add-to-list 'flymake-allowed-file-name-masks
                  '("\\.py\\'" flymake-pyflakes-init)))

   (add-hook 'find-file-hook 'flymake-find-file-hook)))

;;
;; Customization
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector ["#393939" "#f2777a" "#99cc99" "#ffcc66" "#6699cc" "#cc99cc" "#6699cc" "#e8e6df"])
 '(ansi-term-color-vector [unspecified "#393939" "#f2777a" "#99cc99" "#ffcc66" "#6699cc" "#cc99cc" "#6699cc" "#e8e6df"])
 '(custom-enabled-themes (quote (base16-default)))
 '(custom-safe-themes (quote ("de2c46ed1752b0d0423cde9b6401062b67a6a1300c068d5d7f67725adc6c3afb" "53e29ea3d0251198924328fd943d6ead860e9f47af8d22f0b764d11168455a8e" "e53cc4144192bb4e4ed10a3fa3e7442cae4c3d231df8822f6c02f1220a0d259a" "9bac44c2b4dfbb723906b8c491ec06801feb57aa60448d047dbfdbd1a8650897" "f41fd682a3cd1e16796068a2ca96e82cfd274e58b978156da0acce4d56f2b0d5" "ae8d0f1f36460f3705b583970188e4fbb145805b7accce0adb41031d99bd2580" "1affe85e8ae2667fb571fc8331e1e12840746dae5c46112d5abb0c3a973f5f5a" "51bea7765ddaee2aac2983fac8099ec7d62dff47b708aa3595ad29899e9e9e44" default)))
 '(fci-rule-color "#343d46")
 '(global-linum-mode t)
 '(ido-mode (quote both) nil (ido))
 '(menu-bar-mode nil)
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map (quote ((20 . "#bf616a") (40 . "#DCA432") (60 . "#ebcb8b") (80 . "#B4EB89") (100 . "#89EBCA") (120 . "#89AAEB") (140 . "#C189EB") (160 . "#bf616a") (180 . "#DCA432") (200 . "#ebcb8b") (220 . "#B4EB89") (240 . "#89EBCA") (260 . "#89AAEB") (280 . "#C189EB") (300 . "#bf616a") (320 . "#DCA432") (340 . "#ebcb8b") (360 . "#B4EB89"))))
 '(vc-annotate-very-old-color nil))
 ;;
 ;; More customization
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "unknown" :family "Ubuntu Mono")))))
