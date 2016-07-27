;;; base16-eighties-dark-seven-theme.el --- an Emacs 24 theme
;;; based on Base16 Eighties Dark (tmTheme), now it is something completely
;;; different *_*
;;
;;; Author: Seven Flying, Auto Converted to Emacs 24 by tmtheme-to-deftheme (tm2deftheme)
;;; Version: 20160727
;;; Original author: Chris Kempson (http://chriskempson.com)
;;; Url: https://github.com/emacsfodder/tmtheme-to-deftheme
;;; Package-Requires: ((emacs "24.0"))
;;
;;; Commentary:
;;; This theme is a modification of Base16 Eighties
;;
;;; Code:

(deftheme base16-eighties-dark-seven
  "base16-eighties-dark-seven-theme - Created by Seven Flying - 2015-04-10 15:51:50 +0200")

(custom-theme-set-variables
 'base16-eighties-dark-seven)

(custom-theme-set-faces
 'base16-eighties-dark-seven
 ;; basic theming.

 '(default ((t (:foreground "#d3d0c8" :background "#2d2d2d" ))))
 '(region  ((t (:background "#515151"))))
 '(cursor  ((t (:background "#d3d0c8"))))

 ;; Temporary defaults
 '(linum                               ((t (:foreground "#75b5aa"  :background "#2d2d2d" ))))
 '(fringe                              ((t (                       :background "#2d2d2d" ))))

 '(minibuffer-prompt                   ((t (:foreground "orange"   :background nil       :weight bold                                  ))))
 '(escape-glyph                        ((t (:foreground "orange"   :background nil                                                     ))))
 '(highlight                           ((t (:foreground nil        :background "#3e4446"                                                    ))))
 '(shadow                              ((t (:foreground "#447777"  :background nil                                                     ))))

 '(vertical-border                     ((t (:foreground "#282a2e"  :background nil))))
 
 '(trailing-whitespace                 ((t (:foreground "#FFFFFF"  :background "#C74000"                                               ))))
 '(link                                ((t (:foreground "#00b7f0"  :background nil       :underline t                                  ))))
 '(link-visited                        ((t (:foreground "#4488cc"                        :underline t :inherit (link)                  ))))
 '(button                              ((t (:foreground "#FFFFFF"  :background "#444444" :underline t :inherit (link)                  ))))
 '(next-error                          ((t (                                             :inherit (region)                             ))))
 '(query-replace                       ((t (                                             :inherit (isearch)                            ))))
 '(header-line                         ((t (:foreground "#EEEEEE"  :background "#444444" :box nil :inherit (mode-line)                 ))))

 '(mode-line-highlight                 ((t (                                             :box nil                                      ))))
 '(mode-line-emphasis                  ((t (                                             :weight bold                                  ))))
 '(mode-line-buffer-id                 ((t (                                             :box nil :weight bold                         ))))

 '(mode-line-inactive                  ((t (:foreground "#aea99a"  :background "#3e3d3c" :box nil :weight light :inherit (mode-line)   ))))
 '(mode-line                           ((t (:foreground "#d3d0c8"  :background "#3e3d3c" :box nil ))))

 '(isearch                             ((t (:foreground "#99ccee"  :background "#444444"                                               ))))
 '(isearch-fail                        ((t (                       :background "#ffaaaa"                                               ))))
 '(lazy-highlight                      ((t (                       :background "#77bbdd"                                               ))))
 '(match                               ((t (                       :background "#3388cc"                                               ))))

 '(tooltip                             ((t (:foreground "black"    :background "LightYellow" :inherit (variable-pitch)                 ))))

 '(js3-function-param-face             ((t (:foreground "#BFC3A9"                                                                      ))))
 '(js3-external-variable-face          ((t (:foreground "#F0B090"  :bold t                                                             ))))

 '(secondary-selection                 ((t (                       :background "#342858"                                               ))))
 '(cua-rectangle                       ((t (:foreground "#E0E4CC"  :background "#342858" ))))

 ;; Magit hightlight
 '(magit-item-highlight                ((t (:foreground "white" :background "#1278A8" :inherit nil ))))

 ;; flyspell-mode
 '(flyspell-incorrect                  ((t (:underline "#AA0000" :background nil :inherit nil ))))
 '(flyspell-duplicate                  ((t (:underline "#009945" :background nil :inherit nil ))))

 ;; flymake-mode
 '(flymake-errline                     ((t (:underline "#AA0000" :background nil :inherit nil ))))
 '(flymake-warnline                    ((t (:underline "#009945" :background nil :inherit nil ))))

 ;;git-gutter
 '(git-gutter:added                    ((t (:foreground "#609f60" :bold t))))
 '(git-gutter:modified                 ((t (:foreground "#3388cc" :bold t))))
 '(git-gutter:deleted                  ((t (:foreground "#cc3333" :bold t))))

 '(diff-added                          ((t (:background "#305030"))))
 '(diff-removed                        ((t (:background "#903010"))))
 '(diff-file-header                    ((t (:background "#362145"))))
 '(diff-context                        ((t (:foreground "#E0E4CC"))))
 '(diff-changed                        ((t (:foreground "#3388cc"))))
 '(diff-hunk-header                    ((t (:background "#242130"))))


 '(font-lock-comment-face ((t (:foreground "#747369"  ))))
 '(font-lock-keyword-face ((t (:foreground "#cc99cc"  ))))
 '(font-lock-variable-name-face ((t (:foreground "#f2777a"  ))))
 '(font-lock-function-name-face ((t (:foreground "#6699cc"  ))))
 '(font-lock-type-face ((t (:foreground "#ffcc66"  ))))
 '(font-lock-string-face ((t (:foreground "#99cc99"  ))))
 '(font-lock-constant-face ((t (:foreground "#f99157"  ))))
 '(diff-added ((t (:foreground "#99cc99"  ))))
 '(diff-removed ((t (:foreground "#f2777a"  ))))
 '(diff-changed ((t (:foreground "#cc99cc"  ))))
 '(error ((t (:foreground "#f2f0ec" :background "#f2777a" ))))
 '(font-lock-warning-face ((t (:foreground "#f2f0ec" :background "#f2777a" ))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#747369"  ))))

;; Rainbow delimiters
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#cc99cc"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#99cc99"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#ffcc66"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#f99157"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#ffaaaa"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#f2777a"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#6699cc"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#66cccc"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#f2f0ec"))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "#FF0000"))))
) ;; End face definitions

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'base16-eighties-dark-seven)

;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:

;;; base16-eighties-dark-seven-theme.el ends here
