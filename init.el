(package-initialize)
(add-to-list 'load-path "/Users/nbeydon/.emacs.d/vendor/use-package/")
(require 'use-package)
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

;;#################################################################
;;             nice footer bar
;;##################################################################
(setq sublimity-scroll-weight 10
      sublimity-scroll-drift-length 5)
(setq sublimity-map-size 20)
(setq sublimity-map-fraction 0.3)
(setq sublimity-map-text-scale -7)
(setq sublimity-attractive-centering-width 110)
(sublimity-mode 1)


;;#################################################################
;;             linter
;;##################################################################
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(add-to-list 'load-path "/Users/nbeydon/.emacs.d/cl-lib/")
(require 'cl-lib)

(setq backup-directory-alist `(("." . "~/.saves")))


(defalias 'yes-or-no-p 'y-or-n-p)

(setq mac-command-modifier 'super)

(set-keyboard-coding-system nil)

(add-to-list 'package-archives
  	     '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; use web-mode for .jsx files
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

;; http://www.flycheck.org/manual/latest/index.html
(require 'flycheck)

;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  	      (append flycheck-disabled-checkers
  		      '(javascript-jshint)))

(setq flycheck-disabled-checkers '(javascript-jshint))
(setq flycheck-disabled-checkers '(javascript-jscs))
(setq flycheck-disabled-checkers '(javascript-standard))
(setq flycheck-checkers '(javascript-eslint))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  	      (append flycheck-disabled-checkers
  		      '(json-jsonlist)))

;; https://github.com/purcell/exec-path-from-shell
;; only need exec-path-from-shell on OSX
;; this hopefully sets up path and other vars better
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; adjust indents for web-mode to 4 spaces
(defun my-web-mode-hook ()
  "Hooks for Web mode. Adjust indents"
      ;;; http://web-mode.org/
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4))
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; disable tab indent
(setq-default indent-tabs-mode nil)

;; for better jsx syntax-highlighting in web-mode
;; - courtesy of Patrick @halbtuerke
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
  	ad-do-it)
    ad-do-it))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a802c77b818597cc90e10d56e5b66945c57776f036482a033866f5f506257bca" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "01ce486c3a7c8b37cf13f8c95ca4bb3c11413228b35676025fdf239e77019ea1" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tabbar-button ((t (:inherit tabbar-default :background "#3F51B5" :foreground "white"))))
 '(tabbar-button-highlight ((t (:inherit tabbar-default))))
 '(tabbar-default ((t (:inherit variable-pitch :background "#3F51B5" :foreground "#7986CB" :weight bold))))
 '(tabbar-highlight ((t (:underline t))))
 '(tabbar-selected ((t (:inherit tabbar-default :background "#7986CB" :foreground "white"))))
 '(tabbar-separator ((t (:inherit tabbar-default :background "#D50000"))))
 '(tabbar-unselected ((t (:inherit tabbar-default :background "#3F51B5" :foreground "#DDDDDD")))))

;; tern js
;;(add-to-list 'load-path "/usr/local/lib/node_modules/tern/emacs/")
;;(autoload 'tern-mode "tern.el" nil t)

(setq js2-highlight-level 4)

(defun pbcopy ()
  (interactive)
  (call-process-region (point) (mark) "pbcopy")
  (setq deactivate-mark t))

(defun pbpaste ()
  (interactive)
  (call-process-region (point) (if mark-active (mark) (point)) "pbpaste" t t))

(defun pbcut ()
  (interactive)
  (pbcopy)
  (delete-region (region-beginning) (region-end)))

(global-set-key (kbd "<f13> c") 'pbcopy)
(global-set-key (kbd "<f13> v") 'pbpaste)
(global-set-key (kbd "<f13> x") 'pbcut)

;;color everywhere <3
(add-hook 'after-init-hook 'global-color-identifiers-mode)

;;config fiplr
(setq fiplr-root-markers '(".git" ".svn"))
(setq fiplr-ignored-globs '((directories (".git" ".svn" "tmp" "node_modules" "coverage" "build" "platforms" ".idea"))
                            (files ("*.jpg" "*.png" "*.zip" "*~"))))

(global-set-key (kbd "C-p") 'fiplr-find-file)

;;change backup directory (stop poluting git repos)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(let ((faces '(font-lock-comment-face font-lock-comment-delimiter-face font-lock-constant-face font-lock-type-face font-lock-function-name-face font-lock-variable-name-face font-lock-keyword-face font-lock-string-face font-lock-builtin-face font-lock-preprocessor-face font-lock-warning-face font-lock-doc-face)))
  (dolist (face faces)
    (set-face-attribute face nil :foreground nil :weight 'normal :slant 'normal)))

(set-face-attribute 'font-lock-comment-delimiter-face nil :slant 'italic)
(set-face-attribute 'font-lock-comment-face nil :slant 'italic)
(set-face-attribute 'font-lock-doc-face nil :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil :weight 'bold)
(set-face-attribute 'font-lock-builtin-face nil :weight 'bold)
(set-face-attribute 'font-lock-preprocessor-face nil :weight 'bold)

;;expend-region
(require 'expand-region)
(global-set-key (kbd "C-d") 'er/expand-region)

;;expend-line
(global-set-key (kbd "C-l") 'turn-on-expand-line-mode)

;;multiline-cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-x d") 'mc/mark-next-like-this)

;;autocomplete
(ac-config-default)
(setq ac-ignore-case nil)

;; add a line
(require 'whitespace)
(setq whitespace-line-column 100) ;; limit line length
(setq whitespace-style '(face lines-tail))

(add-hook 'prog-mode-hook 'whitespace-mode)

;;override the selection instead of writing around
(delete-selection-mode 1)

;;treat .tpl files as html-mode
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))

;;utf8 !
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;;when fleme on
(require 'mouse)
(xterm-mouse-mode t)
(defun track-mouse (e))
(setq mouse-sel-mode t)

;; show start and end for ( [
;;(show-paren-mode 1)

;; wrap a region
(require 'smartparens-config)
(add-hook 'web-mode #'smartparens-mode)

(wrap-region-mode t)

;;alway enable line numbers
;;(global-linum-mode 1)
;;(setq linum-format "%d | ")

;;#################################################################
;;             tabs
;;##################################################################
(require 'tabbar)

(defun tabbar-buffer-groups-by-dir ()
  "Put all files in the same directory into the same tab bar"
  (with-current-buffer (current-buffer)
    (let ((dir (expand-file-name default-directory)))
      (cond ;; assign group name until one clause succeeds, so the order is important
       ((eq major-mode 'dired-mode)
        (list "Dired"))
       ((memq major-mode
              '(help-mode apropos-mode Info-mode Man-mode))
        (list "Help"))
       ((string-match-p "\*.*\*" (buffer-name))
        (list "Misc"))
       (t (list dir))))))
(setq tabbar-background-color "#3F51B5") ;; the color of the tabbar background

(setq tabbar-cycle-scope (quote tabs))
(setq tabbar-use-images t)
;; BUFFER MODIFICATION STATE INDICATOR
(defadvice tabbar-buffer-tab-label (after fixup_tab_label_space_and_flag activate)
  (setq ad-return-value
        (if (and (buffer-modified-p (tabbar-tab-value tab))
                 (buffer-file-name (tabbar-tab-value tab)))
            (concat " + " (concat ad-return-value " "))
          (concat " " (concat ad-return-value " ")))))
(defun ztl-modification-state-change ()
  (tabbar-set-template tabbar-current-tabset nil)
  (tabbar-display-update))
(defun ztl-on-buffer-modification ()
  (set-buffer-modified-p t)
  (ztl-modification-state-change))
(add-hook 'after-save-hook 'ztl-modification-state-change)
(add-hook 'first-change-hook 'ztl-on-buffer-modification)
(tabbar-mode)
(global-set-key [C-left] 'tabbar-backward)
(global-set-key [C-right] 'tabbar-forward)


;;disable menu-bar
(menu-bar-mode -1)

;;kill buffer
(global-set-key (kbd "C-w") 'kill-buffer)

;;#################################################################
;;             nice footer bar
;;##################################################################
;;(add-to-list 'load-path "/Users/nbeydon/.emacs.d/vendor/emacs-powerline")
;;(require 'powerline)
;;(setq powerline-arrow-shape 'arrow)   ;; the default
;;(setq powerline-arrow-shape 'curve)   ;; give your mode-line curves
;;(setq powerline-arrow-shape 'arrow14) ;; best for small fonts

;; These two lines you really need.
;;(set-face-attribute 'mode-line nil
;;                    :foreground "Black"
;;                    :background "DarkOrange"
;;                    :box nil)
;;(setq powerline-arrow-shape 'arrow14) ;; best for small fonts
(setq sml/theme 'powerline)
(sml/setup)
(setq sml/no-confirm-load-theme t)
