;; Package stuff
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; Package auto-install
(setq package-list '(evil auto-complete color-theme web-mode yasnippet powerline airline-themes projectile bm scss-mode ace-window slime))
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; misc config
(global-auto-revert-mode t)
(setq tab-width 4)
(setq tab-always-indent nil)
(desktop-save-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(show-paren-mode 1)

;; ===== Package initializations =====

(require 'evil)
(evil-mode 1) 

(require 'bm)
(global-set-key (kbd "<C-f8>") 'bm-toggle)
(global-set-key (kbd "<f8>")   'bm-next)
(global-set-key (kbd "<S-f8>") 'bm-previous)

(require 'yasnippet)
(yas-global-mode 1)

;;; auto complete mod
;;; should be loaded after yasnippet so that they can work together
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
;;; set the trigger key so that it can work together with yasnippet on tab key,
;;; if the word exists in yasnippet, pressing tab will cause yasnippet to
;;; activate, otherwise, auto-complete will
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))

(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

;; Set your lisp system and, optionally, some contribs
(setq inferior-lisp-program "clisp")
(require 'slime)
(setq slime-contribs '(slime-fancy))

;(require 'workgroups)
;(workgroups-mode 1)
;(setq wg-prefix-key (kbd "C-c w"))


;; (require 'tabbar)
;; (tabbar-mode t)
;; (setq tabbar-cycle-scope 'tabs)
;; (setq tabbar-buffer-groups-function
;; 	(lambda ()
;; 	    (let ((dir (expand-file-name default-directory)))
;; 	(cond ((member (buffer-name) '("*Completions*"
;; 			"*scratch*"
;; 			"*Messages*"
;; 			"*Ediff Registry*"))
;; 	    (list "#misc"))
;; 	    ((string-match-p "/.emacs.d/" dir)
;; 	    (list ".emacs.d"))
;; 	    (t (list dir))))))

(require 'powerline)
(powerline-center-theme)

(require 'airline-themes)
(load-theme 'airline-badwolf t)

;; correct chars for the powerline
(setq airline-utf-glyph-separator-left      #xe0b0
      airline-utf-glyph-separator-right     #xe0b2
      airline-utf-glyph-subseparator-left   #xe0b1
      airline-utf-glyph-subseparator-right  #xe0b3
      airline-utf-glyph-branch              #xe0a0
      airline-utf-glyph-readonly            #xe0a2
      airline-utf-glyph-linenumber          #xe0a1)

(require 'color-theme)
(color-theme-initialize)
(color-theme-calm-forest)

;; ===== Key bindings =====
(global-set-key (kbd "C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-<down>") 'shrink-window)
(global-set-key (kbd "C-<up>") 'enlarge-window)

(global-set-key (kbd "M-p") 'ace-window)

;; ===== Custom minor modes =====
(ido-mode t)
(projectile-mode 1)

(global-linum-mode 1)
(setq linum-format "%4d \u2502 ")

(defvar custom-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-j") 'tabbar-forward)
    (define-key map (kbd "C-c C-k") 'tabbar-backward)
    map)
  "custom-keys-minor-mode keymap.")

(define-minor-mode custom-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " custom-keys")

(custom-keys-minor-mode 1)

;; ===== CUSTOM SET VARIABLES =====
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("beeb5ac6b65fcccfe434071d4624ff0308b5968bf2f0c01b567d212bcaf66054" "6998bd3671091820a6930b52aab30b776faea41449b4246fdce14079b3e7d125" "e87a2bd5abc8448f8676365692e908b709b93f2d3869c42a4371223aab7d9cf8" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
