
;; Package stuff
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

; list the packages you want
(setq package-list '(evil auto-complete color-theme web-mode yasnippet tabbar))

; list the repositories containing them
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; ===== Package initializations =====
(require 'evil)
(evil-mode 1) 

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

(require 'color-theme)
(color-theme-initialize)

(require 'tabbar)
(tabbar-mode t)
(setq tabbar-cycle-scope 'tabs)
(setq tabbar-buffer-groups-function
	(lambda ()
	    (let ((dir (expand-file-name default-directory)))
	(cond ((member (buffer-name) '("*Completions*"
			"*scratch*"
			"*Messages*"
			"*Ediff Registry*"))
	    (list "#misc"))
	    ((string-match-p "/.emacs.d/" dir)
	    (list ".emacs.d"))
	    (t (list dir))))))

;; ===== Custom minor modes =====
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
