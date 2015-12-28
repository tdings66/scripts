
(setenv "EDITOR" "emacsclient")
(setenv "PAGER" "cat")
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(backup-by-copying t)
 '(backup-directory-alist (quote (("." . "~/.saves"))))
 '(c-default-style "linux")
 '(case-fold-search nil)
 '(delete-old-versions t)
 '(kept-new-versions 6)
 '(kept-old-versions 2)
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(show-paren-mode 1)
 '(split-width-threshold nil)
 '(tool-bar-mode nil)
 '(version-control t))


;;; ******* SVTFS.LOG colorization *****
(add-to-list 'load-path "~/git/scripts/bin")
(require 'log-mode)
  (message "Loaded log-mode")


;;;; ******* GDB STUFF *******
;;;; OPTIONAL, avoid typing full path when starting gdb
(global-set-key (kbd "C-c C-g")
 '(lambda ()(interactive) (gud-gdb (concat "coredebug -o --fullname " (cppcm-get-exe-path-current-buffer))))
)


;; ********** Investigate GNU Global for cscope replacement. *********
;; From : /usr/share/emacs/site-lisp/global/gtags.el
(add-to-list 'load-path "/usr/share/emacs/site-lisp/global")
;(require 'gtags)
(autoload 'gtags-mode "gtags" "" t)

(setq c-mode-hook '(lambda ()(gtags-mode 1)))
(setq cc-mode-hook '(lambda ()(gtags-mode 1)))



;;; ******* Colorize the active window's mode-line
;;;  Use M-x list-faces-display to edit this.
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#FFFFE0" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default"))))
 '(cursor ((t (:background "green" :foreground "black"))))
 '(font-lock-builtin-face ((t (:foreground "color-21"))))
 '(font-lock-function-name-face ((t (:foreground "color-53"))))
 '(font-lock-type-face ((t (:foreground "color-172"))))
 '(font-lock-variable-name-face ((t (:foreground "brightblue"))))
 '(gdb-arrow-face ((t (:background "red" :foreground "brightyellow"))) t)
 '(mode-line ((t (:background "brightgreen" :foreground "black" :box (:line-width -1 :style released-button)))))
 '(mode-line-buffer-id ((t (:inherit nil :background "white" :foreground "red" :weight bold))))
 '(mode-line-emphasis ((t nil)))
 '(mode-line-highlight ((t nil)))
 '(mode-line-inactive ((t (:background "brightred" :foreground "black" :weight light))))
 '(show-paren-match ((t (:inverse-video t))))
 '(vertical-border ((t (:background "white" :foreground "black"))))
 '(vertical-divider ((t (:background "white" :foreground "black"))))
 '(window-divider ((t (:background "white" :foreground "black")))))


;;;(add-hook 'dired-mode-hook 'color-theme-high-contrast)


;;;; ******* EMACS PACKAGE MANAGER ******* Requires emacs v24
(if (fboundp 'package-initialize) (package-initialize) (message "No packages"))
;;;;; Add things after this line that require packages.
;;;;; Keep things above here if they don't need packages.

(if (fboundp 'color-theme-initialize) (color-theme-initialize) (message "No color-theme"))
;; ******* ENABLE A COLOR THEME *******
					;(unless (member "color-theme" package-alist) (package-install "color-theme"))
					;(require 'color-theme)

;       (color-theme-tty-dark)

(if (boundp 'color-themes) (setq my-color-themes color-themes)) ; Start with all of them
					;(setq my-color-themes (delete 'color-theme-aalto-dark my-color-themes))
					;(setq my-color-themes (delete 'color-theme-aalto-light my-color-themes))

(defun car-theme () ;figure out if we need car or caar
  (interactive)
  (cond
   ((consp (car theme-current))
    (caar theme-current))
   (t
    (car theme-current))))
(defun my-theme-set-default () ; Set the first row
  (interactive)
  (if (boundp 'my-color-themes) (setq theme-current my-color-themes) (message "No color theme default"))
  (funcall (car-theme)))
(defun my-describe-theme () ; Show the current theme
  (interactive)
  (message "%s" (car-theme)))
(defun my-theme-cycle ()
  (interactive)
  (setq theme-current (cdr theme-current))
  (if (null theme-current)
      (setq theme-current my-color-themes)
    (funcall (car-theme)))
  (message "%S" (car-theme)))

(if (boundp 'my-color-themes) (setq theme-current my-color-themes))
					;(setq color-theme-is-global nil) ; Initialization
(if (boundp 'my-color-themes) (my-theme-set-default))
(global-set-key [f10] 'my-theme-cycle)


