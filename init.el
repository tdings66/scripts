;;; Glenn's emacs initialization file
; Author: Glenn Virball
; Remember the following useful ways to use emacs:
;
; Emacs Debugging
;  1) M-x describe-variable (also, C-h v)
;  2) M-x eval-buffer Great for testing out lisp snippets or adding features
;  without needing to modify init.el and restart emacs.
;  3) M-x toggle-debug-on-error RET
;
;
; Semantic Debugging Assistance
;  1) M-x semanticdb-find-test-translate-path
;  2) M-x semantic-adebug-searchdb
;  3) M-x semantic-c-describe-environment
;
; Major commands:
;  1) M-x ecb-activate/M-x ecb-deactivate
;  2) M-x compile (allows context for automatic window/source file navigation)
;  3) M-x dired
;  4) M-x speedbar 
;  5) M-x gdb... then go to the GUD->GUI->UI->Show Windows menu
;  6) M-x valgrind
;  7) M-x term
;  8) M-x shell
;
; File navigation and searching (grep) with Helm:
;  1) M-x Helm for commands
;  2) C-x C-f Helm find-file
;  3) TAB Helm action menu
;  4) M-<SPC> Helm mark candidate
;  5) C-z select/navigate to candidate
;  6) M-x Helm for commands
;  7) C-h m Helm show bindings
;
; Code construction:
;  1) C-c // inserts a template
;  2) C-c /C generate a comment
;  3) C-c /G generate get/set methods
;  4) C-c , C-w Kill the current tag
;  5) C-c , M-w Copy the current tag to the kill ring and tag ring
;  6) C-c , C-y Yank a tag from the tag ring
;  7) C-c , r Copy the current tag to a register
;
; Code Browsing - navigation:
;  1) C-c , j Semantic jump to tag locally
;  2) C-c , J Semantic jump to tag globally
;  3) C-c , g Semantic references to tag
;  4) C-c , m Semantic find local members tag
;  5) C-c , n Semantic next tag in file
;  6) C-c , p Semantic previous tag in file
;  7) C-c , u Semantic parent tag (language dependent)
;  8) C-c , l Semantic list possible completions
;  9) C-x C-<SPC> cycle through the global mark-ring, jumping to each mark
; 10) C-u C-<SPC> cycle through the local mark-ring, jumping to each mark
; 11) C-<SPC> C-<SPC> cycle through the local mark-ring, jumping to each mark
; 12) C-x B (M-p/M-n) Return to the most recently edited tag... M-p and M-n cycle through the tags
;
; Standard Bookmarking:
;  1) C-x r m set a bookmark 
;  2) C-x r b jump to a bookmark 
;  3) C-x r l edit bookmark list
;
; Code Browsing - ECB window navigation:
;  1) C-c . gd goto ecb directory window
;  2) C-c . gs goto ecb source window
;  3) C-c . gm goto ecb methods/variables window
;  4) C-c . g# goto ecb edit window number # (Currently, # can only be 1 or 2)
;  5) C-c . gl goto last active ecb edit window
;  6) C-c . lw toggle ecb window visibility
;  7) C-c . \ toggle compile window
;  8) C-c . / toggle expanded compile window
;  9) C-c . lt toggle between ecb window layouts
; 10) C-c . p goto previous ECB history item
; 11) C-c . n goto next ECB history item
;
; Workgroup mode commands:
;  1) C-c z C-s save a workgroup
;  2) C-c z C-f restore a workgroup
;  3) C-c z C-r reload a workgroup
;  4) C-c z C-w switch to a workgroup
;  5) C-c z C-p previous workgroup
;  6) C-c z C-n next workgroup
;  7) C-c z k   kill workgroup
;  8) C-c z A   rename workgroup
;

;;;; Info from Eric Ludlam...

; When you get errors in your configuration, the best thing to do is:
;
; M-x toggle-debug-on-error RET
;
; and get the stack trace which will point at the problem area. Often times that
; is helpful in identifying the configuration issue.
;
; CEDET will try to associate every file with a single project, and all the
; commands that operate in that buffer will be restricted to the bounds of that
; project. For the CScope support, it too will use EDE to identify the root
; directory, and that will help find the cscope.out file, and that is related to
; both the completion and reference tools.
;
; The exception, of course, is the system include path which is usually
; /usr/include or whatever. This is an augmentation to the default system include
; path which is calculated with the GCC support. In one of your C files you can
; do:
;
; M-x semantic-c-describe-environment RET
;
; and that should show what Semantic will try to use.
;
; To double check if CScope is being used for code completion, you can check with:
;
; M-x semanticdb-find-test-translate-path RET
;
; and check the end of the list for some CScope thing.


;;;; TODO -- Todo List
;
;x 1) Cleanup title bar items
;x 2) Cleanup packages versus individual libraries
;  3) Research Template/SRecoder usage
;  4) Verify proper copyright operation
;  5) Verify autofill operation
;  6) Fix CC-mode style
;  7) Research/tune desktop
;x 8) Implement auto save/restore for workgroups
;  9) Add search tags throughout this file to easily find project specific customizations...
;     or better yet, read project specific customizations from a separate file.
; 10) Fix tag folding
;x11) Research/verify eassist functionality
; 12) Research/implement C-Make integration with Semantic, Auto-Complete, and EDE
;x13) Research/implement GNU global support (advantages)
;x14) Research/implement ctags support (advantages)
; 15) Research/implement EDE mode
; 16) Enable JDEE support for Java
; 17) Research/verify/implement GDB utilities and GDB/ECB integration
; 18) Research/debug Neil's shell utilities
; 19) Implement tests for missing packages and workarounds
; 20) Decide on non-window functionality and wrap init.el accordingly
; 21) Test with emacs 24.X on ubuntu
; 22) Test with other major linux distros
; 23) Test with Emacs for MacOS
; 24) Test with Emacs for Windows
; 25) Create install scripts that fetch from github and setup emacs and the GNU toolchain as
;     appropriate to the given OS/environment
;x26) Add basic commands to the list at the beginning of this file
; 27) Test with Emacs versions < 23.X
; 28) Research/implement Ebrowse support (advantages)
; 29) Investigate layout-restore.el
;x30) Investigate helm
; 31) Investigate winner-mode
; 32) 
;


;;;; ******* PROJECT LOCATION INFO *******
(setq myprojdir "~/git/rfs/src")
(setq elpa-root-path "~/.emacs.d/elpa")


;;;; ******* BASIC SETUP *******
;;;; Activate/deactivate various frame modes, mode line, and title bar utilities.
(tool-bar-mode -1)
(menu-bar-mode 1)
(scroll-bar-mode -1)
;(fringe-mode "minimal")
(display-time)
(setq global-mark-ring-max 64)
(setq mark-ring-max 64)

;(setq frame-title-format (list "%b - " (getenv "LOGNAME") "@" system-name))
(setq frame-title-format (list "EMACS [%b - " (getenv "LOGNAME") "]"))
(setq user-mail-address "Tom.Dings@SimpliVity.com")
;(put 'narrow-to-region 'disabled nil)
;(global-unset-key "\C-z")               ;minimize screws with Unity mode
;(global-set-key (kbd "M-C-f") 'grep-find)
;;; (setq grep-find-command "find . -type f -name '*~' -prune -o -print0 | "xargs" -0 -e grep -nH -e ")
;(server-start)

;;;; Libraries
(setq load-path (append (list nil "~/.emacs.d/elisp") load-path))
;(load-library "google-c-style")
					;(load-library "git")
;(load-library "git-blame")
;(load-library "ssh.el")

;;;; ******* EMACS PACKAGE MANAGER *******
(require 'cl)
(require 'package)
;(load-file "~/.emacs.d/package/package.elc")
;(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
;                         ("marmalade" . "http://marmalade-repo.org/packages/")
;                         ("melpa" . "http://melpa.milkbox.net/packages/")))
;; Any add to list for package-archives (to add marmalade or melpa) goes here
;(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(defvar current-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(when (version< emacs-version "24.1")
  (error "Prelude requires at least GNU Emacs 24.1, but you're running %s" emacs-version))

;; Always load newest byte code
(setq load-prefer-newer t)

(defvar prelude-dir (file-name-directory load-file-name)
  "The root dir of the Emacs Prelude distribution.")
(defvar prelude-core-dir (expand-file-name "core" prelude-dir)
  "The home of Prelude's core functionality.")
(defvar prelude-modules-dir (expand-file-name  "modules" prelude-dir)
  "This directory houses all of the built-in Prelude modules.")
(defvar prelude-personal-dir (expand-file-name "personal" prelude-dir)
    "This directory is for your personal configuration.
Users of Emacs Prelude are encouraged to keep their personal configuration
changes in this directory.  All Emacs Lisp files there are loaded automatically
by Prelude.")
(defvar prelude-personal-preload-dir (expand-file-name "preload" prelude-personal-dir)
  "This directory is for your personal configuration, that you want loaded before Prelude.")
(defvar prelude-vendor-dir (expand-file-name "vendor" prelude-dir)
  "This directory houses packages that are not yet available in ELPA (or MELPA).")
(defvar prelude-savefile-dir (expand-file-name "savefile" prelude-dir)
  "This folder stores all the automatically generated save/history-files.")
(defvar prelude-modules-file (expand-file-name "prelude-modules.el" prelude-dir)
  "This files contains a list of modules that will be loaded by Prelude.")

(unless (file-exists-p prelude-savefile-dir)
  (make-directory prelude-savefile-dir))

(defun prelude-add-subfolders-to-load-path (parent-dir)
  "Add all level PARENT-DIR subdirs to the `load-path'."
  (dolist (f (directory-files parent-dir))
    (let ((name (expand-file-name f parent-dir)))
      (when (and (file-directory-p name)
		 (not (string-prefix-p "." f)))
	(add-to-list 'load-path name)
	(prelude-add-subfolders-to-load-path name)))))

;; add Prelude's directories to Emacs's `load-path'
(add-to-list 'load-path prelude-core-dir)
(add-to-list 'load-path prelude-modules-dir)
(add-to-list 'load-path prelude-vendor-dir)
(prelude-add-subfolders-to-load-path prelude-vendor-dir)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; preload the personal settings from `prelude-personal-preload-dir'
(when (file-exists-p prelude-personal-preload-dir)
  (message "Loading personal configuration files in %s..." prelude-personal-preload-dir)
  (mapc 'load (directory-files prelude-personal-preload-dir 't "^[^#].*el$")))

(defvar prelude-packages
  '(git git-blame color-theme)
  "A list of packages to ensure are installed at launch.")

(defun prelude-packages-installed-p ()
  (loop for p in prelude-packages
	when (not (package-installed-p p)) do (return nil)
	finally (return t))
  "Returns true if all packages in prelude-packages are installed, false otherwise.")

(unless (prelude-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs Prelude is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p prelude-packages)
    (when (not (package-installed-p p))
      (package-install p)
      (message "%s%s" "Installed package" p))))

(provide 'prelude-packages)
;;; prelude-packages.el ends here


(message "Loading Prelude's core...")

;; the core stuff
;(require 'prelude-packages)
;(require 'prelude-custom)  ;; Needs to be loaded before core, editor and ui
;(require 'prelude-ui)
;(require 'prelude-core)
;(require 'prelude-mode)
;(require 'prelude-editor)
;(require 'prelude-global-keybindings)

;; OSX specific settings
;(when (eq system-type 'darwin)
;  (require 'prelude-osx))

;(message "Loading Prelude's modules...")

;; the modules
(when (file-exists-p prelude-modules-file)
  (load prelude-modules-file))

;; config changes made through the customize UI will be store here
(setq custom-file (expand-file-name "custom.el" prelude-personal-dir))

;; load the personal settings (this includes `custom-file')
(when (file-exists-p prelude-personal-dir)
  (message "Loading personal configuration files in %s..." prelude-personal-dir)
  (mapc 'load (directory-files prelude-personal-dir 't "^[^#].*el$")))

;(message "Prelude is ready to do thy bidding, Master %s!" current-user)

;(prelude-eval-after-init
 ;; greet the use with some useful tip
;  (run-at-time 5 nil 'prelude-tip-of-the-day))



;;;; Templates
;(require 'template)
;(template-initialize)

;;;; Copyright
(require 'copyright)
(add-hook 'before-save-hook 'copyright-update)

;;;; Auto-fill
(setq-default fill-column 80)
(setq auto-fill-mode 1)


;; ******* SETUP CODING STANDARDS *******
;; Use standard linux kernel coding style, but uses spaces instead of
;; tabs for indentation.
;(add-hook 'c-mode-hook '(lambda () (c-set-style "bsd")
;(setq c-default-style "linux"
;      c-basic-offset 4)
;(setq-default indent-tabs-mode nil)
(c-add-style "modified-google"
             '("google-c-style"
               (c-basic-offset . 4)
               (standard-indent . 4)
               (indent-tabs-mode nil)
               (c-offsets-alist
                (access-label . +))))
;(setq-default indent-tabs-mode nil)
;(setq standard-indent 4)
;(setq indent-tabs-mode nil)
;(setq c-default-style "google"
;      c-basic-offset 4)
;(add-hook 'c-mode-hook 'google-set-c-style)
;(add-hook 'c-mode-hook 'google-make-newline-indent)
(add-hook 'c++-mode-hook '(lambda () (c-toggle-auto-state 1)))
(add-hook 'c-mode-hook '(lambda () (c-toggle-auto-state 1)))
(show-paren-mode 1)


;; ******* ENABLE A COLOR THEME *******
;(unless (member "color-theme" package-alist) (package-install "color-theme"))
;(require 'color-theme)
(color-theme-initialize)
					;(color-theme-oswald)
;(if ((display-color-cells) 20)
    (color-theme-tty-dark)
;  (color-theme-calm-forest))

(setq my-color-themes color-themes) ; Start with all of them


(defun car-theme () ;figure out if we need car or caar
  (interactive)
  (cond
   ((consp (car theme-current))
    (caar theme-current))
   (t
            (car theme-current))))
(defun my-theme-set-default () ; Set the first row
  (interactive)
  (setq theme-current my-color-themes)
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

(setq theme-current my-color-themes)
(setq color-theme-is-global nil) ; Initialization
    (my-theme-set-default)
(global-set-key [f10] 'my-theme-cycle)

; ******* Favorite color-themes *******

;; Dark colored themes...
;(color-theme-dark-laptop)

;(color-theme-goldenrod)
;(color-theme-oswald)
;(color-theme-jsc-dark)
;(color-theme-taylor)
;(color-theme-comidia)
;(color-theme-calm-forest)
;(color-theme-matrix)
;(color-theme-lawrence)

;; Medium colored themes...
;(color-theme-classic)
;(color-theme-kingsajz)
;(color-theme-calm-forest)

					;(color-theme-robin-hood)
;(color-theme-gnome2)

;; Light colored themes...


					;(color-theme-wheat)
;(color-theme-pierson)
;(color-theme-emacs21)

;; A big list of color themes
;(color-theme-arjen)
;(color-theme-calm-forest)
;(color-theme-clarity)
;(color-theme-classic)
;(color-theme-comidia)
;(color-theme-dark-laptop)
;(color-theme-digital-ofs1)
;(color-theme-euphoria)
;(color-theme-example)
;(color-theme-gnome)
;(color-theme-gnome2)
;(color-theme-goldenrod)
;(color-theme-gray30)
;(color-theme-hober)
;(color-theme-infodoc)
;(color-theme-jsc-dark)
;(color-theme-lawrence)
;(color-theme-ld-dark)
;(color-theme-lethe)
;(color-theme-marquardt)
;(color-theme-matrix)
;(color-theme-midnight)
;(color-theme-molokai)
;(color-theme-oswald)
;(color-theme-pierson)
;(color-theme-renegade)
;(color-theme-retro-green)
;(color-theme-retro-orange)
;(color-theme-robin-hood)
;(color-theme-rotor)
;(color-theme-shaman)
;(color-theme-sitaramv-solaris)
;(color-theme-standard)
;(color-theme-subtle-blue)
;(color-theme-subtle-hacker)
;(color-theme-taylor)
;(color-theme-tty-dark)
;(color-theme-whateveryouwant)
;(color-theme-wheat)
;(color-theme-wordperfect)
;(color-theme-xemacs)


;;;; Desktop
;;;; Save the last desktop configuration and buffer list across emacs
;;;; sessions.  Note: if you wish to temporarily come up "clean", use
;;;; --no-desktop when starting emacs from the command line.  See
;;;; saving Emacs Sessions in the manual (C-h r)...
;(require 'desktop)
;(setq desktop-path '("~/.emacs.d/"))
;(setq desktop-buffers-not-to-save
;      (concat "\\("
;	      "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
;	      "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
;	      "\\)$"))
;(add-to-list 'desktop-modes-not-to-save 'dired-mode)
;(add-to-list 'desktop-modes-not-to-save 'Info-mode)
;(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
;(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
;
;(desktop-save-mode 1)


;;;; ******* Sublimity File Map *******
;;;; This provides functionality similar to the scrolling file map in Sublime
;;;; Text.  Good eye-candy, but haven't found a good use for this feature yet.
;;;; Keeping this in init.el commented out in case a good use arrives in the
;;;; future.
;(require 'sublimity)
;(require 'sublimity-scroll)
;(require 'sublimity-map)
;(sublimity-global-mode)
; To enable manually from within emacs, use: M-x sublimity-mode


;;;; ******* CMAKE UTILITIES *******
;;;; Add CMake utilities... cpputils_cmake from Marmalade...
;(add-hook 'c++-mode-hook (lambda () (cppcm-reload-all)))
;(add-hook 'c-mode-hook (lambda () (cppcm-reload-all)))
;; OPTIONAL, somebody reported that they can use this package with Fortran
;(add-hook 'c90-mode-hook (lambda () (cppcm-reload-all)))


;;;; ******* GDB STUFF *******
;;;; OPTIONAL, avoid typing full path when starting gdb
(global-set-key (kbd "C-c C-g")
 '(lambda ()(interactive) (gud-gdb ("~/git/scripts/coredebug/coredebug -o --fullname ")))
)


;;;; ******* SETUP TAGS-FILE *******
;;;; Not needed when using CEDET/Semantic.
;(setq tags-table-list '("/home/tdings/git/rfs/"))
;(setq tags-file-name "/home/tdings/git/rfs/")


;;;; ******* ASCOPE *******
;;;; Cscope interface for emacs.  Useful for debugging the source parsing, but
;;;; not needed when using CEDET/Semantic.  See below.
;(require 'ascope)
;(ascope-init (file-name-as-directory myprojdir))


;;;; ******* CEDET *******
;;;; Note that this is a pre-requisite for ECB.  The following should
;;;; work on a fresh checkout of cedet using bzr scm
(setq cedet-root-path (file-name-as-directory "~/.emacs.d/cedet/"))
;(load-file (concat cedet-root-path "cedet-devel-load.el"))
(add-to-list 'load-path (concat cedet-root-path "contrib"))

;; Enable the following submodes/features of Semantic...
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)                     ; Global support for Semantic DB
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)                ; Automatic tag bookmarking to enable jumping around
;(add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode)                       ; Enable context menu bound to right mouse button
;(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)              ; Highlight first line of function/class
;(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)                  ; Show current tag in top line of buffer 
(add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)                  ; Decorate tags with separate styles
(add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode) ; Highlight local names that match cursor
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)              ; Parse source during idle time
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode)            ; Display possible name completions during idle time
;(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)                ; Display summary info about current tag when idle
(add-to-list 'semantic-default-submodes 'global-semantic-tag-folding-mode)                 ; Allows source code to be rolled up and down as desired

;; Use tag-folding-mode with decoration-mode.
;(defun do-after-decorate () (semantic-tag-folding-mode t) )
;(add-hook 'semantic-decoration-mode-hook 'do-after-decorate) 

;; Activate Semantic
(semantic-mode 1)
;(semantic-tag-folding-mode 1)

;; Add advanced completion functionality, etc...
(require 'semantic/ia)           ; names completion and display of tags
(require 'semantic/bovine/gcc)   ; auto locate system include files
(require 'semantic/sb)           ; Semantic integration with Speedbar
;(require 'semantic/bovine/clang) ; use clang as completion engine
 
;; Add search paths for 3rd party include files, etc...
(semantic-add-system-include "/usr/include/boost/" 'c++-mode)
;(semantic-add-system-include "/usr/include/" 'c++-mode)
;(semantic-add-system-include "/usr/include/" 'c-mode)
 
;; Add search paths for project root
;(semantic-add-project-root "/home/tdings/git/rfs/")

;; Load the contrib library
;(require 'eassist)
 
;; Implementing my own copy of this function since it is required by
;; semantic-ia-fast-jump but this function is not defined in etags.el
;; of GNU emacs (Found at: http://tsengf.blogspot.com/2011/06/semantic-ia-fast-jump-doesnt-push-tag.html.)
;(require 'etags)
;(unless (fboundp 'push-tag-mark)
;  (defun push-tag-mark ()
;    "Push the current position to the ring of markers so that
;    \\[pop-tag-mark] can be used to come back to current position."
;    (interactive)
;    (ring-insert find-tag-marker-ring (point-marker))
;    )
;  )


(defun my-cedet-hook ()
  ;(local-set-key [(control return)] 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-c=" 'semantic-decoration-include-visit)
  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-cb" 'semantic-mrub-switch-tags)
  (local-set-key "\C-cq" 'semantic-ia-show-doc)
  (local-set-key "\C-cs" 'semantic-ia-show-summary)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
  (local-set-key "\C-c+" 'semantic-tag-folding-show-block)
  (local-set-key "\C-c-" 'semantic-tag-folding-fold-block)
  (local-set-key "\C-c\C-c+" 'semantic-tag-folding-show-all)
  (local-set-key "\C-c\C-c-" 'semantic-tag-folding-fold-all)
  )
(add-hook 'c++-mode-hook 'my-cedet-hook)
(add-hook 'c-mode-hook 'my-cedet-hook)
(add-hook 'lisp-mode-hook 'my-cedet-hook)
(add-hook 'scheme-mode-hook 'my-cedet-hook)
(add-hook 'emacs-lisp-mode-hook 'my-cedet-hook)
(add-hook 'erlang-mode-hook 'my-cedet-hook)


(defun my-sr-speedbar-hook ()
  )
(add-hook 'c++-mode-hook 'my-sr-speedbar-hook)
(add-hook 'c-mode-hook 'my-sr-speedbar-hook)
(add-hook 'lisp-mode-hook 'my-sr-speedbar-hook)
(add-hook 'scheme-mode-hook 'my-sr-speedbar-hook)
(add-hook 'emacs-lisp-mode-hook 'my-sr-speedbar-hook)
(add-hook 'erlang-mode-hook 'my-sr-speedbar-hook)

(defun my-c-mode-cedet-hook ()
  (local-set-key "." 'semantic-complete-self-insert)
  (local-set-key ">" 'semantic-complete-self-insert)
  (local-set-key "\C-ct" 'eassist-switch-h-cpp)
  (local-set-key "\C-xt" 'eassist-switch-h-cpp)
  (local-set-key "\C-ce" 'eassist-list-methods)
  (local-set-key "\C-c\C-r" 'semantic-symref)
  (add-to-list 'ac-sources 'ac-source-gtags)
  (add-to-list 'ac-sources 'ac-source-semantic)
  )
(add-hook 'c++-mode-hook 'my-c-mode-cedet-hook)
(add-hook 'c-mode-hook 'my-c-mode-cedet-hook)
 
;; Imenu support
(defun my-semantic-imenu-hook()
  (imenu-add-to-menubar "TAGS"))
(add-hook 'semantic-init-hooks 'my-semantic-imenu-hook)

;; gnu global support
(require 'semantic/db-global)
(when (cedet-gnu-global-version-check t)
  (semanticdb-enable-gnu-global-databases 'c++-mode t)
  (semanticdb-enable-gnu-global-databases 'c-mode t)
  (cedet-gnu-global-create/update-database (file-name-as-directory myprojdir)))
 
;; IDutils support
(require 'semantic/symref/idutils)
(when (cedet-idutils-version-check t)
  (cedet-idutils-create/update-database (file-name-as-directory myprojdir)))

;; cscope support
;(require 'semantic/db-cscope)
;(when (cedet-cscope-version-check t)
;  (semanticdb-enable-cscope-databases)
;  (cedet-cscope-create/update-database (file-name-as-directory myprojdir)))

;; exuberant ctags support
;(when (cedet-ectag-version-check t)
;  (semanticdb-enable-ectags)
;  (semantic-load-enable-secondary-ectags-support))

;; ebrowse support... no need for a check since ebrowse is part of emacs
;(require 'semantic/db-ebrowse)
;(semanticdb-create-ebrowse-database (file-name-as-directory myprojdir))
;(semanticdb-load-ebrowse-caches)


;; SRecode template support 
;(global-srecode-minor-mode 1)            ; Enable template insertion menu

;; Enable EDE for a pre-existing C++ project... this needs to be debugged...
;; use of EDE requires esoteric knowledge to function properly
;(global-ede-mode 'nil)  ; Do NOT use the project management system
(global-ede-mode t)
(setq svtfs-proj-root-file (concat myprojdir "/CMakeLists.txt"))
(ede-cpp-root-project "SVT"
                      :name "SVTFS Project"
                      :file svtfs-proj-root-file
		     :system-include-path '( "/usr/include/boost/" )
		     )


;; (setq ede-cmake-cpp-project-root myprojdir)
;; (setq svtfs-proj-build-root "/var/tmp/build")
;; (add-to-list 'load-path (concat elpa-root-path "ede-cmake"))
;; (require 'ede-cmake)
;; ;; Example only
;; (defvar my-project-root-build-directories
;;   '(("None" . svtfs-proj-build-root)
;;     ("Debug" . svtfs-proj-build-root)
;;     ("Release" . svtfs-proj-build-root))
;;   "Alist of build directories in the project root"
;;   )
;; 
;; (defun my-project-root-build-locator (config root-dir)
;;   "Locates a build directory in the project root, uses
;;  project-root-build-directories to look up the name."
;;   (cdr (assoc config my-project-root-build-directories)))
;; 
;; (defun my-load-project (dir)
;;   "Load a project of type `ede-cmake-cpp-project' for the directory DIR.
;;       Return nil if there isn't one."
;;   (ede-cmake-cpp-project 
;;    (file-name-nondirectory (directory-file-name dir))
;;    :directory dir
;;    :locate-build-directory 'my-project-root-build-locator
;;    :build-tool (cmake-make-build-tool "Make" :additional-parameters "-j4 -kr")
;;    :include-path '( "/" )
;;    :system-include-path (list (expand-file-name "external" dir) )
;;    ))
;; 
;; (ede-add-project-autoload
;;  (ede-project-autoload "CMake"
;; 		      :file 'ede-cmake
;; 		      :proj-file "CMakeLists.txt"
;; 		      :proj-root ede-cmake-cpp-project-root
;; 		      :proj-root-dirmatch ""
;; 		      :load-type 'my-load-project
;; 		      :class-sym 'ede-cmake-cpp-project)
;;  'unique)

;; Enable JAVA support
;(require 'cedet-java)
;(when (cedet-java-version-check t)
;  ())
;(add-to-list 'load-path (concat elpa-root-path "jdee-2.4.1/lisp"))
;(require 'jde)
;(load "jde")


;;;; ******* ECB (CEDET must be installed first) *******
; Emacs Code Browser... adds excellent modes for browsing code during the learning/software construction
; phase of software development.
;
(add-to-list 'load-path "~/.emacs.d/ecb")
(require 'ecb)
(setq ecb-tip-of-the-day nil)
;(ecb-hide-ecb-windows)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-default-style "linux")
; '(ecb-layout-name "gv8")
; '(ecb-layout-window-sizes (quote (("gv8" (ecb-analyse-buffer-name 0.14218009478672985 . 0.33962264150943394) (ecb-methods-buffer-name 0.14218009478672985 . 0.6415094339622641) (ecb-speedbar-buffer-name 0.14218009478672985 . 0.9811320754716981)) ("gv7" (ecb-directories-buffer-name 0.1092436974789916 . 0.24193548387096775) (ecb-sources-buffer-name 0.1092436974789916 . 0.27419354838709675) (ecb-methods-buffer-name 0.1092436974789916 . 0.46774193548387094)) ("gv3" (ecb-directories-buffer-name 0.1703056768558952 . 0.26153846153846155) (ecb-history-buffer-name 0.2183406113537118 . 0.12307692307692308) (ecb-sources-buffer-name 0.2183406113537118 . 0.13846153846153847) (ecb-methods-buffer-name 0.2096069868995633 . 0.26153846153846155) (ecb-analyse-buffer-name 0.22270742358078602 . 0.13846153846153847) (ecb-symboldef-buffer-name 0.22270742358078602 . 0.12307692307692308) (ecb-default-buffer-name 0.1965065502183406 . 0.26153846153846155)) ("gv2" (ecb-directories-buffer-name 0.2149122807017544 . 0.18032786885245902) (ecb-sources-buffer-name 0.2149122807017544 . 0.22950819672131148) (ecb-methods-buffer-name 0.2149122807017544 . 0.3442622950819672) (ecb-analyse-buffer-name 0.2149122807017544 . 0.22950819672131148)) ("left8" (ecb-directories-buffer-name 0.2324561403508772 . 0.1935483870967742) (ecb-sources-buffer-name 0.2324561403508772 . 0.3064516129032258) (ecb-methods-buffer-name 0.2324561403508772 . 0.3387096774193548) (ecb-history-buffer-name 0.2324561403508772 . 0.14516129032258066)))))
 '(ecb-new-ecb-frame nil)
 '(ecb-options-version "2.40")
 '(ecb-process-non-semantic-files (quote nil))
 '(ecb-source-path (quote ("/home/tdings/git/rfs/src")))
 '(ecb-vc-enable-support t)
 '(ede-project-directories (quote ("/home/tdings/git/rfs")))
 '(helm-gtags-auto-update t)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-path-style (quote relative))
 '(semanticdb-project-roots (quote ("/home/tdings/git/rfs")))
 '(which-function-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Remove the annoying file dialog box from within ECB.
(setq use-file-dialog nil)  ; Do NOT use the operating system's file dialog box.


;;;; ******* ADD AUTO-COMPLETE *******
;(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-20130724.1750/dict")
(require 'auto-complete-config)
(ac-config-default)


;;;; Shells - Thanks to Neil Swinton for this functionality!
;
;(setq svt-common-commands (list "cd /home/tdings/git/rfs"))
;
;(setq svtsetup-commands (list "sudo su" "source svtsetup" "cd $SVTBUILD"))
;
;(setq svt-root-bash-commands (list 
;                              "PS1='${debian_chroot:+($debian_chroot)}\\t \\u@\\h:\\w\\$ '"
;                              "export PATH=$PATH:/home/tdings/git/scripts/bin"
;                              "source /home/tdings/.bash_aliases"
;                              "source /home/tdings/.bash_functions"
;                              "export SVTBUILDSTATUS=0"
;                              )
;)
;
;(setq svt-build-bash-commands (list "cd /var/tmp/build" "/home/tdings/bin/svtbuild.py -d"))
;
;
;(defun svt-shell (name commands)
;  (unless (get-buffer name)
;    (shell (get-buffer-create name))
;    (switch-to-buffer name)
;    (dolist (command commands)
;      (comint-send-string (current-buffer) (concat command "\n"))
;      (comint-send-string (current-buffer) "\n")
;      )
;    )
;  )
;
;(defun svt-shells ()
;  "Create a user shell and a build shell and a root shell"
;  (interactive)
;
;  ;; Create the shells we want
;  (svt-shell "*shell*" svt-common-commands)
;  ;; (svt-shell "*build*" (append svt-common-commands svtsetup-commands svt-root-bash-commands svt-build-bash-commands))
;  (svt-shell "*root*"  (append svt-common-commands svtsetup-commands svt-root-bash-commands))
;
;  ;; Put the build buffer & window out of sight
;  ;;(bury-buffer "*build*")
;  ;; (replace-buffer-in-windows "*build*")
;  )
;
;(defun svt-build-shell ()
;  "Create a build shell"
;  (interactive)
;  (save-window-excursion
;    (svt-shell "*build*" (append svt-common-commands svtsetup-commands svt-root-bash-commands svt-build-bash-commands))
;    )
;  )
;
;(defun svt-iometer-shell ()
;  "Create a shell for iometer data handling"
;  (interactive)
;  (svt-shell "*iometer*" (append svt-common-commands))
;  )
;
;(defun svt-iohistory-shell ()
;  "Create a shell for iometer data handling"
;  (interactive)
;  (svt-shell "*iohistory*" (append svt-common-commands))
;  (cd "~/scratch")
;  )
;
;(defun my-current-directory (text)
;  (if (string-match "[a-z0-9@\-]*:\([^$]+\)" text)
;      (setq cur-dir (substring text (match-beginning 1) (match-end 1)))
;    (cd cur-dir)
;    (message "dir tracking %s" cur-dir)))
;(defun my-shell-setup ()
;  "Track current directory"
;  (add-hook 'comint-output-filter-functions 'my-current-directory nil t))
;
;;; (setq shell-mode-hook 'my-shell-setup)
;(add-hook 'shell-mode-hook
;          (lambda()
;            (setq dirtrack-list '("^[0-9]+:[0-9][0-9]:[0-9][0-9]^.*[^ ]+:\\(.*\\)[\$\#] " 1 nil))
;            (shell-dirtrack-mode )))
;
;(defun unix-line-endings()
;  "Change to Unix endings."
;  (interactive)
;  (set-buffer-file-coding-system 'iso-latin-1-unix t)
;)
;
;(defun dos-line-endings()
;  "Change to DOS line endings."
;  (interactive)
;  (set-buffer-file-coding-system 'iso-latin-1-dos t)
;)
;
;(defun mac-line-endings()
;  "Change to Mac line endings."
;  (interactive)
;  (set-buffer-file-coding-system 'iso-latin-1-mac t)
;)
;
;(defun sva(arg)
;  "Log in two sessions to the sva"
;  (interactive "sHost: ")
;  (ssh arg (concat "*ssh-" arg "*"))
;  (ssh arg (concat "*ssh-log" arg "*"))
;)
;
;(defun new-shell(name)
;  "Create a new, named, shell buffer"
;  (interactive "sName: ")
;  (shell (get-buffer-create name))
;  (switch-to-buffer name)
;  )
;
;
;(defun donas()
;  "Log in two sessions to the sva"
;  (interactive)
;  (ssh "donas" "*ssh-donas*")
;  (ssh "donas" "*ssh-logdonas*")
;)
;(custom-set-variables
;  ;; custom-set-variables was added by Custom.
;  ;; If you edit it by hand, you could mess it up, so be careful.
;  ;; Your init file should contain only one such instance.
;  ;; If there is more than one, they won't work right.
; '(c-auto-align-backslashes t)
; '(compile-command "~tdings/bin/svtbuild.py storage")
; '(safe-local-variable-values (quote ((c-default-style . "bsd"))))
; '(sort-fold-case t))
;(custom-set-faces
;  ;; custom-set-faces was added by Custom.
;  ;; If you edit it by hand, you could mess it up, so be careful.
;  ;; Your init file should contain only one such instance.
;  ;; If there is more than one, they won't work right.
; )
;
;(defun errno()
;  "Open the base errno file on ubuntu"
;  (interactive)
;  (find-file-read-only "/usr/include/asm-generic/errno-base.h")
;)

;;;; ******* CL-LIB *******
(add-to-list 'load-path (concat elpa-root-path "cl-lib-0.3"))


;;;; ******* SR-SPEEDBAR *******
(add-to-list 'load-path (concat elpa-root-path "sr-speedbar-20131207.2049"))
(setq sr-speedbar-max-width 100)
(setq sr-speedbar-width-x 60)
(setq sr-speedbar-width-console 60)
(global-set-key "\C-c\C-cso" 'sr-speedbar-open)
(global-set-key "\C-c\C-csc" 'sr-speedbar-close)
(global-set-key "\C-c\C-cst" 'sr-speedbar-toggle)
(global-set-key "\C-c\C-csr" 'sr-speedbar-refresh-turn-on)


;;;; ******* GIT *******
(global-set-key "\C-c\C-cgs" 'git-status)
(global-set-key "\C-c\C-cgb" 'git-blame-mode)


;;;; ******* HELM *******
(add-to-list 'load-path (concat elpa-root-path "helm-20140125.1101"))
(require 'helm-config)
(helm-mode t)
(helm-gtags-mode t)

;; Enable helm-gtags-mode
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

;; customize


;(eval-after-load "helm-gtags"
;  '(progn
;     (define-key helm-gtags-mode-map (kbd "\C-chgm") 'helm-global-mark-ring)
;     (define-key helm-gtags-mode-map (kbd "\C-cham") 'helm-all-mark-rings)
;     (define-key helm-gtags-mode-map (kbd "\C-chgg") 'helm-git-grep)
;     (define-key helm-gtags-mode-map (kbd "\C-cht") 'helm-gtags-find-tag)
;     (define-key helm-gtags-mode-map (kbd "\C-chr") 'helm-gtags-find-rtag)
;     (define-key helm-gtags-mode-map (kbd "\C-chs") 'helm-gtags-find-symbol)
;     (define-key helm-gtags-mode-map (kbd "\C-c\C-c<") 'helm-gtags-previous-history)
;     (define-key helm-gtags-mode-map (kbd "\C-c\C-c>") 'helm-gtags-next-history)
;     (define-key helm-gtags-mode-map (kbd "\M-,") 'helm-gtags-pop-stack)))

;(eval-after-load "helm-gtags"
;  '(progn
;     (define-key helm-gtags-mode-map ("\C-chgm")   'helm-global-mark-ring)
;     (define-key helm-gtags-mode-map ("\C-cham")   'helm-all-mark-rings)
;     (define-key helm-gtags-mode-map ("\C-chgg")   'helm-git-grep)
;     (define-key helm-gtags-mode-map ("\C-cht")    'helm-gtags-find-tag)
;     (define-key helm-gtags-mode-map ("\C-chr")    'helm-gtags-find-rtag)
;     (define-key helm-gtags-mode-map ("\C-chs")    'helm-gtags-find-symbol)
;     (define-key helm-gtags-mode-map ("\C-c\C-c<") 'helm-gtags-previous-history)
;     (define-key helm-gtags-mode-map ("\C-c\C-c>") 'helm-gtags-next-history)
;     (define-key helm-gtags-mode-map ("\M-,")      'helm-gtags-pop-stack)))

(global-set-key "\C-chgm"   'helm-global-mark-ring)
(global-set-key "\C-cham"   'helm-all-mark-rings)
(global-set-key "\C-chgg"   'helm-git-grep)
(global-set-key "\C-cht"    'helm-gtags-find-tag)
(global-set-key "\C-chr"    'helm-gtags-find-rtag)
(global-set-key "\C-chs"    'helm-gtags-find-symbol)
(global-set-key "\C-c\C-c<" 'helm-gtags-previous-history)
(global-set-key "\C-c\C-c>" 'helm-gtags-next-history)
(global-set-key "\M-,"      'helm-gtags-pop-stack)

;(global-set-key "\M-;" 'ww-next-gtag)   ;; M-; cycles to next result, after doing M-. C-M-. or C-M-,
;(global-set-key [(control meta .)] 'helm-gtags-find-rtag)   ;; C-M-. find all references of tag
;(global-set-key [(control meta ,)] 'helm-gtags-find-symbol) ;; C-M-, find all usages of symbol.
;(global-set-key "\M-." 'helm-gtags-find-tag) ;; M-. finds tag


;;;; ******* ORG MODE *******
;(add-to-list 'auto-mode-alist '("\\.org\\" . org-mode))
(global-set-key "\C-col" 'org-store-link)
(global-set-key "\C-coc" 'org-capture)
(global-set-key "\C-coa" 'org-agenda)
(global-set-key "\C-cob" 'org-iswitchb)


;;;; Workgroups - Should be loaded last to properly handle all mode hooks when
;;;; the window layout is restored
(require 'workgroups2)
(setq wg-default-session-file "~/.emacs.d/.emacs_workgroups")
(workgroups-mode 1)

;; Original ordering of the workgroups-related stuff...
;(require 'workgroups2)
;;; if you start as "emacs --daemon" - turn off autoloading of workgroups:
;(setq wg-default-session-file "~/.emacs.d/.emacs_workgroups")
;
;;(setq wg-use-default-session-file nil)(setq wg-prefix-key (kbd "C-c z"))
;(workgroups-mode 1)
;(wg-create-workgroup "prjct-wg")

;;; Save window layout when emacs is killed
;(add-hook 'kill-emacs-hook
;	  '(lambda ()
;	     ; if ecb activated, then
;	     ; name the workgroup prjct-wg
;	     ; if shown, toggle the ecb windows to "off"
;	     ; if shown, toggle the ecb compile window to "off"
;	     ; if not in gdb mode (sanity check), then
;	     ; save the workgroup with the name supplied above
;	     (wg-save-session-on-emacs-exit)))
;
;
;;; Save window layout when ecb is deactivated
;(add-hook 'ecb-deactivate-hook
;	  '(lambda ()
;	     ; name the workgroup prjct-wg
;	     ; if shown, toggle the ecb windows to "off"
;	     ; if shown, toggle the ecb compile window to "off"
;	     ; if not in gdb mode (sanity check), then
;	     ; save the workgroup with the name supplied above
;	     (wg-save-session-on-emacs-exit)))
;
;
;
;;; Restore window layout when ecb is activated
;
;;; If you wish to develop without the ecb windows shown (to maximize screen
;;; real-estate, for example), then simply toggle the ecb windows while ecb
;;; remains activated.
;(add-hook 'ecb-activate-hook
;	  '(lambda ()
;	     ; name the workgroup prjct-wg
;	     ; restore the workgroup with the name supplied above
;	     (wg-save-session-on-emacs-exit)))


;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;(setq frame-background-mode 'dark)

