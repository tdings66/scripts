

;;;###autoload
(add-to-list 'auto-mode-alist (cons (purecopy "\\.log\\'")  'log-mode))

(define-derived-mode log-mode fundamental-mode
  (setq font-lock-defaults
        '('(
            ("\\(^[0-9T:\\.-]+Z\\)" . 'font-lock-comment-face)		; Timestamp

	    ("INFO" . 'font-lock-doc-face)				; severity
            ("WARN" . 'font-lock-type-face)				; severity
	    ("ERROR" . 'font-lock-keyword-face)				; severity

	    ("\\(0x[0-9a-f]*\\)" . 'font-lock-builtin-face)		; thread

	    ("\\(\\[[A-z0-9\\.]*\\]\\)" . 'bookmark-menu-heading)		; directory

	    ("\\([A-z0-9_-]+\\.[a-z]+:[0-9]+\\)" . 'font-lock-constant-face)	; source file
           ;("\\([a-z_-]+\\.[a-z]+:[0-9]+\\)" . (log-source-file))	; source file

	    ("\\([0-9a-f]*-[0-9a-f]*-[0-9a-f]*-[0-9a-f]*-[0-9a-f]*\\)" . 'message-cited-text)


	    )
	  )
	)
  (setq mode-name "Log"))

(defun log-source-file (file)
  ;; Choose a different font for different files.
  'font-lock-constant-face)

(provide 'log-mode)









