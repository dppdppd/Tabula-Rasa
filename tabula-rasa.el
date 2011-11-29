;;; tr-mode.el - Distraction free editing mode

;;(require 'cl)

;; ------ configuration -----
(defvar tr-face-foreground "NavajoWhite"
  "The foreground color of the default face")

(defcustom tr-width 80
  "Width of writing space."
  :type 'integer
  :group 'tabula-rasa
  :set (lambda (symbol value)
         (progn (setq tr-width value)
                (tr-update-window)))
  :initialize 'custom-initialize-default
)

;; -------- code start -------
(setq *tr-memtable* (make-hash-table))

(defun* tr-recall (var &optional (frame (selected-frame)))
  (cdr (assoc var (gethash frame *tr-memtable*))))

(defun* tr-remember (var val &optional (frame (selected-frame)))
  (let* ((varlist (gethash frame *tr-memtable*))
	 (target (assoc var varlist)))
    (cond (target
	   (setf (cdr target) val))
	  (t
	   (puthash frame (cons (cons var val)
				varlist) *tr-memtable*)))))

(defun tr-mode-set-enabled(var)
  (tr-remember 'tr-mode-enabled var))

(defun tr-mode-enabledp()
  (tr-recall 'tr-mode-enabled))

(defun tr-update-window()
  (cond 
   ((or (one-window-p t nil) (window-full-width-p))
    (set-window-margins tr-window
                        (/ (- (frame-width) tr-width) 2)              
                        (/ (- (frame-width) tr-width) 2)))
   (t
    (set-window-margins tr-window 0 0)
    (set-window-margins (next-window) 0 0))))

(defun tr-buffer-face()
  (interactive)
  (setq buffer-face-mode-face '(:height 160
                                :family "IM FELL English Pro"
                                :foreground "black"
                                :background "papaya whip"
                                )))

(defun tr-mode ()
  (interactive)
  (cond ((tr-mode-enabledp)
	 (tr-mode-disable))
	(t
	 (tr-mode-enable))))

(defun tr-mode-enable()
  (interactive)
  (setq tr-window (selected-window))
  (delete-other-windows)
; (set-window-dedicated-p tr-window t)
  (add-hook 'window-configuration-change-hook 'tr-update-window t nil)
  (tr-buffer-face)
  (buffer-face-mode 1)

  (tr-remember 'left-margin-width
                (default-value 'left-margin-width))
  (tr-remember 'right-margin-width
                (default-value 'right-margin-width))
  (tr-update-window)

  (tr-remember 'tr-line-spacing 'line-spacing)
  (setq line-spacing 0.0)

  (tr-remember 'frame-width (frame-parameter nil 'width))
  (tr-remember 'frame-height (frame-parameter nil 'height))
  (tr-remember 'frame-left (frame-parameter nil 'left))
  (tr-remember 'frame-top (frame-parameter nil 'top))

  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_eWM_STATE_FULLSCREEN" 0))

  (tr-mode-set-enabled t)
  (message (format "tr mode enabled on %s" (selected-frame)))
)

(defun tr-mode-disable()
  (interactive)
  (select-window tr-window)
;  (set-window-dedicated-p tr-window nil)
  (remove-hook 'window-configuration-change-hook 'tr-update-window)
  (buffer-face-mode 0)

  ;; ----- margins
  (set-window-margins tr-window
                      (tr-recall 'right-margin-width)
                      (tr-recall 'left-margin-width))
  ; - set
  (setq line-spacing (tr-recall 'tr-line-spacing))

  (tr-recall-frame-size)
  ;; - set
  (tr-mode-set-enabled nil)
  (message (format "tr mode disabled on %s" (selected-frame)))

)

(defun tr-recall-frame-size()
  (modify-frame-parameters (selected-frame)
			   `((left . ,(tr-recall 'frame-left))
			     (top . ,(tr-recall 'frame-top))
			     (width . ,(tr-recall 'frame-width))
			     (height . ,(tr-recall 'frame-height)))))

;;;;;;;;;;;;;;;;; end ;;;;;;;;;;;;;;;;;

(provide 'tr-mode)

;; todo: why doesn't fullscreen work?
;; todo: make a proper face
;; todo: why doesn't tr.el load?
