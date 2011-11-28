;;; tabula-rasa-mode.el - Distraction free editing mode
;;
;; Usage: M-x tr-mode
;; License: free for all usages/modifications/distributions/whatever.

(require 'cl)

;; ------ configuration -----
(defvar tr-mode-face-foreground "NavajoWhite"
  "The foreground color of the default face")

(defvar tr-mode-width 80
  "Width of writing space.")

;; -------- code start -------
(setq *tr-mode-memtable* (make-hash-table))

(defun* tr-recall (var &optional (frame (selected-frame)))
  (cdr (assoc var (gethash frame *tr-mode-memtable*))))

(defun* tr-remember (var val &optional (frame (selected-frame)))
  (let* ((varlist (gethash frame *tr-mode-memtable*))
	 (target (assoc var varlist)))
    (cond (target
	   (setf (cdr target) val))
	  (t
	   (puthash frame (cons (cons var val)
				varlist) *tr-mode-memtable*)))))

(defun tr-mode-set-enabled(var)
  (tr-remember 'tr-mode-enabled var))

(defun tr-mode-enabledp()
  (tr-recall 'tr-mode-enabled))

(defun tr-mode-update-window()
  (set-window-margins (selected-window)
                      (/ (- (window-width) 80) 2)              
                      (/ (- (window-width) 80) 2)))

(defun tr-mode ()
  (interactive)
  (cond ((tr-mode-enabledp)
	 (tr-mode-disable))
	(t
	 (tr-mode-enable))))

(defun tr-mode-enable()
  (interactive)  
  (delete-other-windows)

  (tr-remember 'background-color (face-background 'default))
  (tr-remember 'foreground-color (face-foreground 'default))
  (tr-remember 'fc-bg-region (face-background 'region))
  (tr-remember 'fc-fg-region (face-foreground 'region))
  (tr-remember 'fc-bg-modeline (face-background 'mode-line))
  (tr-remember 'fc-fg-modeline (face-foreground 'mode-line))
  ; - set colors
  (set-face-foreground 'default "black" (selected-frame))
  (set-face-background 'default "papaya whip" (selected-frame))
  (set-face-foreground 'region "black" (selected-frame))
  (set-face-background 'region "peru" (selected-frame))
  (set-face-foreground 'mode-line "gray15" (selected-frame))
  (set-face-background 'mode-line "black" (selected-frame))
  (set-face-attribute 'default nil
                      :height 160
                      :family "IM FELL English Pro")

  (tr-remember 'left-margin-width
                (default-value 'left-margin-width))
  (tr-remember 'right-margin-width
                (default-value 'right-margin-width))

  (set-window-margins (selected-window)
                      (/ (- (window-width) 80) 2)              
                      (/ (- (window-width) 80) 2))

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
;  (set-face-foreground 'default (tr-recall 'foreground-color) (selected-frame))
;  (set-face-background 'default (tr-recall 'background-color) (selected-frame))
  (set-face-foreground 'region (tr-recall 'fc-fg-region) (selected-frame))
  (set-face-background 'region (tr-recall 'fc-bg-region) (selected-frame))
  (set-face-foreground 'mode-line (tr-recall 'fc-fg-modeline) (selected-frame))
  (set-face-background 'mode-line (tr-recall 'fc-bg-modeline) (selected-frame))
  (set-face-attribute 'default nil
                      :height 90
                      :foreground (tr-recall 'foreground-color)
                      :background (tr-recall 'background-color)
                      :family "Terminus")
  ;; ----- margins
  (set-window-margins (selected-window)
                      (tr-recall 'right-margin-width)
                      (tr-recall 'left-margin-width))
  ; - set
  (setq line-spacing (tr-recall 'tr-line-spacing))

  (tr-mode-recall-frame-size)
  ;; - set
  (tr-mode-set-enabled nil)
  (message (format "tr mode disabled on %s" (selected-frame)))

)

(defun tr-mode-recall-frame-size()
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
