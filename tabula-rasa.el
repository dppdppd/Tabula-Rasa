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
  (cond 
   (tr-mode-enabledp (tr-mode-disable))
   (t (tr-mode-enable))))

(defun tr-mode-enable()
  (interactive)
  (setq tr-frame (make-frame '(
                              (fullscreen . fullboth)
                              (unsplittable . t)
                              (vertical-scroll-bars . nil)
                              (left-fringe . 0)
                              (right-fringe . 0)
                              (tool-bar-lines . 0)
                              (menu-bar-lines . 0)
                              (line-spacing . 5)
                              )))
  (setq tr-window (frame-selected-window tr-frame))

  (set-window-dedicated-p tr-window t)
;  (add-hook 'window-configuration-change-hook 'tr-update-window t nil)
;  (tr-buffer-face)
;  (buffer-face-mode 1)

  (tr-update-window)

  (setq tr-mode-enabledp t)
;  (make-local-variable 'tr-mode-enabledp)
  (message (format "tr mode enabled on %s" (selected-frame)))
)

(defun tr-mode-disable()
  (interactive)
  (delete-frame tr-frame)
;  (remove-hook 'window-configuration-change-hook 'tr-update-window)
;  (buffer-face-mode 0)
  (setq tr-mode-enabledp nil)
  (message (format "tr mode disabled on %s" (selected-frame)))

)

;;;;;;;;;;;;;;;;; end ;;;;;;;;;;;;;;;;;

(provide 'tr-mode)

;; todo: why doesn't fullscreen work?
;; todo: make a proper face
;; todo: why doesn't tr.el load?
