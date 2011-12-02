;;; tabula-rasa-mode.el - Distraction free writing

(defcustom tr-width 80
  "Width of writing space."
  :type 'integer
  :group 'tabula-rasa
  :set (lambda (symbol value)
         (progn (setq tr-width value)
                (tr-update-window)))
  :initialize 'custom-initialize-default)

(defface tabula-rasa
  '((t (
     :family "IM FELL English Pro"
     :foreground "black"
     :background "ivory"
     :height 140
    )))
  "Face for tabula-rasa mode."
  :group 'tabula-rasa)

(defun tr-update-window()
  (cond 
   ((not (terminal-live-p tr-frame))
    (tr-mode-disable))
   ((or (one-window-p t tr-frame) (window-full-width-p tr-window))
    (set-window-margins tr-window
                        (/ (- (frame-width tr-frame) tr-width) 2)              
                        (/ (- (frame-width tr-frame) tr-width) 2)))
   (t
    (set-window-margins tr-window 0 0)
    (set-window-margins (next-window) 0 0))))

;width test: 80 chars
;12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789

(setq tr-mode-enabledp nil)
(setq tr-frame nil)

(defun tr-mode ()
  (interactive)
  (cond 
   (tr-mode-enabledp (tr-mode-disable))
   (t (tr-mode-enable))))

;; Emacs bug? It takes 2 calls to set bg color to set frame margin colors.
(defun tr-set-frame-parms ()
  (interactive)
  (modify-frame-parameters tr-frame 
                           `(
                             (foreground-color . ,(face-attribute 'tabula-rasa :foreground))
                             (background-color . ,(face-attribute 'tabula-rasa :background))
                             )))

(defun tr-mode-enable()
  (setq tr-frame (make-frame `(
                               (fullscreen . fullboth)
                               (unsplittable . t)
                               (left-fringe . 0)
                               (right-fringe . 0)
                               (tool-bar-lines . 0)
                               (menu-bar-lines . 0)
                               (vertical-scroll-bars . nil)
                               (line-spacing . 5)
                               (font . ,(face-font "tabula-rasa"))
                               (foreground-color . ,(face-attribute 'tabula-rasa :foreground))
                               (background-color . ,(face-attribute 'tabula-rasa :background))
                               )))
  (tr-set-frame-parms)
  (setq tr-window (frame-selected-window tr-frame))
  (add-hook 'window-configuration-change-hook 'tr-update-window t nil)
  (add-hook 'kill-buffer-hook 'tr-mode-disable)
  (add-hook 'delete-frame-functions 'tr-mode-disable)
  (tr-update-window)
  (setq tr-mode-enabledp t)
  (global-highline-mode 0)

)

(defun tr-mode-disable()
  (remove-hook 'window-configuration-change-hook 'tr-update-window)
  (remove-hook 'kill-buffer-hook 'tr-mode-disable)  
  (remove-hook 'delete-frame-functions 'tr-mode-disable)
  (delete-frame tr-frame)
  (global-highline-mode 1)
  (setq tr-mode-enabledp nil)
)


;;;;;;;;;;;;;;;;; end ;;;;;;;;;;;;;;;;;

(provide 'tabula-rasa)
