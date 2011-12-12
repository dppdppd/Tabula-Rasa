;;; tabula-rasa-mode.el - Distraction free writing

(defcustom tabula-rasa-width 80
  "Width of writing space."
  :type 'integer
  :group 'tabula-rasa
  :set (lambda (symbol value)
         (progn (setq tabula-rasa-width value)
                (tabula-rasa-update-window)))
  :initialize 'custom-initialize-default)

(defface tabula-rasa
  '((t (
     :height 140
    )))
  "Face for tabula-rasa mode."
  :group 'tabula-rasa)

(defun tabula-rasa-update-window()
;;  (interactive)
  (cond 
   ((not (frame-live-p tabula-rasa-frame))
    (tabula-rasa-mode-disable))
   ((or (one-window-p t tabula-rasa-frame) (window-full-width-p tabula-rasa-window))
    (set-window-margins tabula-rasa-window
                        (/ (- (frame-width tabula-rasa-frame) tabula-rasa-width) 2)              
                        (/ (- (frame-width tabula-rasa-frame) tabula-rasa-width) 2)))
   (t
    (set-window-margins tabula-rasa-window 0 0)
    (set-window-margins (next-window) 0 0))))

;width test: 80 chars
;12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789

(setq tabula-rasa-mode nil)

(define-minor-mode tabula-rasa-mode
  "YADFM: Yet Another Distraction Free Writing Mode" 
  :lighter " TR"
  :init-value nil
  :group 'tabula-rasa
; Awkwardly backwards logic. It would appear that define-minor-mode toggles the mode variable prior to evaluating the body.
  (if tabula-rasa-mode
      (tabula-rasa-mode-enable)
    (tabula-rasa-mode-disable)))

(setq tabula-rasa-frame nil)

;; Emacs bug? It takes 2 calls to set bg color to set frame margin colors.
(defun tabula-rasa-set-frame-parms ()
  (interactive)
  (modify-frame-parameters tabula-rasa-frame 
                           `(
                             (foreground-color . ,(face-attribute 'tabula-rasa :foreground))
                             (background-color . ,(face-attribute 'tabula-rasa :background))
                             )))

(defun tabula-rasa-mode-enable()
;;  (interactive)
  (global-highline-mode 0)
  (setq tabula-rasa-frame (make-frame `(
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
  (setq tabula-rasa-window (frame-selected-window tabula-rasa-frame))
  (tabula-rasa-set-frame-parms)
  (add-hook 'window-configuration-change-hook 'tabula-rasa-update-window t nil)
  (add-hook 'delete-frame-functions 'tabula-rasa-mode-disable)
  (ns-toggle-fullscreen)
  (tabula-rasa-update-window)
)

(defun tabula-rasa-mode-disable()
;;  (interactive)
;  (ns-toggle-fullscreen)
  (remove-hook 'window-configuration-change-hook 'tabula-rasa-update-window)
  (remove-hook 'delete-frame-functions 'tabula-rasa-mode-disable)
  (delete-frame tabula-rasa-frame)
  (global-highline-mode 1)
)

;;;;;;;;;;;;;;;;; end ;;;;;;;;;;;;;;;;;

(provide 'tabula-rasa)

;;* next tabula-rasa-mode is buffer-local so if you change the buffer displayed, it breaks toggling the mode.
