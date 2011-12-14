;;; tabula-rasa-mode.el - Distraction free writing

(defgroup tabula-rasa nil
  "Distraction free writing"
  :version 0.1
  :group 'text)

(defcustom tabula-rasa-width 80
  "Width of writing space."
  :type 'integer
  :group 'tabula-rasa
  :set (lambda (symbol value)
         (progn (setq tabula-rasa-width value)
                (tabula-rasa-update-window)))
  :initialize 'custom-initialize-default)

(defcustom tabula-rasa-line-spacing 5
  "Vertical line spacing."
  :type 'integer
  :group 'tabula-rasa)

(defcustom tabula-rasa-minor-mode-states
  '(("global-highline-mode" . nil) ("global-highlight-parentheses-mode" . nil))
  "Alist of minor modes and their states while tabula-rasa mode is active. This allows you to specify modes to force on or off while in Tabula Rasa Mode."
  :type '(repeat (cons :format "%v"
                       (string :tag "Minor Mode")
                       (boolean :tag "State"))))  

(defface tabula-rasa-default
  '((t (
        :font "Monaco"
        :height 140
    )))
  "Face for tabula-rasa mode."
  :group 'tabula-rasa)

(defface tabula-rasa-cursor
  '((t (
        :inherit tabula-rasa-default
        :inverse-video t
    )))
  "Face for tabula-rasa mode."
  :group 'tabula-rasa)

(defface tabula-rasa-region
  '((t (
        :inherit tabula-rasa-default
        :foreground "black"
        :background "gray"
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
  :global t
; Awkwardly backwards logic. It would appear that define-minor-mode toggles the mode variable prior to evaluating the body.
  (if tabula-rasa-mode
      (tabula-rasa-mode-enable)
    (tabula-rasa-mode-disable)))

(setq tabula-rasa-frame nil)

;; Emacs bug? It takes 2 calls to set bg color to set frame margin colors.
(defun tabula-rasa-set-frame-parms ()
;;  (interactive)
  (modify-frame-parameters tabula-rasa-frame 
                           `(
                             (background-color . ,(face-attribute 'tabula-rasa-default :background))
                             )))

(defun tabula-rasa-save-mmodes ()
;;  (interactive)
  (setq tr-saved-mmodes '())
  (mapc (lambda (mode)
          (if (intern-soft (car mode))
              (setq tr-saved-mmodes (cons (list (car mode) (symbol-value (intern (car mode)))) tr-saved-mmodes))))
        tabula-rasa-minor-mode-states)
  (message "%s" tr-saved-mmodes))

(defun tabula-rasa-save-mmodes-alist ()
;;  (interactive)
  (setq tr-saved-mmodes '())
  (mapc (lambda (mode)
          (if (intern-soft (car mode))
              (setq tr-saved-mmodes (cons (cons (car mode) (symbol-value (intern (car mode)))) tr-saved-mmodes))))
        tabula-rasa-minor-mode-states)
  (message "%s" tr-saved-mmodes))


(defun tabula-rasa-set-mmodes (mmodes-alist)
;;  (interactive)
  (mapc (lambda (mode)
          (message "\n\nmode: %s" (cdr mode))
          (message "mode assoc: %s" (assoc mode mmodes-alist))
          (cond
           ((cdr mode) 
            (progn 
               (message "t %s" (concat (car mode) " 1"))
               (eval (read (concat "(" (car mode) " 1)")))
            ))
           (t
            (progn 
               (message "nil %s" (concat (car mode) " 0"))
               (eval (read (concat "(" (car mode) " 0)")))
            ))))
        mmodes-alist))


(defun tabula-rasa-load-mmodes ()
;;  (interactive)
  (mapc (lambda (mode)
          (cond
           ((nth 1 mode) 
            (progn 
               (message "%s" (concat (car mode) " 1"))
               (eval (concat (car mode) " 1"))
            ))
           (t
            (progn 
               (message "%s" (concat (car mode) " 0"))
               (eval (concat (car mode) " 0"))
            ))))
        tr-saved-mmodes))

(defun tabula-rasa-mode-enable()
;;  (interactive)

(tabula-rasa-save-mmodes)
(tabula-rasa-set-mmodes tabula-rasa-minor-mode-states)

;  (setq ns-antialias-text t)
;;

  (setq tabula-rasa-frame (make-frame `(
                               (fullscreen . fullboth)
                               (unsplittable . t)
                               (left-fringe . 0)
                               (right-fringe . 0)
                               (tool-bar-lines . 0)
                               (menu-bar-lines . 0)
                               (vertical-scroll-bars . nil)
                               (line-spacing . ,tabula-rasa-line-spacing)
                               (font . ,(face-font "tabula-rasa-default"))
                               (foreground-color . ,(face-attribute 'tabula-rasa-default :foreground))
                               (background-color . ,(face-attribute 'tabula-rasa-default :background))
                               (cursor-color . ,(face-attribute 'tabula-rasa-cursor :foreground))
                               )))

  (set-face-foreground 'region (face-attribute 'tabula-rasa-region :foreground) tabula-rasa-frame)
  (set-face-background 'region (face-attribute 'tabula-rasa-region :background) tabula-rasa-frame)

  (setq tabula-rasa-window (frame-selected-window tabula-rasa-frame))
  (tabula-rasa-set-frame-parms)
  (add-hook 'window-configuration-change-hook 'tabula-rasa-update-window t nil)
  (add-hook 'delete-frame-functions 'tabula-rasa-mode-disable)
  (ns-toggle-fullscreen)
  (tabula-rasa-update-window)
)

(defun tabula-rasa-mode-disable()
;;  (interactive)
  (select-frame tabula-rasa-frame)
  (ns-toggle-fullscreen)
  (remove-hook 'window-configuration-change-hook 'tabula-rasa-update-window)
  (remove-hook 'delete-frame-functions 'tabula-rasa-mode-disable)
  (delete-frame tabula-rasa-frame)

(tabula-rasa-set-mmodes tr-saved-mmodes)
;  (setq ns-antialias-text nil)

)

;;;;;;;;;;;;;;;;; end ;;;;;;;;;;;;;;;;;

(provide 'tabula-rasa)

;;* next tabula-rasa-mode is buffer-local so if you change the buffer displayed, it breaks toggling the mode.
