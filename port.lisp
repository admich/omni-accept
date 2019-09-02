;;;; An experimental  port in which the application-frame can accept the presentations
;;;; from all the clim application frame running on the same image.

(in-package :clim-clx)

;; The port have an active-frame
(defclass experiment-port (clx-port)
  ((active-frame :initform nil :accessor port-active-frame)))

(setf (get :experiment :port-type) 'experiment-port)
(setf (get :experiment :server-path-parser) 'parse-clx-server-path)

(setf clim:*default-server-path* (list :experiment))

;; the keyboard focus is in the active-frame
(defmethod port-keyboard-input-focus ((port experiment-port))
  (when-let* ((active-frame (port-active-frame port))
              (focus-pane (port-frame-keyboard-input-focus port active-frame)))
    ;; check if this is correct
    (xlib::send-event (sheet-mirror (find-graft)) :client-message (xlib:make-event-mask :substructure-notify)
                    :type :_NET_ACTIVE_WINDOW
                    :window (sheet-mirror (frame-top-level-sheet active-frame))
                    :format 32
                    :data (vector 1 0 0))
    (xlib:display-force-output (clim-clx:clx-port-display port))
    focus-pane))

(defmethod (setf port-keyboard-input-focus) (focus port)
  (when-let* ((frame (pane-frame focus)))
    (setf (port-active-frame port) frame)
    (setf (port-frame-keyboard-input-focus port frame) focus)))

;; The pointer event of an output-recording-stream (that can have presentations)
;; are send to the active frame
(defmethod queue-event ((sheet output-recording-stream) (event pointer-event))
  (let ((queue (climi::frame-event-queue (port-active-frame (find-port))))) 
    (climi::event-queue-append queue event)))

;; If the keyboard focus window of X11 is changed set the active frame to the frame with x keyboard focus
(defmethod distribute-event :before ((port experiment-port) (event keyboard-event))
  (when-let* (x-focused-frame (pane-frame (event-sheet event)))
    (unless (eql (port-active-frame port) x-focused-frame)
      (setf (port-active-frame port)  x-focused-frame))))

(defmethod distribute-event :before ((port experiment-port) (event pointer-button-release-event))
  
  (when-let* ((event-frame (pane-frame (event-sheet event)))
              (active-frame (port-active-frame port)))
    (unless  (or t (eql active-frame event-frame))
      (setf (port-keyboard-input-focus port) (or (port-frame-keyboard-input-focus port active-frame)
                                                 (frame-query-io active-frame)
                                                 (frame-standard-input active-frame))))))


(in-package :climi)

;; Don't change keyboard focus if click on a presentation
(defmethod frame-input-context-button-press-handler :before
    ((frame standard-application-frame)
     (stream interactor-pane)
     button-press-event)
  (unless (frame-find-innermost-applicable-presentation frame
                                                        *input-context* stream
                                                        (pointer-event-x button-press-event)
                                                        (pointer-event-y button-press-event)
                                                        :event button-press-event)  
      (let ((previous (stream-set-input-focus stream)))
        (when (and previous (typep previous 'gadget))
          (let ((client (gadget-client previous))
                (id (gadget-id previous)))
            (disarmed-callback previous client id))))))

;; When click on a presentation the X11 keyboard focused window change.
;; When release the button the active frame return to the previous one
(defun throw-highlighted-presentation (presentation input-context event)
  (let ((x (pointer-event-x event))
        (y (pointer-event-y event))
        (window (event-sheet event)))
    (multiple-value-bind (p translator context)
        (find-innermost-presentation-match input-context
                                           presentation
                                           *application-frame*
                                           (event-sheet event)
                                           x y
                                           event
                                           0
                                           nil)
      (when p
        (alexandria:when-let* ((window window)
                               (event-frame (pane-frame window))
                               (port (port event-frame))
                               (active-frame (clim-clx::port-active-frame port)))
          (unless  (eql active-frame event-frame)
            (setf (port-keyboard-input-focus port)
                  (or (port-frame-keyboard-input-focus port active-frame)
                      (frame-query-io active-frame)
                      (frame-standard-input active-frame)))))
        (multiple-value-bind (object ptype options)
            (call-presentation-translator translator
                                          p
                                          (input-context-type context)
                                          *application-frame*
                                          event
                                          window
                                          x y)
          (when ptype
            (funcall (cdr context) object ptype event options)))))))



#|

|#



