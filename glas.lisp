
(in-package :glas)

(defparameter *RENDERER* nil)

(defun initialize-all-glas-components (width height)
  (glas-initialize-sprites)
  (format t "GLAS: Sprites initialized.~%")
  (glas-initialize-mouse-defaults)
  (format t "GLAS: Mouse initialized.~%")
  (glas-initialize-widget-defaults)
  (format t "GLAS: Widgets initialized.~%")
  (glas-initialize-window-defaults width height)
  (format t "GLAS: Window initialized.~%")
  t)

(defun glas-go (title width height resources# windows# &optional idle#)
  (format t "Starting GLAS.~%")
  (initialize-all-glas-components width height)
  (unwind-protect 
    (sdl2:with-init (:everything)
                    (format t "SDL2 initialized.~%")
                    (sdl2-image:init '(:png))
                    (format t "SDL2-Image initialized.~%")
                    (sdl2:with-window (win :w *WINDOW-WIDTH* :h *WINDOW-HEIGHT* :title title :flags '(:shown))
                                      (format t "SDL2 Window created.~%")
                                      (sdl2:with-renderer (renderer win)
                                                          (setf *RENDERER* renderer)
                                                          (handler-case
                                                            (progn
                                                              (when resources#
                                                                (funcall resources# renderer))
                                                              (when windows#
                                                                (funcall windows#))
                                                              (attach-debug-window)
                                                              (finalize-windows)
                                                              (format t "Allocated SDL2:RECTs ~A~%" *RECTS*)
                                                              (gameloop renderer idle#))
                                                            (error (e)
                                                                   (format t "Caught error: ~A~%" e)
                                                                   (format t "ABORT!~%"))))))
    (destroy-textures)
    (format t "Tjingeling.~%")))

(defun attach-debug-window ()
  (format t "Creating debug window.~%")
  (defwindow :global-hotkeys
             :visible t
             :active t
             :trap-input (lambda (window key)
                           (cond
                             ((key= key :scancode-d)
                              (setf *DEBUG-WIDGET-BORDER* (not *DEBUG-WIDGET-BORDER*))
                              t)
                             ((key= key :scancode-l)
                              (setf *DEBUG-MOUSE-XY-PRINT-WIDGET-TREE* (not *DEBUG-MOUSE-XY-PRINT-WIDGET-TREE*))
                              t)
                             ((key= key :scancode-a)
                              (debug-print-active-windows)
                              t)
                             ((key= key :scancode-w)
                              (debug-print-all-windows)
                              t)
                             ((key= key :scancode-v)
                              (debug-print-visible-windows)
                              t)))))

(defun gameloop (renderer idle#)        
  (format t "Entering game loop.~%")
  (let ((timeadjust 1))
    (when (> internal-time-units-per-second 1000)
      (setf timeadjust (/ internal-time-units-per-second 1000)))
    (format t "Time adjustment: ~A.~%" timeadjust)
    (sdl2:with-event-loop (:method :poll)
                          (:quit () t)
                          
                          ;; KEY
                          (:keydown (:keysym keysym)
                                    (format t "Key DOWN: (~A) ~A.~%"
                                            (sdl2:scancode-value keysym)
                                            (sdl2:scancode keysym))                                  
                                    (window-event-onkey-down keysym))
                          (:keyup (:keysym keysym)
                                  (format t "Key UP: (~A) ~A.~%"
                                          (sdl2:scancode-value keysym)
                                          (sdl2:scancode keysym))                                  
                                  (window-event-onkey-up keysym))
                          
                          ;; MOUSE
                          (:mousemotion (:timestamp timestamp :x x :y y)
                                        (track-mouse-movement x y))
                          (:mousebuttondown (:timestamp timestamp :state state :button button :x x :y y)
                                            (track-mouse-button-down button x y))
                          (:mousebuttonup (:timestamp timestamp :state state :button button :x x :y y)
                                          (track-mouse-button-up button x y))
                          
                          ;; EVENTS
                          (:windowevent (:event event :type type :timestamp timestamp :data1 data1 :data2 data2)
                                        (format t "Window event: ~A.~%" (translate-window-event event))
                                        (cond
                                          ((eq event !MOUSE-LEAVE!)
                                           (track-mouse-movement -1 -1))))
                          
                          (:idle ()
                                 (let ((tick (get-internal-real-time)))
                                   (setf tick (/ tick timeadjust))
                                   (sdl2:set-render-draw-color renderer 255 0 60 0)
                                   (sdl2:render-clear renderer)
                                   (paint-all-windows renderer tick :debug-borders *debug-widget-border*)                                 
                                   (sdl2:render-present renderer)
                                   (when idle#
                                     (funcall idle# renderer tick)))))))