;; sprite.lisp
;; Copyright Parasite Network 2018
;; GPL3

(in-package :glas)

(defstruct texture-descriptor 
  texture
  width
  height)

(defun create-texture-from-file (path renderer)
  (let ((surface (sdl2-image:load-image path)))
    (unless surface
      (error "Failed to create surface for file: ~A." path))
    (let ((width (sdl2:surface-width surface))
          (height (sdl2:surface-height surface)))
      (let ((texture (sdl2:create-texture-from-surface renderer surface)))
        (unless texture
          (error "Failed to create texture for surface: ~A" path))
        (sdl2:free-surface surface)
        (make-texture-descriptor
          :texture texture
          :width width
          :height height)))))

(defparameter *TEXTURE-DESCRIPTORS* nil)
(defparameter *PIXMAP-DESCRIPTORS* nil)

(defun glas-initialize-sprites ()
  (setf *TEXTURE-DESCRIPTORS* (make-hash-table :test #'equal))
  (setf *PIXMAP-DESCRIPTORS* (make-hash-table :test 'eq)))

(defun get-texture (path renderer)
  (let ((texture (gethash path *TEXTURE-DESCRIPTORS*)))
    (if texture
        texture
        (let ((descriptor (create-texture-from-file path renderer)))
          (format t "Loaded texture (~Ax~A): ~A.~%" 
                  (texture-descriptor-width descriptor)
                  (texture-descriptor-height descriptor)
                  path)
          (setf (gethash path *texture-descriptors*) descriptor)
          descriptor))))

(defun destroy-textures ()
  (loop for key being each hash-key of *TEXTURE-DESCRIPTORS*
        using (hash-value value)
        do (progn
             (format t "Destroying texture: ~A.~%" key)
             (sdl2:destroy-texture (texture-descriptor-texture value)))))        

;;------------------------------------------------------------------------------

(defstruct pixmap-descriptor
  "A pixmap descriptor describes a region in a texture."
  id 			; self id
  texture		; the texture-descriptor
  width 		; grid width
  height 		; grid height
  x 			; x index, ie column index
  y 			; y index, ie row index
  absolute-x  ; absolute x within the texture
  absolute-y  ; absolute y within the texture
  rectangle)	; precomputed sdl rectangle

(defgeneric pixmap-width (pixmap))
(defgeneric pixmap-height (pixmap))

(defmethod pixmap-width ((pixmap pixmap-descriptor))
  (pixmap-descriptor-width pixmap))

(defmethod pixmap-height ((pixmap pixmap-descriptor))
  (pixmap-descriptor-height pixmap))

(defun create-pixmap-descriptor (texture id &optional width height x y)
  "By default the pixmap will cover the entire texture.
  We can use WIDTH HEIGHT X Y to restrict it to the grid cell (X;Y) where
  each grid cell has the shape (WIDTH;HEIGHT)."
  (unless width
    (setf width (texture-descriptor-width texture)))
  (unless height 
    (setf height (texture-descriptor-height texture)))
  (unless x
    (setf x 0))
  (unless y
    (setf y 0))
  (let ((absx (* width x))(absy (* height y)))
    (make-pixmap-descriptor
      :id id
      :texture (texture-descriptor-texture texture)
      :width (the fixnum width)
      :height (the fixnum height)
      :x x
      :y y
      :absolute-x absx
      :absolute-y absy
      :rectangle 
      (sdl2:make-rect
        absx
        absy
        width
        height))))

; (TEXTURE-DESCRIPTOR,KEYWORD,&KEY) -> PIXMAP-DESCRIPTOR
(defun register-new-pixmap-descriptor (texture id &optional width height x y)
  (setf (gethash id *pixmap-descriptors*) 
        (create-pixmap-descriptor texture id width height x y)))

(defun defresource (path renderer &rest formats)
  "The DEFRESOURCE function creates pixmaps from a texture.
  
  formats ::= symbol | (width height cell*)
  width ::= a number
  height ::= a number
  cell ::= (id cellx celly)
  id ::= symbol
  cellx ::= a number
  celly ::= a number"
  (let ((texture (get-texture path renderer)))
    (unless formats
      (error "Resource ~S needs at least one identifier!~%" path))
    (format t "Defining resources for: ~A.~%" path)
    (dolist (fmt formats nil)
      (typecase fmt
                (list 
                  (destructuring-bind (width height &rest cells) fmt
                                      (format t "  Shape (~Ax~A):~%" width height)
                                      (dolist (cell cells)
                                        (destructuring-bind (id x y) cell
                                                            (format t "    Id: ~A. @X: ~A. @Y: ~A.~%"
                                                                    id
                                                                    x
                                                                    y)
                                                            (register-new-pixmap-descriptor 
                                                              texture 
                                                              id 
                                                              width 
                                                              height 
                                                              x
                                                              y)))))
                (keyword
                  (let ((id fmt))
                    (format t "  Shape (~Ax~A):~%"
                            (texture-descriptor-width texture)
                            (texture-descriptor-height texture))
                    (format t "    Id: ~A.~%" id )
                    (register-new-pixmap-descriptor texture id)))
                (otherwise
                  (error "Pixmap id needs to be a keyword, not ~A~%" fmt))))))

(defun get-pixmap (id &key on-failure-nil)
  (let ((pixmap (gethash id *pixmap-descriptors*)))
    (if pixmap
        (copy-pixmap pixmap)
        (unless on-failure-nil
          (error "Can't find requested pixmap ~A~%" id)))))

(defgeneric copy-pixmap (pixmap))
(defmethod copy-pixmap ((pixmap pixmap-descriptor))
  (copy-pixmap-descriptor pixmap))

(defun verify-pixmap-id (id)
  (gethash id *pixmap-descriptors*))

(defun verify-pixmap-id-list (lst)
  ;TODO
  t)

;; IMAGE -> T | NIL
(defun verify-image (image)
  "Verifies the existence of the specified image."
  (if (gethash image *pixmap-descriptors*)
      t
      nil))

;; (LIST<LIST<IMAGE>>, FUN) -> T | NIL
(defun verify-all (image-groups verifier)
  (when image-groups
    (when (listp image-groups)
      (every (lambda (group)
               (every (lambda (image)
                        (let ((pixmap (get-pixmap image :on-failure-nil t)))
                          (when pixmap
                            (funcall verifier pixmap))))
                      group))
             image-groups))))

;; LIST<LIST<IMAGE>> -> T | NIL
(defun verify-images-existence (&rest image-groups)
  "Verifies the existence of each image."
  (verify-all image-groups (lambda (pixmap) t)))

;; LIST<LIST<IMAGE>> -> T | NIL
(defun verify-images-same-dimensions (&rest image-groups)
  "Compares all images's dimensions to that of the first one."
  (when image-groups
    (let (target-width target-height)
      (verify-all image-groups (lambda (pixmap)
                                 (let ((width (pixmap-width pixmap))
                                       (height (pixmap-height pixmap)))
                                   (unless target-width
                                     (setf target-width width)
                                     (setf target-height height))
                                   (and 
                                     (equal width target-width)
                                     (equal height target-height))))))))

(defun get-pixmaps-array (ids)
  (let ((pixmaps (make-array (list (length ids)))))
    (loop for id in ids for i from 0 do
          (setf (aref pixmaps i) (get-pixmap id)))
    pixmaps))

;; (LIST<PIXMAP>) -> T | NIL
(defun verify-pixmaps-dimensions (pixmaps)
  (let ((height (pixmap-height (aref pixmaps 0)))
        (width (pixmap-width (aref pixmaps 0))))
    (every (lambda (pixmap)
             (and
               (equal height (pixmap-height pixmap))
               (equal width (pixmap-width pixmap))))
           pixmaps)))

;;------------------------------------------------------------------------------

;; ANIMATE ONCE a,b,c,d
;; ANIMATE LOOP a,b,c,d
;; ANIMATE ONCE a,b LOOP c,d
;; ANIMATE a:50,b:100,c:50,d:100 LOOP e:20,f:20

(defstruct animated-pixmap-descriptor
  timeline
  names
  pixmaps
  base
  index
  total)

(defmethod pixmap-width ((anixmap animated-pixmap-descriptor))
  (with-slots ((pixmaps% pixmaps)
               (index% index)) anixmap
              (pixmap-width (aref pixmaps% index%))))

(defmethod pixmap-height ((anixmap animated-pixmap-descriptor))
  (with-slots ((pixmaps% pixmaps)
               (index% index)) anixmap
              (pixmap-height (aref pixmaps% index%))))

(defmethod copy-pixmap ((anixmap animated-pixmap-descriptor))
  (copy-animated-pixmap-descriptor anixmap))

(defun adjust-timeline-description (timeline pixmaps)
  "The timeline needs to be of the same length as the pixmap list.
  If it too short we duplicate the last element, and if it is too
  long we simply drop the last elements we don't need."
  (let ((timeline# (length timeline))
        (pixmaps# (length pixmaps)))
    (cond
      ((= timeline# pixmaps#)
       timeline)
      ((< timeline# pixmaps#)
       (let ((diff (- pixmaps# timeline#)))
         ;; Duplicate last timeline entry. DOES NOT COPY LISTS!
         (append timeline (make-list diff :initial-element (car (last timeline))))))
      ((> timeline# pixmaps#)
       (let ((diff (- timeline# pixmaps#)))
         ;; Drops the difference.
         (butlast timeline diff))))))

(defun coalesce-keyvalue (kvlist)
  (let (db)
    (dolist (kv kvlist db)
      (destructuring-bind (key value) kv
                          (let ((entry (assoc key db)))
                            (if entry
                                (push (append entry (list value)) db)
                                (push (list key value) db)))))))

(defun extract-timeline-components (timeline)
  (let (timepoints names)
    (loop for component in timeline
          for i from 0
          do (if (listp component)
                 (destructuring-bind (timepoint name) component
                                     (push timepoint timepoints)
                                     (push (list name i) names))
                 (push component timepoints)))
    (values (reverse timepoints) 
            (coalesce-keyvalue names))))

; TODO Accept charmap autoindex
; TODO Add :LOOP :BOUNCE :STOP control values
(defun defanimation (timeline images id &key (control :loop))
  (unless (plusp (length timeline))
    (error "The animation (~A) needs at least one timepoint." id))
  (unless (plusp (length images))
    (error "The animation (~A) needs at least one image." id))
  (let ((fixed-timeline (adjust-timeline-description timeline images)))
    (multiple-value-bind (timepoints names) (extract-timeline-components fixed-timeline)
                         (let* ((timepoints# (length timepoints))
                                (pixmap (make-animated-pixmap-descriptor
                                          :timeline (make-array (list timepoints#) :initial-contents timepoints)
                                          :names names
                                          :pixmaps (get-pixmaps-array images)
                                          :base 0
                                          :index 0
                                          :total timepoints#)))
                           (setf (gethash id *pixmap-descriptors*) pixmap)))))

(defun get-animation-pixmap (anixmap tick)
  (with-slots ((timeline% timeline)
               (pixmaps% pixmaps)
               (base% base)
               (index% index)
               (total% total)) anixmap
              (when (= base% 0)
                (setf base% tick))
              (let ((target (+ base% (aref timeline% index%))))
                (when (>= tick target)
                  (setf base% tick)
                  (setf index% (mod (1+ index%) total%)))
                (aref pixmaps% index%))))

(defun restart-animation (anixmap &key index tick)
  (when index
    (setf index (mod index (animation-descriptor-total anixmap)))
    (setf (animation-descriptor-index anixmap) index))
  (when tick
    (setf (animation-descriptor-tick anixmap) tick)))



;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------

(defstruct charmap-descriptor
  "A CHARMAP-DESCRIPTOR overlays a texture with a grid. Sprites can then
  be extracted through indexing."
  id 			; self id
  texture 	; the texture-descriptor
  width 		; grid width
  height 		; grid height
  row			; number of chars on a row
  total		; total number of chars
  translator
  translator-unknown
  cache)		; computed sprite-descripors

(defparameter *charmaps* (make-hash-table :test 'eq))

(defmethod pixmap-width ((charmap charmap-descriptor))
  (charmap-descriptor-width charmap))

(defmethod pixmap-height ((charmap charmap-descriptor))
  (charmap-descriptor-height charmap))

(defun get-charmap (id &key on-failure-nil)
  (let ((charmap (gethash id *charmaps*)))
    (if charmap
        charmap
        (unless on-failure-nil
          (error "Unable to get charmap ~A~%" id)))))

(defun create-charmap-descriptor (texture id width height row total translator unknown)
  (make-charmap-descriptor
    :id id
    :texture texture
    :width width
    :height height
    :row row
    :total (if total
               total
               (* width height))
    :translator translator ;;; ARRAY
    :translator-unknown unknown
    :cache (make-hash-table :test #'eq)))

(defun defcharmap (path renderer id width height row total &key translator translator-unknown-default)
  "Creates a charmap descriptor over the specified image FILE, and
	 names it ID. WIDTH and HEIGHT are the dimensions of the grid cells,
	 and ROW is the number of cells per row, and TOTAL is the number
	 of valid indexes."
  (let ((texture (get-texture path renderer)))
    (let ((charmap (create-charmap-descriptor texture id width height row total translator translator-unknown-default)))
      (setf (gethash id *charmaps*) charmap))))

(defun extract-pixmap (charmap index)
  "Creates a pixmap descriptor over the charmap at the specified index."
  (unless (or (>= index 0) (< index (charmap-descriptor-total charmap)))
    (error "charmap->sprite: Index ~A is out of bounds for ~A~%" index charmap))
  (let ((descriptor
          (create-pixmap-descriptor
            (charmap-descriptor-texture charmap)
            index
            (charmap-descriptor-width charmap)
            (charmap-descriptor-height charmap)
            (rem index (charmap-descriptor-row charmap)) ; x 
            (floor (/ index (charmap-descriptor-row charmap)))))) ; y
    (format t "Created PIXMAP-DESCRIPTOR for index ~A in charmap (~A).~%" index (charmap-descriptor-id charmap))
    descriptor))

(defun get-charmap-pixmap (charmap index)
  "Retrieves a pixmap descriptor for the CHARMAP descriptor at INDEX.
	 It first checks if there already is one in the cache, otherwise it
	 creates a new one."
  (let ((descriptor (gethash index (charmap-descriptor-cache charmap))))
    (if descriptor
        descriptor
        (let ((pixmap (extract-pixmap charmap index)))
          (update-cached-descriptor charmap index pixmap)
          pixmap))))

(defun export-charmap-pixmap (charmap index id)
	"Names the sprite descriptor over CHARMAP at INDEX to ID and makes
	 it available through FIND-SPRITE."
	(setf (gethash id *pixmap-descriptors*) (get-charmap-pixmap charmap index)))

(defun update-cached-descriptor (charmap index descriptor)
	(setf (gethash index (charmap-descriptor-cache charmap)) descriptor))


;; FIXME SHOULD USE UNKNOWN!
(defun charmap-translate-to-pixmap (charmap key)
  (let ((table (charmap-descriptor-translator charmap)))
    (when table
      (let ((entry (assoc key table)))
        (when entry
          (let ((index (cadr entry)))
            (get-charmap-pixmap charmap index)))))))

;;------------------------------------------------------------------------------

(defstruct (translator-iterator-class)
  charmap
  index
  translator
  size)

(defun make-translator-iterator (charmap &key initial)
  (let ((translator (charmap-descriptor-translator charmap)))
    (unless translator
      (error "The charmap (~A) doesn't have a translator to iterate over." (charmap-descriptor-id charmap)))
    (let ((iterator (make-translator-iterator-class
                      :charmap charmap
                      :index 0
                      :translator translator
                      :size (length translator))))
      (when initial
        (set-current-iterator-key iterator initial))
      (format t " Created TRANSLATOR-ITERATOR-CLASS for charmap (~A).~%" (charmap-descriptor-id charmap))
      iterator)))

(defun modulate (value offset total)
  (mod (+ value offset) total))

(defun next (tic)
  (setf (translator-iterator-class-index tic)
        (modulate (translator-iterator-class-index tic) +1 (translator-iterator-class-size tic))))

(defun prev (tic)
  (setf (translator-iterator-class-index tic)
        (modulate (translator-iterator-class-index tic) -1 (translator-iterator-class-size tic))))

(defun get-iterator-pixmap (tic)
  (let ((charmap (translator-iterator-class-charmap tic))
        (index (translator-iterator-class-index tic)))
    (get-charmap-pixmap charmap index)))

(defun get-current-iterator-key (tic)
  (let ((translator (translator-iterator-class-translator tic))
        (index (translator-iterator-class-index tic)))
    (let ((entry (elt translator index)))
      (car entry))))

(defun translate-key-to-index (tic key)
  (position key (translator-iterator-class-translator tic) :key #'car))

(defun set-current-iterator-key (tic &optional key)
  (let ((index (translate-key-to-index tic key)))
    (if index
        (setf (translator-iterator-class-index tic) index)
        (progn
          (format t "Translator for charmap (~A) could not reset to ~A.~%" 
                  (charmap-descriptor-id (translator-iterator-class-charmap tic)) key)
          (set-current-iterator-key-to-unknown tic)))))

(defun set-current-iterator-key-to-unknown (tic)
  (let ((charmap (translator-iterator-class-charmap tic)))
    (let ((unknown (charmap-descriptor-translator-unknown charmap)))
      (unless unknown
        (error "Charmap (~A) doesn't have an unknown fallback." (charmap-descriptor-id charmap)))
      (let ((index (translate-key-to-index tic unknown)))
        (unless index
          (error "Charmap's (~A) unknown fallback is invalid." (charmap-descriptor-id charmap)))
        (setf (translator-iterator-class-index tic) index)))))

;;------------------------------------------------------------------------------

(defconstant SPRITEX 0)
(defconstant SPRITEY 2)
(defconstant TICKX 1)
(defconstant TICKY 3)

(defstruct (sprite-descriptor (:constructor %new-sprite-descriptor))
  id
  init
  pixmap
  transform-x
  transform-y
  (pos (make-array '(4) :initial-contents '(0 0 0 0)))
  (velocity (make-array '(4) :initial-contents '(0 0 0 0)))
  (acceleration (make-array '(4) :initial-contents '(0 0 0 0)))
  (jerk (make-array '(4) :initial-contents '(0 0 0 0))))

#|
(defclass sprite-descriptor-class ()
  ((id           :initform nil :reader sprite-id           :initarg :id)
   (pixmap       :initform nil :reader sprite-pixmap       :initarg :pixmap)
   (transform-x  :initform nil :reader sprite-transform-x  :initarg :transform-x)
   (transform-y  :initform nil :reader sprite-transform-y  :initarg :transform-y)
   (pos          :initform nil :reader sprite-pos          :initarg :pos)
   (velocity     :initform nil :reader sprite-velocity     :initarg :velocity)
   (acceleration :initform nil :reader sprite-acceleration :initarg :acceleration)
   (jerk         :initform nil :reader sprite-jerk         :initarg :jerk)))

(defun make-sprite-descriptor-class (&key id pixmap pos velocity acceleration jerk transform-x transform-y)
  (let ((sprite (make-instance 'sprite-descriptor-class
                  :id id
                  :pixmap pixmap
                  :pos pos
                  :velocity velocity
                  :acceleration acceleration
                  :jerk jerk
                  :transform-x transform-x
                  :transform-y transform-y)))
    sprite))
|#
    
(defmethod print-object ((sprite sprite-descriptor) stream)
  (format stream "[sprite-descriptor: ~A. pixmap-id: ~A. "
          (sprite-descriptor-id sprite)
          (pixmap-descriptor-id (sprite-descriptor-pixmap sprite)))
  (format stream "(~A;~A) + (~A;~A) px/s¹ + (~A;~A) px/s² + (~A;~A) px/s³]~%"
          (aref (sprite-descriptor-pos sprite) SPRITEX)
          (aref (sprite-descriptor-pos sprite) SPRITEY)
          (aref (sprite-descriptor-velocity sprite) SPRITEX)
          (aref (sprite-descriptor-velocity sprite) SPRITEY)
          (aref (sprite-descriptor-acceleration sprite) SPRITEX)
          (aref (sprite-descriptor-acceleration sprite) SPRITEY)
          (aref (sprite-descriptor-jerk sprite) SPRITEX)
          (aref (sprite-descriptor-jerk sprite) SPRITEY)))

(defun sprite-x (sprite)
  (sprite-descriptor-x sprite))

(defun sprite-descriptor-x (sprite)
  (aref (sprite-descriptor-pos sprite) SPRITEX))

(defun sprite-y (sprite)
  (sprite-descriptor-y sprite))

(defun sprite-descriptor-y (sprite)
  (aref (sprite-descriptor-pos sprite) SPRITEY))

(defun sprite-absolute-right (sprite)
  (+ (sprite-x sprite) (pixmap-width (sprite-descriptor-pixmap sprite))))


(defun sprite-absolute-bottom (sprite)
  (+ (sprite-y sprite) (pixmap-height (sprite-descriptor-pixmap sprite))))

#|
(defun make-sprite (&key id pixmap pos velocity acceleration jerk transform-x transform-y)
  (let ((sprite (make-sprite-descriptor
                  :id id
                  :pixmap pixmap
                  :pos pos
                  :velocity velocity
                  :acceleration acceleration
                  :jerk jerk
                  :transform-x transform-x
                  :transform-y transform-y)))
    (fix-sprite sprite)
    sprite))
|#

(defun make-sprite (pixmap &key id (pos #(0 0)) (velocity #(0 0)) (acceleration #(0 0)) (jerk #(0 0)) transform-x transform-y)
  (initialize-sprite (%new-sprite-descriptor)
                     pixmap
                     :id id 
                     :pos pos 
                     :velocity velocity 
                     :acceleration acceleration 
                     :jerk jerk 
                     :transform-x transform-x 
                     :transform-y transform-y))

(defun initialize-sprite (object pixmap &key 
                                 id 
                                 (pos #(0 0)) 
                                 (velocity #(0 0)) 
                                 (acceleration #(0 0)) 
                                 (jerk #(0 0)) 
                                 transform-x 
                                 transform-y)
  (initialize-struct object
                     :id id
                     :pixmap pixmap
                     :pos (make-array '(4) :initial-contents
                                      (list (elt pos 0)
                                            0
                                            (elt pos 1)
                                            0))
                     :velocity (make-array '(4) :initial-contents
                                           (list (elt velocity 0)
                                                 0
                                                 (elt velocity 1)
                                                 0))
                     :acceleration (make-array '(4) :initial-contents
                                               (list (elt acceleration 0)
                                                     0
                                                     (elt acceleration 1)
                                                     0))
                     :jerk (make-array '(4) :initial-contents
                                       (list (elt jerk 0)
                                             0
                                             (elt jerk 1)
                                             0))
                     :transform-x transform-x
                     :transform-y transform-y))
    
(defun change-sprite-position (sprite px py)
  (let ((pos (sprite-descriptor-pos sprite)))
    (setf (sprite-descriptor-pos sprite)
          (make-array '(4) :initial-contents
                      (list px
                            (aref pos 1)
                            py
                            (aref pos 3))))))

(defun change-sprite-velocity (sprite vx vy)
  (let ((vel (sprite-descriptor-velocity sprite)))
    (setf (sprite-descriptor-velocity sprite) 
          (make-array '(4) :initial-contents
                      (list vx
                            (aref vel 1)
                            vy
                            (aref vel 3))))))

(defun change-sprite-acceleration (sprite ax ay)
  (let ((acc (sprite-descriptor-acceleration sprite)))
    (setf (sprite-descriptor-acceleration sprite)
          (make-array '(4) :initial-contents
                      (list ax
                            (aref acc 1)
                            ay
                            (aref acc 3))))))


(defun change-sprite-jerk (sprite jx jy)
  (let ((jerk (sprite-descriptor-jerk sprite)))
    (setf (sprite-descriptor-jerk sprite)
          (make-array '(4) :initial-contents
                      (list jx
                            (aref jerk 1)
                            jy
                            (aref jerk 3))))))

(defun stop-sprite (sprite)
  (change-velocity sprite 0 0)
  (change-sprite-acceleration sprite 0 0)
  (change-sprite-jerk sprite 0 0))

(defun update-movement (sprite tick)
  (dolist (s (list SPRITEX SPRITEY))
    (update-dependency
      (sprite-descriptor-acceleration sprite)
      (sprite-descriptor-jerk sprite)
      s
      tick)
    (update-dependency
      (sprite-descriptor-velocity sprite)
      (sprite-descriptor-acceleration sprite)
      s
      tick)
    (update-dependency
      (sprite-descriptor-pos sprite)
      (sprite-descriptor-velocity sprite)
      s
      tick)))

(defun update-dependency (dependant depender s tick)
  (let ((xtick (+ 1 s)))
    (if (zerop (aref depender xtick))
        (setf (aref depender xtick) tick)
        (let ((v (aref depender s)))
          (when v
            (let* ((diff (- tick (aref depender xtick)))
                   (frac (/ diff (float INTERNAL-TIME-UNITS-PER-SECOND)))
                   (Δv (truncate (* frac v))))
              (when (not (= Δv 0))
                (incf (aref dependant s) Δv)
                (setf (aref depender xtick) tick))))))))

(defun paint-sprite (sprite renderer tick)
  (update-movement sprite tick)
  (let ((pos (sprite-descriptor-pos sprite)))
    (let ((x (aref pos SPRITEX))
          (y (aref pos SPRITEY)))
      (let ((tx (sprite-descriptor-transform-x sprite)))
        (when tx
          (setf x (funcall tx sprite (- tick (aref pos TICKX))))))
      (let ((ty (sprite-descriptor-transform-y sprite)))
        (when ty
          (setf y (funcall ty sprite (- tick (aref pos TICKY))))))
      (paint-descriptor (sprite-descriptor-pixmap sprite)
                        renderer
                        x
                        y
                        tick))))

;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------

(defgeneric paint-descriptor (descriptor renderer x y tick))
(defmethod paint-descriptor ((descriptor pixmap-descriptor) renderer x y tick)
  (let ((source-rect (pixmap-descriptor-rectangle descriptor)))
    (unless source-rect
      (error "Failed to get rect for ~A~%" descriptor))
    (paint-descriptor-with-rect descriptor renderer x y source-rect)))

(defmethod paint-descriptor ((descriptor animated-pixmap-descriptor) renderer x y tick)
  (let ((pixmap (get-animation-pixmap descriptor tick)))
    (paint-descriptor pixmap renderer x y tick)))

(defun paint-descriptor-with-rect (descriptor renderer x y source-rect)
  (let ((dest-rect (sdl2:make-rect
                     x
                     y
                     (sdl2:rect-width source-rect)
                     (sdl2:rect-height source-rect))))
    (sdl2:render-copy
      renderer
      (pixmap-descriptor-texture descriptor)
      :source-rect source-rect
      :dest-rect dest-rect)
    (sdl2:free-rect dest-rect)))
