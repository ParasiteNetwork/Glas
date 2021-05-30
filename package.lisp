(defpackage :glas
  (:use :cl)
  (:lock t)
  (:export
    ;; SPAREPARTS.LISP
    :keycase
    :initialize-struct
    
    ;; SPRITE.LISP
    :pixmaptype
    :pixmap-descriptor-id
    :pixmap-width
    :pixmap-height
    :pixmap-id
    :defresource
    :get-pixmap
    :verify-images-existence
    :verify-images-same-dimensions
    :get-pixmaps-array
    :defanimation
    :restart-animation
    :update-animation-named-timepoint
    :defcharmap
    :get-charmap
    :get-charmap-pixmap
    :charmap-translate-to-pixmap
    :make-translator-iterator
    :next
    :prev
    :get-iterator-pixmap
    :get-current-iterator-key
    :set-current-iterator-key
    :paint-sprite
    :paint-descriptor
    :paint-pixmap
    :attach-xy-transformer
    :has-xy-transformer-p
    :remove-xy-transformer
    
    ;; SPRITE-DESCRIPTOR
    :sprite-descriptor
    :make-sprite
    :get-sprite-pixmap
    :initialize-sprite
    :sprite-x
    :sprite-y
    :sprite-absolute-right
    :sprite-absolute-bottom
    :change-sprite-position
    :change-sprite-velocity
    :change-sprite-acceleration
    :change-sprite-jerk
    :stop-sprite
    :set-x-transformer
    :set-y-transformer
    :get-sprite-type
    
    :paint-descriptor
    
    :destroy-textures
    
    ;; WINDOW.LISP
    :window
    :get-root-widget
    :get-window-id
    :get-active-status
    :*WINDOW-WIDTH*
    :*WINDOW-HEIGHT*
    :activate
    :deactivate
    :find-window
    :open-window-by-id
    :open-window
    :close-window-by-id
    :close-window
    :open-window-group
    :close-window-group
    :search-stack-xy
    :defwindow
    
    ;; WIDGET.LISP
    :color
    :color-r
    :color-g
    :color-b
    :color-a
    :make-color
    :+WHITE+
    :+RED+
    :+BLACK+
    :+ALPHA+
    :+GREENISH+
    :self-rectangle
    :each-widget-child
    :localize
    :add-adjust-accordingly
    :search-widget-by-type
    :search-widget-by-local-id
    
    :widget
    :widget-children
    :widget-x
    :widget-y
    :widget-offset-x
    :widget-offset-y
    :widget-id
    :widget-local-id
    :widget-height
    :widget-width
    
    :get-widget-children
    :widget-absolute-right
    :widget-absolute-bottom
    :search-widget-xy
    
    :attach-event-handler
    :dispatch-event
    
    :widget-event-handle
    :widget-event-prepaint
    :widget-event-paint
    :widget-event-absolute-xy
    :widget-event-onkey-down
    :widget-event-onkey-up
    :widget-event-open
    :widget-event-close
    :widget-event-init
    :widget-event-mouse-enter
    :widget-event-mouse-leave
    :widget-event-mouse-movement
    :widget-event-mouse-left-click-down
    :widget-event-mouse-left-click-up
    :widget-event-mouse-left-click-cancel
    :widget-event-mouse-middle-click-down
    :widget-event-mouse-middle-click-up
    :widget-event-mouse-middle-click-cancel
    :widget-event-mouse-right-click-down
    :widget-event-mouse-right-click-up
    :widget-event-mouse-right-click-cancel
    :widget-event-mouse-click-down
    :widget-event-mouse-click-up
    :widget-event-mouse-click-cancel
    :widget-event-mouse-click
    
    :widget-propagate-onkey-down
    :widget-propagate-onkey-up
    :widget-propagate-onkey
    :widget-propagate-calculate-xy
    :widget-propagate-calculate-xy-children
    :widget-propagate-open
    :widget-propagate-close
    :widget-propagate-init
    :widget-propagate-paint
    :paint-children
    
    :register-widget
    :unregiter-widget
    :find-widget

    :make-image
    :image-widget-visible
    :switch-image-visibility
    
    :make-background
    
    :make-bag
    
    :make-box
    
    :make-menu
    :make-menu-option
    ;; MOUSE.LISP
    
    ;; GLAS.LISP
    :glas-go
    ))
