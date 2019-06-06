(asdf:defsystem #:glas

:description "Glas SDL2 Graphics Framework"
:author "Parasite Network"
:license "GPL3"

:depends-on (:sdl2 :sdl2-image)
:pathname ""
:serial t

:components
((:file "package")
 (:file "glas")
 (:file "sprite")
 (:file "window")
 (:file "widget")
 (:file "mouse")))
