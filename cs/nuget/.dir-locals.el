;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

; In the .nuspec files we want line sorting for things like <files ...> to
; be CASE SENSITIVE, otherwise Bond.dll and Bond.xml get sorted really far
; away from each other.
((nil
  (sort-fold-case)))
