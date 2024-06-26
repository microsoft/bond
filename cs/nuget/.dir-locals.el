;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

(
 ; In the .nuspec files we want line sorting for things like <files ...> to
 ; be CASE SENSITIVE, otherwise Bond.dll and Bond.xml get sorted really far
 ; away from each other.
 (nil . ((sort-fold-case . nil)))
 ; The nuspec files in this directory are indented with four spaces per
 ; level, instead of the typical two for XML-things in the .NET world (like
 ; .csproj files).
 (nxml-mode . ((nxml-child-indent . 4))))
