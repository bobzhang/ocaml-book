(TeX-add-style-hook "_region_"
 (lambda ()
    (LaTeX-add-environments
     "inverseverbatim")
    (TeX-add-symbols
     '("ChangeLine" 1)
     '("ind" 1)
     "bx"
     "E"
     "dashfill")
    (TeX-run-style-hooks
     "wasysym"
     "fancyvrb"
     "color"
     "verbatim"
     "listings"
     "hyperref"
     "textcomp"
     "fontenc"
     "T1"
     "lmodern"
     "enumerate"
     "tikz"
     "array"
     "float"
     "graphicx"
     "amssymb"
     "amsmath"
     "geometry"
     "letterpaper"
     "inputenc"
     "latin9"
     "latex2e"
     "art12"
     "article"
     "12pt"
     "a4paper")))

