(TeX-add-style-hook
 "bookdown-pdf-template"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("znufethesis" "doctor" "twoside" "chapterhead" "otf")))
   (TeX-run-style-hooks
    "latex2e"
    "creative"
    "abstract"
    "denotation"
    "znufethesis"
    "znufethesis10"
    "listings"
    "fancyvrb"
    "titling")
   (TeX-add-symbols
    '("subtitle" 1)
    '("passthrough" 1)
    "tightlist")
   (LaTeX-add-environments
    '("denotation" LaTeX-env-args ["argument"] 0))
   (LaTeX-add-bibliographies
    "$for(bibliography)$$bibliography$$sep$"
    "$endfor$"))
 :latex)

