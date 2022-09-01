---
project: strff
summary: Strings for Fortran
project_website: https://gitlab.com/everythingfunctional/strff
project_download: https://gitlab.com/everythingfunctional/strff/-/releases
author: Brad Richardson
email: everythingfunctional@protonmail.com
website: https://everythingfunctional.com
twitter: https://twitter.com/everythingfunct
github: https://github.com/everythingfunctional
src_dir: ../src
display: public
         protected
         private
sort: permission-alpha
output_dir: ../public
graph: true
extra_mods: iso_varying_string:https://everythingfunctional.gitlab.io/iso_varying_string/
...

This module defines a library of string functions not available as intrinsics,
but which are generally available in other languages. For example, the generic
`to_string` function is provided for converting intrinsic types to strings. I.e.
`print *, "The answer is: " // to_string(42)`.