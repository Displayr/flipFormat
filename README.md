[![](https://travis-ci.org/Displayr/flipFormat.svg?branch=master)](https://travis-ci.org/Displayr/flipFormat/)
[![Coverage Status](https://coveralls.io/repos/github/Displayr/flipFormat/badge.svg?branch=master)](https://coveralls.io/github/Displayr/flipFormat?branch=master)
# flipFormat

Formatting of R outputs

To install from GitHub
```
require(devtools)
cran.pkgs <- c("colorspace", "htmltools", "htmlwidgets", "rmarkdown", "knitr",
	       "sparkline", "stringr")
install.packages(cran.pkgs)
gh.pkgs <- c("Displayr/flipU", "Displayr/rhtmlMetro",
	     "rstudio/DT@bf60e431578638ba56e227090af4e1aa5f9add55",
	     "renkun-ken/formattable")
devtools::install_github(gh.pkgs)
devtools::install_github("Displayr/flipFormat", dependencies = FALSE)
```

The additional steps are required because the `Suggest`ed package
`flipChoice` is not publicly available. However, the package can still
be installed and used without it.

[![Displayr logo](https://mwmclean.github.io/img/logo-header.png)](https://www.displayr.com)
