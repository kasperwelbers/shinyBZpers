
<!-- README.md is generated from README.Rmd. Please edit that file -->
shinycopy
=========

Trial version for a text reuse Shiny app

This only works with specific data for a project, so it's not yet intended for use outside of those involved (though at some point a more general version will probably be shared)

How to use
----------

This requires the development version of corpustools.

``` r
devtools::install_github('kasperwelbers/corpustools')
devtools::install_github('kasperwelbers/shinyBZcopy')
```

The following code reads the data from amcat and launches an instance at the given port.

``` r
library(amcatr)
conn = amcat.connect('https://amcat.nl')
bz_data = create_bz_data(conn, project=1916, pers_set=79431, nieuws_set=79457, 
                         min.similarity=0.2, ngrams=3)
bz_app(bz_data, port = 6171)
```
