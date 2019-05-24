# austalk

<!-- badges: start -->
<!-- badges: end -->

The `austalk` package provides an interface to the Austalk speech collection on the Alveo Virtual Lab.  It
makes use of the `alveo` package to identify speakers and items for download. 

## Installation

You can install the released version of austalk fro
``` r
library(devtools)
install_github("Alveo", "alveo-r")
install_github("Alveo", "austalk-r")
```

You can then load the library to access Austalk data via the Alveo API.  The library contains functions
to allow you to get a list of speakers in Austalk with associated metadata; to find items for a list of 
speakers and to download item metadata and documents.  It provides an alternate method of accessing
Austalk data to the web based interfaces on [Alveo](https://app.alveo.edu.au) and the [Austalk website](https://austalk.edu.au).  

See the vignette in the package for an introduction to the functionality in the package.


