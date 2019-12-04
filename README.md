
# VFtree

<!-- badges: start -->
<!-- badges: end -->

## Description


An R-package for visualization of Virulence Factors and Phylogenetic Tree of bacterial pan-genome

The goal of VFtree is to build a phylogenetic Tree based on the tree data and the present/absent sheet of the gene/factors to plot a circular prepresentation of the matched information. This package provide a pan-genome approach to easily illustrate the data among strains at the species level.


## Installation

You can install the released version of VFtree  GitHub using the following code:

``` r
 library(devtools)
 install github("<user name>/<package name>")
 library(<package name>)

```
To run the shinyApp

``` r
runVFtree()
```

## Overview

``` r
browseVignettes("VFtree")
```

This package contains 3 .R file composed of 11 functions to visualize the virulence factors along with the circular phylogenetic tree. THere are three plotting functions avaliable for users.

combinedPlot():

ringPlot():


treePlot():

runVFtree(): Is the function that launches the shiny app for this package.

Refer to package vignettes for more details. The package tree structure is provided below (this is optional).

An overview of the package is ullustrated below.

result plot
![GitHub Logo](.inst/metadata/image/pitch.png)

## Contributions

This is a basic example which shows you how to solve a common problem:

``` r
library("VFtree")
lsf.str("package:VFtree")
```

The author of the package is Bihan Zhu. The functions treePlot, ringPlot, CombinedPlot and were authored by Bihan. 

The functions CircularFunc was authored by xxx from stackoverflow. 

Part of the code for InfCriteria_v3 function has been taken from mclust R package. 

Section of the borrowed code is clearly indicated and referenced in the InfCriteria_calculation.R file, the R file corresponding to InfCriteria_v3 function and referenced in help file. Rest of the code was a contribution by Bihan. 

## References


![GitHub Logo](.inst/metadata/image/pitch.png)

