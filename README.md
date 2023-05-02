
# HydRochemistRy

<!-- badges: start -->
<!-- badges: end -->

The goal of HydRochemistRy is to provide functions for water quality dataset analysis. 
Datasets can include field and lab measurements. Special focus is on groundwater quality data. 

Functionalities include:
- calculation of meq/l from concentrations and checking electrical balance of samples;
- acquiring descriptive statistics per parameter;
- creating Piper plots of water types with input of several descriptive columns (sampletype, EC, geology)
- 

## Installation

You can install the development version of HydRochemistRy like so:

``` r
library(HydRochemisRy)
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(HydRochemistRy)
## basic example code
## data structure
str(data)
head(data)

## calculate meql and check electrical balance >10%
meql(data) %>% dplyr::filter(check == "error")

## Descriptive statistics


## PiperPlot
### sampletype


### EC


### geology


### year??


```

