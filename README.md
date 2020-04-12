# Synthetic control using lasso (scul)

This repository contains the R package `scul` that is used in Hollingsworth and Wing (2020) *"Tactics for design and inference in synthetic control studies: An applied example using high-dimensional data."*


<figure style="float:center;">
<img src="https://github.com/hollina/scul/blob/master/vignettes/vignette_output/ReadMeFigure.png"  width="800"  /> 
</figure>


## What does this package do?

**Abstract**: 

The synthetic control methodology is a strategy for estimating causal treatment effects for idiosyncratic historical events.
In the typical application developed by Abadie, Diamond, and Hainmuller (2010), researchers observe time series outcomes for both a treated unit and a number of untreated units.
A weighted average of the untreated series is used to construct a counterfactual estimate of the treated series, which is referred to as a synthetic comparison group.
Weights are chosen to minimize discrepancies between the synthetic comparison group and the treated unit in the pre-treatment time period.
Treatment effect estimates are taken to be the difference between observed outcomes and a synthetic counterfactual.
Statistical inference is normally organized around a placebo analysis; in which, pseudo-treatment effects are estimated for many untreated placebo units, and the distribution of pseudo-estimates represents the null distribution of no treatment effect.


Recent methodological work has proposed a number of alternative strategies for estimating synthetic control weights  [@Arkhangelsky2018; @Doudchenko2017; @Powell2019].
In a similar vein, we use a method called **Synthetic Control Using Lasso (SCUL)** to construct donor weights.  
This method is a flexible, data-driven way to construct synthetic control groups.
It relies on lasso regressions, which are popular in the machine-learning literature, and favor weights that predict well out of sample.
In general, the approach allows for a high-dimensional donor pool that may be larger than the number of time periods, extrapolation from the donor pool, counter-cyclical weights, and the same model selection procedure to be used for target and placebo series. 

This package provides code to implement the SCUL procedure from Hollingsworth and Wing (2020). 

Because the data in our working paper cannot be posted online, we also provide an example that uses publicly available data.
The entire procedure or parts of the method may be useful in many settings.
Feel free to use any or all of the code; it is available under the MIT license.



## Data Sources:

We obtained data on **EVALI cases** in 2019 at the state-level from the Centers for Disease Control and Prevention (CDC). Available here: <http://dx.doi.org/10.15585/mmwr.mm6839e1>


## Software Used:
The package is made for R. and was developed on a Unix machine using R 3.6.1. See session info in the vignette for exact version of every package used. 

## License:
Replication Package (this github repo): [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Working Paper: [![License: CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)