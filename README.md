# Replication Package for Development Economics

As part of [Clément Imbert's](https://sites.google.com/site/clemimbert/) class in Development Economics, this project will use R to replicate 5 exhibits from: 

*Rossi, Federico. 2022. "The Relative Efficiency of Skilled Labor across Countries: Measurement and Interpretation." American Economic Review 112 (1): 235–66.*

The original data and code can be found [here](https://www.openicpsr.org/openicpsr/project/146041/version/V1/view).

The project is ongoing, and is being updated throughout the term. Thanks for your patience!

## Computationl Requirements 

The replication was conducted on R version 4.4.1. The necessary packages to install are: 
- haven 
- tidyr 
- dplyr 
- kableExtra 
- stringr 

## Instructions to Replicators 

Scripts are named after their respective purposes. 

The .r script `_master_script.r` replicates figures 2 and 8, as well as tables 2, 3 and 4, exactly. For this code to work, replicators must first download the relevant `.dta` files from the original replication package [here](https://www.openicpsr.org/openicpsr/project/146041/version/V1/view), which I detail below: 

- `Input/temp/AQ.dta` 
- `Input/temp/Q.dta`
- `Input/temp/devacc.dta`
- `Input/temp/gdp_pwt.dta`

Alternatively, the replicator can replace the entire `Input` folder with the `Input` folder from the original replication package. 

Finally, running 

## Final Comment
