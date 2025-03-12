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

Scripts are named after their respective purposes. `Table_2.r`, `Table_3.r`, `Table_4.r`, `Figure_2.r`, and `Figure_8.r` exactly replicate tables 2, 3, and 4, as well as figures 2 and 8, respectively. The `_master.r` script runs all previously mentionned scripts.

For the code to work, replicators must first download the relevant `.dta` files from the original replication package [here](https://www.openicpsr.org/openicpsr/project/146041/version/V1/view), which I detail below: 

- `Input/temp/AQ.dta` 
- `Input/temp/Q.dta`
- `Input/temp/devacc.dta`
- `Input/temp/gdp_pwt.dta`

Alternatively, the replicator can replace the entire `Input` folder with the `Input` folder from the original replication package. Before running the code, the file structure should at the very least look like this: 

``` 
devpkg/
├── Code/
│   ├── master.r
│   ├── Figures/
│   │    ├── Figure_2.r
│   │    └── Figure_8.r 
│   └── Tables/
│   │    ├── Table_2.r
│   │    ├── Table_3.r
│   │    └── Table_4.r 
├── Input/
│   ├── temp/
│   │   ├── AQ.dta 
│   │   ├── devacc.dta 
│   │   ├── gdp_pwt.dta
│   │   └── Q.dta 
└── Output/
    ├── Figures/
    └── Tables/ 
```

Finally, running the `_master.r` script will replicate all figures. 

## Final Comment

This replication code does not replicate the Rossi (2022) paper in full. In a first step towards doing so, there is a substantial amount of STATA cleaning code to translate, as well as intermediary analysis scripts which finally yeild the working datasets. For this reason, the author kindly provided the final cleaned, working datasets in the `temp` folder of his replciation package, which this package relies on. Extending this package to fully replicate all results from Rossi (2022) may be 