# Replication of Rossi (2022, AER) Exhibits in R

As part of [Clément Imbert's](https://sites.google.com/site/clemimbert/) class in Development Economics, this project will use R to replicate 5 exhibits from: 

*Rossi, Federico. 2022. "The Relative Efficiency of Skilled Labor across Countries: Measurement and Interpretation." American Economic Review 112 (1): 235–66.*

The original data and code can be found [here](https://www.openicpsr.org/openicpsr/project/146041/version/V1/view).

## Computational Requirements 

The replication was conducted on R version 4.4.1. The necessary packages to install are: 
- haven 
- tidyr 
- dplyr 
- kableExtra 
- stringr 

## Instructions to Replicators 

Scripts are named after their respective purposes. `Table_2.r`, `Table_3.r`, `Table_4.r`, `Figure_2.r`, and `Figure_8.r` exactly replicate tables 2, 3, and 4, as well as figures 2 and 8, respectively. The `_master.r` script runs all previously mentionned scripts.

For the code to work, replicators must first download the relevant `.dta` files from the original replication package [here](https://www.openicpsr.org/openicpsr/project/146041/version/V1/view), which I detail below: 

- `2_Input/temp/AQ.dta` 
- `2_Input/temp/Q.dta`
- `2_Input/temp/devacc.dta`
- `2_Input/temp/emphrs_all.dta`
- `2_Input/temp/emphrs_brazil.dta`
- `2_Input/temp/emphrs_canada.dta`
- `2_Input/temp/emphrs_india.dta`
- `2_Input/temp/emphrs_indonesia.dta`
- `2_Input/temp/emphrs_israel.dta`
- `2_Input/temp/emphrs_jamaica.dta`
- `2_Input/temp/emphrs_mexico.dta`
- `2_Input/temp/emphrs_panama.dta`
- `2_Input/temp/emphrs_tt.dta`
- `2_Input/temp/emphrs_uruguay.dta`
- `2_Input/temp/emphrs_usa.dta`
- `2_Input/temp/emphrs_venezuela.dta`
- `2_Input/temp/gdp_pwt_temp.dta`
- `2_Input/data/pwt/gdp_pwt.dta`

Alternatively, the replicator can replace the entire `2_Input` folder with the `Input` folder from the original replication package. Before running the code, the file structure should *at the very least* look like this: 

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
│   ├── data/
│   │   └──pwt/
│   │   │   └──gdp_pwt.dta
│   ├── temp/
│   │   ├── AQ.dta 
│   │   ├── devacc.dta 
│   │   ├── emphrs_all.dta
│   │   ├── emphrs_brazil.dta
│   │   ├── emphrs_canada.dta
│   │   ├── emphrs_india.dta
│   │   ├── emphrs_indonesia.dta
│   │   ├── emphrs_israel.dta
│   │   ├── emphrs_jamaica.dta
│   │   ├── emphrs_mexico.dta
│   │   ├── emphrs_panama.dta
│   │   ├── emphrs_tt.dta
│   │   ├── emphrs_uruguay.dta
│   │   ├── emphrs_usa.dta
│   │   ├── emphrs_venezuela.dta
│   │   ├── gdp_pwt_temp.dta
│   │   └── Q.dta 
└── Output/
    ├── Figures/
    └── Tables/ 
```

Finally, running the `_master.r` script will replicate all figures. 

## Final Comment

This replication code does not replicate the Rossi (2022) paper in full. In a first step towards doing so, there is a substantial amount of STATA cleaning code to translate, as well as intermediary analysis scripts, which finally yeild the working datasets. For this reason, the author kindly provides the final cleaned, working datasets in the `temp` folder of his replciation package, which this package relies on. Extending this package to fully replicate all Rossi (2022) results from scratch is left as a future project. 