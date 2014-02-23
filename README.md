Mackerel Egg Survey Egg Production Estimation
==============

Contains code to quality control egg data from the Mackerel Egg Survey (MEGS) and produce estimates of total annual egg production.

Dependencies
--------------------
The analysis tools are dependent on the following R packages. Many of them may already be installed on your system
* `knitr` - Generation of reports (e.g. )
* `reshape` - Quick and easy manipulation of objects
* `sp`, `raster` - Spatial classes and tools for working with gridded data
* `lattice` - Plotting functions
* `tools` - Part of the base installation, used here to generate MD5 checksums
* `maps`, `mapdata`, `maptools` - Basic map database and tools for working with maps

Workflow
-----------
* Export the database to a `.csv` file, and save it in the `./data` directory. The name is not important, but it needs to be the only `.csv` file in this directory. The data import algorithm can handle both comma and semi-colon separated versions of csv.
* Run the script `Data_import.r` to import the data into R
* Run the script `Quality_assurance.r` to perform quality checking and preparation. This can be done through either a traditional source command:

```
source("src/Quality_assurance.r")
```
Alternatively, an HTML quality assurance report can also be produced via this command. First prepare the necessary environment

```
library(knitr)
opts_knit$set(root.dir=getwd())
options("markdown.HTML.options"=c(getOption("markdown.HTML.options"),"toc"))
```

then run the `spin()` command to build the HTML.

```
spin("src/Quality_assurance.r")
```

* Run the `Index_calculation.r` script to calcuate the indices. Graphical outputs are written to the `plots/` directory.