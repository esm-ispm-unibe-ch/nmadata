## nmadata - network meta-analysis database
R package for accessing ISPM's database of Network meta-analyses.
Provides the database table for quering and access to individual network data.

## Installation for usage
Required CRAN packages:
- devtools
```install.packages("devtools")```
- readxl
```install.packages("readxl")```

Non-CRAN packages 
- dataformatter
```
library(devtools)
install_github("esm-ispm-unibe-ch/dataformatter")
install_github("esm-ispm-unibe-ch/nmadata")
```

## Listing networks
- The database is exported as a data.frame by the ```getNMADB()``` function. so one could do ```catalog = getNMADB()```
- ```nmadatanames(catalog)``` gives refid_autor's name_year of networks in catalog
- ```listVerified()```: get all records with usable* datasets
*checked with ```netmeta```

## Download individual networks
- ```readByID = function(refid,format="long",path)```
you can choose the format data will be **exported**, available formats are **long** (one
treatment per row) and **wide** (one comparison per row), for networks being
reported in **iv** format ```readByID``` outputs **iv**

### CAVEAT
The validity of the database is not guaranteed -- NOT INTENDED FOR REVIEWS

