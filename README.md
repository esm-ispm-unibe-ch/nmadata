##nmadata - network meta-analysis database
R package providing datasets for performing NMA with netmeta
The database is contained in a single ecxel file: ```catalog.xls```.
The package reads the catalog locally or remotely from the [github repo](https://github.com/esm-ispm-unibe-ch/nmadata/blob/master/nmadb/catalog.xlsx) and then can import and run netmeta on individual networks located in ```nmadb``` folder.
###Warning
-- THE DATASETS ARE FOR TESTING PURPOSES ONLY -- their validity is not checked -- NOT TO BE INCLUDED IN REVIEWS

##Installation for usage
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

##listing networks
- ```getCatalog()``` provides the catalog as a dataframe. Example: ```catalog = getCatalog()```
It lists only networks with data ("Outcome Data?"=="YES")
- ```nmadatanames(catalog)``` gives refid_autor's name_year of networks in
  catalog
- ```listVerified()```: get all files that are marked as verified meaning passed
  the ```tests/testData``` function that tries to run netmeta.

Alternitavely you can just check out ```nma/catalog.xls``` file.

##reading networks
- ```readByID = function(refid,format="long",path)```
you can choose the format data will be **exported**, available formats are **long** (one
treatment per row) and **wide** (one comparison per row), for networks being
reported in **iv** format ```readByID``` outputs **iv**

#Testing networks 
In order to test if netmeta can be run on a network run in ```R```
You need ```netmeta```
if for example need to check network 66666

```
source("tests/testDataset.R")
testData(66666)

``` 
