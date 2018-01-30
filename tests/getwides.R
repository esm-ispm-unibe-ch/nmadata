rm(list=ls())
library(devtools)
build()
#source("../R/nmadata.R")
install.packages("~/Documents/nmadata_1.0.tar.gz",repos=NULL)
library(nmadata)

lkj = readnma("diabetes_indr")
lkj
