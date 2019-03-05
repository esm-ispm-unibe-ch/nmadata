
library(devtools)
install_github("esm-ispm-unibe-ch/nmadata")
#source("R/runnetmeta.R")
library(nmadata)

verifyVerified = function () {
  vers = as.vector(verifiedStudies()$"Record.ID")
  tryCatch({
    res = Map(function(v) {
                return (verifyData(v, updateRecord=F))
                    },vers)
    print("SUCCESS")
  }
  , error = function(e) 
    {
      stop(c("Verified column is wrong in database. Error:",e))
    }
  )
  print(res)
}

updateVerified = function () {
  nmadb=getNMADB()
  checkRecords(nmadb[nmadb$Data.available=="True",],T)
}
