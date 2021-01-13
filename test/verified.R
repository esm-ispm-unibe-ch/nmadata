
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


getProblematics = function () {
  lkj = getNMADB()
  bins = subset(subset(subset(lkj, Type.of.Outcome. == "Binary"), Data.available == "True"), Verified == "False")
  write.csv(list(id=bins$Record.ID, diagnosis=bins$Error),"binsn.csv")
  conts = subset(subset(subset(lkj, Type.of.Outcome. == "Continuous"), Data.available == "True"), Verified == "False")
  write.csv(list(id=conts$Record.ID, diagnosis=conts$Error),"contsn.csv")
  survs = subset(subset(subset(lkj, Type.of.Outcome. == "Survival"), Data.available == "True"), Verified == "False")
  write.csv(list(id=survs$Record.ID, diagnosis=survs$Error),"survsn.csv")
  rates = subset(subset(subset(lkj, Type.of.Outcome. == "Rate"), Data.available == "True"), Verified == "False")
  write.csv(list(id=rates$Record.ID, diagnosis=rates$Error),"ratesn.csv")
}
