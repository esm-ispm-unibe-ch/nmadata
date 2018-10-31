
library(devtools)
install_github("esm-ispm-unibe-ch/nmadata",ref="redcap")
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
      stop("Verified column is wrong database")
    }
  )
  print(res)
}
