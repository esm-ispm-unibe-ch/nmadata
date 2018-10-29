#Test if xls file is a valid dataset by running netmeta 
# Should be run in the root folder of the package
Sys.setenv(LANG = "en")
rm(list=ls())
library(devtools)

install_github("esm-ispm-unibe-ch/flow_contribution")
#install.packages("../contribution_0.2.0.tar.gz",repos=NULL)

source("R/nmadata.R");
source("admin.token")

nmadatanames(catalog)

print("verified")
dones = nmadatanames(verifiedStudies())
print(dones)

#print("unverified")
#todos = nmadatanames(unverifiedStudies())
#print(todos)

hatmatrixLocally = function(dataset,model="random"){
library(contribution)
  indata = readByID(dataset,format="long")
  type = indata$type
  sm = switch( type
             , binary={"OR"}
             , continuous={"SMD"}
             , rate={"OR"}
             , survival={"HR"}
       )
  C = getHatMatrix(indata$data,type=longType(indata),model,sm,tau="NA")
  return(C)
}

testData = function(refid) {
  print("checking")
  print(refid)
  tryCatch({
      HM = hatmatrixLocally(refid)
      if (! is.null(HM$H)){
        print("Updated record as verified database")
        out = TRUE
      }else{
        out = FALSE
        #stop("not passed")
      }
    },
    error = function(e) 
    {
      print(e$message) # or whatever error handling code you want
      out = FALSE
      #stop("not passed")
    }
  )
  return(list(refid=refid,passed=out))
}

verifyVerified = function () {
  vers = as.vector(verifiedStudies()$"Ref.ID")
  tryCatch({
    res = Map(function(v) {
                return (testData(v))
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
