#Test if xls file is a valid dataset by running netmeta 
# Should be run in the root folder of the package
Sys.setenv(LANG = "en")
rm(list=ls())
library(devtools)

install_github("esm-ispm-unibe-ch/flow_contribution")
#install.packages("../contribution_0.2.0.tar.gz",repos=NULL)

source("R/nmadata.R");

makeCatalog("nmadb/catalog.xlsx")
catalog = getCatalog()

nmadatanames(catalog)

print("verified")
dones = nmadatanames(verifiedStudies())
print(dones)

print("unverified")
todos = nmadatanames(unverifiedStudies())
print(todos)

hatmatrixLocally = function(dataset,model="random"){
library(contribution)
  indata = readByID(dataset,format="long",path="./nmadb/")
  type = indata$type
  if(type == "binary"){
    sm = "OR"
  }else{
    sm = "SMD"
  }
  C = getHatMatrix(indata$data,type=longType(indata),model,sm,tau="NA")
  return(C)
}

testData = function(refid) {
  checked = length(checkedStudies()$"Ref.ID")
  unchecked = length(uncheckedStudies()$"Ref.ID")
  pers = (checked / (checked + unchecked)) * 100
  print(c(pers,"% are verified"))
  print("checking")
  print(refid)
  tryCatch({
      HM = hatmatrixLocally(refid)
      if (! is.null(HM$H)){
        print("you can mark it as verified in catalog.xlsx")
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
                print(v)
                return (testData(v))
                    },vers)
    print("SUCCESS")
  }
  , error = function(e) 
    {
      stop("verified column is wrong in catalog.xlsx")
    }
  )
  print(res)
}

getVerified = function() {
  alls = as.vector(getCatalog()$"Ref.ID")
  #alls = head(as.vector(getCatalog()$"Ref.ID"),20)
  res = Map(function(v) {
        tryCatch({
            return (testData(v)$passed)
          }
          , error = function(e){
            return(FALSE)
          }
        )
                  },alls)
  return(res)
}
