rm(list=ls())
source("public.token")
library(jsonlite)
library(RCurl)


require(devtools)
require(netmeta)

install_github("esm-ispm-unibe-ch/dataformatter")
require(dataformatter)

getCatalog = function () {
  response <- postForm(
      uri=NMADBURL,
      token=PUBLICTOKEN,
      content='record',
      format='csv',
      type='flat',
      rawOrLabel='label',
      rawOrLabelHeaders='label',
      exportSurveyFields='true',
      exportCheckboxLabel='false',
      exportDataAccessGroups='false',
      returnFormat='json'
  )

  catalog = read.csv(text = response)
  return (catalog)
}

catalog = getCatalog()

nmadatanames = function (studies){
  nmalist = as.vector(paste(studies$Ref.ID
                           , studies$First.Author
                           , studies$Year
                           , sep="_"));
  return (nmalist)
}

verifiedStudies = function (){
  nmalist = catalog[catalog$Verified=="True",]
  return (nmalist)
}

listVerified = function () {
  nmadatanames(verifiedStudies())
}

unverifiedStudies = function (){
  nmalist = catalog[catalog$Verified==FALSE,]
  return (nmalist)
}


exportData = function (recid, filename) {
  print(recid)
  writeBin(as.vector(
              postForm(
                uri=NMADBURL,
                token=PUBLICTOKEN,
                content='file',
                action='export',
                record=as.character(recid),
                field='dataset',
                event='',
                returnFormat='json',
                binary=TRUE
              )
          ), filename) 
}

#refid, format you like the dataset to be: long, wide, iv
readByID = function(refid,format="long") {
  require(dataformatter)
  library(readxl)
  fl = tempfile(fileext=".xlsx")
  exportData(refid,fl)
  dts = as.data.frame(
          read_xlsx(fl))
  dtsformat = catalog[catalog$Ref.ID %in% refid,]$Format 
  dtstype = catalog[catalog$Ref.ID %in% refid,]$Type.of.Outcome
  if(dtsformat == format || dtsformat == "iv"){
    out = dts
  }else{
    if(format=="long"){
      out = wide2long(dts,dtstype)
    }else{
      print(dtstype)
      out = long2wide(dts,dtstype)
    }
    dtsformat = format
  }
  return (list( name   = refid
              , data   = out
              , type   = tolower(dtstype)
              , format = dtsformat))
}

#indata := output of readnma
longType = function (indata) {
  if(indata$format != "iv"){
    paste("long_",indata$type,sep="")
  }else{
    "iv"
  }
}


