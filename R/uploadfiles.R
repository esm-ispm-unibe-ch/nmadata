rm(list=ls())
source("admin.token")
library(jsonlite)
library(RCurl)

response <- postForm(
    uri=NMADBURL,
    token=TOKEN,
    format='csv',
    content='record'
)

recs = read.csv(text = response)

havedata = recs[recs$data_available==1,]

filename = function (recid) { 
  out = paste("nmadb/",recid,".xlsx",sep="")
  return(out)
}

importreq = function (recid) {
  if(file.exists(filename(recid))){
    print(recid)
    postForm(
      uri=NMADBURL,
      token=TOKEN,
      content='file',
      action='import',
      record=as.character(recid),
      field='dataset',
      event='',
      returnFormat='json',
      file=fileUpload(filename(recid))
    )
  }else{
    stop("file",recid," doen't exists")
  }
}

mapply(importreq, havedata$ref_id)

print(importreq)
