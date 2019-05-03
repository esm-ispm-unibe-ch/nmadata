response = function () {
  RCurl::postForm(
    uri=NMADBURL,
    token=PUBLICTOKEN,
    format='csv',
    content='record'
  )
}

recs = function () {
  return(read.csv(text = response()))
}

havedata = function () {
  return (recs()[recs()$data_available==1,])
}

filename = function (recid) { 
  out = paste("nmadb/",recid,".xlsx",sep="")
  return(out)
}

importreq = function (recid) {
  if(file.exists(filename(recid))){
    print(recid)
    RCurl::postForm(
      uri=NMADBURL,
      token=ADMINTOKEN,
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

uploaddatasets = function () {
  mapply(importreq, havedata()$record_id)
  print(importreq)
}

