
verifyData = function(recid, updateRecord=F) {
  print("checking")
  print(recid)
  tryCatch({
      metanet = runnetmeta(recid)
      if (! is.null(metanet)){
        if (updateRecord == T) {
          source("admin.token")
          updateVerifiedStatus(recid, 1)
          print("Updated record as verified in database")
        }
        out = TRUE
      }else{
        out = FALSE
      }
    },
    error = function(e) 
    {
      print(e$message) # or whatever error handling code you want
      if (updateRecord == T) {
        source("admin.token")
        updateVerifiedStatus(recid, 0, e$message)
        print("Updated record as not verified in database")
      }
      out = FALSE
      #stop("not passed")
    }
  )
  return(list(recid=recid,passed=out))
}

updateVerifiedStatus = function (recid, isVerified=0, err='') {
  library(jsonlite)
  if( isVerified == 1){
    datain = toJSON(data.frame(record_id = recid, verified = isVerified, error = ''))
  }else{
    datain = toJSON(data.frame(record_id = recid, verified = isVerified, error = err))
  }
    postForm(
      uri=NMADBURL,
      token=ADMINTOKEN,
      content='record',
      action='import',
      format='json',
      event='',
      data=datain,
      returnFormat='json'
    )
}

checkRecords = function (records, updateDB=F) {
  rcs = as.vector(records$"Record.ID")
  res = Map(function(v) {
          out = list(rec=v,verified=F)
          tryCatch({
            verifyData(v, updateDB)
            out = list(rec=v,verified=T)
          },error = function (e){
            print(e$message)
            out = list(rec=v,verified=F)
          })
         return(out)
        },rcs)
}

