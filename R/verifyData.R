
verifyData = function(recid, updateRecord=T) {
  print("checking")
  print(recid)
  tryCatch({
      metanet = runnetmeta(recid)
      if (! is.null(metanet)){
        if (updateRecord) {
          source("admin.token")
          updateVerifiedStatus(recid, 1)
          print("Updated record as verified database")
        }
        out = TRUE
      }else{
        out = FALSE
        stop("not passed")
      }
    },
    error = function(e) 
    {
      print(e$message) # or whatever error handling code you want
      out = FALSE
      #stop("not passed")
    }
  )
  return(list(recid=recid,passed=out))
}

updateVerifiedStatus = function (recid, isVerified=0) {
  library(jsonlite)
  datain = toJSON(data.frame(record_id = recid, verified = isVerified))
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
  tryCatch({
    res = Map(function(v) {
             verifyData(v, updateDB)
             },rcs)
    print("SUCCESS")
  }
  , error = function(e) 
    {
      print(c("Error in:",e))
    }
  )
  print(res)
}
