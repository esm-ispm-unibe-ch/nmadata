#' Get the catalog of network meta-analysis
#'
#' This function downloads the catalog of networks from ISPM UniBe
#' REDCap database
#' 
#' 
#' 
#' 
#' 
#' @param 
#' @return data.frame with nmas
#' @export
getNMADB = function () {
  response <- RCurl::postForm(
      uri=NMADBURL,
      token=PUBLICTOKEN,
      content='record',
      format='csv',
      type='flat',
      rawOrLabel='label',
      csvDelimiter=';',
      rawOrLabelHeaders='label'
  )
  rfn = tempfile()
  write(file=rfn, response)
  if (file.exists(rfn)){
    catalog = read.csv2(file=file(rfn,encoding="WINDOWS-1252"), header = T, sep=";")
    file.remove(rfn)
  }else{
    stop("could not create tmp db file")
  }
  return (catalog)
}

nmadatanames = function (studies){
  nmalist = as.vector(paste(studies$Record.ID
                           , studies$First.Author
                           , studies$Year
                           , sep="_"));
  return (nmalist)
}

verifiedStudies = function (){
  catalog = getNMADB()
  nmalist = catalog[catalog$Verified=="True",]
  return (nmalist)
}

listVerified = function () {
  nmadatanames(verifiedStudies())
}

unverifiedStudies = function (){
  catalog = getNMADB()
  nmalist = catalog[catalog$Verified==FALSE,]
  return (nmalist)
}

exportData = function (recid, filename) {
  print(paste(c("getting dataset:",recid)))
  writeBin(as.vector(
              RCurl::postForm(
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

#' Download dataset of nma from REDCap
#'
#' ReadByID downloads the dataset and main characteristics of network.
#' You can list all ids from the catalog by calling \code{getNMADB}
#' 
#' @param recid Record id of network
#' @return list with the name (id), data (dataset), type (continuous, binary,
#' rate, survival), effect (type of measure: RR OR RR RD ...), format (long,
#' wide, iv)
#' @export
readByID = function(recid) {
  catalog = getNMADB()
  fl = tempfile(fileext=".xlsx")
  exportData(recid,fl)
  dts = as.data.frame(read_xlsx(fl))
  file.remove(fl)
  dtsformat = 
    catalog[catalog$Record.ID %in% recid,]$Format 
  dtstype = 
    catalog[catalog$Record.ID %in% recid,]$Type.of.Outcome
  effecttype = 
    as.character(catalog[catalog$Record.ID %in% recid,]$Effect.Measure)
  dtseffect = switch( effecttype
                    , "odds ratio"={"OR"}
                    , "risk ratio"={"RR"}
                    , "hazard ratio"={"HR"}
                    , "rate ratio"={"IRR"}
                    , "risk difference"={"RD"}
                    , "mean difference"={"MD"}
                    , "standardized mean difference"={"SMD"}
                    , "other"={"other"}
                    )
  out = dts
  return (list( name   = recid
              , data   = out
              , type   = tolower(dtstype)
              , effect = dtseffect
              , format = dtsformat))
}
