
getNMADB = function () {
  library(RCurl)
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

readByID = function(recid) {
  catalog = getNMADB()
  library(readxl)
  fl = tempfile(fileext=".xlsx")
  exportData(recid,fl)
  dts = as.data.frame(read_xlsx(fl))
  dtsformat = catalog[catalog$Record.ID %in% recid,]$Format 
  dtstype = catalog[catalog$Record.ID %in% recid,]$Type.of.Outcome
  effecttype = as.character(catalog[catalog$Record.ID %in% recid,]$Effect.Measure)
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
