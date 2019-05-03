#' NMA catalog
#'
#' Petropoulou et al. compiled a database of network meta-analyses to 
#' serve as a source for empirical studies [1].
#' The database is hosted in a REDcap database at the 
#' Institute of Social and Preventive Medicine (ISPM) in the University of Bern.
#' Function getNMADB downloads the list of networks included in [1].
#'
#' @references  
#' [1] Petropoulou M. et al. 2016 Bibliographic study showed improving statistical methodology 
#' of network meta-analyses published between 1999 and 2015,
#' \emph{Journal of Clinical Epidemiology}, 20--28 
#' \cite{DOI:10.1016/j.jclinepi.2016.11.002}
#' 
#' @examples
#' catalog = getNMADB()
#' #Networks that labeled Verified have outcome data 
#' #that allow the analysis to be repeated.
#' nmalist = catalog[catalog$Verified=="True",]
#' nmalist
#' @return A data.frame with the network meta-analyses included in [1]. 
#' Several characterstics related to the publications 
#' (e.g. Journal.Name, Title) are included in the data.frame.
#' @export getNMADB
getNMADB = function () {
  tryCatch({
  response <- RCurl::postForm(
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
  catalog = utils::read.csv(text = response)
  return (catalog)
  },error=function(cond){
    message(paste("could not open database connection",cond))
    return(NULL)
  })
}

#' Read NMA dataset
#'
#' ReadByID downloads the dataset and main characteristics 
#' of the specified network meta-analysis.
#' You can list all ids from the catalog by calling \code{getNMADB}.
#' 
#' @param recid Record id of network
#' @return A list with the name (id), data (dataset), type (continuous, binary,
#' rate, survival), effect (type of measure: RR OR RR RD ...), 
#' format (long, wide, iv). 
#' -'long' refers to data where each row represents a study treatment arm, 
#' -'wide' refers to data where each row represents a study treatment comparison
#' -'iv' refers to an 'inverse variance' format, where a comparison specific 
#'   estimate of the treatemtn effect and its standard error are reported.  
#' @export readByID
readByID = function(recid) {
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
  catalog = getNMADB()
  fl = tempfile(fileext=".xlsx")
  tryCatch({
    exportData(recid,fl)
  dts = as.data.frame(readxl::read_xlsx(fl))
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
  },error=function(cond){
    message(paste("Could not download dataset ",cond))
    return(NULL)
  })
}
