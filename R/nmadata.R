
require(devtools)
require(netmeta)

catalogpath = "https://raw.githubusercontent.com/esm-ispm-unibe-ch/nmadata/master/"

#should be run from the root directory every time changes are made in the
#catalog.xlsx file
makeCatalog = function(path) {
  library(readxl)
  if (missing(path)){
    cfp = paste(catalogpath,"nmadb/catalog.xlsx",sep="")
    download.file(csf,"tmpfile")
    catalog = as.data.frame(
            read_xlsx("tmpfile",skip=1))
  }else{
    catalog = as.data.frame(
            read_xlsx(path,skip=1))
  }
  havedata = catalog[catalog$"Outcome Data?"=="YES",]
  write.csv2(havedata,"data/nmacatalog.csv")
  return (havedata)
}

getCatalog = function (locally=TRUE) {
  if (locally){
    catalog = read.csv2("data/nmacatalog.csv")
  }else{
    catalog = read.csv2(paste(catalogpath,"data/nmacatalog.csv",sep=""))
  }
  return (catalog)
}

nmadatanames = function (studies){
  nmalist = as.vector(paste(studies$Ref.ID,studies$First.Author,studies$Year,sep="_"));
  return (nmalist)
}

listVerified = function () {
  nmadatanames(verifiedStudies())
}

verifiedStudies = function (){
  nmacatalog = getCatalog(F)
  nmalist = nmacatalog[nmacatalog$verified==TRUE,]
  return (nmalist)
}

localVerifiedStudies = function (){
  nmacatalog = getCatalog()
  nmalist = nmacatalog[nmacatalog$verified==TRUE,]
  return (nmalist)
}

unverifiedStudies = function (){
  nmacatalog = getCatalog()
  nmalist = nmacatalog[nmacatalog$verified==FALSE,]
  return (nmalist)
}

checkedStudies = function (){
  nmacatalog = getCatalog()
  nmalist = nmacatalog[nmacatalog$checked==TRUE,]
  return (nmalist)
}

uncheckedStudies = function (){
  nmacatalog = getCatalog()
  nmalist = nmacatalog[nmacatalog$checked==FALSE,]
  return (nmalist)
}

readnmalocally = function(filename,format="long") {
  require(dataformatter)
  library(dataformatter)
  nmacatalog = getCatalog()
  data(nmacatalog)
  data(list=filename)
  dts = eval(parse(text=filename))
  dtsformat = nmacatalog[nmacatalog$label %in% filename,]$format 
  dtstype = nmacatalog[nmacatalog$short_name %in% filename,]$type
  if(dtsformat == format || dtsformat == "iv"){
    out = dts
  }else{
    if(format=="long"){
      out = wide2long(dts,dtstype)
    }else{
      out = long2wide(dts,dtstype)
    }
    dtsformat = format
  }
  return (list( name = filename
              , data   = out
              , type   = dtstype
              , format = dtsformat))
}

readnma = function(filename,format="long") {
  require(dataformatter)
  library(dataformatter)
  nmacatalog = getCatalog(F)
  data(nmacatalog)
  data(list=filename)
  dts = eval(parse(text=filename))
  dtsformat = nmacatalog[nmacatalog$label %in% filename,]$format 
  dtstype = nmacatalog[nmacatalog$short_name %in% filename,]$type
  if(dtsformat == format || dtsformat == "iv"){
    out = dts
  }else{
    if(format=="long"){
      out = wide2long(dts,dtstype)
    }else{
      out = long2wide(dts,dtstype)
    }
    dtsformat = format
  }
  return (list( name = filename
              , data   = out
              , type   = dtstype
              , format = dtsformat))
}

readByID = function(refid,format="long",path) {
  require(dataformatter)
  library(dataformatter)
  library(readxl)
  nmacatalog = getCatalog()
  if(missing(path)){
    file = paste(catalogpath,"nmadb/",refid,".xlsx",sep="")
    download.file(file,"tmpfile")
    dts = as.data.frame(
            read_xlsx("tmpfile"))
  }else{
    file = paste(path,refid,".xlsx",sep="")
    dts = as.data.frame(
            read_xlsx(file))
  }
  dtsformat = nmacatalog[nmacatalog$Ref.ID %in% refid,]$format 
  dtstype = nmacatalog[nmacatalog$Ref.ID %in% refid,]$Type.of.outcome
  if(dtsformat == format || dtsformat == "iv"){
    out = dts
  }else{
    if(format=="long"){
      out = wide2long(dts,dtstype)
    }else{
      out = long2wide(dts,dtstype)
    }
    dtsformat = format
  }
  return (list( name = refid
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

