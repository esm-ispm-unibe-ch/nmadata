
nmadatasummary = function() {
  library(nmadata)
  print("Here is the list of available datasets")
  #nmalist = read.csv("../data/catalog.csv",header=T
                     #, colClasses = c(rep("character",4),"numeric"))
  data(nmacatalog)
  nmalist = nmacatalog;
  return (nmalist)
}

nmadatanames = function (){
  library(nmadata)
  data(nmacatalog)
  nmalist = as.vector(nmacatalog$short_name);
  return (nmalist)
}

readnma = function(filename,format="long") {
  require(dataformatter)
  library(dataformatter)
  data(nmacatalog)
  data(list=filename)
  dts = eval(parse(text=filename))
  dtsformat = nmacatalog[nmacatalog$short_name %in% filename,]$format 
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

#indata := output of readnma
longType = function (indata) {
  if(indata$format != "iv"){
    paste("long_",indata$type,sep="")
  }else{
    "iv"
  }
}
