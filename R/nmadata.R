
listnmadata = function() {
  library(nmadata)
  print("Here is the list of available datasets")
  #nmalist = read.csv("../data/catalog.csv",header=T
                     #, colClasses = c(rep("character",4),"numeric"))
  data(nmacatalog)
  nmalist = nmacatalog;
  return (nmalist)
}

readnma = function(filename,format="long") {
  require(dataformatter)
  library(dataformatter)
  library(nmadata)
  data(nmacatalog)
  data(list=filename)
  dts = eval(parse(text=filename))
  dtsformat = nmacatalog[nmacatalog$short_name %in% filename,]$format 
  dtstype = nmacatalog[nmacatalog$short_name %in% filename,]$type
  if( dtsformat == format){
    out = dts
  }else{
    if(format=="long"){
      out = wide2long(dts,dtstype)
    }else{
      out = long2wide(dts,dtstype)
    }
  }
  return (out)
}
