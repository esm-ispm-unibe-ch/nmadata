
runnetmeta = function(recid,model="random"){
  indata = readByID(recid)
  type = indata$type
  format = as.character(indata$format)
  longType = function (indata) {
    if(indata$format != "iv"){
      paste("long_",indata$type,sep="")
    }else{
      "iv"
    }
  }
  sm = switch( type
             , binary={"OR"}
             , continuous={"SMD"}
             , rate={"OR"}
             , survival={"HR"}
       )
  makelong = function () {
                  library(devtools)
                  install_github("esm-ispm-unibe-ch/dataformatter")
                  library(dataformatter)
                  return(wide2long(indata$data,indata$type))
             }
  data = switch (format
                , long = {indata$data}
                , wide = {makelong()}
                , iv = {indata$data}
                )
  print(data)
  C = getmetaNetw(data,type=longType(indata),model,sm,tau="NA")
  return(C)
}


getmetaNetw = function(indata,type,model="fixed",tau=NA, sm){
  
  require(netmeta)
  library(meta)
  library(plyr)
  
  D <- indata
  
  #network meta-analysis
  
  if (type=="long_binary"){
    Dpairs=pairwise(treat=t,event=r,n=n, data=D, studlab = id, sm= sm)
    metaNetw<-netmeta(TE,seTE,treat1,treat2,studlab,data=Dpairs,sm=sm,comb.fixed =F,comb.random = T)
  } 
  
  if (type=="long_continuous"){
    Dpairs=pairwise(treat=t,mean=y,sd=sd,n=n,data=D, studlab =id, sm=sm)
    metaNetw<-netmeta(TE,seTE,treat1,treat2,studlab,data=Dpairs,sm=sm,comb.fixed =F,comb.random = T, tol.multiarm=0.05)
  }
  
  if (type=="iv"){
    metaNetw=netmeta(effect,se,t1,t2,id,data=D,sm=sm,comb.fixed =F,comb.random = T, tol.multiarm=0.05)
  }

  return(metaNetw)
}
