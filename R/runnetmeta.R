isOutcomeCompatibleWithMeasure = function(outcome, measure){
 binaries = c("OR","RR","RD") 
 continuouses = c("MD","SMD")
 rates = c("IRR")
 survivals = c("HR")
 iscomp = function(compatibles, sm) {
    out = F
    if (sm == "other"){
      out = F
    }else{
      out = sm %in% compatibles
    }
   return (out)
 }
 res = switch ( outcome
              , binary = {iscomp(binaries, measure)}
              , continuous = {iscomp(continuouses, measure)}
              , survival = {iscomp(survivals, measure)}
              , rate = {iscomp(rates, measure)}
              )
 return (res)
}

runnetmeta = function(recid,model="random", measure="notset"){
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
  dataeffect = indata$effect
  if(indata$effect == "other"){
      stop("don't know how to analyze atypical effect measure")
  }else{
    if(measure != "notset"){
    if (indata$format == "iv"){
      if (indata$effect == measure){
        sm = measure
      }else{
        stop("cannot change effect measure type in inverse variance dataset")
      }
    }else{
      if(isOutcomeCompatibleWithMeasure(type,measure)){
        sm = measure
      }else{
        stop(paste(type, " not compatible with ",measure))
      }
    }
    }else{
      sm = indata$effect
    }
  }
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
    Dpairs=pairwise(treat=t,event=r,n=n, data=D, studlab = id, sm= sm, allstudies = TRUE)
    metaNetw<-netmeta(TE,seTE,treat1,treat2,studlab,data=Dpairs,sm=sm,comb.fixed =F,comb.random = T, details.chkmultiarm=TRUE)
  } 
  
  if (type=="long_continuous"){
    Dpairs=pairwise(treat=t,mean=y,sd=sd,n=n,data=D, studlab =id, sm=sm, allstudies = TRUE)
    metaNetw<-netmeta(TE,seTE,treat1,treat2,studlab,data=Dpairs,sm=sm,comb.fixed =F,comb.random = T, details.chkmultiarm=TRUE, tol.multiarm=0.2)
  }

  if (type=="long_rate"){
    Dpairs=pairwise(treat=t,event=r,n=n,time=time, data=D, studlab =id, sm=sm, allstudies = TRUE)
    metaNetw<-netmeta(TE,seTE,treat1,treat2,studlab,data=Dpairs,sm=sm,comb.fixed =F,comb.random = T, details.chkmultiarm=TRUE)
  }
  
  if (type=="iv"){
    metaNetw=netmeta(TE=effect,seTE=se,treat1=t1,treat2=t2,studlab=id,data=D,sm=sm,comb.fixed =F,comb.random = T, details.chkmultiarm=TRUE, tol.multiarm=0.5)
  }

  return(metaNetw)
}
