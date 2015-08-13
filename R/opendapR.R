
#get longnames of varaibles
longnames <- function(nc){
  l = names(nc$var)
  names(l) = sapply(nc$var,function(x) x$longname)
  l
}
#for var return dim names

dimi <- function(var,name) which(vardims(var)==name)
dimunits <- function(var,name) {var$dim[[dimi(var,name)]]$units}
dimmin <- function(var,name) min(var$dim[[dimi(var,name)]]$vals)
dimmax <- function(var,name) max(var$dim[[dimi(var,name)]]$vals)
dimres <- function(var,name) ggplot2::resolution(var$dim[[dimi(var,name)]]$vals)

dateRange<-function(var) c(datelist(var)[1],tail(datelist(var),1))
vardims <- function(var) sapply(var$dim,function(x) x$name)
hasTime <- function(var) T%in%grepl('time',vardims(var))
hasVert<-function(var) as.logical(length(var$varsize)-(hasTime(var)+2))
vertid <- function(var) ifelse(hasTime(var),length(var$dim)-1,length(var$dim))

coordmatch <- function(x,y) which(abs(x-y)==min(abs(x-y)))[1]

dimvals <-   function(var,start,count){
  i <- 1:length(start)#which(count!=1)
  names(i)=vardims(var)#[i]
  lapply(i,function(x) var$dim[[x]]$vals[start[x]:(start[x]+(count[x]-1))])
}


datelist <- function(var){
  #var
  units=var$dim[[length(var$dim)]]$units
  if(grepl('seconds since',units)){
    t=as.POSIXct(var$dim[[length(var$dim)]]$vals,origin=gsub('seconds since','',units))
  }
  if(grepl('days since',units)){
    t=as.POSIXct(var$dim[[length(var$dim)]]$vals*86400,origin = gsub('days since','',units))
  }
  #t=as.Date(t)
  names(t)=var$dim[[length(var$dim)]]$vals
  t
}



buildQuery <- function(var,xr,yr,vr=NULL,tr=NULL,nc=NULL){
  #   xr <- c(df$x[1],df$x[1])
  #   yr <- c(df$y[1],df$y[1])
  #   vr <- c(1,25)
  #   tr <- c(df$t[1],df$t[1])

  ###############################

  if(class(var)=="character"){var <- nc$var[[var]]}

  bbox = list(x=xr,y=yr)

  dateR <- tr
  #   print(bbox)
  #   print(dateR)

  var.dims <- var%>%vardims()
  var.ndims <- length(var.dims)

  var.hasTime <- var%>%hasTime()
  var.hasVert<-var%>%hasVert()


  start1 <- rep(1,var.ndims)
  start1[1] <- coordmatch(var$dim[[1]]$vals,bbox$x[1])
  start1[2] <- coordmatch(var$dim[[2]]$vals,bbox$y[1])

  end1 <- rep(1,var.ndims)
  end1[1] <- coordmatch(var$dim[[1]]$vals,bbox$x[2])
  end1[2] <- coordmatch(var$dim[[2]]$vals,bbox$y[2])

  if(var.hasTime){
    var.dl <- as.Date(datelist(var))
    di=var.dl>=dateR[1]&var.dl<=dateR[2]
    start1[length(start1)] = min(which(di == TRUE))#which(cumsum(di)==1)
    end1[length(end1)] = max(which(di == TRUE))#which(cumsum(di)==max(cumsum(di)))[1]
  }

  if(var.hasVert){
    var.vid <-var%>%vertid()
    vdim <- var$dim[[var.vid]]
    start1[var.vid]=coordmatch(vdim$vals,vr[1])
    end1[var.vid]=coordmatch(vdim$vals,vr[2])
  }

  count1=(end1-start1)+1

  start<-apply(cbind(start1,end1),1,min)
  count<-abs(count1)
  #print(start)
  #print(count)
  dimV <- dimvals(var,start,count)
  if(var.hasTime){
    dimV$time <- as.character(var.dl[di])#[as.character(dimV$time)]
    names(dimV$time) <- NULL
  }

  list(start=start,count=count,dimV=dimV)
}

getQuery <- function(nc,var,qry){

  v1 <- get.var.ncdf(nc,var,start=qry$start,count = qry$count)
  dimnames(v1) <- qry$dimV[qry$count!=1]
  df <- melt(v1)%>%filter(!is.na(value))
  if(nrow(df)==0){return(NULL)}
  if(1%in%qry$count){
    df <- cbind(df,as.data.frame(qry$dimV[qry$count==1]))
  }
  df[,c(names(qry$dimV),'value')]
}

