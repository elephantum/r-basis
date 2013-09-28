library('rjson')
library('RCurl')
library('reshape')

getBasisData <- function(report_date='2013-09-14') {  
  user_id <- readLines('basis_user_id.txt')
  
  #          '&start_offset=-10800',
  #          '&end_offset=10800',

  basis.url <- sprintf(
    paste0('https://app.mybasis.com/api/v1/chart/%s.json',
          '?summary=true',
          '&interval=60',
          '&units=ms',
          '&start_date=%s',
          '&heartrate=true',
          '&steps=true',
          '&calories=true',
          '&gsr=true',
          '&skin_temp=true',
          '&air_temp=true',
          '&bodystates=true'),
    user_id, 
    report_date)

  print(sprintf('Downloading data for date %s: %s', report_date, basis.url))
  
  basis.data.string <- getURL(basis.url)
  
  basis.data.json <- fromJSON(basis.data.string)
  
  NAifNULL <- function (X){
    if(length(X) == 0) 
      return(NA)
    else 
      return(X[1])
  }

  basis.data <- data.frame(
    timestamp=seq(
      from=as.POSIXct(as.POSIXlt(basis.data.json$starttime, origin='1970-01-01', tz='Europe/Moscow')), 
      to=as.POSIXct(as.POSIXlt(basis.data.json$endtime, origin='1970-01-01', tz='Europe/Moscow')), 
      by=basis.data.json$interval),
    heartrate=sapply(X=basis.data.json$metrics$heartrate$values, FUN=NAifNULL),
    skin_temp=sapply(X=basis.data.json$metrics$skin_temp$values, FUN=NAifNULL),
    air_temp=sapply(X=basis.data.json$metrics$air_temp$values, FUN=NAifNULL),
    calories=sapply(X=basis.data.json$metrics$calories$values, FUN=NAifNULL),
    gsr=sapply(X=basis.data.json$metrics$gsr$values, FUN=NAifNULL),
    steps=sapply(X=basis.data.json$metrics$steps$values, FUN=NAifNULL)
  )

  return(basis.data)
}


basis.data.by.date <- list()
for(i in c(
  '2013-09-11',
  '2013-09-12',
  '2013-09-13',
  '2013-09-14',
  '2013-09-15',
  '2013-09-16',
  '2013-09-17',
  '2013-09-18',
  '2013-09-19',
  '2013-09-20',
  '2013-09-21',
  '2013-09-22',
  '2013-09-23',
  '2013-09-24',
  '2013-09-25',
  '2013-09-26'
)) {
  basis.data.by.date[[i]] <- getBasisData(i)  
}

basis.data <- Reduce(x=basis.data.by.date, f=rbind)
basis.data$start <- as.POSIXct(trunc(basis.data$timestamp, 'days'))
basis.data$hour <- as.POSIXlt(basis.data$timestamp)$hour

drawLines <- function() {
  
drawSmoothLine <- function (x, y, col=NULL, lwd=1) {
  x.clean <- x[!is.na(y)]
  y.clean <- y[!is.na(y)]
  
  if(length(y.clean) > 0) {
    spl <- smooth.spline(x=x.clean, y=y.clean)
    lines(spl, col=col, lwd=lwd)
  }
}

colnum = 1
drawDay <- function(d) {
  ts <- difftime(d$timestamp, d$start, units='hours')
  hr <- d$heartrate
  
  drawSmoothLine(y=hr, x=ts, col=colnum)
  colnum <<- colnum + 1
}

plot(
  x=NULL, 
  xlim=c(0, 24), 
  xlab='hour',
  ylim=c(0, 200),
  ylab='hr')

tapply(
  X=1:nrow(basis.data),
  INDEX=basis.data$start,
  FUN=function(x)drawDay(basis.data[x,]))
}

drawHeatmap <- function(){

hr_by_date_hour <- aggregate(
  basis.data$heartrate, by=list(date=basis.data$start, hour=basis.data$hour), 
  FUN=mean,
  na.rm=TRUE)

hr_by_date_hour.matrix <- as.matrix(cast(hr_by_date_hour, hour ~ date))

image(z=hr_by_date_hour.matrix, axes=FALSE)

x.names <- rownames(hr_by_date_hour.matrix)
axis(
  1, 
  at=seq(from=0, to=1, along.with=x.names), 
  labels=x.names)

y.names <- colnames(hr_by_date_hour.matrix)
axis(
  2, 
  at=seq(from=0, to=1, along.with=y.names), 
  labels=y.names)

}