library('rjson')
library('RCurl')

getBasisData <- function(report_date='2013-09-14') {
  print(sprintf('Downloading data for date %s', report_date))
  
  user_id <- readLines('user_id.txt')
  
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
      from=as.POSIXct(basis.data.json$starttime, origin='1970-01-01'), 
      to=as.POSIXct(basis.data.json$endtime, origin='1970-01-01'), 
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


drawSmoothLine <- function (x, y, col, lwd=1) {
  x.clean <- x[!is.na(y)]
  y.clean <- y[!is.na(y)]

  spl <- smooth.spline(x=x.clean, y=y.clean)
  lines(spl, col=col, lwd=lwd)
}


basis.data <- rbind(
  getBasisData('2013-09-11'), 
  getBasisData('2013-09-12'), 
  getBasisData('2013-09-13'), 
  getBasisData('2013-09-14'))


plot(y=basis.data$heartrate, x=basis.data$timestamp, type='p', pch=20, col='grey')
drawSmoothLine(x=basis.data$timestamp, y=basis.data$heartrate, col='red', lwd=2)