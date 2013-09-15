library('rjson')
library('RCurl')

getBasisData <- function(report_date='2013-09-14') {
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

basis.data.13 <- getBasisData('2013-09-13')
basis.data.14 <- getBasisData('2013-09-14')

basis.data <- rbind(basis.data.13, basis.data.14)


ts <- basis.data.13$timestamp - basis.data.13$timestamp[1]

hr.ts.13 <- basis.data.13$timestamp[!is.na(basis.data.13$heartrate)] - basis.data.13$timestamp[1]
hr.13 <- basis.data.13$heartrate[!is.na(basis.data.13$heartrate)]

spl.13 <- smooth.spline(x=hr.ts.13, y=hr.13)

hr.ts.14 <- basis.data.14$timestamp[!is.na(basis.data.14$heartrate)] - basis.data.14$timestamp[1]
hr.14 <- basis.data.14$heartrate[!is.na(basis.data.14$heartrate)]

spl.14 <- smooth.spline(x=hr.ts.14, y=hr.14)

plot(x=hr.ts.13, y=hr.13, col='grey')
lines(x=hr.ts.14, y=hr.14, col='grey', type='p')
lines(spl.13, col='red')
lines(spl.14, col='blue')