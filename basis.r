library('rjson')
library('RCurl')

getBasisData <- function() {
  user_id <- readLines('user_id.txt')
  
  basis.url <- sprintf('https://app.mybasis.com/api/v1/chart/%s.json?summary=true&interval=60&units=ms&start_date=%s&start_offset=-10800&end_offset=10800&heartrate=true&steps=true&calories=true&gsr=true&skin_temp=true&air_temp=true&bodystates=true',
                 user_id, 
                 '2013-09-14')
  
  basis.data.string <- getURL(basis.url)
  
  basis.data.json <- fromJSON(basis.data.string)
  
  NAifNULL <- function (X){
    if(length(X) == 0) 
      return(NA)
    else 
      return(X[1])
  }
  
  basis.data <- data.frame(
    timestamp=seq(from=basis.data.json$starttime, to=basis.data.json$endtime, by=basis.data.json$interval),
    heartrate=sapply(X=basis.data.json$metrics$heartrate$values, FUN=NAifNULL),
    skin_temp=sapply(X=basis.data.json$metrics$skin_temp$values, FUN=NAifNULL),
    air_temp=sapply(X=basis.data.json$metrics$air_temp$values, FUN=NAifNULL),
    calories=sapply(X=basis.data.json$metrics$calories$values, FUN=NAifNULL),
    gsr=sapply(X=basis.data.json$metrics$gsr$values, FUN=NAifNULL),
    steps=sapply(X=basis.data.json$metrics$steps$values, FUN=NAifNULL)
  )

  return(basis.data)
}