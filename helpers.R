#' UNIX time (that is, seconds since midnight GMT on 1 Jan 1970)
#'
#' @param x A object to be conveted using \code{as.POSIXct}.
unixTime<-function(x) as.numeric(as.POSIXct(x), origin="1970-01-01")


#' Retrieve weather data for a specific time period
#'
#' Query for a specific time, past or future (for many places, 60 years in the past to 10 years in
#' the future). If \code{from} and \code{to} not are specified then retrive 7 day forecast.
#'
#' @param lat Latitude vector (decimal format)
#' @param lon Longitude vector (decimal format)
#' @param from Start date
#' @param to End date
#'
#' @return A data table of hourly wind data orderd by key unixTime.
#'
#' @note You must have set your \code{FORECASTIO_API_KEY}
getWeatherData<-function(lat, lon, from, to) {
   datH<-NULL
   days<-seq(as.Date(from), as.Date(to), "days")
   for (i in  1:length(days)) {
      #incProgress(detail = paste("  Get data for date:", format(days[i],"%Y-%m-%d"), "\n"))
      cat("  Get data for date:", format(days[i],"%Y-%m-%d"), "\n")
      for (j in 1:length(lat)) {
         incProgress(0.02,detail = paste("\nFetch data for date: ", format(days[i],"%Y-%m-%d")," and (lat,lon) = (", lat[j],",",lon[j], ")\n", sep="") )
         cat("    (lat,lon)=(", lat[j],",",lon[j], ")\n", sep="")
         fioList <- get_forecast_for(lat[j],lon[j], timestamp = format(days[i],"%Y-%m-%dT%H:%M:%S"), units="si", exclude = "currently,minutely,daily,alerts,flags")
         datH<-rbind(datH, cbind(lat=lat[j],lon=lon[j],fioList$hourly[,c('time','windSpeed','windBearing')]) )
      }
   }
   datH$unixTime<-unixTime(datH$time)
   datH$timeStr<-as.character(datH$time)
   datH<-as.data.table(datH)
   setkey(datH,"unixTime")
   #datD$unixTime<-unixTime(datD$time)
   #datD$timeStr<-as.character(datD$time)
   return(datH)
}


#' Calculate distance in kilometers between two points \code{(long1,lat1)}
#' and \code{(long2,lat2)}
earthDist <- function (long1, lat1, long2, lat2)
{
   rad <- pi/180
   a1 <- lat1 * rad
   a2 <- long1 * rad
   b1 <- lat2 * rad
   b2 <- long2 * rad
   dlon <- b2 - a2
   dlat <- b1 - a1
   a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
   c <- 2 * atan2(sqrt(a), sqrt(1 - a))
   R <- 6378.145
   d <- R * c
   return(d)
}

getMap<-function(cLon,cLat,zoomLevel){
   get_map(c(cLon,cLat), zoom = zoomLevel, maptype = "roadmap", source = "google")
}

mergeWeatherData<-function(fromDate=Sys.Date(),toDate=Sys.Date()) {
   # create grid of map points
   dat<-expand.grid(lon = seq(w$ll.lon+dLon/8, w$ur.lon-dLon/8, length.out=cols), lat = seq(w$ll.lat+dLat/8, w$ur.lat-dLat/8, length.out=cols))
   # retrive wind data
   if (diagDist<30) { # use just the center point to get wind data (faster)
      tmp<-getWeatherData(cLat,cLon, from = fromDate, to = toDate)
      tmp$lat<-NULL; tmp$lon<-NULL
      datH<-do.call("rbind", replicate(length(dat$lon), tmp, simplify = FALSE))
      datH$lon<-rep(dat$lon,each=length(tmp$windSpeed))
      datH$lat<-rep(dat$lat,each=length(tmp$windSpeed))
   } else { # get wind for each coordinate
      datH<-getWeatherData(dat$lat,dat$lon, from = fromDate, to = toDate)
   }
   setkey(datH,"unixTime")
   datH
   # convert wind to arrows
   length=diagDist/3000
   angle <- 90 - datH$windBearing # windBearing = 0 corresponds to north
   datH$lonEnd <- datH$lon+length*cos(angle*pi/180)
   datH$latEnd <- datH$lat+length*sin(angle*pi/180)
   datH$lonText <- datH$lon+dLon/40
   datH$latText <- datH$lat+dLat/40
   datH
}


# function for drawing at a specific unix time
drawWind<-function(uTime,dat) {
   p <- ggmap(map, extent = "device") +
      geom_segment(aes(x = lon, xend = lonEnd, y = lat, yend = latEnd), data=dat, size=1, color="black", arrow = arrow(length = unit(0.3,"cm"), ends = "first"), alpha=0.5, na.rm = TRUE) +
      #geom_point(data=dat, mapping=aes(x=lon-dLon/(2*cols), y=lat-dLat/(2*cols), fill=hcl(windSpeed)), size=6, shape=21, color="black", alpha = 0.8, hjust=0.5, vjust=0.5) +
      geom_text(data=dat, mapping=aes(x=lonText, y=latText, label=round(windSpeed,0)), size=8, hjust=0.45, vjust=0.4) +
      annotate('text', x=w$ur.lon-dLon/20, y=w$ur.lat-dLat/20, label = format(as.POSIXct(uTime, origin="1970-01-01"), format="%d-%m-%Y %H:%M"), colour = I('black'), size = 12, hjust=1) +
      theme(legend.position = "none")
   print(p)
}

animateFunc <- function(datH) {
   v = unique(datH$unixTime)
   lapply(v, function(i) {
      dat<-datH[J(i)]
      drawWind(i,dat)
      ani.pause()
   })
}

## move files
my.file.rename <- function(from, to) {
   todir <- dirname(to)
   if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
   file.rename(from = from,  to = to)
}
