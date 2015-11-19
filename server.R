library(shiny)
library(Rforecastio)    #install_github("hrbrmstr/Rforecastio")
library(data.table)
library(ggmap)
library(grid)
library(animation)
source("helpers.R", local=TRUE)

# center of map
adr<-"Stauning, Denmark"
cord<-geocode(adr, source = "google")
cLat<-cord$lat
cLon<-cord$lon
zoomLevel = 13
map<-getMap(cLon,cLat,zoomLevel)
w<-attr(map,"bb")
dLat<-w$ur.lat-w$ll.lat  # delta lat (map length lat)
dLon<-w$ur.lon-w$ll.lon  # delta lon (map length lon)
cols<-4
diagDist<-earthDist(w$ll.lon,w$ll.lat,w$ur.lon,w$ur.lat)
oopt <- animation::ani.options(interval = 0.5, nmax=48)


shinyServer(function(input, output, session) {

   observe({
      curHour<-as.integer(format(Sys.time(), "%H", tz="Europe/Paris") )
      fromDate = format(Sys.Date(), "%Y-%m-%d", tz="Europe/Paris")
      toDate = format(Sys.Date()+1, "%Y-%m-%d", tz="Europe/Paris")
      updateSliderInput(session, "myslider", value = curHour)  # use plot for current hour

      # should we remake the plots (done if current plots are 4 hours old)
      if (file.exists("lastRun.csv")) {
         lastGen<-read.csv("lastRun.csv")
         if (lastGen$fromDate!=fromDate || (lastGen$fromDate==fromDate & curHour-lastGen$curHour>4) || length(list.files("www/img"))<23 )
            rerun = TRUE else rerun <- FALSE
      } else {rerun <- TRUE }

      if (rerun) {
         withProgress(message = 'Making plot (be patient) ...', value = 0, {
            Sys.setenv(FORECASTIO_API_KEY = readLines("forecastioApi.txt"))
            datH<-mergeWeatherData(fromDate,toDate)
            #datH<-datH[!(hour(datH$time)<as.numeric(curHour) & format(datH$time, "%Y-%m-%d", tz="Europe/Paris")==fromDate), ]
            dir.create("www")
            dir.create("www/img")
            unlink("www/img/*", recursive = TRUE)
            incProgress(detail = paste0("Save pictures") )
            v = unique(datH$unixTime)
            j<-1
            lapply(v, function(i) {
               dat<-datH[J(i)]
               png(paste0("www/img/wind",j,".png"), width = 1280, height = 1280)
               drawWind(i,dat)
               dev.off()
               j<<-j+1
            })
            write.csv(data.frame(fromDate=fromDate,curHour=curHour), file="lastRun.csv")
            rerun<<-FALSE
         })
      }
   })

   imgurl <- reactive({
      i=input$myslider
      return(paste0("./img/wind",i+1,".png")) #the path of pictures
   })

   output$ui <- renderUI({
      tags$div(
         tags$img(src = imgurl(),  width="100%", height="100%", style="max-width:600px")
      )
   })

})

# png("test.png", width = 1280, height = 1280)
# drawWind(unixTime(Sys.Date()),datH[J(unixTime(Sys.Date()))])
# dev.off()
