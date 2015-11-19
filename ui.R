library(shiny)
shinyUI(fluidPage(
   tags$link(rel = 'stylesheet', type = 'text/css', href = 'progress.css'),

   titlePanel(h4("Wind direction and speed (today and tomorrow)", align="center")),

   fluidRow(
      column(12,
         uiOutput("ui")
      ), align="center"
   ),
   br(),
   fluidRow(
      column(12,
             sliderInput('myslider',
                         'Hour (next day +24)',
                         min=0, max=47, value=0, post = ":00",
                         animate=animationOptions(interval = 2000,loop=F)
             )
      ), align="center"
   )
))
