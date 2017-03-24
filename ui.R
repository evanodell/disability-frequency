
library(shiny)
library(shinyjs)
library(ggplot2)
library(scales)
library(grid)
library(data.table)

shinyUI(fluidPage(
  
  fluidPage(
    tags$head(tags$style(HTML(".multicol{font-size:12px;
                              height:auto;
                              -webkit-column-count: 4;
                              -moz-column-count: 4;
                              column-count: 4;
                              }"))),
    fluidRow(
      tags$div(style = 'height:430px;',
      plotOutput("hansardplot"))),
    
    fluidRow(
      
      tags$div(align = "left", 
               class = "multicol",
               style = 'width:750px;',
               
               checkboxGroupInput("category_input",
                                  "Words and Phrases",
                                  c("Person/People With Disability" = "People With Disability",
                                    "Disabled Person/People"="Disabled Person", 
                                    "Disabled Men",
                                    "Disabled Women",          
                                    "Disabled Children",
                                    "All Disability",
                                    "Children With Disability",
                                    "Any Other With Disability",
                                    "Independent Living",
                                    "Wheelchair",
                                    "Paralympic",
                                    "Spastic",
                                    "Sub-Normal",
                                    "Amputee",
                                    "Retard"),
                                  selected =c("Disabled Person","People With Disability"))),
      
      sliderInput("year", "Year", 1936, 2016, value = c(1936, 2016), sep="")
    )
    )
))
