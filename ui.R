#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(ggplot2)
library(scales)
library(grid)
library(data.table)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  fluidPage(
    tags$head(tags$style(HTML(".multicol{font-size:12px;
                              height:auto;
                              -webkit-column-count: 4;
                              -moz-column-count: 4;
                              column-count: 4;
                              }"))),
    fluidRow(
      
      h4("Disability Discussion Frequency Chart"),
      plotOutput("hansardplot")),
    
    fluidRow(
      
      tags$div(align = "left", 
               class = "multicol",
               
               checkboxGroupInput("category_input",
                                  "Words and Phrases",
                                  c("Person/People With Disability" = "People With Disability",
                                    "Disabled Person/People"="Disabled Person", 
                                    "Disabled Men",
                                    "Disabled Women",          
                                    "Disabled Children",
                                    "Disability Other",
                                    "Children With Disability",
                                    "Any With Disability",
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
