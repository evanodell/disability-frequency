
library(shiny)
library(shinyjs)
library(ggplot2)
library(scales)
library(grid)
library(data.table)
library(readr)

base_breaks <- function(n = 10) {
    function(x) {
        axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
    }
}

fmt_dcimals <- function(decimals = 0) {
    function(x) as.character(round(x, decimals))
}

line_colours <- c(`Disabled Person` = "#006109", `Disabled Men` = "#8a2093", `Disabled Women` = "#00d38d", `Disabled Children` = "#ff50a0", 
    `All Disability` = "#5b5700", `People With Disability` = "#7f8cff", `Children With Disability` = "#fa7422", `Any Other With Disability` = "#134eae", 
    `Independent Living` = "#d70039", Wheelchair = "#97bcff", Paralympic = "#8a3a1b", Spastic = "#936996", `Sub-Normal` = "#ff9176", Amputee = "#9b2535", 
    Retard = "#ff7798")

line_styles <- c(`Disabled Person` = "solid", `Disabled Men` = "solid", `Disabled Women` = "solid", `Disabled Children` = "solid", `All Disability` = "longdash", 
    `People With Disability` = "longdash", `Children With Disability` = "longdash", `Any Other With Disability` = "longdash", `Independent Living` = "twodash", 
    Wheelchair = "twodash", Paralympic = "twodash", Spastic = "dotdash", `Sub-Normal` = "dotdash", Amputee = "dotdash", Retard = "dotdash")

disability_phrase_groups <- read_rds("./data/disability_phrase_groups.rds")

shinyServer(function(input, output, session) {
    
    getDataSet <- reactive({
        
        all_data <- disability_phrase_groups[disability_phrase_groups$Term %in% input$category_input & disability_phrase_groups$Year >= 
            input$year[1] & disability_phrase_groups$Year <= input$year[2], ]
    })
    
    output$hansardplot <- renderPlot({
        
        dataSet <- getDataSet()
        
        p3 <- ggplot(dataSet, aes(x = Date, group = Term, col = Term))
        p3 + geom_smooth(aes(y = value, linetype = Term, col = Term), size = 1.5, formula = y ~ log(x), se = FALSE) + coord_cartesian(xlim = c(as.Date(min(dataSet$Date)), 
            as.Date(max(dataSet$Date)))) + scale_linetype_manual(values = line_styles) + scale_color_manual(values = line_colours) + scale_x_date(date_breaks = "5 year", 
            date_labels = "%Y") + scale_y_continuous(trans = log_trans(5), breaks = base_breaks(), name = "Average Mentions per Day (Logarithmic Scale)", 
            labels = fmt_dcimals(3)) + theme(axis.text.x = element_text(angle = 30, hjust = 1), text = element_text(size = 14), legend.position = "bottom", 
            legend.background = element_rect())
        
    }, height = 430, width = 700)
    
})
