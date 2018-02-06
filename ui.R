library(shiny)
library(shinythemes)

# Define UI for dataset viewer application
navbarPage("Baby Names in the U.S., 1880 - 2016",
           theme = shinytheme("flatly"),
    tabPanel("Trend Over Time",
           sidebarPanel(textInput(
             inputId = "list", 
             label = "Enter a name:", 
             value = "Jessie"),
             submitButton("Refresh View"), 
             width = 3),
           mainPanel(plotOutput("view"))),
    
  tabPanel("Sex Distribution",
           sidebarPanel(
             numericInput(
               inputId = "pickYear",
               label = "Choose a year (1880-2016):",
               value = 1984,
               min = 1880, 
               max = 2016,
               step = 1,
               width = '100%'),
             #radioButtons(
               #inputId = "pickFrequency",
               #label = "Display the top 10:",
               #choices = c("Most common",
               #            "Least common"),
               #selected = "Most common",
               #width = '100%'),
             selectInput(
               inputId = "pickPct",
               label = "Choose:",
               choices = c("Very masculine",
                           "Mostly masculine",
                           "Androgynous",
                           "Mostly feminine",
                           "Very feminine"),
               selected = "Androgynous",
               multiple = F),
             submitButton("Refresh View"),
             width = 4),
           mainPanel(DT::dataTableOutput("TopSexTable"))),
  
  tabPanel("By Year",
           sidebarPanel(
             numericInput(
               inputId = "year",
               label = "Choose a year (1880-2016):",
               value = 1984,
               min = 1880, 
               max = 2016,
               step = 1,
               width = '100%'),
             sliderInput(
               inputId = "pickN",
               label = "Choose # of names to display:",
               value = 10,
               min = 1, max = 15,
               step = 1, 
               ticks = F,
               sep = ""),
             submitButton("Refresh View"),
             width = 3),
           mainPanel(plotOutput("TopTable"))),
  
  tags$footer("Notes: This page takes 30 seconds to load the first time you open it. Data are from US Social Security applications via data.gov. For privacy, data only includes names with >= 5 babies per year. Errors in data submission are not corrected. Unfortunately the Social Security Administration only codes as male or female, but the author recognizes that almost 2% of babies are intersex. Enter names without special characters or spaces.", style = "font-size: 65%;"))