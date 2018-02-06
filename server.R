##Set  wd & environment if running code locally----
#setwd("/Users/OldJess/Dropbox/R Stuff (Home)/ShinyNames")

##load packages------
#library(datasets)
library(ggplot2)
library(viridis)
library(ggthemes)
library(gridExtra)
library(gtable)
library(grid)
library(dplyr)
library(rdrop2)
library(shiny)
library(xtable)
library(data.table)

##Import files----
base <- read.table("data/NationalNames2016.txt", header = T, stringsAsFactors = FALSE,  sep = ",", row.names = NULL, na.strings = c("NA","","#MULTIVALUE"))
pct <- read.table("data/NationalNamesPct.txt", header = T, stringsAsFactors = FALSE,  sep = ",", na.strings = c("NA","","#MULTIVALUE"))

#base <- fread("data/NationalNames2016.txt",#header = TRUE, 
              #stringsAsFactors = FALSE,  sep = ",", 
             # na.strings = c("NA","","#MULTIVALUE"), 
             # verbose = T)
#pct <- fread("data/NationalNamesPct.txt", 
   #          header = T, 
    #         stringsAsFactors = FALSE,  
     #        sep = ",", 
      #       na.strings = c("NA","","#MULTIVALUE"), 
       #      verbose = T)


##Create functions for display----
#function to create line & heat charts
lineHeatCharts <- function(pickaname){
  pickanameLower <- tolower(pickaname)
  subDf <- subset(base[base$name == pickanameLower,])
  title <- max(subDf$Name)
  title <- textGrob(paste(
    "There are a lot of ", max(subDf$Name),"s in the U.S.!", sep = ""),
                    gp = gpar(fontsize = 20, 
                              col = "black", 
                              fontface = "bold"))

  #heatmap chart:
  heat <- ggplot(subDf, aes(x = Year, 
                            y = Sex, 
                            fill = Count)) +
    scale_fill_viridis(name = "",
                       option = "B", 
                       limits = c(0, max(subDf$Count))) +
    geom_tile(color = "white", size = 0) +
    theme_tufte() +
    theme(axis.text.x = element_text(angle = 90, 
                                     vjust = 1, 
                                     hjust = 1),
          axis.ticks.x = element_blank(),
          text = element_text(size = 14)) +
    scale_x_continuous(breaks = seq(min(subDf$Year), 
                                    max(subDf$Year), 
                                    by = 5)) +
    labs(x = "Year", y = "")
  #line chart:
  line <- ggplot(subDf, aes(x = Year, 
                            y = Count, 
                            fill = Sex)) +
    geom_line(aes(colour = factor(subDf$Sex)), 
              size = 1.5) +
    theme_tufte() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          text = element_text(size = 14)) +
    scale_x_continuous(breaks = seq(min(subDf$Year), 
                                    max(subDf$Year), 
                                    by = 5)) +
    labs(x = "", 
         y = "", 
         color = "")
  return(grid.arrange(line, heat, 
                      ncol = 1, nrow = 2,
                      heights = c(5, 2), 
                      top = title))
} 

#function to create table of top ten names by selected year and sex
tableTop <- function(pickYear, pickN){
  padding <- unit(5, "mm")
  titleTop <- textGrob(paste(
    "In", pickYear, "the top", pickN, "names were:"), 
    gp = gpar(fontsize = 20,
              col = "black",
              fontface = "bold"))
  #set up theme for tables
  myTheme <- ttheme_default(
    base_size = 16,
    core = list(bg_params = list(col = NA, hjust = 1, x = 0)),
    colhead = list(fg_params = list(col = "black")),
    rowhead = list(fg_params = list(col = "black", hjust = 1, x = 0.9)))

  #set up girls names table:
  subDfF <- head(base[base$Year == pickYear & 
                        base$Sex == "F",], 
                 pickN)
  titleF <- textGrob("Girls Names", 
                     gp = gpar(fontsize = 18,
                               col = "#F8766D"))
  grobF <- tableGrob(subDfF[,c(2,4)], 
                     rows = c(1:pickN),
                     theme = myTheme)
  tableF <- gtable_add_rows(grobF, 
                            heights = grobHeight(titleF) + 
                              padding, 
                            pos = 0)
  tableF <- gtable_add_grob(tableF, titleF, 1,1,1, ncol(tableF))
  
  #set up boys names table:
  subDfM <- head(base[base$Year == pickYear & 
                        base$Sex == "M",], 
                 pickN)
  titleM <- textGrob("Boys Names", 
                     gp = gpar(fontsize = 18,
                               col = "#00BFC4"))
  grobM <- tableGrob(subDfM[,c(2,4)], 
                     rows = c(1:pickN), 
                     theme = myTheme)
  tableM <- gtable_add_rows(grobM, 
                            heights = grobHeight(titleM) + 
                              padding, 
                            pos = 0)
  tableM <- gtable_add_grob(tableM, titleM, 1,1,1, ncol(tableM))
  #return both girl & boy names table:
  return(grid.arrange(tableF, tableM,
                      nrow = 1, 
                      ncol = 2, 
                      top = titleTop))
  }

#function to create table of top ten names by femininity #pickFrequency
pctFem <- function(pickYear, pickPct){
  #set up page title
  mainTitle <- textGrob(
    paste("In ", pickYear, " the ", " ", tolower(pickPct)," names are: ", sep = ""),
    gp = gpar(fontsize = 20, 
              col = "black", 
              fontface = "bold"))
  
  #set up format theme:
  myTheme <- ttheme_default(
    base_size = 16,
    core = list(bg_params = list(col = NA, hjust = 1, x = 0)),
    colhead = list(fg_params = list(col = "black")),
    rowhead = list(fg_params = list(col = "black", hjust = 1, x = 0.9)))
  
  #set up plot title:
  #title <- textGrob(pickPct, gp = gpar(fontsize = 18, col = "#F8766D"))
  
  #set up range:
  minPct <- ifelse(pickPct == "Very masculine", 0, 
                   ifelse(pickPct == "Mostly masculine", 11,
                          ifelse(pickPct == "Androgynous", 41,
                                 ifelse(pickPct == "Mostly feminine", 61, 
                                        ifelse(pickPct == "Very feminine", 90, 0)))))
  maxPct <- ifelse(pickPct == "Very masculine", 10, 
                   ifelse(pickPct == "Mostly masculine", 40, 
                          ifelse(pickPct == "Androgynous", 60, 
                                 ifelse(pickPct == "Mostly feminine", 90, 
                                        ifelse(pickPct == "Very feminine", 100, 100)))))
  
  #  0-10 "Very Masculine"
  #  11-40 "Mostly Masculine"
  #  41-60 "Mostly Androgynous"
  #  61-90 "Mostly Feminine"
  #  91-100 "Very Feminine"
  
  
  #set up table:
  subDf <- pct[pct$Year == pickYear & 
                 pct$Percent.Feminine >= minPct &
                 pct$Percent.Feminine <= maxPct,]
  
  #sort table by descending total count of kids:
  #subDf <- arrange(subDf, desc(Total.Babies))
  
  #Display most or least common names:
  #subDf <- if (pickFrequency == "Most common") 
    #subDf else   #formerly head(subDf, 10)
      #tail(subDf[!is.na(subDf$Total.Babies),], 10)
  
  
  #grob <- tableGrob(subDf[,c(2, 7, 8)], #formerly rows = c(1:10),
   #                 theme = myTheme)
  table <- as.table(subDf)
  #table <- gtable_add_rows(grob, heights = grobHeight(title), pos = 0)
  #table <- gtable_add_grob(table, title, 1, 1, 1, ncol(table))
  
  #return table:
  #return(grid.arrange(grob,
   #                   nrow = 1, ncol = 1, 
    #                  top = mainTitle))
  return(table)
  }

## Define server logic ----
function(input, output) {

  output$view <- renderPlot({
    lineHeatCharts(input$list)
  })
  output$TopTable <- renderPlot({
    tableTop(input$year, input$pickN)
  })
  output$TopSexTable <- renderTable({
    pctFem(input$pickYear, input$pickPct)
  })
}
