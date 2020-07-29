library(shiny)
library(corrplot)
library(ggplot2)
library(reshape2)

fluidPage(
  titlePanel("Projekt zaliczeniowy"),
  sidebarLayout(
    sidebarPanel(
      img(src='logo.png', height = 300, width = 300),
      fileInput("fileInPath", 
                label= h4("Import danych")),
      textInput("wykres", "Nazwa eksportowanego wykresu", ""),
      downloadButton("DownloadPlot", 
                     label = "Pobierz wykres"),
      textInput("macierz", "Nazwa eksportowanej macierzy", ""),
      downloadButton("downloadMatrix",
                     label = "Pobierz macierz"),
      uiOutput("checkbox")
      ),
    
  mainPanel( 
  #uiOutput("checkbox"),
  tabsetPanel(type = "tabs",
              tabPanel("Dane", tableOutput("daneIn")),
              tabPanel("Macierz korelacji", tableOutput("data")),
              tabPanel("Macierz kolrelacji heatmap", plotOutput("plot")),
              tabPanel("Wizualizacja", plotOutput("wizualizacja")))
  
    )
  )
)