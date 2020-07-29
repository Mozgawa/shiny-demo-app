listofVariables <- function(dane){
  x=numeric()
  for(col in colnames(dane)){
    if(sapply(dane[col], is.numeric)==TRUE){
      x<-c(x,assign(col, col))
    }
  }
  return(x)
}

function(input, output) {
  dataIn <- reactive({
    inFile <- input$fileInPath
    
    if (is.null(inFile)) {
      return(NULL)
    }
    dane<- read.table(file=inFile$datapath,sep=",",dec=".",header=T,stringsAsFactors=FALSE)
    dane$ID <- seq.int(nrow(dane))
    dane<-dane[,c(ncol(dane),1:ncol(dane)-1)]
    for(col in colnames(dane)){
      if(sapply(dane[,col], is.numeric)!=TRUE){
        dane<-dane[,-which(names(dane) %in% c(col))]
      }
    }
    dane
  })
  
  lisvar <- eventReactive(input$fileInPath,{listofVariables(read.csv(input$fileInPath$datapath))})
  
  output$daneIn <- renderTable({
    ret <- rbind(
      dataIn()
    )
    
    return(ret)
    
  },include.rownames=FALSE)
  
  
  output$data <- renderTable({
    cor(read.csv(input$fileInPath$datapath)[c(input$variable)])
  }, rownames = TRUE)
  
  output$plot <- renderPlot({corrplot(cor(read.csv(input$fileInPath$datapath)[input$variable]),method = 'color')})
  
  output$checkbox <- renderUI({
    checkboxGroupInput("variable", "Wybierz zmienne do wizualizacji:",
                       choices = lisvar())})
  
  output$wizualizacja <- renderPlot({
    ggplot(data = data.frame(x=rep(1:nrow(read.csv(input$fileInPath$datapath)[ , which(names(read.csv(input$fileInPath$datapath)) %in% c(input$variable))]),ncol(read.csv(input$fileInPath$datapath)[ , which(names(read.csv(input$fileInPath$datapath)) %in% c(input$variable))])), val=unlist(read.csv(input$fileInPath$datapath)[ , which(names(read.csv(input$fileInPath$datapath)) %in% c(input$variable))]), variable=rep(paste0(colnames(read.csv(input$fileInPath$datapath)[ , which(names(read.csv(input$fileInPath$datapath)) %in% c(input$variable))])), each=nrow(read.csv(input$fileInPath$datapath)[ , which(names(read.csv(input$fileInPath$datapath)) %in% c(input$variable))]))), aes(x=x, y=val)) + geom_line(aes(colour=variable))
  })
  
  
  output$DownloadPlot <- downloadHandler(
    filename = function(){ paste(input$wykres,"png", sep=".")
    },
    
    content = function(file){
      ggsave(file, ggplot(data = data.frame(x=rep(1:nrow(read.csv(input$fileInPath$datapath)[ , which(names(read.csv(input$fileInPath$datapath)) %in% c(input$variable))]),ncol(read.csv(input$fileInPath$datapath)[ , which(names(read.csv(input$fileInPath$datapath)) %in% c(input$variable))])), val=unlist(read.csv(input$fileInPath$datapath)[ , which(names(read.csv(input$fileInPath$datapath)) %in% c(input$variable))]), variable=rep(paste0(colnames(read.csv(input$fileInPath$datapath)[ , which(names(read.csv(input$fileInPath$datapath)) %in% c(input$variable))])), each=nrow(read.csv(input$fileInPath$datapath)[ , which(names(read.csv(input$fileInPath$datapath)) %in% c(input$variable))]))), aes(x=x, y=val)) + geom_line(aes(colour=variable)), width = 16, height = 10.4)
    }
  )
  
  output$downloadMatrix <- downloadHandler(
    filename =function(){ paste(input$macierz, "png", sep=".")
    },
    
    content = function(file){
      png(file)
      corrplot(cor(read.csv(input$fileInPath$datapath)[input$variable]),method = 'color')
      dev.off()
    }
  )
  
}