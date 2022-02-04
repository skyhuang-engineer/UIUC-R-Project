shinyServer(function(input, output) {
  

  output$StandardPlot <- renderPlot({
    
    All_controls %>%
      filter(Rating <= input$Rating[2],
             Rating >= input$Rating[1],
             Age <= input$Age[2],
             Age >= input$Age[1],
             Country %in% input$country,
             Kfactor %in% input$Kfactor,
             Gender %in% input$dataset_standard,
             Flag %in% input$Activity,
             Time %in% input$dataset) %>%
      
      select(Rating, Age, Gender) %>%
      
      ggplot(data=., aes_string(x=input$Rage, color='Gender'))+
      
      geom_density()+
      
      labs(title="Density curve") +
      
      labs(x=input$Rage, y="Percentage of population")
    
    
  })
  
  
  output$StandardPlot1<- renderPlot({
    
    All_controls %>%
      filter(Rating <= input$Rating[2],
             Rating >= input$Rating[1],
             Age <= input$Age[2],
             Age >= input$Age[1],
             Country %in% input$country,
             Kfactor %in% input$Kfactor,
             Gender %in% input$dataset_standard,
             Flag %in% input$Activity,
             Time %in% input$dataset) %>%
      
      select(Age, Rating, Gender) %>%
      
      
      ggplot(data=., aes_string(x=input$Rage1))+
      
      geom_histogram(aes(fill=Gender), bins = input$Sbins, col="black")+ 
      
      labs(title="Histogram") +
      
      labs(x=input$Rage, y="Number of players in population")
    
    
  })
  
  output$StandardPlot2 <- renderPlot({
    
    All_controls %>% 
      filter(Rating <= input$Rating[2],
             Rating >= input$Rating[1],
             Age <= input$Age[2], 
             Age >= input$Age[1],
             Country %in% input$country,
             Kfactor %in% input$Kfactor,
             Gender %in% input$dataset_standard,
             Flag %in% input$Activity,
             Time %in% input$dataset) %>% 
      select(Rating, Age, Gender)%>%
      
      ggplot(data=., aes_string(x=input$Rage2))+
      
      geom_histogram(aes(y = ..density..), bins = input$Sbins1, col= "yellow")+ 
      
      geom_density(aes(fill=factor(Gender)), size=2, alpha=.4)+
      
      scale_fill_manual( values = c("red","blue"))+
      
      labs(title="Histogram overlayed with Density curve") +
      
      labs(x=input$Rage, y="Percentage of players in population")
    
    
  })
  
  
  
  
  output$StandardPlot4 <- renderPlot({
    
    All_controls %>% 
      filter(Rating <= input$Rating[2],
             Rating >= input$Rating[1],
             Age <= input$Age[2], 
             Age >= input$Age[1],
             Country %in% input$country,
             Kfactor %in% input$Kfactor,
             Gender %in% input$dataset_standard,
             Flag %in% input$Activity,
             Time %in% input$dataset) %>% 
      
      
      select(Gender, Country) %>%
      ggplot(., aes(Country)) + geom_bar(aes(fill = Gender)) +  xlab("Country Names") + ylab("Number of players")
    
    
  })
  
  
  
  
  
  #   A simple visualisation. In shiny apps, need to register observers
  #   and tell shiny where to put the controls
  
  
  output$view <- renderGvis({
    All_controls <- All_controls %>% 
      filter(Rating <= input$Rating[2],
             Rating >= input$Rating[1],
             Age <= input$Age[2], 
             Age >= input$Age[1],
             Kfactor %in% input$Kfactor,
             Gender %in% input$dataset_standard,
             Flag %in% input$Activity,
             Time %in% input$dataset) %>% 
      select(Country)
    
    counts <- as.data.frame(table(All_controls[, c("Country")]))
    counts <- counts[ which( ! counts$Var1 %in% "All Countries") , ]
    
    
    gvisGeoMap(counts, locationvar="Var1", numvar="Freq",
               options=list(dataMode="regions", height = 500, width= 800))
  })
  

  
  output$view1 <- renderGvis({
    load("Finalized.Rda")
    Finalized$Deviations <- Finalized$Average_Rating/Finalized$SD_Rating
    Finalized<- Finalized %>% filter(Deviations <= 600) %>% select(Country, Year, Average_Rating, SD_Rating,Max_Ratng, Deviations, Freq)
    Finalized<- na.omit(Finalized)
    gvisMotionChart(Finalized, idvar="Country", timevar="Year")
  })
  
  output$dygraph <- renderDygraph({
    
    
    Timed_set_filtered <- Timed_set %>% filter(Name %in% input$Name) %>% select(Name,id_number, Rating, Year)
    
    Timed_set_filtered$Year <- as.Date((Timed_set_filtered$Year),format="%Y%m%d")
    
    for (i in 1:nrow(as.data.frame(unique(Timed_set_filtered$Name)))) {
      assign(paste("Player",i,sep=""), subset(Timed_set_filtered, Name == unique(Timed_set_filtered$Name)[i]))
    }
    
    lst <- mget(ls(pattern='^Player\\d+'))
    
    
    for (i in 1:nrow(as.data.frame(unique(Timed_set_filtered$Name)))) {
      names(lst[[i]]) [names(lst[[i]]) == 'Rating'] <- eval(unique(Timed_set_filtered$Name)[i])
    }
    

    df <- join_all(lst, by = 'Year', match = "all")
    
    
    df <- df[ , -which(names(df) %in% c("Name","id_number"))]
  
    
    
    Player<- xts(df[,which(colnames(df)!= "Year")],df$Year)
    
    dygraph(Player, main = "Player's rating over Time") %>%
      dyOptions(colors = RColorBrewer::brewer.pal(eval(ncol(df)-1), "Set1"),
                drawPoints = TRUE, pointSize = 2,
                drawGrid = TRUE) %>%
      dyHighlight(highlightCircleSize = 5)%>%
      dyLegend(show = "always", hideOnMouseOut = FALSE, width = 800)%>%
      dyAxis("y", label = "Rating", valueRange = c(2000, 2900))%>% 
      dyLegend(labelsDiv = "legendDivID")%>%
      dyRangeSelector()
    
    
  })

  output$dygraphY <- renderDygraph({

    region_counts%>%
      .[,-1]%>%
      group_by(region, Date)%>%
      summarise(count = sum(counts))%>%
      filter(region %in% input$Region) %>%
      spread(region, count) %>%
      xts(.[,which(colnames(.)!= "Date")],order.by =  .$Date)%>%
      .[,-1]%>%
      dygraph(., main = "Region counts over time") %>%
      dyAxis("y", label = "Counts")%>%
      dyRangeSelector()%>%
      dyLegend(labelsDiv = "legendDivID1")
  })


  output$bargraph <- renderPlot({
    Other_titles %>%
                filter(Fed %in% input$Federation) %>%
                                                   ggplot(., aes(Fed)) + 
                                                   geom_bar(aes(fill = str1)) +  
                                                   xlab('Name of "other" title') + 
                                                   ylab("Number of players")
    
  })
  
    
})

