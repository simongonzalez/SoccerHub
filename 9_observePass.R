#pitch plot sequence....................................................................
#................................................................................

output$sequenceTeamPitchui <- renderUI({
  selectInput('sequenceTeamPitch', 'Team',
              choices = sort(unique(dfevent$team.name)))
})

output$sequencePlayPitchNui <- renderUI({
  
  if(is.null(input$sequenceTeamPitch))
    return()
  
  dfinseq <- dfevent %>% filter(team.name == input$sequenceTeamPitch) %>%
    group_by(possession) %>% summarise(n = n())
  
  sliderInput("sequencePlayPitchN", label = 'Number of Plays', 
              min = min(dfinseq$n), 
              max = max(dfinseq$n), 
              value = c(min(dfinseq$n), max(dfinseq$n)),
              step = 1)
})

output$sequencePlayPitchui <- renderUI({
  if(is.null(input$sequencePlayPitchN))
    return()
  
  #dfinseq <- dfevent %>% 
  #  filter(team.name == input$sequenceTeamPitch)
  
  dfinseq <- dfevent %>% filter(team.name == input$sequenceTeamPitch) %>%
    group_by(possession) %>% summarise(n = n()) %>%
    filter(n >= input$sequencePlayPitchN[1]) %>%
    filter(n <= input$sequencePlayPitchN[2])
  
  selectInput('sequencePlayPitch', 'Play Number',
              choices = sort(unique(dfinseq$possession)))
})

output$plotPlaySequencePitch <- renderPlotly({
  if(is.null(input$sequencePlayPitch))
    return()
  
  plotnwdf <- dfevent %>% filter(team.name == input$sequenceTeamPitch) %>%
    filter(possession == input$sequencePlayPitch) #%>%
  #filter(type.name != "Ball Recovery")
  
  plotnw <- data.frame(x=plotnwdf$x, y=plotnwdf$y, 
                       type = plotnwdf$type.name,
                       player = plotnwdf$playerName,
                       playdur = plotnwdf$duration)
  
  plotnw <- plotnw[complete.cases(plotnw),]
  
  plotnw$n <- 1:nrow(plotnw)
  
  plotnw$playdurttl <- plotnw$playdur
  
  x2 <- c(plotnw$x[2:length(plotnw$x)],
          plotnw$x[length(plotnw$x)])
  y2 <- c(plotnw$y[2:length(plotnw$y)],
          plotnw$y[length(plotnw$y)])
  
  plotnw$xend <- x2
  plotnw$yend <- y2
  
  plotnw$label <- paste0('[', plotnw$n, '] ', plotnw$type,
                         ' (', plotnw$player, ')')
  
  if(input$sequencePlayPitchType){
    p <- ggplot() +
      #pitch rectangle
      geom_rect(data=rectangledf, 
                aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), 
                color="#5db746", fill = "#5db746") +
      geom_rect(data=rectangledf2, 
                aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), 
                color="#46a147", fill = "#46a147") +
      #pitch lines
      geom_segment(aes(x = x1, y = y1, 
                       xend = x2, yend = y2), colour = "black", data = pitchLines) +
      #pitch points
      geom_point(aes(x = x, y = y), color = 'black',
                 data = pitchPoints, show.legend = FALSE) +
      #pitch center
      geom_point(aes(x = x, y = y), size = 25, 
                 alpha = 0.6, color = 'black', fill = NA, pch = 1,
                 data = data.frame(x = 60, y = 40), show.legend = FALSE) +
      geom_edges(data = plotnw,
                 aes(x = x, y = y, xend = xend, yend = yend,
                     linetype = type), color = "white") +
      geom_point(data = plotnw, aes(x, y, text = label, frame = n), 
                 color = 'white', size = 10) +
      geom_text(data = plotnw, aes(x=(x-1), y, label = n), color = 'black') +
      theme_void()
  }else{
    p <- ggplot() +
      #pitch rectangle
      geom_rect(data=rectangledf, 
                aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), 
                color="#5db746", fill = "#5db746") +
      geom_rect(data=rectangledf2, 
                aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), 
                color="#46a147", fill = "#46a147") +
      #pitch lines
      geom_segment(aes(x = x1, y = y1, 
                       xend = x2, yend = y2), colour = "black", data = pitchLines) +
      #pitch points
      geom_point(aes(x = x, y = y), color = 'black',
                 data = pitchPoints, show.legend = FALSE) +
      #pitch center
      geom_point(aes(x = x, y = y), size = 25, 
                 alpha = 0.6, color = 'black', fill = NA, pch = 1,
                 data = data.frame(x = 60, y = 40), show.legend = FALSE) +
      geom_edges(data = plotnw,
                 aes(x = x, y = y, xend = xend, yend = yend,
                     linetype = type), color = "white") +
      geom_point(data = plotnw, aes(x, y, text = label), 
                 color = 'white', size = 10) +
      geom_text(data = plotnw, aes(x=(x-1), y, label = n), color = 'black') +
      theme_void()
  }
  
  ggplotly(p)
})

round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}

output$passpatternplotteamui <- renderUI({
  tmpoptions <- sort(unique(dfevent$team.name))
  selectInput("passpatternplotteam", 'Team',
              choices = tmpoptions, selected = tmpoptions[1])
  # switchInput(inputId = "passpatternplotteam", value = TRUE,
  #             label = 'Team', onLabel = tmpoptions[1], offLabel = tmpoptions[2], onStatus = T, offStatus = T, size = 'large', handleWidth = 100)
})


output$passpatternplot <- renderUI({
  if(is.null(input$passpatternplotteam))
    return()
  #all speakers
  #add player sonars
  tmpoptions <- sort(unique(dfevent$team.name))
  
  tmpteamoption <- input$passpatternplotteam
  
  dfplayerall <- dfevent %>% filter(team.name == tmpteamoption)
  
  dfplayerallpass <- dfplayerall[dfplayerall$type.name == 'Pass',]
  
  dfplayerallpass$pass.angle.floor <- floor(dfplayerallpass$pass.angle)
  
  dfplayerallpass$n <- 1
  
  dfsonarall <- dfplayerallpass %>% 
    group_by(playerName, pass.angle.floor) %>% 
    summarise(pass.length = mean(pass.length), n = sum(n))
  
  
  dfsonarall$deg <- rad2deg(dfsonarall$pass.angle.floor)
  dfsonarall$deg2 <- dfsonarall$deg
  dfsonarall[dfsonarall$deg2 < 0, 'deg2'] <- 
    180 + (180 - (dfsonarall[dfsonarall$deg2 < 0, 'deg2'] * -1))
  
  
  for(i in 1:nrow(dfsonarall)){
    dfsonarall$deg2 <- round_any(dfsonarall$deg2 , 10)
  }
  
  dfsonar1all <- data.frame(playerName = sort(unique(dfsonarall$playerName))[1],
                            pass.angle.floor = 3.141593,
                            pass.length = 0, n = 0, 
                            deg = 360, deg2 = 360)
  dfsonarall <- rbind(dfsonar1all, as.data.frame(dfsonarall))
  
  dfsonarall <- dfsonarall[dfsonarall$playerName %in% 
                             sort(unique(dfsonarall$playerName))[1:11],]
  dfsonarall$angleStart <- 0
  dfsonarall$angleSide1 <- dfsonarall$pass.length
  dfsonarall$angleSide2 <- dfsonarall$pass.length
  dfsonarall$angleEnd <- 0
  dfsonarall$thetaStart <- 0
  dfsonarall$thetaSide1 <- dfsonarall$deg2
  dfsonarall$thetaSide2 <- dfsonarall$deg2
  dfsonarall$thetaEnd <- 0
  
  tmpcorrs <- data.frame(n = sort(unique(dfsonarall$n)))
  tmpcorrs$colours <- heat.colors(nrow(tmpcorrs))
  
  dfsonarall <- merge(dfsonarall, tmpcorrs, by = 'n')
  
  dfsonarallplot <- dfsonarall %>% select(playerName, deg2, 
                                          pass.length, colours, n)
  for(spi in dfsonarallplot$playerName){
    dfsonarallploti <- dfsonarallplot %>% filter(playerName == spi)
    for(i in seq(0, 350, 10)){
      if(i %nin% dfsonarallploti$deg2){
        dfsonarall1plotAppend <- data.frame(playerName = spi,
                                            deg2 = i, pass.length = 0, 
                                            colours = '#FFFFFF', n = 0)
        
        dfsonarallplot <- rbind(dfsonarallplot, dfsonarall1plotAppend)
      }
    }
  }
  
  dfsonarallplot <- dfsonarallplot %>%
    arrange(playerName)
  
  map(sort(unique(dfsonarallplot$playerName)), function(x){
    
    tpmindf <- dfsonarallplot %>%
      filter(playerName == x) %>% 
      arrange(deg2)
    
    highchart() %>% 
      hc_chart(polar = TRUE) %>% 
      hc_title(text = x, style = list(fontSize = 11)) %>% 
      hc_xAxis(categories = tpmindf$deg2,
               tickmarkPlacement = "on", 
               lineWidth = 0) %>% 
      hc_yAxis(gridLineInterpolation = "polygon",
               lineWidth = 0,
               min = 0, max = max(tpmindf$pass.length)) %>% 
      hc_series(
        list(
          name = "Length",
          data = tpmindf$pass.length,
          pointPlacement = "on",
          colorByPoint = TRUE,
          type = "column", colors = tpmindf$colours
        )
      ) %>% 
      hc_legend(enabled = F) %>%
      hc_add_theme(hc_theme_null())
  }) %>%
    hw_grid(rowheight = 300, ncol = 3)
  
})