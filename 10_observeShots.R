output$shotsequenceTeamPitchui <- renderUI({
  selectInput('shotsequenceTeamPitch', 'Team',
              choices = sort(unique(dfevent$team.name)))
})

output$shotsequencePlayPitchsortui <- renderUI({
  if(is.null(input$shotsequenceTeamPitch))
    return()
  if(is.null(input$shotsequenceTypePitch))
    return()
  if(input$shotsequenceTypePitch == 'All Shots')
    return()
  
  radioButtons("shotsequencePlayPitchsort", 
               label = 'Sort by',
               choices = c('Play', 'Player', 'Outcome'), 
               selected = 'Play', inline = T)
})

output$shotsequencePlayPitchui <- renderUI({
  if(is.null(input$shotsequenceTeamPitch))
    return()
  if(is.null(input$shotsequenceTypePitch))
    return()
  if(input$shotsequenceTypePitch == 'All Shots')
    return()
  if(is.null(input$shotsequencePlayPitchsort))
    return()
  
  dfshot <- dfevent %>% 
    filter(team.name == input$shotsequenceTeamPitch) %>%
    filter(shot.end_location != 'NULL') %>%
    filter(shot.freeze_frame != 'NULL')
  
  dfshot$n <- 1:nrow(dfshot)
  
  if(input$shotsequencePlayPitchsort == 'Play'){
    dfshot2 <- dfshot %>% arrange(n)
  }else if(input$shotsequencePlayPitchsort == 'Player'){
    dfshot2 <- dfshot %>% arrange(playerName)
  }else if(input$shotsequencePlayPitchsort == 'Outcome'){
    dfshot2 <- dfshot %>% arrange(shot.outcome.name)
  }
  
  tmpList <- list()
  
  for(i in 1:nrow(dfshot2)){
    
    if(input$shotsequencePlayPitchsort == 'Play'){
      tmpPaste <- paste(dfshot2$n[i], 
                        dfshot2$shot.outcome.name[i],
                        dfshot2$playerName[i],
                        sep = ' - ')
    }else if(input$shotsequencePlayPitchsort == 'Player'){
      tmpPaste <- paste(dfshot2$playerName[i], 
                        dfshot2$shot.outcome.name[i],
                        dfshot2$n[i],
                        sep = ' - ')
    }else if(input$shotsequencePlayPitchsort == 'Outcome'){
      tmpPaste <- paste(dfshot2$shot.outcome.name[i],
                        dfshot2$playerName[i], 
                        dfshot2$n[i],
                        sep = ' - ')
    }
    
    tmpList[[tmpPaste]] <- dfshot2$n[i]
  }
  
  selectInput('shotsequencePlayPitch', 'Play',
              choices = tmpList, selected = tmpList[[1]][1])
})

output$shotplotPlaySequencePitch <- renderPlotly({
  if(is.null(input$shotsequenceTeamPitch))
    return()
  
  dfshot <- dfevent %>% 
    filter(team.name == input$shotsequenceTeamPitch) %>%
    filter(shot.end_location != 'NULL') %>%
    distinct(X, .keep_all = T) %>% arrange(X)
  
  dfshot_locs <- 
    as.data.frame(str_split_fixed(gsub('c\\(|\\)|,', '', 
                                       dfshot$location), ' ', 2))
  names(dfshot_locs) <- c('loc_x_start', 'loc_y_start')
  dfshot <- cbind(dfshot, dfshot_locs)
  
  dfshot$n <- 1:nrow(dfshot)
  
  if(input$shotsequenceTypePitch == 'All Shots'){
    
    plotnwdf <- dfshot #%>% filter(team.name == input_passsequenceTeamPitch)
    
    plotnw <- data.frame(x=plotnwdf$loc_x_start, 
                         y=plotnwdf$loc_y_start, 
                         type = plotnwdf$shot.outcome.name,
                         player = plotnwdf$playerName,
                         playdur = plotnwdf$duration,
                         xend = plotnwdf$shot_x,
                         yend = plotnwdf$shot_y, stringsAsFactors = F)
    plotnw$x <- as.numeric(as.character(plotnw$x))
    plotnw$y <- as.numeric(as.character(plotnw$y))
    
    plotnw$n <- 1:nrow(plotnw)
    
    plotnw$playdurttl <- plotnw$playdur
    
    plotnw$label <- paste0('[', plotnw$n, '] ', plotnw$type,
                           ' (', plotnw$player, ')')
    
    rectangledfWhite <- rectangledf
    
    ggplotly(ggplot() +
               #pitch rectangle
               geom_rect(data=rectangledf, 
                         aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), 
                         color="#5db746", fill = "#5db746", alpha = 0.1) +
               geom_rect(data=rectangledf2, 
                         aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), 
                         color="#46a147", fill = "#46a147", alpha = 0.1) +
               geom_rect(data=rectangledfWhite, 
                         aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), 
                         color="white", fill = "white") +
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
               #plot shots
               geom_point(data = plotnw,
                          aes(x, y, text = label, fill = type),
                          size = 5, color = 'black') +
               geom_edges(data = plotnw,
                          aes(x = x, y = y, xend = xend, 
                              yend = yend, color = type)) +
               #geom_text(data = plotnw, aes(x=(x-1), y, label = n), color = 'black') +
               theme_void())
  }else{
    
    plotnwdfPlay <- dfshot %>% 
      filter(n == input$shotsequencePlayPitch)
    
    shotFreezeName <- plotnwdfPlay$shot.freeze_frame
    
    tmpLocations <- unlist(strsplit(shotFreezeName, '='))[c(2, 5, 8, 9)]
    
    #tmp locs
    tmpShotLocs <- as.data.frame(str_split_fixed(unlist(strsplit(
      gsub('^ list\\(c\\(|\\)\\), player $', '', 
           tmpLocations[1]), '\\), c\\(')), ', ', 2), stringsAsFactors = F)
    
    names(tmpShotLocs) <- c('xfreeze', 'yfreeze')
    tmpShotLocs$xfreeze <- as.numeric(tmpShotLocs$xfreeze)
    tmpShotLocs$yfreeze <- as.numeric(tmpShotLocs$yfreeze)
    
    #tmp names
    tmpShotLocs$names <- unlist(strsplit(
      gsub('^ c\\(|\\"|\\)\\)|\n|, position $', '', 
           tmpLocations[2]), ', '))
    
    tmpShotLocs$positions <- unlist(strsplit(
      gsub('^ c\\(|\\"|\\)\\)|\n|, teammate $', '', 
           tmpLocations[3]), ', '))
    
    tmpShotLocs$sameTeam <- unlist(strsplit(
      gsub('^ c\\(|\\)\\)', '', 
           tmpLocations[4]), ', '))
    
    tmpShotLocs$team <- ifelse(tmpShotLocs$sameTeam, 
                               'Same', 'Opposite')
    
    plotnwdfPlay$x <- as.numeric(as.character(plotnwdfPlay$x))
    plotnwdfPlay$y <- as.numeric(as.character(plotnwdfPlay$y))
    plotnwdfPlay$loc_x_start <- as.numeric(as.character(plotnwdfPlay$loc_x_start))
    plotnwdfPlay$loc_y_start <- as.numeric(as.character(plotnwdfPlay$loc_y_start))
    
    #prepare the play
    if(!is.na(plotnwdfPlay$shot.key_pass_id)){
      tmpPreviousPlayID <- plotnwdfPlay$shot.key_pass_id
      tmpPreviousPlay <- dfevent %>% filter(id == tmpPreviousPlayID)
      tmpPreviousPlaydf <- data.frame(x = tmpPreviousPlay$x,
                                      y = tmpPreviousPlay$y, 
                                      x2 = tmpPreviousPlay$pass_x,
                                      y2 = tmpPreviousPlay$pass_y,
                                      x3 = plotnwdfPlay$x,
                                      y3 = plotnwdfPlay$y,
                                      player1 = tmpPreviousPlay$playerName,
                                      player2 = tmpPreviousPlay$pass.recipient.name)
    }
    
    #plotnwdfPlay <- plotnwdfPlay[complete.cases(plotnwdfPlay),]
    #tmpPreviousPlaydf <- tmpPreviousPlaydf[complete.cases(tmpPreviousPlaydf),]
    
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
      #plot shots
      geom_point(data = tmpShotLocs,
                 aes(xfreeze, yfreeze, 
                     fill = team, text = names), 
                 size = 4, color = 'black', alpha = 0.9)
    
    if(!is.na(plotnwdfPlay$shot.key_pass_id)){
      
      p <- p + geom_point(data = tmpPreviousPlaydf,
                          aes(x, y, text = player1),
                          color = 'white', size = 5) +
        geom_point(data = tmpPreviousPlaydf,
                   aes(x2, y2, text = player2),
                   color = 'white', size = 5) +
        geom_edges(data = tmpPreviousPlaydf,
                   aes(x = x, y = y, 
                       xend = x2, yend = y2), 
                   color = 'white', linetype = 'dotted') +
        geom_edges(data = tmpPreviousPlaydf,
                   aes(x = x2, y = y2, 
                       xend = x3, yend = y3), 
                   color = 'white', linetype = 'dotted')
    }
    
    p <- p + geom_point(data = plotnwdfPlay,
                        aes(x, y, text = playerName),
                        color = 'white', size = 5) +
      geom_edges(data = plotnwdfPlay,
                 aes(x = x, y = y, 
                     xend = shot_x, yend = shot_y), 
                 color = 'white') +
      theme_void()
    
    ggplotly(p)
    
  }
})

#comparison plots

allpatterns <- unlist(strsplit('shot.outcome.name shot.type.name shot.body_part.name shot.technique.name', ' '))

for(i in allpatterns){
  dfi <- dfevent[complete.cases(dfevent[[i]]),] %>%
    rename(Team = team.name, tmpVariable = i)
  dfi <- dfi %>% group_by(Team, tmpVariable) %>%
    dplyr::count() %>% rename(Value = n, Stat = tmpVariable)
  dfi$pattern <- str_to_title(gsub(' name', '', gsub('\\.|_', ' ', i)))
  
  if(i == allpatterns[1]){
    alldf <- dfi
  }else{
    alldf <- rbind(alldf, dfi)
  }
}

dfevent_shots_both <- alldf %>% group_by(Stat) %>% 
  mutate(pairN=n()) %>%
  filter(pairN > 1) %>%
  select(-one_of('pairN')) %>%
  #set_colnames(unlist(strsplit('Team,Stat,Value', ','))) %>% 
  group_by(Stat) %>%
  mutate(Total = sum(Value)) %>%
  mutate(Percentage = round(Value * 100 / Total))

dfevent_shots_both$Stat <- 
  paste0('[', dfevent_shots_both$pattern, '] ', 
         dfevent_shots_both$Stat)

output$shots_comparison_plot <- renderHighchart({
  hchart(dfevent_shots_both, "column", 
         hcaes(x = Stat, y = Percentage, group = Team)) %>%
    hc_chart(inverted = TRUE) %>%
    hc_plotOptions(
      series = list(stacking = "percent")
    ) %>% 
    hc_yAxis(
      plotLines = list(
        list(
          color = "#FF0000",
          width = 2,
          value = 50.0
        )
      )
    )
})