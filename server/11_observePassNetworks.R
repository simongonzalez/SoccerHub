output$passNetwork_passpatternplotteamui <- renderUI({
  tmpoptions <- sort(unique(dfevent$team.name))
  
  selectInput("passNetwork_passpatternplotteam", 'Team',
              choices = tmpoptions, selected = tmpoptions[1])
  # 
  # switchInput(inputId = "passNetwork_passpatternplotteam", value = TRUE,
  #             label = 'Team', onLabel = tmpoptions[1], offLabel = tmpoptions[2], onStatus = T, offStatus = T, size = 'large', handleWidth = 100)
})

output$passNetwork_passpatternplot <- renderVisNetwork({
  if(is.null(input$passNetwork_passpatternplotteam))
    return()
  
  if(is.null(input$passNetworkTypePlot))
    return()
  
  #all speakers
  #add player sonars
  tmpoptions <- sort(unique(dfevent$team.name))
  
  tmpteamoption <- input$passNetwork_passpatternplotteam
  
  dfplayerall <- dfevent %>% filter(team.name == tmpteamoption)
  
  #pass player
  passPlayerNamedf <- dfplayerall %>% select(player_name, playerName) %>% distinct()
  passPlayerName <- setNames(passPlayerNamedf$playerName, passPlayerNamedf$player_name)
  
  dfplayerall$playerPassName <- passPlayerName[as.character(dfplayerall$pass.recipient.name)]
  
  #all speakers
  #add player sonars
  dfplayerallpass <- dfplayerall[dfplayerall$type.name == 'Pass',]
  
  #changes based on input
  if(input$passNetworkTypePlot == 'Player to Player'){
    #all network
    dfplayerallpassall <- dfplayerallpass[complete.cases(dfplayerallpass$pass.recipient.name),]
    
    dfpatterns <- data.frame(from = dfplayerallpassall$playerName, to = dfplayerallpassall$playerPassName)
    dfpatternsN <- dfpatterns %>% group_by(from, to) %>% dplyr::count() %>%
      filter(n >= 4)
    
    allunique <- data.frame(name = c(as.character(dfpatternsN$from), 
                                     as.character(dfpatternsN$to))) %>% group_by(name) %>% 
      dplyr::count()
    
    nodes <- data.frame(id = 1:length(allunique$name), 
                        label = allunique$name,
                        value = allunique$n, group = allunique$name,
                        title = allunique$name)
  }else{
    
    if(input$passNetworkTypePlot == 'Pass Outcome'){
      selColumnIn <- 'pass.outcome.name'
      
      #all network
      dfplayerallpassall <- dfplayerallpass %>% replace_na(list(pass.outcome.name = 'Complete'))
    }else{
      if(input$passNetworkTypePlot == 'Pass Pattern'){
        selColumnIn <- 'play_pattern.name'
      }else if(input$passNetworkTypePlot == 'Pass Height'){
        selColumnIn <- 'pass.height.name'
      }else if(input$passNetworkTypePlot == 'Body Part Involved'){
        selColumnIn <- 'pass.body_part.name'
      }else if(input$passNetworkTypePlot == 'Pass Type'){
        selColumnIn <- 'pass.type.name'
      }
      
      #all network
      dfplayerallpassall <- dfplayerallpass[complete.cases(dfplayerallpass[[selColumnIn]]),]
    }
    
    dfpatterns <- data.frame(from = dfplayerallpassall$playerName, 
                             to = dfplayerallpassall[[selColumnIn]])
    
    dfpatternsN <- dfpatterns %>% group_by(from, to) %>% dplyr::count()
    
    allunique <- data.frame(name = c(as.character(dfpatternsN$from), 
                                     as.character(dfpatternsN$to))) %>% group_by(name) %>% 
      dplyr::count()
    allunique$group <- ifelse(grepl('\\[|\\]', allunique$name), 'Player', 'Play')
    
    nodes <- data.frame(id = 1:length(allunique$name), 
                        label = allunique$name,
                        value = allunique$n, 
                        group = allunique$group,
                        title = allunique$name)
  }
  
  tmpcorr <- setNames(nodes$id, nodes$label)
  
  edges <- data.frame(from = dfpatternsN$from,
                      to = dfpatternsN$to,
                      value = dfpatternsN$n)
  
  edges$from <- tmpcorr[as.character(edges$from)]
  edges$to <- tmpcorr[as.character(edges$to)]
  
  visNetwork(nodes, edges, height = "500px", width = "100%") %>% 
    visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T),
               nodesIdSelection = TRUE)
})
#.............................................................................
#.............................................................................
dfplayerallpass <- dfevent[dfevent$type.name == 'Pass',]
dfplayerallpass <- dfplayerallpass %>% replace_na(list(pass.outcome.name = 'Complete'))

allpatterns <- unlist(strsplit('play_pattern.name pass.height.name pass.body_part.name pass.type.name pass.outcome.name', ' '))

for(i in allpatterns){
  dfi <- dfplayerallpass %>% 
    replace_na(list(pass.outcome.name = 'Complete'))
  
  dfi <- dfi[complete.cases(dfi[[i]]),] %>%
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

dfevent_totalPasses_both <- alldf %>% group_by(Stat) %>% 
  mutate(pairN=n()) %>%
  filter(pairN > 1) %>%
  select(-one_of('pairN')) %>%
  group_by(Stat) %>%
  mutate(Total = sum(Value)) %>%
  mutate(Percentage = round(Value * 100 / Total))

output$Pass_Accuracy_plot <- renderHighchart({
  tmpdfplot <- dfevent_totalPasses_both %>% 
    filter(pattern == 'Pass Outcome')
  hchart(tmpdfplot, "column", 
         hcaes(x = Stat, y = Percentage, group = Team)) %>% 
    hc_chart(inverted = TRUE)  %>%
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

output$Pass_Body_Part_plot <- renderHighchart({
  tmpdfplot <- dfevent_totalPasses_both %>% 
    filter(pattern == 'Pass Body Part')
  hchart(tmpdfplot, "column", 
         hcaes(x = Stat, y = Percentage, group = Team)) %>% 
    hc_chart(inverted = TRUE)  %>%
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

output$Pass_Height_plot <- renderHighchart({
  tmpdfplot <- dfevent_totalPasses_both %>% 
    filter(pattern == 'Pass Height')
  hchart(tmpdfplot, "column", 
         hcaes(x = Stat, y = Percentage, group = Team)) %>% 
    hc_chart(inverted = TRUE)  %>%
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

output$Pass_Type_plot <- renderHighchart({
  tmpdfplot <- dfevent_totalPasses_both %>% 
    filter(pattern == 'Pass Type')
  hchart(tmpdfplot, "column", 
         hcaes(x = Stat, y = Percentage, group = Team)) %>% 
    hc_chart(inverted = TRUE)  %>%
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

output$Play_Pattern_plot <- renderHighchart({
  tmpdfplot <- dfevent_totalPasses_both %>% 
    filter(pattern == 'Play Pattern')
  hchart(tmpdfplot, "column", 
         hcaes(x = Stat, y = Percentage, group = Team)) %>% 
    hc_chart(inverted = TRUE)  %>%
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