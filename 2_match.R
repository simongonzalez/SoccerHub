output$competitionSelectionui <- renderUI({
  tmpchoices <- sort(unique(matchesdf$competition.competition_name))
  selectInput("competitionSelection", "Competition",
              choices = tmpchoices, 
              selected = tmpchoices[1])
})

output$selectTeamui <- renderUI({
  if(is.null(input$competitionSelection))
    return()
  
  df <- matchesdf[matchesdf$competition.competition_name == 
                    input$competitionSelection,]
  
  tmpoptions <- sort(unique(c(as.character(df$home_team.home_team_name), 
                              as.character(df$away_team.away_team_name))))
  
  selectInput("selectTeam", "Team", tmpoptions)
})

output$selectSeasonui <- renderUI({
  if(is.null(input$competitionSelection))
    return()
  
  if(is.null(input$selectTeam))
    return()
  
  dfhome <- matchesdf[matchesdf$competition.competition_name == 
                        input$competitionSelection &
                        matchesdf$home_team.home_team_name == input$selectTeam,]
  
  dfaway <- matchesdf[matchesdf$competition.competition_name == 
                        input$competitionSelection &
                        matchesdf$away_team.away_team_name == input$selectTeam,]
  
  if(nrow(dfhome) == 0 & nrow(dfaway) != 0){
    df <- dfaway 
  }else if(nrow(dfhome) != 0 & nrow(dfaway) == 0){
    df <- dfhome
  }else{
    df <- rbind(dfhome, dfaway)
  }
  
  tmpoptions <- sort(unique(as.character(df$season.season_name)))
  
  selectInput("selectSeason", "Season", tmpoptions)
})


output$selectStageui <- renderUI({
  if(is.null(input$competitionSelection))
    return()
  
  if(is.null(input$selectTeam))
    return()
  
  if(is.null(input$selectSeason))
    return()
  
  dfhome <- matchesdf[matchesdf$competition.competition_name == 
                        input$competitionSelection &
                        matchesdf$home_team.home_team_name == input$selectTeam &
                        matchesdf$season.season_name == input$selectSeason,]
  
  dfaway <- matchesdf[matchesdf$competition.competition_name == 
                        input$competitionSelection &
                        matchesdf$away_team.away_team_name == input$selectTeam &
                        matchesdf$season.season_name == input$selectSeason,]
  
  if(nrow(dfhome) == 0 & nrow(dfaway) != 0){
    df <- dfaway 
  }else if(nrow(dfhome) != 0 & nrow(dfaway) == 0){
    df <- dfhome
  }else{
    df <- rbind(dfhome, dfaway)
  }
  
  tmpoptions <- sort(unique(as.character(df$competition_stage.name)))
  
  selectInput("selectStage", "Stage", tmpoptions)
})

output$selectVsui <- renderUI({
  if(is.null(input$competitionSelection))
    return()
  
  if(is.null(input$selectTeam))
    return()
  
  if(is.null(input$selectSeason))
    return()
  
  if(is.null(input$selectStage))
    return()
  
  dfhome <- matchesdf[matchesdf$competition.competition_name == 
                        input$competitionSelection &
                        matchesdf$home_team.home_team_name == input$selectTeam &
                        matchesdf$season.season_name == input$selectSeason &
                        matchesdf$competition_stage.name == input$selectStage,]
  
  dfaway <- matchesdf[matchesdf$competition.competition_name == 
                        input$competitionSelection &
                        matchesdf$away_team.away_team_name == input$selectTeam &
                        matchesdf$season.season_name == input$selectSeason &
                        matchesdf$competition_stage.name == input$selectStage,]
  
  if(nrow(dfhome) == 0 & nrow(dfaway) != 0){
    df <- dfaway 
  }else if(nrow(dfhome) != 0 & nrow(dfaway) == 0){
    df <- dfhome
  }else{
    df <- rbind(dfhome, dfaway)
  }
  
  tmpoptions <- sort(unique(c(as.character(df$home_team.home_team_name), 
                              as.character(df$away_team.away_team_name))))
  
  tmpoptions <- sort(unique(tmpoptions[tmpoptions != input$selectTeam]))
  
  selectInput("selectVs", "Vs", tmpoptions)
})

output$selectDateui <- renderUI({
  if(is.null(input$competitionSelection))
    return()
  
  if(is.null(input$selectTeam))
    return()
  
  if(is.null(input$selectSeason))
    return()
  
  if(is.null(input$selectStage))
    return()
  
  if(is.null(input$selectVs))
    return()
  
  dfhome <- matchesdf[matchesdf$competition.competition_name == 
                        input$competitionSelection &
                        matchesdf$home_team.home_team_name == input$selectTeam &
                        matchesdf$season.season_name == input$selectSeason &
                        matchesdf$competition_stage.name == input$selectStage &
                        matchesdf$away_team.away_team_name == input$selectVs,]
  
  dfaway <- matchesdf[matchesdf$competition.competition_name == 
                        input$competitionSelection &
                        matchesdf$away_team.away_team_name == input$selectTeam &
                        matchesdf$season.season_name == input$selectSeason &
                        matchesdf$competition_stage.name == input$selectStage &
                        matchesdf$home_team.home_team_name == input$selectVs,]
  
  if(nrow(dfhome) == 0 & nrow(dfaway) != 0){
    df <- dfaway 
  }else if(nrow(dfhome) != 0 & nrow(dfaway) == 0){
    df <- dfhome
  }else{
    df <- rbind(dfhome, dfaway)
  }
  
  cntr <- 1
  
  for(dfi in 1:nrow(df)){
    if(df$home_team.home_team_name == input$selectTeam){
      dfin <- df %>% select(match_date, away_team.away_team_name) %>%
        rename(vsteam = away_team.away_team_name)
      if(cntr == 1){
        tmpdf <- dfin
      }else{
        tmpdf <- rbind(dfin, tmpdf)
      }
      cntr <- cntr + 1
    }else{
      dfin <- df %>% select(match_date, home_team.home_team_name) %>%
        rename(vsteam = home_team.home_team_name)
      if(cntr == 1){
        tmpdf <- dfin
      }else{
        tmpdf <- rbind(dfin, tmpdf)
      }
      cntr <- cntr + 1
    }
  }
  
  tmpdf <- tmpdf %>% group_by(vsteam) %>% mutate(n=n())
  
  if(unique(tmpdf$n) > 1){
    tmpoptions <- sort(unique(tmpdf$match_date))
    
    selectInput("selectDate", "Date", tmpoptions)
  }else{
    return()
  }
  
})
