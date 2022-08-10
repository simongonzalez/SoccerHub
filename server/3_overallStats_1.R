output$homeNameui <- renderText({
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
  
  if(nrow(dfhome) > 0 & !is.null(input$selectDate)){
    dfhome <- dfhome[dfhome$match_date == input$selectDate,]
  }
  
  dfaway <- matchesdf[matchesdf$competition.competition_name == 
                        input$competitionSelection &
                        matchesdf$away_team.away_team_name == input$selectTeam &
                        matchesdf$season.season_name == input$selectSeason &
                        matchesdf$competition_stage.name == input$selectStage &
                        matchesdf$home_team.home_team_name == input$selectVs,]
  
  if(nrow(dfaway) > 0 & !is.null(input$selectDate)){
    dfaway <- dfaway[dfaway$match_date == input$selectDate,]
  }
  
  if(nrow(dfhome) == 0 & nrow(dfaway) != 0){
    print(paste(input$selectVs, ' (', dfaway$home_score, ')'))
  }else if(nrow(dfhome) != 0 & nrow(dfaway) == 0){
    print(paste(input$selectTeam, ' (', dfhome$home_score, ')'))
  }
  
})

output$awayNameui <- renderText({
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
  
  if(nrow(dfhome) > 0 & !is.null(input$selectDate)){
    dfhome <- dfhome[dfhome$match_date == input$selectDate,]
  }
  
  dfaway <- matchesdf[matchesdf$competition.competition_name == 
                        input$competitionSelection &
                        matchesdf$away_team.away_team_name == input$selectTeam &
                        matchesdf$season.season_name == input$selectSeason &
                        matchesdf$competition_stage.name == input$selectStage &
                        matchesdf$home_team.home_team_name == input$selectVs,]
  
  if(nrow(dfaway) > 0 & !is.null(input$selectDate)){
    dfaway <- dfaway[dfaway$match_date == input$selectDate,]
  }
  
  if(nrow(dfhome) == 0 & nrow(dfaway) != 0){
    print(paste(input$selectTeam, ' (', dfaway$away_score, ')'))
  }else if(nrow(dfhome) != 0 & nrow(dfaway) == 0){
    print(paste(input$selectVs, ' (', dfhome$away_score, ')'))
  }
  
})

output$teamhomeplot <- renderFormattable({
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
  
  if(nrow(dfhome) > 0 & !is.null(input$selectDate)){
    dfhome <- dfhome[dfhome$match_date == input$selectDate,]
  }
  
  dfaway <- matchesdf[matchesdf$competition.competition_name == 
                        input$competitionSelection &
                        matchesdf$away_team.away_team_name == input$selectTeam &
                        matchesdf$season.season_name == input$selectSeason &
                        matchesdf$competition_stage.name == input$selectStage &
                        matchesdf$home_team.home_team_name == input$selectVs,]
  
  if(nrow(dfaway) > 0 & !is.null(input$selectDate)){
    dfaway <- dfaway[dfaway$match_date == input$selectDate,]
  }
  
  if(nrow(dfhome) == 0 & nrow(dfaway) != 0){
    dfin <- dfaway 
  }else if(nrow(dfhome) != 0 & nrow(dfaway) == 0){
    dfin <- dfhome
  }
  
  tmpmatch <- dfin$match_id
  dfmatch <- matchesdf %>% filter(match_id == tmpmatch)
  dflineup <- lineupsdf %>% filter(game_id == tmpmatch)
  dfevent <- df %>% filter(fileid == tmpmatch) %>%
    rename(player_name = player.name)
  dfeventPosition <- dfevent %>% select(player_name, position.name) %>%
    distinct()
  dflineup <- dflineup %>% right_join(dfeventPosition)
  dflineupUnique <- dflineup %>% distinct(player_name, .keep_all = T)
  
  homelineup <- dflineupUnique[dflineupUnique$team_id == 
                                 sort(unique(dfmatch$home_team.home_team_id)),] %>%
    select(country_name, player_nickname,
           jersey_number, position.name) %>%
    rename(origin = country_name, name = player_nickname,
           number = jersey_number, position = position.name) %>%
    arrange(number)
  
  homelineup <- homelineup[complete.cases(homelineup),]
  
  formattable(homelineup, list(
    number = color_tile("green", "orange"),
    name = formatter("span", 
                     style = x ~ formattable::style(font.weight = "bold")),
    position = formatter("span", 
                         style = x ~ formattable::style(font.weight = "italic",
                                                        color = 'blue'))
  ))
})

tmpmatchid <- reactiveValues(id=NULL)

output$teamawayplot <- renderFormattable({
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
  
  if(nrow(dfhome) > 0 & !is.null(input$selectDate)){
    dfhome <- dfhome[dfhome$match_date == input$selectDate,]
  }
  
  dfaway <- matchesdf[matchesdf$competition.competition_name == 
                        input$competitionSelection &
                        matchesdf$away_team.away_team_name == input$selectTeam &
                        matchesdf$season.season_name == input$selectSeason &
                        matchesdf$competition_stage.name == input$selectStage &
                        matchesdf$home_team.home_team_name == input$selectVs,]
  
  if(nrow(dfaway) > 0 & !is.null(input$selectDate)){
    dfaway <- dfaway[dfaway$match_date == input$selectDate,]
  }
  
  if(nrow(dfhome) == 0 & nrow(dfaway) != 0){
    dfin <- dfaway 
  }else if(nrow(dfhome) != 0 & nrow(dfaway) == 0){
    dfin <- dfhome
  }
  
  tmpmatch <- dfin$match_id
  
  tmpmatchid$id <- tmpmatch
  
  dfmatch <- matchesdf %>% filter(match_id == tmpmatch)
  dflineup <- lineupsdf %>% filter(game_id == tmpmatch)
  dfevent <- df %>% filter(fileid == tmpmatch) %>%
    rename(player_name = player.name)
  dfeventPosition <- dfevent %>% select(player_name, position.name) %>%
    distinct()
  dflineup <- dflineup %>% right_join(dfeventPosition)
  dflineupUnique <- dflineup %>% distinct(player_name, .keep_all = T)
  
  awaylineup <- dflineupUnique[dflineupUnique$team_id == 
                                 sort(unique(dfmatch$away_team.away_team_id)),] %>%
    select(country_name, player_nickname,
           jersey_number, position.name) %>%
    rename(origin = country_name, name = player_nickname,
           number = jersey_number, position = position.name) %>%
    arrange(number)
  
  awaylineup <- awaylineup[complete.cases(awaylineup),]
  
  formattable(awaylineup, list(
    number = color_tile("green", "orange"),
    name = formatter("span", 
                     style = x ~ formattable::style(font.weight = "bold")),
    position = formatter("span", 
                         style = x ~ formattable::style(font.weight = "italic",
                                                        color = 'blue'))
  ))
})