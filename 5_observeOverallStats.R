#total passes
dfevent_totalPasses <- dfevent %>% group_by(possession_team.name, type.name) %>% 
  tally() %>% 
  group_by(type.name) %>% mutate(pairN=n()) %>%
  filter(pairN > 1) %>%
  select(-one_of('pairN')) %>%
  set_colnames(unlist(strsplit('Team,Stat,Value', ','))) %>% 
  group_by(Stat) %>%
  mutate(Total = sum(Value)) %>%
  mutate(Percentage = round(Value * 100 / Total))

performance_offensive <- unlist(strsplit('Ball Recovery,Carry,Dribble,Duel,Pass,Shot', ','))
performance_defensive <- unlist(strsplit('Ball Receipt*,Block,Clearance,Dispossessed,Dribbled Past,Goal Keeper,Interception,Pressure', ','))
performance_errors <- unlist(strsplit('Foul Committed,Foul Won,Miscontrol,Offside', ','))

performance_offensive_df <- dfevent_totalPasses[dfevent_totalPasses$Stat %in% performance_offensive,]
performance_defensive_df <- dfevent_totalPasses[dfevent_totalPasses$Stat %in% performance_defensive,]
performance_errors_df <- dfevent_totalPasses[dfevent_totalPasses$Stat %in% performance_errors,]

output$offensive_plot <- renderHighchart({
  hchart(performance_offensive_df, "column", 
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

output$defensive_plot <- renderHighchart({
  hchart(performance_defensive_df, "column", 
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

output$error_plot <- renderHighchart({
  hchart(performance_errors_df, "column", 
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