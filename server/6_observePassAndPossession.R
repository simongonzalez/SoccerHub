#possessions
#whole possession
dfdur <- dfevent %>% group_by(possession_team.name) %>% 
  summarise(duration = sum(duration, na.rm = T)) %>%
  set_colnames(unlist(strsplit('Team,Value', ','))) %>% 
  mutate(Stat = 'Possession') %>% 
  relocate(c('Team', 'Stat', 'Value'))

#periods
dfdur_periods <- dfevent %>% filter(period %in% c(1,2)) %>%
  group_by(period, possession_team.name) %>% summarise(duration = sum(duration, na.rm = T)) %>% 
  mutate(Stat = 'Poss_half') %>% unite(Stat, Stat, period, sep = '') %>% 
  set_colnames(unlist(strsplit('Stat,Team,Value', ','))) %>% 
  relocate(c('Team', 'Stat', 'Value'))

#per play
dfdur_possessions <- dfevent %>% group_by(possession, possession_team.name) %>% 
  summarise(allsums = sum(duration, na.rm = T)) %>% 
  group_by(possession_team.name) %>% 
  summarise(duration = mean(allsums, na.rm = T)) %>%
  set_colnames(unlist(strsplit('Team,Value', ','))) %>% mutate(Stat = 'Poss_per_play') %>% 
  relocate(c('Team', 'Stat', 'Value'))
#...............................................................................
#...............................................................................
#passes length
#whole possession
dfdur_pass <- dfevent %>% group_by(possession_team.name) %>% 
  summarise(passlength = (mean(pass.length, na.rm = T) / 1.094)) %>%
  set_colnames(unlist(strsplit('Team,Value', ','))) %>% mutate(Stat = 'PassLenth') %>% 
  relocate(c('Team', 'Stat', 'Value'))
#periods
dfdur_periods_pass <- dfevent %>% filter(period %in% c(1,2)) %>%
  group_by(period, possession_team.name) %>% 
  summarise(passlength = (mean(pass.length, na.rm = T) / 1.094)) %>% 
  mutate(Stat = 'PassLg_half') %>% unite(Stat, Stat, period, sep = '') %>% 
  set_colnames(unlist(strsplit('Stat,Team,Value', ','))) %>% 
  relocate(c('Team', 'Stat', 'Value'))

#per play
dfdur_possessions_pass <- dfevent %>% group_by(possession, possession_team.name) %>% 
  summarise(passlengthall = (mean(pass.length, na.rm = T) / 1.094)) %>% 
  group_by(possession_team.name) %>% 
  summarise(passlength = mean(passlengthall, na.rm = T)) %>%
  set_colnames(unlist(strsplit('Team,Value', ','))) %>% mutate(Stat = 'PassL_per_play') %>% 
  relocate(c('Team', 'Stat', 'Value'))

alldfs <- do.call("rbind", list(dfdur, dfdur_periods, dfdur_possessions, 
                                dfdur_pass, dfdur_periods_pass, dfdur_possessions_pass))

alldfs <- alldfs %>% 
  group_by(Stat) %>%
  mutate(Total = sum(Value)) %>%
  mutate(Percentage = round(Value * 100 / Total))
#...............................................................................
#...............................................................................

output$passpossession_plot <- renderHighchart({
  hchart(alldfs, "column", 
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