`%nin%` <- Negate(`%in%`)
#load data
show_modal_spinner(text = 'Loading Data. Please wait!')
matchesdf <- read.csv('matchesdf2.csv')
lineupsdf <- read.csv('lineupsdf2.csv')
lineupsdf_player <- lineupsdf %>% select(player_id, player_nickname, jersey_number, team_id)
load('dfdata.RData')
df <- dfdata
df <- df %>%
  group_by(possession, fileid) %>%
  mutate(possession_n = 1:n())
eventsDescription <- read.csv('events.csv')
remove_modal_spinner()