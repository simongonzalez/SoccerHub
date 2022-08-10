dfmatch <- matchesdf %>% filter(match_id == tmpmatchid$id)
dflineup <- lineupsdf %>% filter(game_id == tmpmatchid$id)
dfevent <- df %>% filter(fileid == tmpmatchid$id) %>%
  rename(player_name = player.name)

lineupsdf_player_dat <- lineupsdf_player %>% filter(team_id %in% sort(unique(dfevent$team.id))) %>%
  select(-one_of('team_id')) %>% unique() %>%
  rename(player.id = player_id)

dfevent <- merge(dfevent, lineupsdf_player_dat, by = 'player.id')
dfevent$playerPosition <- gsub("(?<=[A-Z])[^A-Z]+", "", dfevent$position.name, perl = TRUE)

dfevent <- dfevent %>% mutate(
  playerName = paste(player_nickname, ' [', jersey_number, '] ', playerPosition, sep = "")
)