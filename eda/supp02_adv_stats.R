new_season_stats = read.csv(file.path(Sys.getenv("USERPROFILE"), "Downloads\\Seasons_Stats.csv"))
fix_player_id = read.csv(file.path(Sys.getenv("USERPROFILE"), "Downloads\\fix_players.csv"))

master_df = read.csv(file.path(data_path, "basketball_master.csv"))

unique_players = players_df %>% select(
  playerID,
  tmID,
  year
) %>% mutate(
  tmId = as.character(tmID),
  playerID = as.character(playerID)
) %>% unique()

player_names = master_df %>% mutate(
  playerID = as.character(bioID),
  player = tolower(paste(
    ifelse(
      as.character(useFirst)=="", 
      as.character(firstName), 
      as.character(useFirst)
    ), 
    lastName
  ))
) %>% select(
  playerID,
  player
)

join_players = unique_players %>% left_join(
  player_names,
  by="playerID"
)

fix_names_season = new_season_stats %>% mutate(
  player = tolower(gsub("[*]", "", Player)),
  tmID = as.character(Tm),
  year = Year - 1
)

new_season_join = fix_names_season %>% left_join(
  join_players,
  by=c("player", "tmID", "year")
) %>% filter(
  year <= 2011 & Tm != "TOT"
) %>% left_join(
  fix_player_id,
  by=c("player", "tmID", "year")
)

adv_stats_df = new_season_join %>% mutate(
  playerID = ifelse(is.na(playerID), as.character(playerID_update), playerID)
) %>% select(
  playerID,
  tmID,
  year,
  OWS,
  DWS,
  WS,
  TRB.,
  USG.,
  VORP,
  BPM,
  PER
) %>% rename(
  off_win_share = OWS,
  def_win_share = DWS,
  win_share = WS,
  tot_reb_percent = TRB.,
  usage_percent = USG.,
  val_over_rep_player = VORP,
  plus_minus = BPM,
  player_eff_rating = PER
)

write.csv(adv_stats_df, file.path(data_path, "basketball_adv_stats.csv"), row.names = FALSE)
