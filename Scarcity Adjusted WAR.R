IndivWARData <- readRDS("C:/Users/sethl/OneDrive/Important Stuff/R/R files/NFL/Shiny Apps/WAR_positiongroup/data/IndivWARData.rds")

Top5PosWAR <- IndivWARData %>% 
  group_by(position) %>% 
  arrange(desc(WAR)) %>% 
  slice_head(n = 5) %>% 
  summarise(Top5WAR = mean(WAR))

ScarcityWAR <- IndivWARData %>% 
  left_join(Top5PosWAR) %>% 
  mutate(PercentofTop = WAR/Top5WAR)


yearlystability <- IndivWARData %>% 
filter(season != 2023, position != "FB", position != "ST", position != "P") %>% 
  group_by(player_id) %>% 
  arrange(desc(season)) %>% 
  mutate(LastSeasonWAR = lead(WAR, 1)) %>%
  select(position, WAR, LastSeasonWAR) %>% 
  na.omit() %>% 
  group_by(position) %>% 
  summarise(WARcor = cor(WAR, LastSeasonWAR)+0.37)


SDWAR <- IndivWARData %>% 
  left_join(yearlystability) %>% 
  filter(season != 2023, position != "FB", position != "ST", position != "P") %>% 
  group_by(position) %>% 
  mutate(SD = sd(WAR, na.rm = T)) %>% 
  group_by() %>% 
  mutate(PR = 1-percent_rank(SD),
         MinSD = min(SD, na.rm = T),
         Ratio = SD/MinSD,
         WarRatio = WAR/Ratio*11,
         NewWAR = WAR*WARcor
         )
  group_by(position) %>% 
  slice_head(n = 200) %>% 
  summarise(MeanWAR = mean(StabileWAR, na.rm = T))

ScarcitySDWAR <- IndivWARData %>% 
  group_by(season, position) %>% 
  mutate(SD = sd(WAR, na.rm = T),
         MeanWar = mean(WAR, na.rm = T),
         ZScore = percent_rank((WAR - MeanWar)/SD) + 0.5 * MeanWar,
         NewWAR = WAR + 0.07 * ZScore) 
  select(season, team_name, player, position, WAR,
         ZScore, NewWAR)  
  
  Cap <- 224800000 * 0.95 - 50000000 - 1000000*25

BestRoster <- SDWAR  %>% 
  group_by(season, team_name, position) %>% 
  arrange(desc(NewWAR)) %>% 
  mutate(Team_Rank = seq(1, n())) %>% 
  group_by(position) %>% 
  arrange(desc(NewWAR)) %>% 
  mutate(Pos_Rank = seq(1, n())) %>% 
  filter(season != 2023, position != "QB",
         position != "FB", position != "ST", position != "P", Team_Rank <= 2) %>%
  group_by(position, Team_Rank) %>%
  arrange((Pos_Rank)) %>% 
  slice_head(n = 1) %>% 
  summarise(AvgTop = mean(NewWAR, na.rm = T)) %>% 
  group_by() %>% 
  mutate(TeamWAR = sum(AvgTop),
         TeamPerc = AvgTop / TeamWAR,
         ExpCap = TeamPerc * Cap)


TeamWAR <- ScarcitySDWAR %>% 
  group_by(season, team_name) %>% 
  summarise(TeamWAR = sum(WAR, na.rm = T),
         NewTeamWAR = sum(NewWAR, na.rm = T),
         NonQBTeamWAR = sum(WAR[position != "QB"], na.rm = T),
         NonQBNewTeamWAR = sum(NewWAR[position != "QB"], na.rm = T)) %>% 
  arrange(desc(NonQBNewTeamWAR)) %>% 
  arrange(desc(season)) %>% 
  group_by(team_name) %>% 
  mutate(Improvement = NonQBNewTeamWAR - lead(NonQBNewTeamWAR, 1))
  
  

SDWAR <- IndivWARData %>% 
  group_by(position) %>% 
  mutate(SD = sd(WAR, na.rm = T),
         MeanWar = mean(WAR, na.rm = T),
         ZScore = (WAR - MeanWar)/SD) %>% 
  select(season, player, position, WAR, ZScore) %>% 
  group_by(season, position) %>% 
  arrange(desc(ZScore)) %>% 
  slice_head(n = 2) %>% 
  mutate(Diff = head(ZScore, 1) - tail(ZScore, 1)) %>% 
  group_by(season, position) %>% 
  arrange(desc(ZScore)) %>% 
  slice_head(n = 1)
