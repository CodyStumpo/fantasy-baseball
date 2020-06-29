library(tidyverse)

##### Inputs 

# last year's league standings, scaled to 60 games
read_csv("sgp/standings2019.csv") -> scaled2019

# Hitter's projections: Name, PA, AB, R, HR, RBI, OBP, SLG, NSB
read_csv("sgp/hitters60.csv") -> hittersInput

# Pitcher's projections: Name, IP, ERA, WHIP, K9, SV, QS
read_csv("sgp/pitchers60.csv") -> pitchersInput

# Position players' eligibility: Name, 1B, 2B, 3B, SS, OF, C, DHonly
read_csv("sgp/positions.csv") -> positions

# Keepers: Name, price
read_csv("sgp/kept.csv") %>% rename(keeperPrice = 4) -> kept

# Note that Name is the key across these tables.

############ Constants
teams = 12
budget = 260
pitcherFTE = 13 # (team stats ~ 13x individual pitcher)
hitterFTE = 10 # (team stats ~10x individual hitter)
draftedHitters = 132
draftedPitchers = 156
avgPA = 225 
avgAB = 200

#######  SGP definition
# Define Standings-Gained Point Interval in each category
# 12-team league was pretty linear from position 2 to position 11 last year

sgInterval = function(vec) {
  sort(vec) -> tmp
  tmp[2:length(tmp)-1] -> tmp
  (last(tmp) - first(tmp)) / (length(tmp) - 1)
}

scaled2019 %>% summarise_all(sgInterval) -> SGP

# marginal 2.4 HR is as good as 4.4 R is as good as 3 SV is as good as .055 team ERA...

######### Compute hitters

scaled2019 %>% 
  select(R, HR, RBI, NSB, OBP, SLG) %>% 
  summarise_all(mean) %>% 
  mutate_at(c("R","HR","RBI","NSB"), funs(./10)) -> avgHitter 

# this is a modification of SGP, compute SGP above average

hittersInput %>% mutate(R = (R - avgHitter$R) /SGP$R,
                   HR = (HR - avgHitter$HR) /SGP$HR,
                   RBI = (RBI - avgHitter$RBI) /SGP$RBI,
                   OBP = (OBP - avgHitter$OBP) / (SGP$OBP * (hitterFTE * avgPA / PA )),
                   SLG = (SLG - avgHitter$SLG) / (SGP$SLG * (hitterFTE * avgAB / AB )),
                   NSB = (NSB - avgHitter$NSB)/ SGP$NSB) %>%
  mutate(SGP = R+HR+RBI+OBP+SLG) %>%
  select(Name, SGP) %>%
  arrange(desc(SGP)) -> hitters

# this is a simplification, not considering position for who's likely to get drafted.
replacementHitter = hitters[draftedHitters, "SGP"][[1]]

hitters[1:draftedHitters,] %>% 
  mutate(SGPAR = SGP - replacementHitter,
         position = "H") -> lgHitters

  
getAvgSGPARbyPos <- function(pos) {
lgHitters %>% 
  left_join(positions) %>%
  filter({{pos}} > 0) %>% 
  summarise(avgSGPAR = mean(SGPAR)) %>% 
    pull(avgSGPAR)
}

positionAverages <- data.frame(position = colnames(positions[-1]), 
                               avg=c(getAvgSGPARbyPos(`1B`),
                                      getAvgSGPARbyPos(`2B`),
                                      getAvgSGPARbyPos(`3B`),
                                      getAvgSGPARbyPos(SS),
                                      getAvgSGPARbyPos(OF),
                                      getAvgSGPARbyPos(C),
                                      getAvgSGPARbyPos(DHonly)
                                     )
                               ) %>% 
  arrange(desc(avg))

overallAvg <- getAvgSGPARbyPos("any")
positionAverages %>% mutate(boost = overallAvg-avg) -> positionAverages

positions %>%
  mutate(boost = case_when(C > 0 ~ (positionAverages %>% filter(position=='C'))$boost,
                           `2B` > 0 ~ (positionAverages %>% filter(position=='2B'))$boost,
                           `1B` > 0 ~ (positionAverages %>% filter(position=='1B'))$boost,
                           OF > 0 ~ (positionAverages %>% filter(position=='OF'))$boost,
                           `3B` > 0 ~ (positionAverages %>% filter(position=='3B'))$boost,
                           SS > 0 ~ (positionAverages %>% filter(position=='SS'))$boost,
                           DHonly > 0 ~ (positionAverages %>% filter(position=='DHonly'))$boost,
                           T ~ 0)) -> positions

lgHitters %>% 
  left_join(positions) %>%
  mutate(SGPAR = SGPAR + boost) %>%
  select(Name, SGP, SGPAR, position) -> lgHitters

############### compute pitchers
scaled2019 %>% 
  select(IP, ERA, WHIP, K9, SV, QS) %>% 
  summarise_all(mean) %>% 
  mutate_at(c("IP", "SV", "QS"), funs(./pitcherFTE)) -> avgPitcher 

pitchersInput %>% mutate(
                   ERA = (avgPitcher$ERA - ERA) /(SGP$ERA * (pitcherFTE * avgPitcher$IP / IP )),
                   WHIP = (avgPitcher$WHIP - WHIP) /(SGP$WHIP * (pitcherFTE * avgPitcher$IP / IP )),
                   K9 = (K9 - avgPitcher$K9) / (SGP$K9 * (pitcherFTE * avgPitcher$IP / IP )),
                   SV = (SV - avgPitcher$SV) / SGP$SV,
                   QS = (QS - avgPitcher$QS)/ SGP$QS,
                   IP = (IP - avgPitcher$IP) /SGP$IP) %>%
  mutate(SGP = IP+ERA+WHIP+K9+SV+QS) %>%
  select(Name, SGP) %>%
  arrange(desc(SGP)) -> pitchers

replacementPitcher = pitchers[draftedPitchers, "SGP"][[1]]

pitchers[1:draftedPitchers,] %>% 
  mutate(SGPAR = SGP - replacementPitcher,
         position = "P") -> lgPitchers

######## Price
  
rbind(lgHitters , lgPitchers) %>%
  mutate(price = teams*budget * SGPAR / sum(SGPAR, na.rm = T)) %>%
  arrange(desc(price)) %>% 
  mutate(rank=row_number())-> prices 


kept %>% 
  left_join(prices, by="Name") %>% 
  summarise(SGPAR = sum(SGPAR, na.rm = T), 
            keeperPrice=sum(keeperPrice, na.rm = T)
            ) -> keptOut

prices %>% 
  summarise(SGPAR = sum(SGPAR, na.rm=T),
            price=sum(price, na.rm=T)
  ) -> preKeeper

inflatedRate = (sum(prices$price, na.rm = T) - sum(keptOut$keeperPrice)) / 
  (sum(prices$SGPAR, na.rm = T) - sum(keptOut$SGPAR))

prices %>% 
  anti_join(kept) %>% 
  mutate(postKeeper = SGPAR * inflatedRate) -> draftDay
## draftDay is the prices to expect for unkept players on draft day

draftDay %>% 
  left_join(hittersInput) %>% 
  left_join(pitchersInput) %>% 
  select(-c(price, SGP, position)) %>%
  mutate_at(vars(SGPAR, postKeeper), round, 0) %>%
  left_join(positions[,-9]) -> bigBoard
