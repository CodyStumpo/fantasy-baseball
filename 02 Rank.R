library(XML)
library(RCurl)
library(magrittr)
library(tidyverse)

source(here::here("01 GetProjections.R"))




chq <- compositeH %>% 
  filter(R > 30) %>%
  map_if(is.numeric, function(x) ecdf(x)(x)) %>% data.frame(stringsAsFactors = F) %>% 
  mutate(total=R+HR+RBI+OBP+SLG+NSB) %>% 
  mutate(rank=rank(desc(total))) %>% 
  arrange(rank)

## Everything is bell-curve shaped except NSB - where sqrt(NSB) might work better - except log(0) = -Inf, messes up scale function
chz <- compositeH %>% 
  filter(R>30) %>%
  mutate(NSB = replace_na(NSB, 0)) %>%
  mutate(NSB = sqrt(NSB)) %>%
  map_if(is.numeric, function(x) scale(x)) %>% data.frame (stringsAsFactors = F) %>% 
  mutate(total=R+HR+RBI+OBP+SLG+NSB) %>% 
  mutate(rank=rank(desc(total))) %>% 
  arrange(rank)

# think about position effect
#Need to add in fielding position, to get an effect
# could crib from fangraphs calc for this league
# reliever = 11, starter = 10
# OF = 13, 3B = 12, 1B = 13, SS = 12
# C = 33, 2B = 16
#Just add 15 for C, subtract 5 for everything but 2B?
# in actual league, people are not paying C premium, but are paying $10 closer premium

#fix a few guys yahoo has positions for, that are DH here

y1B <- c('Kendrys Morales', 'Jose Abreu')
y3B <- 'Carlos Santana'
yOF <- c('Giancarlo Stanton', 'Khris Davis', 'Shin-Soo Choo', 'J.D. Martinez')

chz$POS[match(y1B, chz$Name)] <- "1B"
chz$POS[match(y3B, chz$Name)] <- "3B"
chz$POS[match(yOF, chz$Name)] <- "OF"


top150H_AvgZ <- chz %>% 
  head(150) %>% 
  summarise(total=mean(total)) %>% 
  as.numeric #3.2
posEffectZ <- chz %>% 
  head(150) %>% 
  group_by(POS) %>% 
  summarise(PosTotal=mean(total)) %>% 
  mutate(posEffect = top150H_AvgZ - PosTotal) # Add this to chz$total


chz %<>% left_join(posEffectZ) %>% 
  mutate(adjTotal = total + posEffect, adjRank = rank(desc(adjTotal))) %>% 
  arrange(adjRank)




#############
###OK Now, do pitchers.  Need to deal with saves, only scoring within relievers, to give starters 0 credit for this category and differentiate relievers more
########


savers <- compositeP %>% 
  filter(SV > 0) %>%
  map_if(is.numeric, function(x) ecdf(x)(x)) %>% data.frame(stringsAsFactors = FALSE) %>% 
  select(Name, SV)
SV2 <- compositeP %>% 
  left_join(savers, by = "Name") %>% 
  mutate(SV = replace_na(SV.y, 0)) %>% 
  select(Name, SV)


cpq <- compositeP %>%
  map_if(is.numeric, function(x) ecdf(x)(x)) %>% 
  data.frame(stringsAsFactors = FALSE) %>% 
  right_join(SV2, by="Name") %>% 
  select(-SV.x) %>% rename(SV=SV.y) %>% 
  mutate(total=IP+ERSaved+WHSaved+KSurplus+QS+SV) %>% 
  mutate(rank=rank(desc(total))) %>% 
  arrange(rank)




overallCq <- rbind(chq %>% select(Name, total), cpq %>% select(Name, total)) %>% arrange(desc(total))
#underrating pitchers

cpz <- compositeP %>% map_if(is.numeric, function(x) scale(x)) %>% 
  data.frame(stringsAsFactors = FALSE) %>% 
  right_join(SV2, by="Name") %>% 
  select(-SV.x) %>% rename(SV=SV.y) %>% 
  mutate(total=IP+ERSaved+WHSaved+KSurplus+QS+SV)  %>%
  mutate(rank=rank(desc(total))) %>% 
  arrange(rank)

overallCz <- rbind(chz %>% select(Name, total=adjTotal), 
                   cpz %>% select(Name, total)) %>% 
  arrange(desc(total))



#not giving closers enough boost.  Market priceis more for them
