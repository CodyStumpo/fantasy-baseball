#Get projections
library(XML)
library(RCurl)
library(magrittr)
library(tidyverse)


####### SCRAPE Composite Hitter Projections, and prepare for rating / pricing
########
######################
url <- "https://www.rotochamp.com/baseball/PlayerRankings.aspx?StatType=Composite&Position=AllHitters"
compositeH <- getURL(url) %>% readHTMLTable(stringsAsFactors=F) %>% data.frame %>% head(150)
#Just have to clean up the names and column names
colnames(compositeH) <- c('Rank', 'Name', 'Team', 'POS', 'AB', 'R', 'HR', 'RBI', 'SB', 'AVG', 'OBP', 'SLG', 'OPS', 'BB', 'K', 'MixedValue')
#really need to clean up Player name.  Get rid of everything after first \r
cutRC <- function(i, df) {
  cut <- str_locate(df$Name[i], "\r") [1] - 1
  df$Name[i] %>% str_sub(1,cut)
}
cleanNames <- c()
for(i in 1:nrow(compositeH)) cleanNames <-c(cleanNames, (cutRC(i, compositeH)))
compositeH$Name <- cleanNames
#Now drop NA
compositeH <- compositeH[complete.cases(compositeH),] 

#This doesn't have CS, have to get that somewhere else.  

FGhitters <- read_csv(here::here("Steamer Hitters.csv")) %>% 
  mutate(NSB = SB-CS) %>%
  select(Name, R, HR, RBI, OBP, SLG, NSB, SB, CS) 
sbRate <- read_csv(here::here("Steamer Hitters.csv")) %>% 
  mutate(SBRate = SB/(SB+CS)) %>%
  select(Name,  SBRate) 

nsb <- function(sb, sbrate) 2 * sb - sb/sbrate


problemFGH <- c("Jackie Bradley Jr.", "Ronald Acuna Jr.", "Nicholas Castellanos", "Cedric Mullins II")
replaceFGH <- c("Jackie Bradley", "Ronald Acuna", "Nick Castellanos", "Cedric Mullins")
sbRate$Name[match(problemFGH, sbRate$Name)] <- replaceFGH

col2Convert = c("R", "HR", "RBI", "SB", "OBP", "SLG")
compositeH %<>% select(Name, POS, col2Convert) %>% mutate_at(3:8, as.numeric)
compositeH %<>% left_join(sbRate) %>% mutate(NSB = nsb(SB, SBRate))

#######################
###########
####### SCRAPE Composite Pitcher Projections, and prepare for rating / pricing
########
######################

url <- "https://www.rotochamp.com/baseball/PlayerRankings.aspx?StatType=Composite&Position=AllPitchers"
compositeP <- getURL(url) %>% readHTMLTable(stringsAsFactors=F) %>% data.frame %>% head(150)
#Just have to clean up the names and column names
colnames(compositeP) <- c('Rank', 'Name', 'Team', 'POS', 'IP', 'W', 'L', 'ERA', 'WHIP', 'K', 'BB', 'SV', 'MixedValue')


cleanNames <- c()
for(i in 1:nrow(compositeP)) cleanNames <-c(cleanNames, (cutRC(i, compositeP)))
compositeP$Name <- cleanNames
#Now drop NA
compositeP <- compositeP[complete.cases(compositeP),] 

col2Convert = c("IP", "ERA", "WHIP", "K", "SV")
compositeP %<>% select(Name, POS, col2Convert) %>% mutate_at(3:7, as.numeric)

## A few issues with pitchers - need QS, and need to value WH, ER, K better than average so we can compare RP and SP
## We don't even have GS, but can get that from fangraphs
FGpitchers <- read_csv(here::here("Steamer Pitchers.csv")) %>% 
  select(Name, IP, SV, ERA, WHIP, K9='K/9', GS)


problemFGP <- c("Vince Velasquez", "Matthew Boyd")
replaceFGP <- c("Vincent Velasquez", "Matt Boyd")
FGpitchers$Name[match(problemFGP, FGpitchers$Name)] <- replaceFGP


compositeP %<>% left_join(FGpitchers %>% select(Name, GS))


####### Build a model for QS, based on GS & IP
qspg <- data.frame(stringsAsFactors=FALSE,
                   Name = c("Jacob deGrom", "Max Scherzer", "Aaron Nola", "Kyle Freeland",
                            "Corey Kluber", "Justin Verlander", "Clayton Kershaw",
                            "Trevor Bauer", "Zack Greinke", "Mike Clevinger", "Jameson Taillon",
                            "Miles Mikolas", "Zack Wheeler", "Gerrit Cole", "Blake Snell",
                            "Reynaldo Lopez", "German Marquez", "David Price", "Mike Leake",
                            "Charlie Morton", "Patrick Corbin", "Carlos Carrasco",
                            "Andrew Heaney", "Kyle Gibson", "James Shields", "Dallas Keuchel",
                            "Tanner Roark", "Trevor Williams", "Jose Berrios", "Julio Teheran",
                            "Chris Sale", "Tyler Anderson", "Kevin Gausman", "Kyle Hendricks",
                            "Luis Severino", "Jon Lester", "Sean Newcomb", "Cole Hamels",
                            "Jake Arrieta", "Dylan Bundy", "Mike Fiers", "J.A. Happ",
                            "Lucas Giolito", "Jakob Junis", "Matthew Boyd", "Marco Gonzales",
                            "Zack Godley", "Rick Porcello", "Jon Gray", "Jose Quintana",
                            "Jose Urena", "Mike Foltynewicz", "Luis Castillo", "Gio Gonzalez",
                            "Jhoulys Chacin", "Jake Odorizzi", "Wade LeBlanc", "Derek Holland",
                            "Nick Pivetta"),
                   Value = c(0.88, 0.85, 0.76, 0.74, 0.74, 0.73, 0.69, 0.65, 0.64, 0.64,
                             0.62, 0.62, 0.62, 0.62, 0.61, 0.59, 0.59, 0.58, 0.58, 0.58,
                             0.58, 0.58, 0.57, 0.56, 0.56, 0.56, 0.55, 0.55, 0.53, 0.53, 0.53,
                             0.52, 0.5, 0.5, 0.5, 0.48, 0.48, 0.48, 0.48, 0.48, 0.48, 0.47,
                             0.47, 0.47, 0.45, 0.45, 0.42, 0.42, 0.42, 0.41, 0.39, 0.36, 0.35,
                             0.32, 0.32, 0.31, 0.31, 0.31, 0.27)
) #these are their 2018 actuals.  Should predict from 2018 actual IPGS and ERA, but probably close enough if we fit on pojected 2019

df <- qspg %>% left_join(compositeP) %>% mutate(IPGS = IP/GS)
qsfit  <- glm(Value ~ IPGS + ERA, df, family = binomial(link='logit'))
compositeP$QS <- predict(qsfit, compositeP %>% mutate(IPGS = IP/GS), type = 'response') * compositeP$GS
#This looks reasonable!  Issue with SP/RP where IPGS is falsely high.
# could always double back and improve, but lets go with this.




#### Now, what is the average WHIP, ERA, K9 for this league

standings <- data.frame(stringsAsFactors=FALSE,
                        Rank = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 12),
                        Team = c("Bad Boy Records", "Melancon in the Dark",
                                 "All in Dee Phamily", "Stealing First", "Snelly Farts",
                                 "UnBaezed Judge", "TBD", "Újar the,Chosen Juan",
                                 "Wil Myers is Missing", "Rhys's Pieces", "Javy for MVP", "Duke Newcomb"),
                        Total.GP = c(1587, 1567, 1588, 1583, 1574, 1590, 1558, 1588, 1530, 1542,1573, 1536),
                        R = c(934, 900, 876, 856, 899, 864, 870, 840, 775, 790, 779, 796),
                        HR = c(300, 253, 246, 239, 254, 235, 227, 233, 219, 240, 242, 209),
                        RBI = c(903, 802, 852, 832, 813, 833, 794, 825, 798, 824, 794, 761),
                        OBP = c(0.356, 0.3424, 0.3423, 0.344, 0.3395, 0.33263, 0.338, 0.34, 0.3343,0.33261, 0.33, 0.3338),
                        SLG = c(0.483, 0.461, 0.459, 0.44387, 0.4536, 0.448, 0.445, 0.442, 0.437,0.4535, 0.4439, 0.436),
                        NSB = c(78, 80, 76, 62, 124, 52, 72, 71, 65, 51, 39, 34),
                        IP = c(1577.1, 1862.2, 1722.1, 819.1, 1504, 1584.1, 1696, 1616,1609.2, 1521.1, 1518.2, 1350),
                        SV = c(71, 110, 74, 126, 88, 98, 51, 91, 71, 71, 105, 70),
                        ERA = c(3.3, 3.58, 3.74, 3.39, 3.764, 3.54, 3.83, 3.8, 3.85, 4.8, 3.89,3.76),
                        WHIP = c(1.14, 1.199, 1.235, 1.1, 1.26, 1.19, 1.196, 1.23, 1.24, 1.42, 1.27,1.21),
                        K9 = c(10.38, 9.04, 8.77, 10.91, 9.311, 9.14, 9.44, 9.306, 9.08, 8.84,9, 8.69),
                        QS = c(135, 134, 119, 42, 98, 113, 126, 105, 121, 88, 98, 85)
)


ratioAverages <- standings %>% select(OBP, SLG, ERA, WHIP, K9) %>% colMeans
#that'll more directly translate to a player's positive / negative impact on this game
# ratioAverages[["ERA"]]



#Get ER WH K better than this league's average
compositeP %<>% mutate(K9 = K / (IP/9),
                      ERSaved = (ratioAverages[["ERA"]]-ERA)/9 * IP, 
                      WHSaved = (ratioAverages[["WHIP"]]-WHIP) * IP, 
                      KSurplus = (K9 - ratioAverages[["K9"]])*IP/9)


###########
#####
#### Other stuff we can do with standings
######
###############
standingsSD <- apply(standings %>% select(-c(Rank, Team)), sd,MARGIN = 2)
#Roughly (50R     25HR     35RBI    25NSB    250IP  20SV  25QS 
# = .007OBP  .013SLG   .375ERA  .08WHIP  .07K9) Multiply rate states by 10?  because

scores <- cbind(standings %>% select(Rank, Team),
                standings %>% select(R, HR, RBI, OBP, SLG, NSB, IP, SV, K9, QS) %>% mutate_all(funs(rank)),
                standings %>% select(ERA, WHIP) %>% mutate_all(funs(rank(desc(.))))
)

scores$Total <- scores %>% select (-one_of(c("Rank", "Team"))) %>% rowSums


#save H & P projections for when website no longer serves them up
#write_csv(compositeH, "~/Documents/Code/Personal/fbb/compositeH.csv")
#write_csv(compositeP, "~/Documents/Code/Personal/fbb/compositeP.csv")
