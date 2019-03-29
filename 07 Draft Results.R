# Run all the other stuff first


postKeeperDraft <- data.frame(stringsAsFactors=FALSE,
        Pick = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18,
                 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33,
                 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49,
                 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65,
                 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81,
                 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97,
                 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110,
                 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123),
      Player = c("Max Scherzer (Was - SP)", "Mike Trout (LAA - OF)",
                 "Chris Sale (Bos - SP)", "James Paxton (NYY - SP)",
                 "Stephen Strasburg (Was - SP)", "Craig Kimbrel (Bos - RP)",
                 "Roberto Osuna (Hou - RP)", "Matt Olson (Oak - 1B)",
                 "Starling Marte (Pit - OF)", "Trevor May (Min - RP)", "Cody Allen (LAA - RP)",
                 "Drew Steckenrider (Mia - RP)", "Miles Mikolas (StL - SP)",
                 "Nelson Cruz (Min - Util)", "Josh Donaldson (Atl - 3B)",
                 "Masahiro Tanaka (NYY - SP)", "Clayton Kershaw (LAD - SP)",
                 "Justin Upton (LAA - OF)", "Jurickson Profar (Oak - 1B,2B,3B,SS)",
                 "José Abreu (CWS - 1B)", "Billy Hamilton (KC - OF)", "Mike Moustakas (Mil - 3B)",
                 "Andrew McCutchen (Phi - OF)", "David Peralta (Ari - OF)",
                 "David Robertson (Phi - RP)", "David Price (Bos - SP)",
                 "Edwin Encarnación (Sea - 1B)", "Yu Darvish (ChC - SP)", "Robbie Ray (Ari - SP)",
                 "Rich Hill (LAD - SP)", "Wil Myers (SD - 3B,OF)",
                 "José Peraza (Cin - SS)", "Andrew Miller (StL - RP)",
                 "Rougned Odor (Tex - 2B)", "Jonathan Schoop (Min - 2B,SS)", "Chris Archer (Pit - SP)",
                 "Dallas Keuchel (Hou - SP)", "Miguel Cabrera (Det - 1B)",
                 "Wilson Ramos (NYM - C)", "Aaron Hicks (NYY - OF)",
                 "Cole Hamels (ChC - SP)", "Julio Urías (LAD - RP)", "Tim Anderson (CWS - SS)",
                 "Byron Buxton (Min - OF)", "José Quintana (ChC - SP)",
                 "Ramón Laureano (Oak - OF)", "Shohei Ohtani (Pitcher) (LAA - SP)",
                 "Wily Peralta (KC - RP)", "Jake Bauers (Cle - 1B,OF)",
                 "Rick Porcello (Bos - SP)", "Nick Pivetta (Phi - SP)",
                 "Archie Bradley (Ari - RP)", "Danny Jansen (Tor - C)", "Jackie Bradley (Bos - OF)",
                 "Paul DeJong (StL - SS)", "Garrett Hampson (Col - 2B,SS)",
                 "Mychal Givens (Bal - RP)", "Joe Musgrove (Pit - SP)",
                 "Harrison Bader (StL - OF)", "Matt Barnes (Bos - RP)", "Kyle Tucker (Hou - OF)",
                 "Stephen Piscotty (Oak - OF)", "Jesse Winker (Cin - OF)",
                 "Ender Inciarte (Atl - OF)", "Mike Zunino (TB - C)",
                 "J.A. Happ (NYY - SP)", "Jake Arrieta (Phi - SP)", "Luke Voit (NYY - 1B)",
                 "Zack Britton (NYY - RP)", "Mallex Smith (Sea - OF)",
                 "Ian Desmond (Col - 1B,OF)", "Manuel Margot (SD - OF)",
                 "Nathan Eovaldi (Bos - SP)", "Elvis Andrus (Tex - SS)", "Welington Castillo (CWS - C)",
                 "Kenta Maeda (LAD - SP,RP)", "Adam Eaton (Was - OF)",
                 "Josh Bell (Pit - 1B)", "Carlos Rodón (CWS - SP)",
                 "Jon Lester (ChC - SP)", "Joey Lucchesi (SD - SP)", "Domingo Santana (Sea - OF)",
                 "Franmil Reyes (SD - OF)", "Pedro Strop (ChC - RP)",
                 "Carlos Martínez (StL - SP,RP)", "Asdrúbal Cabrera (Tex - 2B,3B,SS)",
                 "Miguel Sanó (Min - 1B,3B)", "Eduardo Escobar (Ari - 3B,SS)",
                 "DJ LeMahieu (NYY - 2B)", "A.J. Minter (Atl - RP)",
                 "Ke'Bryan Hayes (Pit - 3B)", "Hunter Strickland (Sea - RP)", "Brad Boxberger (KC - RP)",
                 "Brandon Lowe (TB - 2B,OF)", "Ryan McMahon (Col - 1B,2B,3B)",
                 "Brandon Morrow (ChC - RP)", "Francisco Cervelli (Pit - C)",
                 "MacKenzie Gore (SD - SP)", "Hunter Greene (Cin - SP)",
                 "Matt Strahm (SD - SP,RP)", "César Hernández (Phi - 2B)",
                 "Jon Gray (Col - SP)", "Julio Rodriguez (Sea - OF)", "Brendan McKay (TB - 1B)",
                 "Brandon Nimmo (NYM - OF)", "Diego Castillo (TB - SP,RP)",
                 "Casey Mize (Det - SP)", "Jimmy Nelson (Mil - SP)",
                 "Christin Stewart (Det - OF)", "Amed Rosario (NYM - SS)",
                 "Sixto Sánchez (Phi - SP)", "Ryan Brasier (Bos - RP)", "Jazz Chisholm (Ari - SS)",
                 "Michael Kopech (CWS - SP)", "Jonathan India (Cin - 3B)",
                 "Max Kepler (Min - OF)", "Kyle Freeland (Col - SP)",
                 "Ryan Pressly (Hou - RP)", "Joe Jiménez (Det - RP)", "Justin Smoak (Tor - 1B)",
                 "Chris Taylor (LAD - 2B,SS,OF)", "Jordyn Adams (LAA - OF)",
                 "Ryan Braun (Mil - 1B,OF)"),
        Cost = c("$76", "$120", "$78", "$59", "$61", "$26", "$41", "$21",
                 "$37", "$22", "$25", "$15", "$18", "$29", "$21", "$15", "$41",
                 "$12", "$22", "$30", "$9", "$30", "$22", "$14", "$25", "$18",
                 "$17", "$19", "$31", "$13", "$16", "$3", "$10", "$2", "$2",
                 "$21", "$4", "$8", "$12", "$9", "$5", "$16", "$6", "$10", "$5",
                 "$14", "$6", "$3", "$7", "$2", "$20", "$22", "$6", "$3", "$2", "$10",
                 "$10", "$5", "$4", "$8", "$6", "$5", "$5", "$1", "$2", "$3",
                 "$1", "$2", "$2", "$2", "$2", "$1", "$3", "$1", "$1", "$6", "$1",
                 "$1", "$3", "$1", "$6", "$1", "$4", "$2", "$2", "$1", "$3",
                 "$2", "$1", "$2", "$1", "$4", "$1", "$2", "$1", "$1", "$1", "$2",
                 "$1", "$6", "$1", "$1", "$1", "$1", "$1", "$1", "$2", "$1", "$1",
                 "$1", "$1", "$2", "$1", "$1", "$1", "$2", "$1", "$1", "$1",
                 "$1", "$1", "$1", "$1"),
        Team = c("Rhys Lightning", "Gone Fishin'", "Javy for MVP",
                 "Snelly Farts", "Darth Hader", "TBD", "All in Dee Phamily",
                 "Duke Newcomb", "All in Dee Phamily", "Rhys Lightning", "TBD",
                 "Walker f’ing Bu...", "Snelly Farts", "Duke Newcomb",
                 "Duke Newcomb", "Duke Newcomb", "TBD", "Duke Newcomb", "Walker f’ing Bu...",
                 "Boba Chette", "Javy for MVP", "Rhys Lightning", "Snelly Farts",
                 "TBD", "Walker f’ing Bu...", "All in Dee Phamily",
                 "Darth Hader", "All in Dee Phamily", "Rhys Lightning", "Walker f’ing Bu...",
                 "TBD", "Walker f’ing Bu...", "All in Dee Phamily",
                 "Gone Fishin'", "Rhys Lightning", "Boba Chette", "Duke Newcomb",
                 "Duke Newcomb", "All in Dee Phamily", "Walker f’ing Bu...", "Duke Newcomb",
                 "Walker f’ing Bu...", "Duke Newcomb", "Gone Fishin'",
                 "Boba Chette", "Rhys Lightning", "TBD", "Sendzel me a Miracle",
                 "Snelly Farts", "Javy for MVP", "All in Dee Phamily", "Snelly Farts",
                 "Rhys Lightning", "Walker f’ing Bu...", "Rhys Lightning",
                 "Rhys Lightning", "Walker f’ing Bu...", "Rhys Lightning",
                 "Rhys Lightning", "Sendzel me a Miracle", "Rhys Lightning", "Boba Chette",
                 "All in Dee Phamily", "Stealing First", "Sendzel me a Miracle",
                 "Duke Newcomb", "TBD", "Stealing First", "Javy for MVP",
                 "Sendzel me a Miracle", "Darth Hader", "Boba Chette", "TBD",
                 "Stealing First", "Snelly Farts", "Stealing First", "TBD", "Gone Fishin'",
                 "Sendzel me a Miracle", "Javy for MVP", "Stealing First",
                 "Boba Chette", "Stealing First", "Sendzel me a Miracle",
                 "Walker f’ing Bu...", "TBD", "Gone Fishin'", "Javy for MVP", "Javy for MVP",
                 "Snelly Farts", "Boba Chette", "Snelly Farts", "Stealing First",
                 "Snelly Farts", "Gone Fishin'", "Sendzel me a Miracle",
                 "Javy for MVP", "Boba Chette", "Boba Chette", "Darth Hader",
                 "Stealing First", "Snelly Farts", "Gone Fishin'", "Sendzel me a Miracle",
                 "Javy for MVP", "Darth Hader", "All in Dee Phamily",
                 "All in Dee Phamily", "Stealing First", "Gone Fishin'",
                 "Sendzel me a Miracle", "Darth Hader", "Darth Hader", "Boba Chette", "Stealing First",
                 "Sendzel me a Miracle", "Javy for MVP", "Darth Hader",
                 "Javy for MVP", "Darth Hader", "Javy for MVP", "Darth Hader",
                 "Javy for MVP")
)

postKeeperDraft %<>% mutate(Name=sub(" *\\(.*", "",Player))
postKeeperDraft$Name <- iconv(postKeeperDraft$Name, from='UTF-8', to='ASCII//TRANSLIT') %>% str_replace_all(pattern="'",replacement="")

mgrTeam <- data.frame(stringsAsFactors = F,
                      manager = c("JJ", "Tony", "Tam", "Matt", "Mike", "Nate", "John", "David", "Brian", "Diego", "Rob", "Cody"),
                      Team = c("All in Dee Phamily", "Boba Chette", "Darth Hader", "Duke Newcomb", "Gone Fishin'", "Javy for MVP", 
                      "Rhys Lightning", "Sendzel me a Miracle", "Snelly Farts", "Stealing First", "TBD", "Walker f’ing Bu..."))

postKeeperDraft %<>% left_join(mgrTeam) %>% select(Name, manager, Cost) %>% 
  mutate(Cost = str_extract(Cost, "\\d+") %>% as.numeric)

initialTeams <- rbind(actualKeepers %>% filter(Keeper==T) %>% select(Name, manager, Cost),
      postKeeperDraft)





hitterCount <- compositeH %>% 
  inner_join(initialTeams) %>% 
  group_by(manager) %>% 
  summarise(hitters=n())
# everybody has9,10,11 except Diego & David - so add David's guys and then just normalize to 10 in the counting stats
add2David <- compositeH[1:4,]
add2David$Name <- c("Mallex Smith", "Mike Zunino", "Max Kepler", "Nick Senzel")
add2David[,2:10] <- NA              
xtrasbrates <- sbRate %>% filter(Name %in% add2David$Name) # just going to type these in below
add2David[1,] <- c("Mallex Smith", "OF", 69, 4, 42, 39, .334, .369, .741, nsb(39,.741))
add2David[2,] <- c("Mike Zunino", "C", 46, 21, 56, 1, .281, .420, .5, nsb(1, 0.5))
add2David[3,] <- c("Max Kepler", "OF", 72, 20, 67, 6, .330, .443, .7, nsb(6,.7))
add2David[4,] <- c("Nick Senzel", "3B", 53, 13, 53, 11, .336, .445, .688, nsb(11, .688))
compositeH <- rbind(compositeH, add2David)

hitterCount <- compositeH %>% 
  inner_join(initialTeams) %>% 
  group_by(manager) %>% 
  summarise(hitters=n())

compositeH %>% 
  inner_join(initialTeams) %>% 
  group_by(manager) %>%  
  summarise(R=sum(as.numeric(R)), 
            HR = sum(as.numeric(HR)), 
            RBI=sum(as.numeric(RBI)), 
            OBP=mean(as.numeric(OBP)), 
            SLG=mean(as.numeric(SLG)), 
            NSB=sum(as.numeric(NSB), na.rm = T)) -> initialHstats
# Need to deal with playing time - Diego has a ton of hitters, 
# David has a bunch of hitters not even in CompositeH - Mallex, Robles, Kepler, Moncada, Zunino


compositeP %>% 
  inner_join(initialTeams) %>% 
  group_by(manager) %>%  
  mutate(IP=as.numeric(IP), ERA=as.numeric(ERA), WHIP=as.numeric(WHIP), K9=as.numeric(K9), QS=as.numeric(QS), SV=as.numeric(SV)) %>%
  summarise(IPt=sum(IP), 
            ERA = sum(ERA*IP)/sum(IP), 
            WHIP=sum(WHIP*IP)/sum(IP), 
            K9=sum(K9*IP)/sum(IP), 
            QS=sum(QS), 
            SV=sum(SV)) %>% 
  rename(IP=IPt)-> initialPstats




initialHstats %<>% 
  left_join(hitterCount) %>%
  mutate(R=R*10/hitters,
         HR=HR*10/hitters, 
         RBI=RBI*10/hitters, 
         NSB=NSB*10/hitters) %>% 
  select(-hitters)



initialStandings <- initialHstats %>% left_join(initialPstats) 

initialScores <- cbind(initialStandings %>% select(manager),
                initialStandings %>% select(R, HR, RBI, OBP, SLG, NSB, IP, SV, K9, QS) %>% mutate_all(funs(rank)),
                initialStandings %>% select(ERA, WHIP) %>% mutate_all(funs(rank(desc(.))))
)

initialScores$Total <- initialScores %>% select (-manager) %>% rowSums

initialScores %<>% arrange(desc(Total))



initialStandings %>% mutate(R = round(R), HR=round(HR), RBI=round(RBI), OBP=round(OBP,4), SLG=round(SLG,4), NSB=round(NSB),
                            IP=round(IP), ERA=round(ERA,3), WHIP=round(WHIP,3), K9=round(K9,2), QS=round(QS), SV=round(SV)) %>%
  View


############################################################
############################################################
############################################################
############################################################          trade Eval
############################################################
############################################################
############################################################
#Iglesias and JBJ for Bieber and Cain
tradeEval <- initialTeams
tradeEval[tradeEval$Name=="Raisel Iglesias", "manager"] <- "Rob"
tradeEval[tradeEval$Name=="Jackie Bradley", "manager"] <- "Rob"
tradeEval[tradeEval$Name=="Shane Bieber", "manager"] <- "Cody"
tradeEval[tradeEval$Name=="Lorenzo Cain", "manager"] <- "Cody"
#tradeEval <- rbind(tradeEval, c("Sonny Gray", "Cody", 0)) 
#got sonny gray from waivers

#DO this all again

hitterCountTE <- compositeH %>% 
  inner_join(tradeEval) %>% 
  group_by(manager) %>% 
  summarise(hitters=n())

compositeH %>% 
  inner_join(tradeEval) %>% 
  group_by(manager) %>%  
  summarise(R=sum(as.numeric(R)), 
            HR = sum(as.numeric(HR)), 
            RBI=sum(as.numeric(RBI)), 
            OBP=mean(as.numeric(OBP)), 
            SLG=mean(as.numeric(SLG)), 
            NSB=sum(as.numeric(NSB), na.rm = T)) -> teHstats


compositeP %>% 
  inner_join(tradeEval) %>% 
  group_by(manager) %>%  
  mutate(IP=as.numeric(IP), ERA=as.numeric(ERA), WHIP=as.numeric(WHIP), K9=as.numeric(K9), QS=as.numeric(QS), SV=as.numeric(SV)) %>%
  summarise(IPt=sum(IP), 
            ERA = sum(ERA*IP)/sum(IP), 
            WHIP=sum(WHIP*IP)/sum(IP), 
            K9=sum(K9*IP)/sum(IP), 
            QS=sum(QS), 
            SV=sum(SV)) %>% 
  rename(IP=IPt)-> tePstats




teHstats %<>% 
  left_join(hitterCountTE) %>%
  mutate(R=R*10/hitters,
         HR=HR*10/hitters, 
         RBI=RBI*10/hitters, 
         NSB=NSB*10/hitters) %>% 
  select(-hitters)



teStandings <- teHstats %>% left_join(tePstats) 

teScores <- cbind(teStandings %>% select(manager),
                       teStandings %>% select(R, HR, RBI, OBP, SLG, NSB, IP, SV, K9, QS) %>% mutate_all(funs(rank)),
                       teStandings %>% select(ERA, WHIP) %>% mutate_all(funs(rank(desc(.))))
)

teScores$Total <- teScores %>% select (-manager) %>% rowSums

teScores %<>% arrange(desc(Total))



teStandings %>% mutate(R = round(R), HR=round(HR), RBI=round(RBI), OBP=round(OBP,4), SLG=round(SLG,4), NSB=round(NSB),
                            IP=round(IP), ERA=round(ERA,3), WHIP=round(WHIP,3), K9=round(K9,2), QS=round(QS), SV=round(SV)) %>%
  View


## brings me to 98, Rob to 71
# I was at 91.5, him at 73
cbind((initialStandings %>% arrange(manager) %>% select(manager)),
(teStandings %>% arrange(manager) %>% select(-1)) - 
  (initialStandings %>% arrange(manager) %>% select(-1)) 
  ) %>% filter(manager=="Cody")

cbind((initialScores%>% arrange(manager) %>% select(manager)),
      (teScores %>% arrange(manager) %>% select(-1)) - 
        (initialScores %>% arrange(manager) %>% select(-1)) 
) %>% filter(manager=="Cody")

# 4 points jump each in OBP and NSB, + 3 QS + 3 IP counterbalances 4 point loss in SV, and maringal losses in HR, RBI, SLG, K9
# This is actually quite complicated evaluation, but a clear win for me, for this year! 
# The categories are so tight, it's hard to really rely on these projection-driven results too much.
# But indeed at the end of the year the categories WERE that tight.