library(magrittr)
library(tidyverse)
library(googlesheets)

source(here::here("02 Rank.R"))



#should get price from keeper sheet / draft

keepers <- gs_title("Tam Dynasty Keeper Log")
keepersDF <- gs_read(ss=keepers, ws="Keeper Log") %>% data.frame
keepersDF %<>% rename(Name=Player)
draft2018 <- read_csv(here::here("2018TamDynastyDraft.csv"))[,1:2]
colnames(draft2018) <- c("Name", "Price2018")
#yahoo uses accents, not matching
draft2018$Name <- iconv(draft2018$Name, from='UTF-8', to='ASCII//TRANSLIT') %>% str_replace_all(pattern="'",replacement="")
#everyone's price this year is their draft price or $5 if not drafted except those in keepersDF where expired or extended

expired <- keepersDF %>% filter(X2019=="Expired") %>% pull(Name) 
extended <- keepersDF %>% filter(!is.na(Extension.Years)) %>% select(Name, X2019) %>% filter(X2019 !="Expired")

keeperCost2019 <- overallCz %>% left_join(draft2018) %>% replace_na(list(Price2018= 5)) 
keeperCost2019[which(keeperCost2019$Name %in% expired),"Price2018"] <- 260
keeperCost2019 %<>% left_join(extended) %>% mutate(KeeperPrice = ifelse(is.na(X2019), Price2018, X2019)) 
rbind(keeperCost2019, c("Yoenis Cespedes", -10, 20,25, 13)) -> keeperCost2019 #assumes I will drop him


### Pricing function of rank
price2019 <- function(rank) {( 88-(16.2 * log(rank)))}
price2018 <- function(position) {68 - 12*log(position)}

priceAList <- function(lst) {
  x<-data.frame(stringsAsFactors = FALSE, Name=lst)
  x$rankz <- match( x$Name,overallCz$Name)
  x$pricez <- price2019(x$rankz)
  return(x)
}

# eval teams and guess keepers


## Guess keeper function

guessKeepers <- function(manager, names, N=12){
  Players <- data.frame(stringsAsFactors = F,
                          manager=manager, 
                          Name=names,
                          Keeper = F,
                          Cost=0
                          
  )
  Players$Cost = Players %>% left_join(keeperCost2019) %>% pull(KeeperPrice) %>% as.numeric
  Players$Extended <- Players$Name %in% extended$Name
  
  Players$Rank <- priceAList(Players$Name)$rankz
  Players$Value <- priceAList(Players$Name)$pricez
  Players %<>% mutate(profit = Value - Cost) %>% arrange(desc(Extended), desc(profit)) 
  Players$Keeper[1:N] <- T
  return(Players)
}



### CODY

CodyNames <- c('Yasmani Grandal', 'Jesus Aguilar', 'Yasiel Puig', 'A.J. Pollock', 'Anthony Rendon', 'Aaron Judge',
               'Noah Syndergaard', 'Corey Kluber', 'Raisel Iglesias', 'Jose Leclerc', 'Mike Foltynewicz', 'Walker Buehler',
               'Aaron Hicks', 'Jean Segura', 'Jose Peraza', 'David Price', 'Roberto Osuna', 'Hyun-Jin Ryu', 'Ryan Pressly', 'Yoenis Cespedes')
CodyPlayers <- guessKeepers("Cody", CodyNames)

#Now go in and tweak for extension, personal preference,...
CodyPlayers[which(CodyPlayers$Name %in% c("Raisel Iglesias", "Jesus Aguilar")),"Keeper"] <- T
CodyPlayers[which(CodyPlayers$Name %in% c("Aaron Hicks")),"Keeper"] <- F
# will have 13, but drop Cespedes

keeperGuess <- CodyPlayers
rm(CodyPlayers, CodyNames)

### JJ - All in Dee Phamily
JJnames <- c("Cody Bellinger", "Xander Bogaerts", "Eugenio Suarez", "Eddie Rosario",  "Matt Chapman", "Mallex Smith", 
             "Byron Buxton", "Tommy Pham", "Jack Flaherty", "Robbie Ray", "Brad Hand",  "Alex Reyes", "Justin Upton",
             "Scooter Gennett", "Gleyber Torres", "Wilson Ramos", "Miles Mikolas", "Trevor May", "Mike Soroka")
JJPlayers <- guessKeepers("JJ", JJnames)

keeperGuess <- rbind(keeperGuess, JJPlayers)
rm(JJPlayers, JJnames)


## Tam - cinco de Arenado
TamNames <- c("Mike Trout", "Nolan Arenado", "Ronald Acuna", "Alex Bregman", "Bryce Harper", "Vladimir Guerrero Jr.", 
              "Gary Sanchez", "Jonathan Villar", "Eloy Jimenez", "David Peralta", "Yoan Moncada", "Amed Rosario", "Matt Olson",
              "Max Scherzer", "Chris Sale", "Clayton Kershaw", "Trevor Bauer", "Aroldis Chapman", "German Marquez", "Sean Doolittle",
              "Josh Hader", "Jimmy Nelson", "Drew Steckenrider")
TamPlayers <- guessKeepers("Tam", TamNames)
TamPlayers[which(TamPlayers$Name %in% c("Aroldis Chapman", "Eloy Jimenez")),"Keeper"] <- T
TamPlayers[which(TamPlayers$Name %in% c("Jonathan Villar", "David Peralta")),"Keeper"] <- F

keeperGuess <- rbind(keeperGuess, TamPlayers)
rm(TamPlayers, TamNames)


### Matt - Duke Newcomb

MattNames <- c("Ozzie Albies", "Joey Votto", "Michael Brantley", "Andrew McCutchen", "Brian Dozier", "Nomar Mazara", "Luke Voit",
               "Luis Castillo", "Kyle Hendricks", "Kirby Yates", "J.A. Happ", "Andrew Miller", "Sean Newcomb", "Danny Salazar")
MattPlayers <- guessKeepers("Matt", MattNames)
#MattPlayers[which(Mattlayers$Name %in% c("Aroldis Chapman", "Eloy Jimenez")),"Keeper"] <- T
MattPlayers[which(MattPlayers$Name %in% c("Sean Newcomb", "Andrew McCutchen", "Nomar Mazara")),"Keeper"] <- F

keeperGuess <- rbind(keeperGuess, MattPlayers)
rm(MattPlayers, MattNames)


## Nate - Javy for MVP

NateNames <- c("Javier Baez", "Daniel Murphy", "Robinson Cano", "Max Muncy", "Mike Moustakas", "Salvador Perez", "Carlos Santana", 
               "Justin Verlander", "Aaron Nola", "Kenley Jansen", "Sonny Gray", "Shane Greene", "Avisail Garcia", "Eduardo Escobar", "Shin-soo Choo")
NatePlayers <- guessKeepers("Nate", NateNames)
#NatePlayers[which(Natelayers$Name %in% c("Aroldis Chapman", "Eloy Jimenez")),"Keeper"] <- T
NatePlayers[which(NatePlayers$Name %in% c("Eduardo Escobar", "Sonny Gray", "Carlos Santana", "Robinson Cano")),"Keeper"] <- F

keeperGuess <- rbind(keeperGuess, NatePlayers)
rm(NatePlayers, NateNames)


# Tony - Melancon in the Dark 

TonyNames <- c("Francisco Lindor", "Jose Altuve", "Kris Bryant", "Jose Abreu", "Matt Carpenter", "Nelson Cruz", "Adalberto Mondesi", "J.T. Realmuto", "Buster Posey",
               "Jacob deGrom", "James Paxton", "Blake Treinen", "Craig Kimbrel", "Ken Giles", "Nick Pivetta", "Kyle Freeland", "Dellin Betances")
TonyPlayers <- guessKeepers("Tony", TonyNames)
TonyPlayers[which(TonyPlayers$Name %in% c("Matt Carpenter", "Craig Kimbrel")),"Keeper"] <- T  #Craig Kimbrel @ 36?
TonyPlayers[which(TonyPlayers$Name %in% c("Ken Giles", "Nick Pivetta")),"Keeper"] <- F  

keeperGuess <- rbind(keeperGuess, TonyPlayers)
rm(TonyPlayers, TonyNames)

# John - Rhys's Pieces

JohnNames <- c("Trevor Story", "Rhys Hoskins", "Marcell Ozuna", "Mitch Haniger", "Rougned Odor", "Harrison Bader", "Jose Martinez", "Jorge Soler",
               "Carlos Martinez", "Andrew Heaney", "Ross Stripling", "Jordan Hicks", "Seranthony Dominguez", "Roenis Elias", "Michael Kopech")
JohnPlayers <- guessKeepers("John", JohnNames)
#JohnPlayers[which(JohnPlayers$Name %in% c("Matt Carpenter", "Craig Kimbrel")),"Keeper"] <- T  #Craig Kimbrel @ 36?
JohnPlayers[which(JohnPlayers$Name %in% c("Carlos Martinez", "Jorge Soler")),"Keeper"] <- F  

keeperGuess <- rbind(keeperGuess, JohnPlayers)
rm(JohnPlayers, JohnNames)


# Brian - Snelly Farts

BrianNames <- c("Jose Ramirez", "J.D. Martinez", "Trea Turner", "Charlie Blackmon", "Whit Merrifield", "Edwin Encarnacion", "Rafael Devers",
               "Blake Snell", "Chris Archer", "Jose Quintana", "Tyler Glasnow", "Jake Arrieta", "Jon Lester", "Joey Lucchesi", "Arodys Vizcaino", "Will Smith")
BrianPlayers <- guessKeepers("Brian", BrianNames)
#BrianPlayers[which(BrianPlayers$Name %in% c("Matt Carpenter", "Craig Kimbrel")),"Keeper"] <- T  #Craig Kimbrel @ 36?
BrianPlayers[which(BrianPlayers$Name %in% c("Jake Arrieta")),"Keeper"] <- F  

keeperGuess <- rbind(keeperGuess, BrianPlayers)
rm(BrianPlayers, BrianNames)

# Diego - Snelly Farts

DiegoNames <- c("Mookie Betts", "Paul Goldschmidt", "Andrew Benintendi", "Anthony Rizzo", "George Springer", "Justin Turner", "Travis Shaw", "Justin Smoak", "Marcus Semien", "Matt Kemp", "Chris Taylor",
                "Gerrit Cole", "Edwin Diaz", "Wade Davis", "Joshua James", "Mychal Givens", "Chad Green")
DiegoPlayers <- guessKeepers("Diego", DiegoNames)
DiegoPlayers[which(DiegoPlayers$Name %in% c("Justin Turner")),"Keeper"] <- T  
DiegoPlayers[which(DiegoPlayers$Name %in% c("Justin Smoak")),"Keeper"] <- F  

keeperGuess <- rbind(keeperGuess, DiegoPlayers)
rm(DiegoPlayers, DiegoNames)


# Rob - TBD

RobNames <- c("Christian Yelich", "Lorenzo Cain", "Josh Donaldson", "Joey Gallo", "Eric Hosmer", "Yadier Molina", "Jonathan Schoop", "Didi Gregorius", "Nick Markakis",
              "Luis Severino", "Masahiro Tanaka", "Madison Bumgarner", "Rich Hill", "Rick Porcello", "Cody Allen", "Julio Teheran", "Jeurys Familia", "Adam Ottavino", "Jhoulys Chacin") #Can't do Ohtani - he's in draft twice.  Might keep him as H for $1 
RobPlayers <- guessKeepers("Rob", RobNames)
RobPlayers[which(RobPlayers$Name %in% c("Masahiro Tanaka")),"Keeper"] <- T  
RobPlayers[which(RobPlayers$Name %in% c("Adam Ottavino")),"Keeper"] <- F  

keeperGuess <- rbind(keeperGuess, RobPlayers)
rm(RobPlayers, RobNames)

# David - Wil Myers is Missing

DavidNames <- c("Freddie Freeman", "Giancarlo Stanton", "Carlos Correa", "Khris Davis", "Corey Seager", "Wil Myers", "Victor Robles", "Danny Jansen", "Jed Lowrie", "Nick Senzel", "Brandon Nimmo", "Billy Hamilton",
                "Carlos Carrasco", "Zack Greinke", "Patrick Corbin", "Jameson Taillon", "Cole Hamels", "Yu Darvish", "Alex Colome", "Kelvin Herrera") 
DavidPlayers <- guessKeepers("David", DavidNames)
#DavidPlayers[which(DavidPlayers$Name %in% c("Masahiro Tanaka")),"Keeper"] <- T  
#DavidPlayers[which(DavidPlayers$Name %in% c("Adam Ottavino")),"Keeper"] <- F  

keeperGuess <- rbind(keeperGuess, DavidPlayers)
rm(DavidPlayers, DavidNames)

# Mike - Ujar the chosen one

MikeNames <- c("Manny Machado", "Juan Soto", "Starling Marte", "Nick Castellanos", "Miguel Andujar", "Michael Conforto", "David Dahl", "Cesar Hernandez", "Jurickson Profar", "Willson Contreras", "Miguel Sano",
               "Jose Berrios", "Mike Clevinger", "Felipe Vazquez", "Zack Wheeler", "Eduardo Rodriguez", "Corey Knebel", "Steven Matz", "Jose Alvarado") 
MikePlayers <- guessKeepers("Mike", MikeNames)
#MikePlayers[which(MikePlayers$Name %in% c("Masahiro Tanaka")),"Keeper"] <- T  #probably actually keeping Marte, Andujar but whatever
#MikePlayers[which(MikePlayers$Name %in% c("Adam Ottavino")),"Keeper"] <- F  

keeperGuess <- rbind(keeperGuess, MikePlayers)
rm(MikePlayers, MikeNames)







# http://crunchtimebaseball.com/master.csv
playerIds <- read_csv(here::here("playerIds.csv"))

# need to deal with a few NA - either just remove NA or fix Acuna, vlad at least
#Try making a URL like
#https://www.fangraphs.com/auctiontool.aspx?type=bat&proj=fangraphsdc&pos=1,1,1,1,3,1,0,0,0,2,2,2,4,6,0&dollars=260&teams=12&mp=20&msp=5&mrp=5&mb=1&split=&points=c|1,2,4,5,6,16|0,1,2,3,6,10&lg=MLB&rep=0&drp=0&pp=C,SS,2B,3B,OF,1B&players=9218|61,11579|60

predicate <- playerIds %>% 
  select(Name=fg_name, fg_id) %>% 
  right_join(keeperGuess) %>%
  arrange(desc(profit)) %>%
  filter(Keeper==T) %>% 
  select(fg_id, Cost) %>% 
  mutate(pred = paste0(fg_id, "|",Cost)) %>% 
  filter(!is.na(fg_id)) %>% 
  #head(70) %>% 
  pull(pred) %>% paste(collapse=",")

lg<- "https://www.fangraphs.com/auctiontool.aspx?type=bat&proj=fangraphsdc&pos=1,1,1,1,3,1,0,0,0,2,2,2,4,6,0&dollars=260&teams=12&mp=5&msp=3&mrp=5&mb=1&split=&points=c|1,2,4,5,6,16|0,1,2,3,6,10&lg=MLB&rep=0&drp=0&pp=C,SS,2B,3B,OF,1B&players="

paste0(lg,predicate)

#export link for that is a JS thing from that page.
fgAuctionCalc <- rbind(
  read_csv(here::here("FG AuctionCalc Tam19H.csv")) %>% select(Name, POS, Adjusted),
  read_csv(here::here("FG AuctionCalc Tam19P.csv")) %>% select(Name, POS, Adjusted)
) %>% arrange(desc(Adjusted))
  #Note- this is also best srource of position eligibility since it was encoded in the URL to match yahoo rules

#could add &split=60 or something so pitchers get at least 40%
draft2018 %>% left_join(rbind(compositeH %>% select(Name, POS), compositeP %>% select(Name, POS))) %>% group_by(POS) %>% summarise(Price2018 = sum(Price2018)) -> x
#of the ndiscoverable Positions, 48% Pitching, 52% hitting
