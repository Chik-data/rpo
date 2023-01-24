## code to prepare `BB_dataset` dataset goes here

library(tidyverse); library(readr); library(clock); library(lubridate)
#Datas importation from 'data-raw/archive' repository:

##quelques nettoyages de certaines valeurs:
epi1=read.csv("data-raw/archive/breaking_bad_episodes.csv", header = TRUE, sep = "," ,dec = ".")
epi1$original_air_date=as.Date(epi1[, 7], format = "%Y-%m-%d")
epi1
epi2=read.csv("data-raw/archive/episodes.csv", header = TRUE, sep = "," )
epi2
death=read.csv("data-raw/archive/deaths.csv", header = TRUE, sep = "," )
vote=read.csv("data-raw/archive/breaking_bad_imdb.csv", header = TRUE, sep = "," )
vote
char=read.csv("data-raw/archive/characters.csv", header = TRUE, sep = "," )
char[3,4] <-rbind("Book Keeper, Car Wash Manager, Taxi Dispatcher")
char[6,4] <-rbind("Radiology technician") ; char$status= ifelse(char$status=="Alive", 1, 0)#1=Alive
char$birthday =as.Date(char[, 3], format = "%m-%d-%Y")
char
epi1_cols= names(epi1)
epi1_cols[!(epi1_cols %in% names(epi2))] ; epi1_cols[epi1_cols %in% names(death)]; epi1_cols[epi1_cols %in% names(vote)]; epi1_cols[epi1_cols %in% names(char)]



death$responsible[5]="Hank Schrader"
death$responsible[15]="The Cousins"
death$responsible[16]="Marco Salamanca"
death$responsible[17]="Marco Salamanca"
death$responsible[18]="Hank Schrader"
death$responsible[19]="Mike Erhmantraut"
char$name[45]="Tomas Cantillo"
death$last_words[35]="Salud!"
death$last_words[33]="Tu?"
epi1$title[25]="Mas"; epi1$title[35]="Thirty Eight Snub" ; epi1$title[21]="No Mas"
epi2$title[25]="Mas"; epi2$title[35]="Thirty Eight Snub" ; epi2$title[21]="No Mas"
vote$title[25]="Mas"; vote$title[35]="Thirty Eight Snub" ; vote$title[21]="No Mas"
tesfin =epi1 %>% count(season, directed_by, wt = us_viewers)

usethis::use_data(epi1, overwrite = TRUE)
usethis::use_data(epi2, overwrite = TRUE)
usethis::use_data(death, overwrite = TRUE)
usethis::use_data(vote, overwrite = TRUE)
usethis::use_data(char, overwrite = TRUE)
dim(epi1)

#Duration of each season in weeks:
tmp=data.frame(cbind(rep(0, 5)),0 )
names(tmp)=c("season", "duration_season_in_weeks")
for (i in unique(epi1$season))
  tmp[i,]=cbind( epi1 %>% filter(season==i) %>% group_by(season) %>% summarise(duration_season=  difftime(original_air_date[length(original_air_date)], original_air_date[1], units="weeks") )
  )
tmp


# le nombre de personnages morts dans l’ensemble de la série:
(nb_deaths_total_serie=sum(death$number_of_deaths))
#nb de morts par saison:
(season.d=death %>% group_by(season) %>% summarise( nb_deaths_per_season=sum(number_of_deaths)) %>% arrange(nb_deaths_per_season))
#les epiosdes les + meutriers par saison:
(episodes.meurtrier.by.season=death %>% group_by(season, episode) %>% summarise(nb_deaths_per_epiANDseason = sum(number_of_deaths)) %>% arrange(desc(nb_deaths_per_epiANDseason)))
#les 5 plus grands meurtriers de la série:

death %>% group_by(responsible, season) %>% summarise(nb_deaths_per_responsible=sum(number_of_deaths)) %>% arrange(desc(nb_deaths_per_responsible))

death %>% group_by(responsible, season) %>% summarise(nb_deaths_per_responsible=sum(number_of_deaths)) %>% filter(season==1)

#nb de morts par saison et responsable:
(tabi=death %>% group_by(responsible, season) %>% summarise(nb_deaths_per_responsible=sum(number_of_deaths)) %>% arrange(desc(nb_deaths_per_responsible)))

#nb de morts par saison et certains meurtriers:
(deaths.by.resp.by.season= filter(tabi, responsible %in% c("Walter White", "Gustavo Fring", "The Cousins", "Juarez Cartel", "Mike Ehrmantraut", "Jesse Pinkman", "Hank Schrader", "Hector Salamanca", "Tuco Salamanca"))
)

#nb de vus aux usa/episodes:
epi1 %>% select(episode_num_in_season, season , us_viewers, written_by) %>% arrange(desc(us_viewers))
# /saison
tmp.vus=epi1 %>% group_by(season) %>% summarise(nb_us_viewers_per_season= sum(us_viewers)) %>% arrange(nb_us_viewers_per_season)
#nb vus moy. (aux us) par sem pour chaque saison:
merge(tmp, tmp.vus) %>% mutate(nb_vu_per_sem= nb_us_viewers_per_season/duration_season_in_weeks)
# / director:
epi1 %>% group_by(written_by, episode_num_in_season, season) %>% summarise(nb_us_view_per_writter= sum(us_viewers)) #%>% arrange(desc(nb_us_view_per_writter))
# /writter:
epi1 %>% group_by(directed_by, episode_num_in_season, season) %>% summarise(nb_us_view_per_dir=sum(us_viewers))
#nb d'episodes par ecrivaine(n):
epi1 %>% group_by(written_by, season) %>% summarise(nb_epi_per_writter_seas=n())
#nb d'episodes par directrice(r):
epi1 %>% group_by(directed_by, season) %>% summarise(nb_epi_per_direrctor_seas=n())

# nombre de morts cumulé et temps (sem.) passé depuis la 1ere episode ??:
(nb.cum.death = death %>% group_by(season) %>% mutate(nb_cum_death_seas=cumsum(number_of_deaths)))

# nb de morts cumulé par saison et temps passé depuis la 1ere saison episode 1:
jointure1=list(tmp, tmp.vus, season.d) %>% reduce(full_join, by='season') %>% mutate(nb_cum_death_season=cumsum(nb_deaths_per_season), t.cum.in.weeks.from.s1= cumsum(duration_season_in_weeks))
