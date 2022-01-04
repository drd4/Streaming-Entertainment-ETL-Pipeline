
# Load Cleaning Libraries
library(data.table)
library(lubridate)
library(tidyverse)

# Read in Data
dat <- read.csv("C:/Users/Owner/Downloads/output.csv")

# Drop ID Column
dat = dat[,-1]

# Replace Blanks with NA
dat$director[dat$director==""] <- NA
dat$cast[dat$cast==""] <- NA
dat$country[dat$country==""] <- NA
dat$rating[dat$rating==""] <- NA
dat$date_added[dat$date_added==""] <- NA
dat$listed_in[dat$listed_in==""] <- NA
dat$revenue[dat$revenue==""] <- NA
dat$budget[dat$budget==""] <- NA
dat$duration[dat$duration==""] <- NA
dat$vote_average[dat$vote_average==""] <- NA
dat$episode_run_time[dat$episode_run_time==""] <- NA
dat$number_of_episodes[dat$number_of_episodes==""] <- NA

# Create platform_id 
dat$platform_id[dat$platform=="Netflix"] <- 1
dat$platform_id[dat$platform=="Hulu"] <- 2
dat$platform_id[dat$platform == "Disney"] <- 3
dat$platform_id[dat$platform == "Amazon"] <- 4

# Clean Special Characters
dat$title <- iconv(dat$title,to="ASCII//TRANSLIT")
dat$title[dat$title == "A FRIENDSHIP"] <- "A Friendship"
dat$title[dat$title == "A?on Flux"] <- "Aeon Flux"

# Clean dates
mdy <- mdy(dat$date_added) 
dmy <- dmy(dat$date_added) 
mdy[is.na(mdy)] <- dmy[is.na(mdy)] # some dates are ambiguous, here we give 
dat$date_added <- mdy  
dat$date_added <- as.character(dat$date_added)
dat$release_year <- as.character(dat$release_year)

# Clean episode_run_time
dat$episode_run_time <- gsub("[[:punct:]]", "", dat$episode_run_time)
dat$episode_run_time <- as.numeric(dat$episode_run_time)
episodes <- dat %>% tidyr::separate(episode_run_time, sep=" ", into=c("ert1","ert2","ert3"), remove = TRUE)
episodes$ert1 <- as.numeric(episodes$ert1)
episodes$ert2 <- as.numeric(episodes$ert2)
episodes$ert3 <- as.numeric(episodes$ert3)
dat$episode_run_time <- rowMeans(episodes[,c("ert1","ert2","ert3")],na.rm=TRUE)
dat$episode_run_time[is.nan(dat$episode_run_time)]<-NA

# Remove Duplicate Rows
dat <- unique(dat)
duplicate_titles <- dat[duplicated(dat$title),]

# Remove 67 movies and shows with Duplicated Data
platform_repeated <- dat %>% select(title, type, platform_id)
dupes <- platform_repeated[duplicated(platform_repeated),]
issues <- unique(dupes$title)
dat <- subset(dat, !(title %in% issues))

# Clean type variable
dat$type[grepl("Season", dat$duration)] <- "TV Show"

# Movies and Shows with Same Title
tv_shows <- dat %>% filter(type== "TV Show")
tv_titles <- tv_shows$title
movies <- dat %>% filter(type== "Movie")
movie_titles <- movies$title
same_titles <- subset(movies, title %in% tv_titles)
same_titles <- same_titles$title
dat$title <- ifelse(dat$type == "Movie"&dat$title %in% same_titles, paste(dat$title, " - Movie"), paste(dat$title))

# Clean Ratings
dat$rating[dat$rating == "13+"] <- "PG-13"
dat$rating[dat$rating == "All"] <- "G"
dat$rating[dat$rating == "ALL"] <- "G"
dat$rating[dat$rating == "NOT RATED"] <- "NR"
dat$rating[dat$rating == "NOT_RATE"] <- "NR"
dat$rating[dat$rating == "ALL_AGES"] <- "G"
dat$rating[dat$rating == "AGES_16_"] <- "R"
dat$rating[dat$rating == "16+"] <- "TV-14"
dat$rating[dat$rating == "AGES_18_"] <- "R"
dat$rating[dat$rating == "18+"] <- "R"
dat$rating[dat$rating == "UNRATED"] <- "NR"
dat$rating[dat$rating == "UR"] <- "NR"
dat$rating[dat$rating == "TV-NR"] <- "NR"
dat$rating[dat$rating == "16"] <- "TV-14"
dat$rating[dat$rating == "TV-Y7-FV"] <- "TV-Y7"
dat$rating[dat$rating == "7+"]<- "TV-Y7"

# Clean Budget and Revenue
dat$budget[dat$budget == "0"]<- NA
dat$revenue[dat$revenue == "0"]<- NA
dat$budget[dat$budget < 10]<- NA
dat$revenue[dat$revenue < 10]<- NA
format(dat$budget, scientific = F)
format(dat$revenue, scientific = F)

## CREATE MOVIE TABLE
movie <- dat %>% filter(type == "Movie") %>%
  select(title, type, release_year, rating, duration, budget, revenue, vote_average) %>%
  unique()
movie <- movie[!duplicated(movie$title),]
movie$movie_id <- 1:nrow(movie)
movie$movie_id <- as.character(movie$movie_id)
movie$budget <- as.character(movie$budget)
movie$revenue <- as.character(movie$revenue)
movie$vote_average <- as.character(movie$vote_average)
col_names <- c("movie_id","type","title","release_year","rating","duration","budget", "revenue","vote_average")
movie <- movie[,col_names]

## CREATE TV TABLE
tv <- dat %>% filter(type == "TV Show") %>%
  select(type, title, release_year, rating, episode_run_time, number_of_episodes, duration, vote_average) %>%
  unique()
tv <- tv[!duplicated(tv$title),]
tv$tv_id <- 1:nrow(tv)
tv$tv_id <- as.character(tv$tv_id)
tv$episode_run_time <- as.character(tv$episode_run_time)
tv$number_of_episodes <- as.character(tv$number_of_episodes)
tv$vote_average <- as.character(tv$vote_average)
tv$duration <- gsub("Seasons", "", tv$duration)
tv$duration <- gsub("Season", "", tv$duration)
tv$duration <- str_trim(tv$duration, side = "both")
col_names <- c("tv_id","type","title","release_year","rating","episode_run_time",
               "number_of_episodes","duration","vote_average")
tv <- tv[,col_names]
names(tv)[names(tv)=='episode_run_time']<-'average_episode_duration'
names(tv)[names(tv)=='number_of_episodes']<-'episodes'
names(tv)[names(tv)=='duration']<-'seasons'

## CREATE PLATFORM TABLE
platform = c("Netflix", "Hulu", "Disney", "Amazon")
platform_id = c(1,2,3,4)
platform_id = as.character(platform_id)
platform <- data.frame(platform_id, platform)

## CREATE MOVIE PLATFORM TABLE
movie_platform <- merge(movie, dat, by = "title") %>% select(movie_id, platform_id, date_added, description)
movie_platform$platform_id <- as.character(movie_platform$platform_id)

## CREATE TV PLATFORM TABLE
tv_platform <- merge(tv, dat, by = "title") %>% select(tv_id, platform_id, date_added, description)
tv_platform$platform_id <- as.character(tv_platform$platform_id)

# Convert movie table to 1NF, separate combined column values into multiple rows
dat_movie <- merge(dat, movie, by="title", all = TRUE) %>% select(title, movie_id, platform_id, cast, director, country, listed_in)
dat2 <- merge(dat_movie, tv, by="title", all=TRUE) %>% select(title, tv_id, movie_id, platform_id, cast, director, country, listed_in)

dat2 <- dat2 %>%
  separate_rows(director, sep = ",") %>%
  separate_rows(cast, sep=",") %>%
  separate_rows(country, sep = ",") %>%
  separate_rows(listed_in, sep=",")

# Trim leading/trailing spaces
dat2$director <- str_trim(dat2$director, side = "both")
dat2$cast <- str_trim(dat2$cast, side = "both")
dat2$country <- str_trim(dat2$country, side = "both")
dat2$listed_in <- str_trim(dat2$listed_in, side = "both")

dat2$director[dat2$director==""] <- NA
dat2$cast[dat2$cast==""] <- NA
dat2$country[dat2$country==""] <- NA
dat2$listed_in[dat2$listed_in==""] <- NA

names(dat2)[names(dat2)=='cast']<-'actor'
names(dat2)[names(dat2)=='listed_in']<-'genre'

## CREATE DIRECTOR TABLE
director <- dat2 %>% select(director) %>% unique() %>% na.omit
director$director_id <- 1:nrow(director)
director$director_id <- as.character(director$director_id)

## CREATE MOVIE DIRECTOR TABLE
movie_director <- merge(dat2, director, by = "director") %>% select(movie_id, director_id) %>% unique() %>% na.omit

## CREATE TV DIRECTOR TABLE
tv_director <- merge(dat2, director, by = "director") %>% select(tv_id, director_id) %>% unique() %>% na.omit

## CREATE ACTOR TABLE
actor <- dat2 %>% select(actor) %>% unique() %>% na.omit
actor$actor_id <- 1:nrow(actor)
actor$actor_id <- as.character(actor$actor_id)

## CREATE MOVIE CAST TABLE
movie_cast <- merge(dat2, actor, by ="actor") %>% select(movie_id, actor_id) %>% unique()  %>% na.omit

## CREATE TV CAST TABLE
tv_cast <- merge(dat2, actor, by = "actor") %>% select(tv_id, actor_id) %>% unique() %>% na.omit

## CREATE COUNTRY TABLE
country <- dat2 %>% select(country) %>% unique() %>% na.omit
country$country_id <- 1:nrow(country)
country$country_id <- as.character(country$country_id)

## CREATE MOVIE COUNTRY TABLE
movie_country <- merge(dat2, country, by ="country") %>% select(movie_id, country_id) %>% unique()  %>% na.omit

## CREATE TV COUNTRY TABLE
tv_country <- merge(dat2, country, by ="country") %>% select(tv_id, country_id) %>% unique()  %>% na.omit

## CREATE GENRE TABLE
genre <- dat2 %>% select(genre) %>% unique() %>% na.omit
genre$genre_id <- 1:nrow(genre)
genre$genre_id <- as.character(genre$genre_id)

## CREATE MOVIE LISTED_IN TABLE
movie_listed_in <- merge(dat2, genre, by ="genre") %>% select(movie_id, genre_id) %>% unique()  %>% na.omit

## CREATE TV LISTED IN TABLE
tv_listed_in <- merge(dat2, genre, by ="genre") %>% select(tv_id, genre_id) %>% unique()  %>% na.omit
