
# Append Tables to pgAdmin 

# Load Packages
library(odbc)
require('RPostgreSQL')

# Load the PostgreSQL driver:
drv <- dbDriver('PostgreSQL')

# Connect to the Database
con <- dbConnect(drv, dbname = 'gp',
                 host = 'localhost', port = 5432,
                 user = 'postgres', password = '123')

movie = movie[]
movie_table <- sqlAppendTable(con, "movie", movie, row.names=FALSE)
dbExecute(con, movie_table)

tv = tv[]
tv_table <- sqlAppendTable(con, "tv", tv, row.names= FALSE)
dbExecute(con, tv_table)

director_table <- sqlAppendTable(con, "director", director, row.names= FALSE)
dbExecute(con, director_table)

movie_director_table <- sqlAppendTable(con, "movie_director", movie_director, row.names= FALSE)
dbExecute(con, movie_director_table)

tv_director_table <- sqlAppendTable(con, "tv_director", tv_director, row.names= FALSE)
dbExecute(con, tv_director_table)

actor_table <- sqlAppendTable(con, "actor", actor, row.names= FALSE)
dbExecute(con, actor_table)

movie_cast_table <- sqlAppendTable(con, "movie_cast", movie_cast, row.names= FALSE)
dbExecute(con, movie_cast_table)

tv_cast_table <- sqlAppendTable(con, "tv_cast", tv_cast, row.names= FALSE)
dbExecute(con, tv_cast_table)

country_table <- sqlAppendTable(con, "country", country, row.names= FALSE)
dbExecute(con, country_table)

movie_country_table <- sqlAppendTable(con, "movie_country", movie_country, row.names= FALSE)
dbExecute(con, movie_country_table)

tv_country_table <- sqlAppendTable(con, "tv_country", tv_country, row.names= FALSE)
dbExecute(con, tv_country_table)

genre_table <- sqlAppendTable(con, "genre", genre, row.names= FALSE)
dbExecute(con, genre_table)

movie_listed_in_table <- sqlAppendTable(con, "movie_listed_in", movie_listed_in, row.names= FALSE)
dbExecute(con, movie_listed_in_table)

tv_listed_in_table <- sqlAppendTable(con, "tv_listed_in", tv_listed_in, row.names= FALSE)
dbExecute(con, tv_listed_in_table)

platform_table <- sqlAppendTable(con, "platform", platform, row.names= FALSE)
dbExecute(con, platform_table)

movie_platform_table <- sqlAppendTable(con, "movie_platform", movie_platform, row.names= FALSE)
dbExecute(con, movie_platform_table)

tv_platform_table <- sqlAppendTable(con, "tv_platform", tv_platform, row.names= FALSE)
dbExecute(con, tv_platform_table)

