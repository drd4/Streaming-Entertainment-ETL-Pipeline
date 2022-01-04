options(scipen = 100)

# Create Table Schema in pgAdmin

# Load Packages
library(odbc)
require('RPostgreSQL')

# Load the PostgreSQL driver:
drv <- dbDriver('PostgreSQL')

# Connect to the Database
con <- dbConnect(drv, dbname = 'gp',
                 host = 'localhost', port = 5432,
                 user = 'postgres', password = '123')

# Pass the SQL statements that creates tables
stmt = "
DROP TABLE IF EXISTS movie CASCADE;
create table movie
		(movie_id varchar(8),
		 type varchar(10),
		 title varchar(200) NOT NULL,
		 release_year SMALLINT,
		 rating varchar(10),
		 duration varchar(15),
		 budget BIGINT,
		 revenue BIGINT,
		 vote_average NUMERIC(3,1),
		 primary key (movie_id)
);
DROP TABLE IF EXISTS tv CASCADE;
create table tv
		(tv_id varchar(8),
		 type varchar(10),
		 title varchar(150)NOT NULL,
		 release_year SMALLINT,
		 rating varchar(10),
		 average_episode_duration INT,
		 episodes SMALLINT,
		 seasons SMALLINT,
		 vote_average NUMERIC(3,1),
		 primary key (tv_id)
		);
DROP TABLE IF EXISTS director CASCADE;
create table director
		(director_id varchar(8),
		 director varchar(200),
		 primary key (director_id)
		);
DROP TABLE IF EXISTS movie_director CASCADE;	
create table movie_director
		(movie_id varchar(8),
		 director_id varchar(8),
		 primary key (movie_id, director_id),
		 foreign key (movie_id) references movie (movie_id) on delete cascade,
		 foreign key (director_id) references director (director_id) on delete cascade
		 );
DROP TABLE IF EXISTS tv_director CASCADE;
create table tv_director
		(tv_id varchar(8),
		 director_id varchar(8),
		 primary key (tv_id, director_id),
		 foreign key (tv_id) references tv (tv_id) on delete cascade,
		 foreign key (director_id) references director (director_id) on delete cascade
		);
DROP TABLE IF EXISTS actor CASCADE;
create table actor
		(actor_id varchar(8),
		 actor varchar(250),
		 primary key (actor_id)
		);
DROP TABLE IF EXISTS movie_cast CASCADE;
create table movie_cast
		(movie_id varchar(8),
		 actor_id varchar(8),
		 primary key (movie_id, actor_id),
		 foreign key (movie_id) references movie (movie_id) on delete cascade,
		 foreign key (actor_id) references actor (actor_id) on delete cascade
		);
DROP TABLE IF EXISTS tv_cast CASCADE;
create table tv_cast
		(tv_id varchar(8),
		 actor_id varchar(8),
		 primary key (tv_id, actor_id),
		 foreign key (tv_id) references tv (tv_id) on delete cascade,
		 foreign key (actor_id) references actor (actor_id) on delete cascade
		);
DROP TABLE IF EXISTS country CASCADE;
create table country
		(country_id varchar(8),
		 country varchar(200),
		 primary key (country_id)
		);
DROP TABLE IF EXISTS movie_country CASCADE;
create table movie_country
		(movie_id varchar(8),
		 country_id varchar(8),
		 primary key (movie_id,country_id),
		 foreign key (movie_id) references movie (movie_id) on delete cascade,
		 foreign key (country_id) references country (country_id) on delete cascade
		);
DROP TABLE IF EXISTS tv_country CASCADE;
 create table tv_country
		(tv_id varchar(8),
		 country_id varchar(8),
		 primary key (tv_id,country_id),
		 foreign key (tv_id) references tv (tv_id) on delete cascade,
		 foreign key (country_id) references country (country_id) on delete cascade
		);
DROP TABLE IF EXISTS genre CASCADE;
create table genre
		(genre_id varchar(8),
		 genre varchar(200),
		 primary key (genre_id)
		);
DROP TABLE IF EXISTS movie_listed_in CASCADE;
create table movie_listed_in
		(movie_id varchar(8),
		 genre_id varchar(8),
		 primary key (movie_id,genre_id),
		 foreign key (movie_id) references movie (movie_id) on delete cascade,
		 foreign key (genre_id) references genre (genre_id) on delete cascade
		);
DROP TABLE IF EXISTS tv_listed_in CASCADE;		
create table tv_listed_in
		(tv_id varchar(8),
		 genre_id varchar(8),
		 primary key (tv_id,genre_id),
		 foreign key (tv_id) references tv (tv_id) on delete cascade,
		 foreign key (genre_id) references genre (genre_id) on delete cascade
		);		
DROP TABLE IF EXISTS platform CASCADE;
create table platform
		(platform_id varchar(8),
		 platform varchar(15),
		 primary key (platform_id)
		);		
DROP TABLE IF EXISTS movie_platform CASCADE;
create table movie_platform
		(movie_id varchar(8) NOT NULL,
		 platform_id varchar(15) NOT NULL,
		 date_added date,
		 description varchar(300),
		 primary key (movie_id, platform_id),
		 foreign key (movie_id) references movie (movie_id) on delete cascade,
		 foreign key (platform_id) references platform (platform_id) on delete cascade
		);		
DROP TABLE IF EXISTS tv_platform CASCADE;
create table tv_platform
		(tv_id varchar(8) NOT NULL,
		 platform_id varchar(15) NOT NULL,
		 date_added date,
		 description varchar(300),
		 primary key (tv_id, platform_id),
		 foreign key (tv_id) references tv (tv_id) on delete cascade,
		 foreign key (platform_id) references platform (platform_id) on delete cascade
		);		
"

# Execute "Create table" statements on PostgreSQL
rs <- dbSendQuery(con, stmt)

# Close the Connection
dbDisconnect(con)
closeAllConnections()
