library(shiny)
library(shinythemes)
library(dplyr)
library(DBI)
library(odbc)
#library(MajorTom)
library(feather)
library(DT)
library(tidyverse)
#connect SQl to R
con <- dbConnect(odbc::odbc(), .connection_string = "driver={SQL Server};server=GCSQL03;database=GroundControl2;trusted_connection=true")

Player_data_query <- "with aa as (


select 
concat(b.last_name + ', ' + b.first_name,' ', '(',p.groundcontrol_id,')') as Player,
case when b.primary_position in ('OF', 'CF', 'LF', 'RF', 'INF', 'IF', 'UTL', 'SS', '3B', '2B', 'MIF', '1B', 'C') then 'B' else 'P'  end as 'Player Type',
p.groundcontrol_id as 'GC ID'
, b.player_guid as 'GC Profile',

 b.signing_elig_year as J2,
b.country as 'Country', 
dbo.GetAge_Decimal(p.birthdate, getdate()) as 'Cur Age',
 b.primary_position as 'Pos',

case when h.market_status_id = 1 then 'Avail'
when h.market_status_id = 2 then 'Interest'
when h.market_status_id = 3 then 'Committed'
when h.market_status_id = 4 then 'Signed'
when h.market_status_id is null then 'Avail'
else 'ERROR' end as 'Status'
,
case when h.decision_id = 1 then 'In'
when h.decision_id = 2 then 'Flw'
when h.decision_id = 3 then 'Out'
else ''
end as 'Dec'


 

from scout.intl_bios b 
left join astros.players p on b.player_guid = p.intl_guid
left join scout.Intl_Bios_History h
on p.intl_guid = h.player_guid and b.player_guid = h.player_guid and h.seq = (select  max(seq) from scout.intl_bios_history hh where hh.player_guid = h.player_guid)
where b.signing_elig_year > 2017
)

select * from aa 
order by [Player] asc;"
Player_data <- dbGetQuery(con, Player_data_query)
Player_data <- mutate_if( Player_data, is.character, str_replace_all, pattern = "ñ", replacement = "n")
Player_data <- mutate_if( Player_data, is.character, str_replace_all, pattern = "É", replacement = "e")
Player_data <- mutate_if( Player_data, is.character, str_replace_all, pattern = "Ó", replacement = "o")
Player_data <- mutate_if( Player_data, is.character, str_replace_all, pattern = "í", replacement = "i")
Player_data <- mutate_if( Player_data, is.character, str_replace_all, pattern = "é", replacement = "e")
Player_data <- mutate_if( Player_data, is.character, str_replace_all, pattern = "Ñ", replacement = "n")
Player_data <- mutate_if( Player_data, is.character, str_replace_all, pattern = "Í", replacement = "i")
Player_data <- mutate_if( Player_data, is.character, str_replace_all, pattern = "Ó", replacement = "o")
Player_data <- mutate_if( Player_data, is.character, str_replace_all, pattern = "Ú", replacement = "u")
Player_data <- mutate_if( Player_data, is.character, str_replace_all, pattern = "Á", replacement = "a")
Player_data <- Player_data %>% mutate_at(vars(Player, `GC ID`, `GC Profile`, `Player Type`, Pos, Country,  Status, Dec), ~as.factor(.))
Player_data <- Player_data %>% mutate_at(vars(J2), ~as.integer(.))
write_feather(Player_data, "Player_data.feather")


Player_data_columns <- c(	"GC ID",	"J2",	"Country",	"Cur Age",	"Pos",	"Status",	"Dec")


Scout_query <- "with aa as (
select distinct
concat(b.last_name + ', ' + b.first_name,' ', '(',a.groundcontrol_id,')') as Player,
a.groundcontrol_id as 'GC ID',
round(max(o.overall_interest),2) as 'Overall Interest'
, max(case when follow_grade_tech in (0,6) then null else follow_grade_tech * 1.0 end) as 'Tech Grade'
, max(case when follow_grade_field in (0,6) then null else follow_grade_field * 1.0 end) as 'Field Grade'

from scout.follow_reports fr
left join astros.players a on fr.groundcontrol_id = a.groundcontrol_id
left join scout.Follow_Overall_Interest o on a.groundcontrol_id = o.groundcontrol_id and fr.draft_signing_year = o.draft_signing_year
left join scout.Intl_Bios b
on b.player_guid = a.intl_guid

where
fr.is_r4 = 0
and fr.draft_signing_year between 2018 and 2022
and b.signing_elig_year between 2018 and 2022
group by  b.first_name, b.last_name,  a.groundcontrol_id)
select * from aa
order by Player asc;"

Scout_Follows <- dbGetQuery(con, Scout_query)
Scout_Follows <- mutate_if( Scout_Follows, is.character, str_replace_all, pattern = "ñ", replacement = "n")
Scout_Follows <- mutate_if( Scout_Follows, is.character, str_replace_all, pattern = "É", replacement = "e")
Scout_Follows <- mutate_if( Scout_Follows, is.character, str_replace_all, pattern = "Ó", replacement = "o")
Scout_Follows <- mutate_if( Scout_Follows, is.character, str_replace_all, pattern = "í", replacement = "i")
Scout_Follows <- mutate_if( Scout_Follows, is.character, str_replace_all, pattern = "é", replacement = "e")
Scout_Follows <- mutate_if( Scout_Follows, is.character, str_replace_all, pattern = "Ñ", replacement = "n")
Scout_Follows <- mutate_if( Scout_Follows, is.character, str_replace_all, pattern = "Í", replacement = "i")
Scout_Follows <- mutate_if( Scout_Follows, is.character, str_replace_all, pattern = "Ó", replacement = "o")
Scout_Follows <- mutate_if( Scout_Follows, is.character, str_replace_all, pattern = "Ú", replacement = "u")
Scout_Follows <- mutate_if( Scout_Follows, is.character, str_replace_all, pattern = "Á", replacement = "a")
Scout_Follows <- Scout_Follows %>% mutate_at(vars(Player, `GC ID`), ~as.factor(.))
Scout_Follows <- Scout_Follows %>% mutate_at(vars(`Overall Interest`), ~as.numeric(.))
Scout_Follows <- Scout_Follows %>% mutate_at(vars( `Tech Grade`, `Field Grade`), ~as.integer(.))

write_feather(Scout_Follows, "Scout_Follows.feather")

Scout_Follows_columns <- c("Overall Interest",	"Tech Grade",	"Field Grade")

Hitter_workout_query <- "with work as (
select 
concat(a.last_name + ', ' + a.first_name,' ', '(',s.groundcontrol_id,')') as Player,

s.groundcontrol_id as 'GC ID',
 convert(varchar, s.entry_date, 101) as 'Date Recorded',

 dbo.GetAge_Decimal(a.birthdate, s.entry_date) as 'Age Recorded',

 round(avg(case when s.scouting_metric_id = 12 then s.value else null end),1) as 'Avg Bat Speed'
, round(max(case when s.scouting_metric_id = 12 then s.value else null end),1) as 'Max Bat Speed'
, round(max(case when s.scouting_metric_id = 9 then s.value else null end),1) as 'Max OF Throw'
, round(max(case when s.scouting_metric_id = 10 then s.value else null end),1) as 'Max IF Throw'
, round(max(case when s.scouting_metric_id = 11 then s.value else null end),1) as 'Max C Throw'
,round(avg(case when s.scouting_metric_id = 5 then s.value else null end), 1) as 'Avg C POP'
, round(max(case when s.scouting_metric_id = 19 then s.value else null end), 1) as 'Max Run and Gun'
, round(avg(case when s.scouting_metric_id = 19 then s.value else null end), 1) as 'Avg Run and Gun'
, round(max(case when s.scouting_metric_id = 20 then s.value else null end), 1) as 'Max Step and Throw'
, round(avg(case when s.scouting_metric_id = 20 then s.value else null end), 1) as 'Avg Step and Throw'
, round(max(case when s.scouting_metric_id = 21 then s.value else null end), 1) as 'Max Broad Jump'
, round(avg(case when s.scouting_metric_id = 21 then s.value else null end), 1) as 'Avg Broad Jump',
round(avg(case when s.scouting_metric_id = 13 then s.value else null end), 1) as 'Both Eyes',
round(avg(case when s.scouting_metric_id = 14 then s.value else null end), 1) as 'Left Eye',
round(avg(case when s.scouting_metric_id = 15 then s.value else null end), 1) as 'Right Eye',
round(avg(case when s.scouting_metric_id = 1 then s.value else null end), 1) as 'Avg 30 Yd',
round(avg(case when s.scouting_metric_id = 2 then s.value else null end), 1) as 'Avg 60 Yd',
round(avg(case when s.scouting_metric_id = 3 then s.value else null end), 1) as 'Avg RHH H21',
round(avg(case when s.scouting_metric_id = 4 then s.value else null end), 1) as 'Avg LHH H21'

from 
scout.scouting_metrics s 
left join astros.Players a
on s.groundcontrol_id = a.groundcontrol_id
left join scout.Intl_Bios b
on a.intl_guid = b.player_guid

where s.groundcontrol_id < 0
and b.signing_elig_year > 2017
group by
s.groundcontrol_id,
a.first_name,
a.last_name,
a.birthdate,
entry_date),

work1 as (
select 
concat(a.last_name + ', ' + a.first_name,' ', '(',s.groundcontrol_id,')') as Player,
s.groundcontrol_id as 'GC ID'
, 'Total' as 'Date Recorded',
 dbo.GetAge_Decimal(a.birthdate, getdate()) as 'Age Recorded',
 round(avg(case when s.scouting_metric_id = 12 then s.value else null end),1) as 'Avg Bat Speed'
, round(max(case when s.scouting_metric_id = 12 then s.value else null end),1) as 'Max Bat Speed'
, round(max(case when s.scouting_metric_id = 9 then s.value else null end),1) as 'Max OF Throw'
, round(max(case when s.scouting_metric_id = 10 then s.value else null end),1) as 'Max IF Throw'
, round(max(case when s.scouting_metric_id = 11 then s.value else null end),1) as 'Max C Throw'
,round(avg(case when s.scouting_metric_id = 5 then s.value else null end), 1) as 'Avg C POP'
, round(max(case when s.scouting_metric_id = 19 then s.value else null end), 1) as 'Max Run and Gun'
, round(avg(case when s.scouting_metric_id = 19 then s.value else null end), 1) as 'Avg Run and Gun'
, round(max(case when s.scouting_metric_id = 20 then s.value else null end), 1) as 'Max Step and Throw'
, round(avg(case when s.scouting_metric_id = 20 then s.value else null end), 1) as 'Avg Step and Throw'
, round(max(case when s.scouting_metric_id = 21 then s.value else null end), 1) as 'Max Broad Jump'
, round(avg(case when s.scouting_metric_id = 21 then s.value else null end), 1) as 'Avg Broad Jump',
round(avg(case when s.scouting_metric_id = 13 then s.value else null end), 1) as 'Both Eyes',
round(avg(case when s.scouting_metric_id = 14 then s.value else null end), 1) as 'Left Eye',
round(avg(case when s.scouting_metric_id = 15 then s.value else null end), 1) as 'Right Eye',
round(avg(case when s.scouting_metric_id = 1 then s.value else null end), 1) as 'Avg 30 Yd',
round(avg(case when s.scouting_metric_id = 2 then s.value else null end), 1) as 'Avg 60 Yd',
round(avg(case when s.scouting_metric_id = 3 then s.value else null end), 1) as 'Avg RHH H21',
round(avg(case when s.scouting_metric_id = 4 then s.value else null end), 1) as 'Avg LHH H21'
from 
scout.scouting_metrics s 
left join astros.Players a
on s.groundcontrol_id = a.groundcontrol_id
left join scout.Intl_Bios b
on a.intl_guid = b.player_guid

where s.groundcontrol_id < 0
and b.signing_elig_year > 2017
group by
s.groundcontrol_id,
a.first_name,
a.last_name,
a.birthdate)


select * from work w
where w.[Avg Bat Speed] is not null or w.[Max Bat Speed] is not null or w.[Max OF Throw] is not null or w.[Max IF Throw] is not null or w.[Max C Throw] is not null or w.[Avg C POP] is not null or w.[Max Run and Gun] is not null or w.[Max Step and Throw] is not null or w.[Avg Run and Gun] is not null or w.[Avg Step and Throw] is not null or w.[Max Broad Jump] is not null or w.[Avg Broad Jump] is not null or w.[Both Eyes] is not null or w.[Left Eye] is not null or w.[Right Eye] is not null or w.[Avg 30 Yd] is not null or w.[Avg 60 Yd] is not null or w.[Avg LHH H21] is not null or w.[Avg RHH H21] is not null

union
select * from work1 w1
where w1.[Avg Bat Speed] is not null or w1.[Max Bat Speed] is not null or w1.[Max OF Throw] is not null or w1.[Max IF Throw] is not null or w1.[Max C Throw] is not null or w1.[Avg C POP] is not null or w1.[Max Run and Gun] is not null or w1.[Max Step and Throw] is not null or w1.[Avg Run and Gun] is not null or w1.[Avg Step and Throw] is not null or w1.[Max Broad Jump] is not null or w1.[Avg Broad Jump] is not null or w1.[Both Eyes] is not null or w1.[Left Eye] is not null or w1.[Right Eye] is not null or w1.[Avg 30 Yd] is not null or w1.[Avg 60 Yd] is not null or w1.[Avg LHH H21] is not null or w1.[Avg RHH H21] is not null"

Hitter_Workout_data <- dbGetQuery(con, Hitter_workout_query)
Hitter_Workout_data <- mutate_if( Hitter_Workout_data, is.character, str_replace_all, pattern = "ñ", replacement = "n")
Hitter_Workout_data <- mutate_if( Hitter_Workout_data, is.character, str_replace_all, pattern = "É", replacement = "e")
Hitter_Workout_data <- mutate_if( Hitter_Workout_data, is.character, str_replace_all, pattern = "Ó", replacement = "o")
Hitter_Workout_data <- mutate_if( Hitter_Workout_data, is.character, str_replace_all, pattern = "í", replacement = "i")
Hitter_Workout_data <- mutate_if( Hitter_Workout_data, is.character, str_replace_all, pattern = "é", replacement = "e")
Hitter_Workout_data <- mutate_if( Hitter_Workout_data, is.character, str_replace_all, pattern = "Ñ", replacement = "n")
Hitter_Workout_data <- mutate_if( Hitter_Workout_data, is.character, str_replace_all, pattern = "Í", replacement = "i")
Hitter_Workout_data <- mutate_if( Hitter_Workout_data, is.character, str_replace_all, pattern = "Ó", replacement = "o")
Hitter_Workout_data <- mutate_if( Hitter_Workout_data, is.character, str_replace_all, pattern = "Ú", replacement = "u")
Hitter_Workout_data <- mutate_if( Hitter_Workout_data, is.character, str_replace_all, pattern = "Á", replacement = "a")
Hitter_Workout_data <- Hitter_Workout_data %>% mutate_at(vars(Player, `GC ID`, `Date Recorded`), ~as.factor(.))
Hitter_Workout_data <- Hitter_Workout_data %>% mutate_at(vars(`Age Recorded`), ~as.numeric(.))

write_feather(Hitter_Workout_data, "Hitter_Workout_data.feather")

Hitter_Workout_columns <- c("Avg Bat Speed",	"Max Bat Speed",	"Max OF Throw",	"Max IF Throw",	"Max C Throw",	"Avg C POP",	"Max Run and Gun",	"Avg Run and Gun",	"Max Step and Throw",	"Avg Step and Throw",	"Max Broad Jump",	"Avg Broad Jump",	"Both Eyes",	"Left Eye",	"Right Eye",	"Avg 30 Yd",	"Avg 60 Yd",	"Avg RHH H21",	"Avg LHH H21")

Pitcher_workout_query <- "with PW as ( 
select
concat(b.last_name + ', ' + b.first_name,' ', '(',a.groundcontrol_id,')') as Player,
 a.groundcontrol_id as 'GC ID',
 convert(varchar, m.entry_date, 101) as 'Date Recorded',

 dbo.GetAge_Decimal(a.birthdate, m.entry_date) as 'Age Recorded'
 , round( max(case when  i.scouting_metric_id = 6 and p.pitch_type_id = 6  then value else null end), 2) as 'Max FB'
 , round( avg(case when i.scouting_metric_id = 8 and p.pitch_type_id = 6  then value else null end), 2) as 'Avg FB'
 , round( Min(case when i.scouting_metric_id = 7 and p.pitch_type_id = 6  then value else null end), 2) as 'Min FB'

 , round( max(case when i.scouting_metric_id = 6 and p.pitch_type_id = 4 then value else null end), 2) as 'Max CB'
 , round( avg(case when i.scouting_metric_id = 8 and p.pitch_type_id = 4 then value else null end), 2) as 'Avg CB'
 , round( Min(case when i.scouting_metric_id = 7 and p.pitch_type_id = 4 then value else null end), 2) as 'Min CB'

 , round( max(case when i.scouting_metric_id = 6 and p.pitch_type_id = 20 then value else null end), 2) as 'Max SL'
 , round( avg(case when i.scouting_metric_id = 8 and p.pitch_type_id = 20 then value else null end), 2) as 'Avg SL'
 , round( Min(case when i.scouting_metric_id = 7 and p.pitch_type_id = 20 then value else null end), 2) as 'Min SL'

 , round( max(case when i.scouting_metric_id = 6 and p.pitch_type_id = 3 then value else null end), 2) as 'Max CH'
 , round( avg(case when i.scouting_metric_id = 8 and p.pitch_type_id = 3 then value else null end), 2) as 'Avg CH'
 , round( Min(case when i.scouting_metric_id = 7 and p.pitch_type_id = 3 then value else null end), 2) as 'Min CH'

 , round( max(case when i.scouting_metric_id = 6 and p.pitch_type_id = 11 then value else null end), 2) as 'Max SP'
 , round( avg(case when i.scouting_metric_id = 8 and p.pitch_type_id = 11 then value else null end), 2) as 'Avg SP'
 , round( Min(case when i.scouting_metric_id = 7 and p.pitch_type_id = 11 then value else null end), 2) as 'Min SP'

 , round( max(case when i.scouting_metric_id = 6 and p.pitch_type_id = 18 then value else null end), 2) as 'Max OT'
 , round( avg(case when i.scouting_metric_id = 8 and p.pitch_type_id = 18 then value else null end), 2) as 'Avg OT'
 , round( Min(case when i.scouting_metric_id = 7 and p.pitch_type_id = 18 then value else null end), 2) as 'Min OT'

   from scout.Intl_Bios b
   join astros.players a
   on b.player_guid = a.intl_guid
   join scout.scouting_metrics m
   on m.groundcontrol_id = a.groundcontrol_id
   join scout.Scouting_Metric_Ids i
   on m.scouting_metric_id = i.scouting_metric_id
   left join astros.lk_pitch_types p
   on m.pitch_type_id = p.pitch_type_id
   
   where b.signing_elig_year > 2017
 
   
   group by b.first_name, b.last_name, a.groundcontrol_id, a.birthdate, m.entry_date),

   PW1 as (
   
select
concat(b.last_name + ', ' + b.first_name,' ', '(',a.groundcontrol_id,')') as Player,
 a.groundcontrol_id as 'GC ID',
'Total' as 'Date Recorded',

 dbo.GetAge_Decimal(a.birthdate, GETDATE())  as 'Age Recorded'
 , round( max(case when  i.scouting_metric_id = 6 and p.pitch_type_id = 6  then value else null end), 2) as 'Max FB'
 , round( avg(case when i.scouting_metric_id = 8 and p.pitch_type_id = 6  then value else null end), 2) as 'Avg FB'
 , round( Min(case when i.scouting_metric_id = 7 and p.pitch_type_id = 6  then value else null end), 2) as 'Min FB'

 , round( max(case when i.scouting_metric_id = 6 and p.pitch_type_id = 4 then value else null end), 2) as 'Max CB'
 , round( avg(case when i.scouting_metric_id = 8 and p.pitch_type_id = 4 then value else null end), 2) as 'Avg CB'
 , round( Min(case when i.scouting_metric_id = 7 and p.pitch_type_id = 4 then value else null end), 2) as 'Min CB'

 , round( max(case when i.scouting_metric_id = 6 and p.pitch_type_id = 20 then value else null end), 2) as 'Max SL'
 , round( avg(case when i.scouting_metric_id = 8 and p.pitch_type_id = 20 then value else null end), 2) as 'Avg SL'
 , round( Min(case when i.scouting_metric_id = 7 and p.pitch_type_id = 20 then value else null end), 2) as 'Min SL'

 , round( max(case when i.scouting_metric_id = 6 and p.pitch_type_id = 3 then value else null end), 2) as 'Max CH'
 , round( avg(case when i.scouting_metric_id = 8 and p.pitch_type_id = 3 then value else null end), 2) as 'Avg CH'
 , round( Min(case when i.scouting_metric_id = 7 and p.pitch_type_id = 3 then value else null end), 2) as 'Min CH'

 , round( max(case when i.scouting_metric_id = 6 and p.pitch_type_id = 11 then value else null end), 2) as 'Max SP'
 , round( avg(case when i.scouting_metric_id = 8 and p.pitch_type_id = 11 then value else null end), 2) as 'Avg SP'
 , round( Min(case when i.scouting_metric_id = 7 and p.pitch_type_id = 11 then value else null end), 2) as 'Min SP'

 , round( max(case when i.scouting_metric_id = 6 and p.pitch_type_id = 18 then value else null end), 2) as 'Max OT'
 , round( avg(case when i.scouting_metric_id = 8 and p.pitch_type_id = 18 then value else null end), 2) as 'Avg OT'
 , round( Min(case when i.scouting_metric_id = 7 and p.pitch_type_id = 18 then value else null end), 2) as 'Min OT'

   from scout.Intl_Bios b
   join astros.players a
   on b.player_guid = a.intl_guid
   join scout.scouting_metrics m
   on m.groundcontrol_id = a.groundcontrol_id
   join scout.Scouting_Metric_Ids i
   on m.scouting_metric_id = i.scouting_metric_id
   left join astros.lk_pitch_types p
   on m.pitch_type_id = p.pitch_type_id
   
   where b.signing_elig_year > 2017
 
   
   group by b.first_name, b.last_name, a.groundcontrol_id, a.birthdate)


   select * from PW p
   where p.[Max FB] is not null or p.[Avg FB] is not null or p.[Min FB] is not null or p.[Max CB] is not null or p.[Avg CB] is not null or p.[Max SL] is not null or p.[Avg SL] is not null or p.[Min CB] is not null or p.[Min SL] is not null or p.[Avg SP] is not null or p.[Max SP] is not null or p.[Min SP] is not null or p.[Max OT] is not null or p.[Avg OT] is not null or p.[Min OT] is not null
      union
   select * from PW1 p1
      where p1.[Max FB] is not null or p1.[Avg FB] is not null or p1.[Min FB] is not null or p1.[Max CB] is not null or p1.[Avg CB] is not null or p1.[Max SL] is not null or p1.[Avg SL] is not null or p1.[Min CB] is not null or p1.[Min SL] is not null or p1.[Avg SP] is not null or p1.[Max SP] is not null or p1.[Min SP] is not null or p1.[Max OT] is not null or p1.[Avg OT] is not null or p1.[Min OT] is not null"


Pitcher_Workout_data <- dbGetQuery(con, Pitcher_workout_query)
Pitcher_Workout_data <- mutate_if( Pitcher_Workout_data, is.character, str_replace_all, pattern = "ñ", replacement = "n")
Pitcher_Workout_data <- mutate_if( Pitcher_Workout_data, is.character, str_replace_all, pattern = "É", replacement = "e")
Pitcher_Workout_data <- mutate_if( Pitcher_Workout_data, is.character, str_replace_all, pattern = "Ó", replacement = "o")
Pitcher_Workout_data <- mutate_if( Pitcher_Workout_data, is.character, str_replace_all, pattern = "í", replacement = "i")
Pitcher_Workout_data <- mutate_if( Pitcher_Workout_data, is.character, str_replace_all, pattern = "é", replacement = "e")
Pitcher_Workout_data <- mutate_if( Pitcher_Workout_data, is.character, str_replace_all, pattern = "Ñ", replacement = "n")
Pitcher_Workout_data <- mutate_if( Pitcher_Workout_data, is.character, str_replace_all, pattern = "Í", replacement = "i")
Pitcher_Workout_data <- mutate_if( Pitcher_Workout_data, is.character, str_replace_all, pattern = "Ó", replacement = "o")
Pitcher_Workout_data <- mutate_if( Pitcher_Workout_data, is.character, str_replace_all, pattern = "Ú", replacement = "u")
Pitcher_Workout_data <- mutate_if( Pitcher_Workout_data, is.character, str_replace_all, pattern = "Á", replacement = "a")
Pitcher_Workout_data <- Pitcher_Workout_data %>% mutate_at(vars(Player, `GC ID`, `Date Recorded`), ~as.factor(.))
Pitcher_Workout_data <- Pitcher_Workout_data %>% mutate_at(vars(`Age Recorded`), ~as.numeric(.))

write_feather(Pitcher_Workout_data, "Pitcher_Workout_data.feather")


Pitcher_Workout_columns <- c(	"Max FB",	"Avg FB",	"Min FB",	"Max CB",	"Avg CB",	"Min CB",	"Max SL",	"Avg SL",	"Min SL",	"Max CH",	"Avg CH",	"Min CH",	"Max SP",	"Avg SP",	"Min SP",	"Max OT",	"Avg OT",	"Min OT")

Speed_gate_query <- "
   with SG as (
   select distinct
concat(b.last_name + ', ' + b.first_name,' ', '(',a.groundcontrol_id,')') as Player,
 a.groundcontrol_id as 'GC ID',
  dbo.GetAge_Decimal(a.birthdate, m.entry_date) as 'Age Recorded',
 convert(varchar, m.entry_date, 101) as 'Date Recorded'
 
 , round( max(case when  i.scouting_metric_id = 16 then value else null end), 2) as 'SG Max 10yd'
 , round( avg(case when  i.scouting_metric_id = 16 then value else null end), 2) as 'SG Avg 10yd'
 , round( Min(case when  i.scouting_metric_id = 16 then value else null end), 2) as 'SG Min 10yd'
 , round( max(case when  i.scouting_metric_id = 18 then value else null end), 2) as 'SG Max 30yd'
 , round( avg(case when  i.scouting_metric_id = 18 then value else null end), 2) as 'SG Avg 30yd'
 , round( Min(case when  i.scouting_metric_id = 18 then value else null end), 2) as 'SG Min 30yd'
   from scout.Intl_Bios b
   join astros.players a
   on b.player_guid = a.intl_guid
   join scout.scouting_metrics m
   on m.groundcontrol_id = a.groundcontrol_id
   join scout.Scouting_Metric_Ids i
   on m.scouting_metric_id = i.scouting_metric_id
   where b.signing_elig_year > 2017
   group by b.first_name, b.last_name, a.groundcontrol_id
   , m.entry_date, a.birthdate),

   SG2  as (
    select distinct
concat(b.last_name + ', ' + b.first_name,' ', '(',a.groundcontrol_id,')') as Player,
 a.groundcontrol_id as 'GC ID',
   dbo.GetAge_Decimal(a.birthdate, GETDATE()) as 'Age Recorded',
'Total' as 'Date Recorded'

 , round( max(case when  i.scouting_metric_id = 16 then value else null end), 2) as 'SG Max 10yd'
 , round( avg(case when  i.scouting_metric_id = 16 then value else null end), 2) as 'SG Avg 10yd'
 , round( Min(case when  i.scouting_metric_id = 16 then value else null end), 2) as 'SG Min 10yd'
 , round( max(case when  i.scouting_metric_id = 18 then value else null end), 2) as 'SG Max 30yd'
 , round( avg(case when  i.scouting_metric_id = 18 then value else null end), 2) as 'SG Avg 30yd'
 , round( Min(case when  i.scouting_metric_id = 18 then value else null end), 2) as 'SG Min 30yd'

   from scout.Intl_Bios b
   join astros.players a
   on b.player_guid = a.intl_guid
   join scout.scouting_metrics m
   on m.groundcontrol_id = a.groundcontrol_id
   join scout.Scouting_Metric_Ids i
   on m.scouting_metric_id = i.scouting_metric_id

   where b.signing_elig_year > 2017
   group by b.first_name, b.last_name, a.groundcontrol_id
   , a.birthdate)

   select * from SG s1
   where s1.[SG Max 10yd] is not null or s1.[SG Avg 30yd] is not null or s1.[SG Max 30yd] is not null or s1.[SG Avg 10yd] is not null or s1.[SG Min 10yd] is not null or s1.[SG Max 10yd] is not null or s1.[SG Min 30yd] is not null
   union
   select * from SG2 s2
   where s2.[SG Max 10yd] is not null or s2.[SG Avg 30yd] is not null or s2.[SG Max 30yd] is not null or s2.[SG Avg 10yd] is not null or s2.[SG Min 10yd] is not null or s2.[SG Max 10yd] is not null or s2.[SG Min 30yd] is not null"


SG_data <- dbGetQuery(con, Speed_gate_query)
SG_data <- mutate_if( SG_data, is.character, str_replace_all, pattern = "ñ", replacement = "n")
SG_data <- mutate_if( SG_data, is.character, str_replace_all, pattern = "É", replacement = "e")
SG_data <- mutate_if( SG_data, is.character, str_replace_all, pattern = "Ó", replacement = "o")
SG_data <- mutate_if( SG_data, is.character, str_replace_all, pattern = "í", replacement = "i")
SG_data <- mutate_if( SG_data, is.character, str_replace_all, pattern = "é", replacement = "e")
SG_data <- mutate_if( SG_data, is.character, str_replace_all, pattern = "Ñ", replacement = "n")
SG_data <- mutate_if( SG_data, is.character, str_replace_all, pattern = "Í", replacement = "i")
SG_data <- mutate_if( SG_data, is.character, str_replace_all, pattern = "Ó", replacement = "o")
SG_data <- mutate_if( SG_data, is.character, str_replace_all, pattern = "Ú", replacement = "u")
SG_data <- mutate_if( SG_data, is.character, str_replace_all, pattern = "Á", replacement = "a")
SG_data <- SG_data %>% mutate_at(vars(Player, `GC ID`, `Date Recorded`), ~as.factor(.))
SG_data <- SG_data %>% mutate_at(vars(`Age Recorded`), ~as.numeric(.))

write_feather(SG_data, "SG_data.feather")

SG_columns <- c("SG Date Recorded",	"SG Age Recorded",	"SG Max 10yd",	"SG Avg 10yd",	"SG Min 10yd",	"SG Max 30yd",	"SG Avg 30yd",	"SG Min 30yd")

Rap_query <- "
with rap as (
select
concat(a.last_name + ', ' + a.first_name,' ', '(',r.groundcontrol_id,')') as Player,
---convert(varchar, a.birthdate, 101) as  'Birth',
 r.groundcontrol_id as 'GC ID', 

 max(round(dbo.GetAge_Decimal(a.birthdate, r.date),2)) as 'Age Recorded',

--, r.file_name as 'event',
min(convert(varchar(10),r.date,101)) as 'Date Recorded',
---max(convert(date,r.date)) as 'Date Collected', 
---, max(r.date) as recent_date,
---rl.level,
sum(case when r.exit_speed is not null then 1 else 0 end) as 'RAP BIP'
, cast(round(sum(case when r.exit_speed >= '90' and r.launch_angle between '10' and '35' then 1.0 else 0.0 end)
/ nullif(sum(case when r.exit_speed is not null then 1.0 end),0), 1)as decimal(10,2)) as 'RAP BP Barrel Rate'
, cast(round(sum(case when r.exit_speed >= 90 and r.launch_angle >-70 and r.launch_angle < 90 then 1.0 else 0.0 end)
/ nullif(sum(case when r.exit_speed is not null then 1.0 else 0.0 end),0), 1) as decimal(10,1)) as 'RAP 90 EV % BIP'
, cast(round(sum(case when r.exit_speed <= 85 and r.launch_angle >-70 and r.launch_angle < 90 then 1.0 else 0.0 end)
/ nullif(sum(case when r.exit_speed is not null then 1.0 else 0.0 end),0),1) as decimal(10,1)) as 'RAP Weak EV % BIP'
, cast(round(max(r.exit_speed),1) as decimal(10,2)) as 'RAP Max EV'
, cast(round(max(r.exit_speed) + 0.2*(85-(avg(r.pitch_ball_speed))), 2) as decimal (10,2)) as 'RAP 90MPH Equiv EV'
, cast(round(max(case when r.distance is not null then r.distance else null end),1) as decimal (10,1)) as 'RAP Max Distance'
, cast(round(avg(case when r.distance is not null then r.distance else null end),1) as decimal (10,1)) as 'RAP Avg Distance'
, cast(round(avg(case when r.exit_speed is not null then r.launch_angle else null end),1) as decimal(10,1)) as 'RAP Launch Angle'
, cast(round(avg(case when r.exit_speed is not null then r.exit_speed else null end),1) as decimal (10,1)) as 'RAP Launch Direction'
, cast(round(sum(case when r.launch_angle < '10' then 1 else 0 end) / nullif(sum(case when r.exit_speed is not null then 1.0 else 0 end),0),1) as decimal (10,1)) as 'RAP Ground Ball %'
, cast(round(sum(case when r.launch_angle between '10' and '25' then 1 else 0 end) / nullif(sum(case when r.exit_speed is not null then 1.0 else 0 end),0),1) as decimal (10,1)) as 'RAP Line Drive %'
, cast(round(sum(case when r.launch_angle between '25' and '50' then 1 else 0 end) / nullif(sum(case when r.exit_speed is not null then 1.0 else 0 end),0),1) as decimal (10,1)) as 'RAP Fly Ball %'
, cast(round(sum(case when r.launch_angle > '50' then 1 else 0 end) / nullif(sum(case when r.exit_speed is not null then 1.0 else 0 end),0), 1) as decimal (10,1)) as 'RAP Pop Up %'
from 
rapsodo.hitting_raw r
left join astros.players a on r.groundcontrol_id = a.groundcontrol_id
left join rapsodo.file_level rl on r.file_name = rl.file_name
left join scout.bios b on a.groundcontrol_id = b.groundcontrol_id
left join scout.intl_bios bi on a.intl_guid = bi.player_guid
left join MLB_eBis.GBL_SCHTEAM m on b.school_id = m.SCHTEAM_ID
where
r.date >= 2017
and a.intl_guid is not null
and signing_elig_year in ('2018', '2019', '2020', '2021')
group by 
r.groundcontrol_id,
a.first_name, a.last_name,
a.birthdate,
CASE WHEN b.draft_elig_year is null then signing_elig_year else draft_elig_year end,
(CASE WHEN a.intl_guid is NULL then 'N' else 'Y' end),
--r.file_name,
rl.level,
 bi.country
HAVING
count(*) > 4
), 
 
 rap2  as (

select 

concat(a.last_name + ', ' + a.first_name,' ', '(',r.groundcontrol_id,')') as Player,
------convert(varchar, a.birthdate, 101) as  'Birth',
 r.groundcontrol_id as 'GC ID', 
dbo.GetAge_Decimal(a.birthdate, getdate()) as 'Age Recorded',


--, r.file_name as 'event',
'Total' as 'Date Recorded',
---max(convert(date,r.date)) as 'Date Collected', 
---, max(r.date) as recent_date,
---rl.level,
sum(case when r.exit_speed is not null then 1 else 0 end) as 'RAP BIP'
, cast(round(sum(case when r.exit_speed >= '90' and r.launch_angle between '10' and '35' then 1.0 else 0.0 end)
/ nullif(sum(case when r.exit_speed is not null then 1.0 end),0), 1)as decimal(10,2)) as 'RAP BP Barrel Rate'
, cast(round(sum(case when r.exit_speed >= 90 and r.launch_angle >-70 and r.launch_angle < 90 then 1.0 else 0.0 end)
/ nullif(sum(case when r.exit_speed is not null then 1.0 else 0.0 end),0), 1) as decimal(10,1)) as 'RAP 90 EV % BIP'
, cast(round(sum(case when r.exit_speed <= 85 and r.launch_angle >-70 and r.launch_angle < 90 then 1.0 else 0.0 end)
/ nullif(sum(case when r.exit_speed is not null then 1.0 else 0.0 end),0),1) as decimal(10,1)) as 'RAP Weak EV % BIP'
, cast(round(max(r.exit_speed),1) as decimal(10,2)) as 'RAP Max EV'
, cast(round(max(r.exit_speed) + 0.2*(85-(avg(r.pitch_ball_speed))), 2) as decimal (10,2)) as 'RAP 90MPH Equiv EV'
, cast(round(max(case when r.distance is not null then r.distance else null end),1) as decimal (10,1)) as 'RAP Max Distance'
, cast(round(avg(case when r.distance is not null then r.distance else null end),1) as decimal (10,1)) as 'RAP Avg Distance'
, cast(round(avg(case when r.exit_speed is not null then r.launch_angle else null end),1) as decimal(10,1)) as 'RAP Launch Angle'
, cast(round(avg(case when r.exit_speed is not null then r.exit_speed else null end),1) as decimal (10,1)) as 'RAP Launch Direction'
, cast(round(sum(case when r.launch_angle < '10' then 1 else 0 end) / nullif(sum(case when r.exit_speed is not null then 1.0 else 0 end),0),1) as decimal (10,1)) as 'RAP Ground Ball %'
, cast(round(sum(case when r.launch_angle between '10' and '25' then 1 else 0 end) / nullif(sum(case when r.exit_speed is not null then 1.0 else 0 end),0),1) as decimal (10,1)) as 'RAP Line Drive %'
, cast(round(sum(case when r.launch_angle between '25' and '50' then 1 else 0 end) / nullif(sum(case when r.exit_speed is not null then 1.0 else 0 end),0),1) as decimal (10,1)) as 'RAP Fly Ball %'
, cast(round(sum(case when r.launch_angle > '50' then 1 else 0 end) / nullif(sum(case when r.exit_speed is not null then 1.0 else 0 end),0), 1) as decimal (10,1)) as 'RAP Pop Up %'
from
rapsodo.hitting_raw r
left join astros.players a on r.groundcontrol_id = a.groundcontrol_id
left join rapsodo.file_level rl on r.file_name = rl.file_name
left join scout.bios b on a.groundcontrol_id = b.groundcontrol_id
left join scout.intl_bios bi on a.intl_guid = bi.player_guid
left join MLB_eBis.GBL_SCHTEAM m on b.school_id = m.SCHTEAM_ID
where
r.date >= 2017
and a.intl_guid is not null
and signing_elig_year in ('2018', '2019', '2020', '2021')
group by 
r.groundcontrol_id,
a.first_name, a.last_name,
a.birthdate,
CASE WHEN b.draft_elig_year is null then signing_elig_year else draft_elig_year end,
(CASE WHEN a.intl_guid is NULL then 'N' else 'Y' end),
--r.file_name,
---rl.level
 bi.country
HAVING
count(*) > 4
)  

select * from rap
union
select * from rap2
order by Player"

Rap_data <- dbGetQuery(con, Rap_query)
Rap_data <- mutate_if( Rap_data, is.character, str_replace_all, pattern = "ñ", replacement = "n")
Rap_data <- mutate_if( Rap_data, is.character, str_replace_all, pattern = "É", replacement = "e")
Rap_data <- mutate_if( Rap_data, is.character, str_replace_all, pattern = "Ó", replacement = "o")
Rap_data <- mutate_if( Rap_data, is.character, str_replace_all, pattern = "í", replacement = "i")
Rap_data <- mutate_if( Rap_data, is.character, str_replace_all, pattern = "é", replacement = "e")
Rap_data <- mutate_if( Rap_data, is.character, str_replace_all, pattern = "Ñ", replacement = "n")
Rap_data <- mutate_if( Rap_data, is.character, str_replace_all, pattern = "Í", replacement = "i")
Rap_data <- mutate_if( Rap_data, is.character, str_replace_all, pattern = "Ó", replacement = "o")
Rap_data <- mutate_if( Rap_data, is.character, str_replace_all, pattern = "Ú", replacement = "u")
Rap_data <- mutate_if( Rap_data, is.character, str_replace_all, pattern = "Á", replacement = "a")
Rap_data <- Rap_data %>% mutate_at(vars(Player, `GC ID`, `Date Recorded`), ~as.factor(.))
Rap_data <- Rap_data %>% mutate_at(vars(`Age Recorded`), ~as.numeric(.))

write_feather(Rap_data, "Rap_data.feather")

RAP_columns <- c("RAP BIP",	"RAP BP Barrel Rate",	"RAP 90 EV % BIP",	"RAP Weak EV % BIP",	"RAP Max EV",	"RAP 90MPH Equiv EV",	"RAP Max Distance",	"RAP Avg Distance",	"RAP Launch Angle",	"RAP Launch Direction",	"RAP Ground Ball %",	"RAP Line Drive %",	"RAP Fly Ball %",	"RAP Pop Up %")


TM_live_query <- "with TML as (

select
concat(a.last_name + ', ' + a.first_name,' ', '(',a.groundcontrol_id,')') as Player,
a.groundcontrol_id as 'GC ID'
, convert(varchar, s.sched_date, 101) as 'Date Recorded'
, dbo.GetAge_Decimal(a.birthdate, s.sched_date) as 'Age Recorded'
, sum(cast(e.pa as int)) as PA
, count(1) as 'TM Live Pitches Seen'
, sum(case when f.pitch_result in ('foul', 'foul_tip', 'hit_into_play' , 'hit_into_play_no_out' , 'hit_into_play_score', 'swinging_strike', 'swinging_strike_blocked') then 1 else 0 end) as 'TM Live Swings'
, sum(case when f.pitch_result in ('foul', 'foul_tip', 'hit_into_play' , 'hit_into_play_no_out' , 'hit_into_play_score') then 1 else null end) as 'TM Live Contacts'
, sum(case when f.pitch_result in ('foul', 'foul_tip', 'hit_into_play' , 'hit_into_play_no_out' , 'hit_into_play_score') and h.hit_exit_speed is not null then 1 else 0 end) as 'TM Live Tracked Contacts'
, sum(case when f.pitch_result_id in (12,13,14,18,19,20) then 1 else 0 end) as 'TM Live BIP'
, sum(case when f.pitch_result_id in (12,13,14,18,19,20) and h.hit_exit_speed is not null then 1 else 0 end) as 'TM Live Tracked BIP'
, round(cast(avg(g.swing_decision_grade_2080) as float),2) as 'TM Live Swing Decs'
, round(cast(avg(case when f.pitch_result in ('foul', 'foul_tip', 'hit_into_play' , 'hit_into_play_no_out' , 'hit_into_play_score', 'swinging_strike', 'swinging_strike_blocked') then 1.0 else 0.0 end) as float),2) as 'TM Live Swing %'
, round(cast(sum(case when f.pitch_result in ('foul', 'foul_tip', 'hit_into_play' , 'hit_into_play_no_out' , 'hit_into_play_score', 'swinging_strike', 'swinging_strike_blocked') and f.quad_nomid in ('OUA', 'ODA', 'OUI', 'ODI') then 1.0 else 0.0 end) / nullif(sum(case when f.quad_nomid in ('OUA', 'ODA', 'OUI', 'ODI') then 1.0 else 0.0 end),0) as float),2) as 'TM Live Chase %' 
, round(cast(sum(case when f.pitch_result in ('foul', 'foul_tip', 'hit_into_play' , 'hit_into_play_no_out' , 'hit_into_play_score', 'swinging_strike', 'swinging_strike_blocked') and f.quad_nomid in ('UA','UI','DA','DI') then 1.0 else 0.0 end) / nullif(sum(case when f.quad_nomid in ('UA','UI','DA','DI') then 1.0 else 0.0 end),0) as float),2) as 'TM Live inZ Swing %'

, round(cast(sum(case when f.pitch_result in ('foul', 'foul_tip', 'hit_into_play' , 'hit_into_play_no_out' , 'hit_into_play_score') then 1.0 else 0.0 end) / 
         nullif(sum(case when f.pitch_result in ('foul', 'foul_tip', 'hit_into_play' , 'hit_into_play_no_out' , 'hit_into_play_score', 'swinging_strike', 'swinging_strike_blocked') then 1.0 else null end),0) as float),2) as 'Contact %'
, round(cast(avg(case when f.pitch_result in ('swinging_strike' , 'swinging_strike_blocked') then 1.0 else 0.0 end) as float),2) as 'SwStr %'
, round(cast(sum(case when f.pitch_result in ('foul', 'foul_tip', 'hit_into_play' , 'hit_into_play_no_out' , 'hit_into_play_score') and  f.quad_nomid in ('UA','UI','DA','DI') then 1.0 else 0.0 end) / 
         nullif(sum(case when f.pitch_result in ('foul', 'foul_tip', 'hit_into_play' , 'hit_into_play_no_out' , 'hit_into_play_score', 'swinging_strike', 'swinging_strike_blocked') and f.quad_nomid in ('UA','UI','DA','DI') then 1.0 else null end),0) as float),2) as 'InZ Contact %'

from
astros.players a
left join astros.pitches_view f
	on f.batter_id = a.groundcontrol_id 
left join astros.Schedule_View s
	on s.sched_id = f.sched_id and s.sched_type = 'b' and s.description not like ('%machine%')
left join astros.Hits h 
	on f.sched_id = h.sched_id and f.pitch_id = h.pitch_id
left join astros.events e
	on f.cur_event_id = e.event_id and f.sched_id = e.sched_id
join scout.intl_bios b
on b.player_guid = a.intl_guid
left join astros.pitches_grades g on g.sched_id = f.sched_id and g.pitch_id = f.pitch_id

where 
s.level_code in ('int')
and f.release_speed >= 67

and b.signing_elig_year > 2017
and a.groundcontrol_id < 0


group by 
a.groundcontrol_id,
a.first_name , a.last_name
, a.birthdate
, s.sched_date),

TML2 as (
select
concat(a.last_name + ', ' + a.first_name,' ', '(',a.groundcontrol_id,')') as Player,
a.groundcontrol_id as 'GC ID',

 'Total' as 'Date Recorded',
dbo.GetAge_Decimal(a.birthdate, getdate()) as  'Age Recorded'
, sum(cast(e.pa as int)) as PA
, count(1) as 'TM Live Pitches Seen'
, sum(case when f.pitch_result in ('foul', 'foul_tip', 'hit_into_play' , 'hit_into_play_no_out' , 'hit_into_play_score', 'swinging_strike', 'swinging_strike_blocked') then 1 else 0 end) as 'TM Live Swings'
, sum(case when f.pitch_result in ('foul', 'foul_tip', 'hit_into_play' , 'hit_into_play_no_out' , 'hit_into_play_score') then 1 else null end) as 'TM Live Contacts'
, sum(case when f.pitch_result in ('foul', 'foul_tip', 'hit_into_play' , 'hit_into_play_no_out' , 'hit_into_play_score') and h.hit_exit_speed is not null then 1 else 0 end) as 'TM Live Tracked Contacts'
, sum(case when f.pitch_result_id in (12,13,14,18,19,20) then 1 else 0 end) as 'TM Live BIP'
, sum(case when f.pitch_result_id in (12,13,14,18,19,20) and h.hit_exit_speed is not null then 1 else 0 end) as 'TM Live Tracked BIP'
, round(cast(avg(g.swing_decision_grade_2080) as float),2) as 'TM Live Swing Decs'
, round(cast(avg(case when f.pitch_result in ('foul', 'foul_tip', 'hit_into_play' , 'hit_into_play_no_out' , 'hit_into_play_score', 'swinging_strike', 'swinging_strike_blocked') then 1.0 else 0.0 end) as float),2) as 'TM Live Swing %'
, round(cast(sum(case when f.pitch_result in ('foul', 'foul_tip', 'hit_into_play' , 'hit_into_play_no_out' , 'hit_into_play_score', 'swinging_strike', 'swinging_strike_blocked') and f.quad_nomid in ('OUA', 'ODA', 'OUI', 'ODI') then 1.0 else 0.0 end) / nullif(sum(case when f.quad_nomid in ('OUA', 'ODA', 'OUI', 'ODI') then 1.0 else 0.0 end),0) as float),2) as 'TM Live Chase %' 
, round(cast(sum(case when f.pitch_result in ('foul', 'foul_tip', 'hit_into_play' , 'hit_into_play_no_out' , 'hit_into_play_score', 'swinging_strike', 'swinging_strike_blocked') and f.quad_nomid in ('UA','UI','DA','DI') then 1.0 else 0.0 end) / nullif(sum(case when f.quad_nomid in ('UA','UI','DA','DI') then 1.0 else 0.0 end),0) as float),2) as 'TM Live inZ Swing %'

, round(cast(sum(case when f.pitch_result in ('foul', 'foul_tip', 'hit_into_play' , 'hit_into_play_no_out' , 'hit_into_play_score') then 1.0 else 0.0 end) / 
         nullif(sum(case when f.pitch_result in ('foul', 'foul_tip', 'hit_into_play' , 'hit_into_play_no_out' , 'hit_into_play_score', 'swinging_strike', 'swinging_strike_blocked') then 1.0 else null end),0) as float),2) as 'Contact %'
, round(cast(avg(case when f.pitch_result in ('swinging_strike' , 'swinging_strike_blocked') then 1.0 else 0.0 end) as float),2) as 'SwStr %'
, round(cast(sum(case when f.pitch_result in ('foul', 'foul_tip', 'hit_into_play' , 'hit_into_play_no_out' , 'hit_into_play_score') and  f.quad_nomid in ('UA','UI','DA','DI') then 1.0 else 0.0 end) / 
         nullif(sum(case when f.pitch_result in ('foul', 'foul_tip', 'hit_into_play' , 'hit_into_play_no_out' , 'hit_into_play_score', 'swinging_strike', 'swinging_strike_blocked') and f.quad_nomid in ('UA','UI','DA','DI') then 1.0 else null end),0) as float),2) as 'InZ Contact %'

from
astros.players a
left join astros.pitches_view f
	on f.batter_id = a.groundcontrol_id 
left join astros.Schedule_View s
	on s.sched_id = f.sched_id and s.sched_type = 'b' and s.description not like ('%machine%')
left join astros.Hits h 
	on f.sched_id = h.sched_id and f.pitch_id = h.pitch_id
left join astros.events e
	on f.cur_event_id = e.event_id and f.sched_id = e.sched_id
join scout.intl_bios b
on b.player_guid = a.intl_guid
left join astros.pitches_grades g on g.sched_id = f.sched_id and g.pitch_id = f.pitch_id

where 
s.level_code in ('int')
and f.release_speed >= 67

and b.signing_elig_year > 2017
and a.groundcontrol_id < 0


group by 
a.groundcontrol_id,
a.first_name , a.last_name
, a.birthdate)

 select * from TML
 union
 select * from TML2"


TM_Live_data <- dbGetQuery(con, TM_live_query)
TM_Live_data <- mutate_if( TM_Live_data, is.character, str_replace_all, pattern = "ñ", replacement = "n")
TM_Live_data <- mutate_if( TM_Live_data, is.character, str_replace_all, pattern = "É", replacement = "e")
TM_Live_data <- mutate_if( TM_Live_data, is.character, str_replace_all, pattern = "Ó", replacement = "o")
TM_Live_data <- mutate_if( TM_Live_data, is.character, str_replace_all, pattern = "í", replacement = "i")
TM_Live_data <- mutate_if( TM_Live_data, is.character, str_replace_all, pattern = "é", replacement = "e")
TM_Live_data <- mutate_if( TM_Live_data, is.character, str_replace_all, pattern = "Ñ", replacement = "n")
TM_Live_data <- mutate_if( TM_Live_data, is.character, str_replace_all, pattern = "Í", replacement = "i")
TM_Live_data <- mutate_if( TM_Live_data, is.character, str_replace_all, pattern = "Ó", replacement = "o")
TM_Live_data <- mutate_if( TM_Live_data, is.character, str_replace_all, pattern = "Ú", replacement = "u")
TM_Live_data <- mutate_if( TM_Live_data, is.character, str_replace_all, pattern = "Á", replacement = "a")
TM_Live_data <- TM_Live_data %>% mutate_at(vars(Player, `GC ID`, `Date Recorded`), ~as.factor(.))

write_feather(TM_Live_data, "TM_Live_data.feather")


TM_Live_columns <- c(	"PA",	"TM Live Pitches Seen",	"TM Live Swings",	"TM Live Contacts",	"TM Live Tracked Contacts",	"TM Live BIP",	"TM Live Tracked BIP",	"TM Live Swing Decs",	"TM Live Swing %",	"TM Live Chase %",	"TM Live inZ Swing %",	"Contact %",	"SwStr %",	"InZ Contact %")

TM_BP_query <- "
with BP as (

select
concat(a.last_name + ', ' + a.first_name,' ', '(',a.groundcontrol_id,')') as Player,
a.groundcontrol_id as 'GC ID'

, convert(varchar, s.sched_date, 101) as 'Date Recorded'
, dbo.GetAge_Decimal(a.birthdate, s.sched_date) as 'Age Recorded'

, cast(sum(case when h.hit_exit_speed is not null then 1.0 else 0.0 end) as float) as 'TM BP Contacts'
--, sum(case when h.hit_exit_speed >= 90 and h.hit_vertical_angle >= 10 and h.hit_vertical_angle <= 35 then 1 else 0 end) as 'TM BP Barrels' -------------This should not appear in the leaderboard. But it is useful 
-- to calculate overall barrel rate. That's barrels/contacts. 

, round(avg(case when h.hit_useful_exit_speed is not null then h.hit_useful_exit_speed else null end),2) as 'TM BP Avg UEV'
, round(max(case when h.hit_useful_exit_speed is not null then h.hit_useful_exit_speed else null end),2) as 'TM BP Max UEV'
, round(avg(case when h.hit_useful_exit_speed is not null then h.hit_useful_exit_speed else null end) + 0.2*(85-(avg(f.plate_speed))),2) as 'TM BP Avg UEV vs 90mph'
, round(max(case when h.hit_useful_exit_speed is not null then h.hit_useful_exit_speed else null end) + 0.2*(85-(avg(f.plate_speed))),2) as 'TM BP Max UEV vs 90mph'

, round(avg(case when h.hit_exit_speed is not null then h.hit_exit_speed else null end),2) as 'TM BP Avg EV'
, round(max(case when h.hit_exit_speed is not null then h.hit_exit_speed else null end),2) as 'TM BP Max EV'
, round(avg(case when h.hit_exit_speed is not null then h.hit_exit_speed else null end) + 0.2*(85-(avg(f.plate_speed))),2) as 'TM BP Avg EV vs 90mph'
, round(max(case when h.hit_exit_speed is not null then h.hit_exit_speed else null end) + 0.2*(85-(avg(f.plate_speed))),2) as 'TM BP Max EV vs 90mph'


, round(cast(sum(case when h.hit_vertical_angle >= 10 and h.hit_vertical_angle <= 35 then 1.0 else 0.0 end) as float) 
/ nullif(cast(sum(case when h.hit_exit_speed is not null then 1.0 else 0.0 end) as float),0),3) as 'TM BP Launch Rate'	  

, round(cast(sum(case when h.hit_exit_speed >= 90 and h.hit_vertical_angle >= 10 and h.hit_vertical_angle <= 35 then 1.0 else 0.0 end) as float)
/ nullif(cast(sum(case when h.hit_exit_speed is not null then 1.0 else 0.0 end) as float),0),3) as 'TM BP Barrel Rate'

, round(cast(avg(case when h.hit_exit_speed is not null then h.hit_vertical_angle else null end) as float),2)as 'TM BP Avg Launch Angle'
, round(cast(avg(case when h.hit_exit_speed is not null then h.hit_horizontal_angle else null end) as float),2)as 'TM BP Avg Horz Angle'

, round(cast(sum(case when f.pitch_result not like 'foul%' and h.hit_vertical_angle < '10' then 1 else 0 end) / nullif(sum(case when h.hit_exit_speed is not null and f.pitch_result not like 'foul%' then 1.0 else 0 end),0) as float),3)as 'TM BP GB Rate'
, round(cast(sum(case when f.pitch_result not like 'foul%' and h.hit_vertical_angle between '10' and '25' then 1 else 0 end) / nullif(sum(case when h.hit_exit_speed is not null and f.pitch_result not like 'foul%' then 1.0 else 0 end),0) as float),3) as 'TM BP LD Rate'
, round(cast(sum(case when f.pitch_result not like 'foul%' and h.hit_vertical_angle between '25' and '50' then 1 else 0 end) / nullif(sum(case when h.hit_exit_speed is not null and f.pitch_result not like 'foul%' then 1.0 else 0 end),0) as float),3)as 'TM BP Fly Ball Rate'
, round(cast(sum(case when f.pitch_result not like 'foul%' and h.hit_vertical_angle > '50' then 1 else 0 end) / nullif(sum(case when h.hit_exit_speed is not null and f.pitch_result not like 'foul%' then 1.0 else 0 end),0) as float),3) as 'TM BP Pop Up Rate'

, round(cast(sum(case when f.pitch_result not like 'foul%' and h.hit_horizontal_angle > 105 then 1 else 0 end) / nullif(sum(case when h.hit_horizontal_angle is not null and f.pitch_result not like 'foul%' then 1.0 else 0 end),0) as float),3)as 'TM BP LF Rate'
, round(cast(sum(case when f.pitch_result not like 'foul%' and h.hit_horizontal_angle between 75 and 105 then 1 else 0 end) / nullif(sum(case when h.hit_horizontal_angle is not null and f.pitch_result not like 'foul%' then 1.0 else 0 end),0) as float),3)as 'TM BP CF Rate'
, round(cast(sum(case when f.pitch_result not like 'foul%' and h.hit_horizontal_angle < 75 then 1 else 0 end) / nullif(sum(case when h.hit_horizontal_angle is not null and f.pitch_result not like 'foul%' then 1.0 else 0 end),0) as float),3)as 'TM BP RF Rate'




from
astros.players a
left join astros.pitches_view f
	on f.batter_id = a.groundcontrol_id 
left join astros.Schedule_View s
	on s.sched_id = f.sched_id and s.sched_type = 'b' and s.description not like ('%machine%')
join astros.Hits h 
	on f.sched_id = h.sched_id and f.pitch_id = h.pitch_id and f.release_speed < 67
left join astros.events e
	on f.cur_event_id = e.event_id and f.sched_id = e.sched_id
join scout.Intl_Bios b
on b.player_guid = a.intl_guid

where 
s.level_code in ('int')
-- a.groundcontrol_id = -777116


group by 
a.groundcontrol_id,
a.first_name , a.last_name
, a.birthdate
, s.sched_date

) ,
BP2 as (select
concat(a.last_name + ', ' + a.first_name,' ', '(',a.groundcontrol_id,')') as Player,
a.groundcontrol_id as 'GC ID',

'Total' as 'Date Recorded',

dbo.GetAge_Decimal(a.birthdate, getdate()) as 'Age Recorded'

, cast(sum(case when h.hit_exit_speed is not null then 1.0 else 0.0 end) as float) as 'TM BP Contacts'
--, sum(case when h.hit_exit_speed >= 90 and h.hit_vertical_angle >= 10 and h.hit_vertical_angle <= 35 then 1 else 0 end) as 'TM BP Barrels' -------------This should not appear in the leaderboard. But it is useful 
-- to calculate overall barrel rate. That's barrels/contacts. 

, round(avg(case when h.hit_useful_exit_speed is not null then h.hit_useful_exit_speed else null end),2) as 'TM BP Avg UEV'
, round(max(case when h.hit_useful_exit_speed is not null then h.hit_useful_exit_speed else null end),2) as 'TM BP Max UEV'
, round(avg(case when h.hit_useful_exit_speed is not null then h.hit_useful_exit_speed else null end) + 0.2*(85-(avg(f.plate_speed))),2) as 'TM BP Avg UEV vs 90mph'
, round(max(case when h.hit_useful_exit_speed is not null then h.hit_useful_exit_speed else null end) + 0.2*(85-(avg(f.plate_speed))),2) as 'TM BP Max UEV vs 90mph'

, round(avg(case when h.hit_exit_speed is not null then h.hit_exit_speed else null end),2) as 'TM BP Avg EV'
, round(max(case when h.hit_exit_speed is not null then h.hit_exit_speed else null end),2) as 'TM BP Max EV'
, round(avg(case when h.hit_exit_speed is not null then h.hit_exit_speed else null end) + 0.2*(85-(avg(f.plate_speed))),2) as 'TM BP Avg EV vs 90mph'
, round(max(case when h.hit_exit_speed is not null then h.hit_exit_speed else null end) + 0.2*(85-(avg(f.plate_speed))),2) as 'TM BP Max EV vs 90mph'


, round(cast(sum(case when h.hit_vertical_angle >= 10 and h.hit_vertical_angle <= 35 then 1.0 else 0.0 end) as float) 
/ nullif(cast(sum(case when h.hit_exit_speed is not null then 1.0 else 0.0 end) as float),0),3) as 'TM BP Launch Rate'	  

, round(cast(sum(case when h.hit_exit_speed >= 90 and h.hit_vertical_angle >= 10 and h.hit_vertical_angle <= 35 then 1.0 else 0.0 end) as float)
/ nullif(cast(sum(case when h.hit_exit_speed is not null then 1.0 else 0.0 end) as float),0),3) as 'TM BP Barrel Rate'

, round(cast(avg(case when h.hit_exit_speed is not null then h.hit_vertical_angle else null end) as float),2)as 'TM BP Avg Launch Angle'
, round(cast(avg(case when h.hit_exit_speed is not null then h.hit_horizontal_angle else null end) as float),2)as 'TM BP Avg Horz Angle'

, round(cast(sum(case when f.pitch_result not like 'foul%' and h.hit_vertical_angle < '10' then 1 else 0 end) / nullif(sum(case when h.hit_exit_speed is not null and f.pitch_result not like 'foul%' then 1.0 else 0 end),0) as float),3)as 'TM BP GB Rate'
, round(cast(sum(case when f.pitch_result not like 'foul%' and h.hit_vertical_angle between '10' and '25' then 1 else 0 end) / nullif(sum(case when h.hit_exit_speed is not null and f.pitch_result not like 'foul%' then 1.0 else 0 end),0) as float),3) as 'TM BP LD Rate'
, round(cast(sum(case when f.pitch_result not like 'foul%' and h.hit_vertical_angle between '25' and '50' then 1 else 0 end) / nullif(sum(case when h.hit_exit_speed is not null and f.pitch_result not like 'foul%' then 1.0 else 0 end),0) as float),3)as 'TM BP Fly Ball Rate'
, round(cast(sum(case when f.pitch_result not like 'foul%' and h.hit_vertical_angle > '50' then 1 else 0 end) / nullif(sum(case when h.hit_exit_speed is not null and f.pitch_result not like 'foul%' then 1.0 else 0 end),0) as float),3) as 'TM BP Pop Up Rate'

, round(cast(sum(case when f.pitch_result not like 'foul%' and h.hit_horizontal_angle > 105 then 1 else 0 end) / nullif(sum(case when h.hit_horizontal_angle is not null and f.pitch_result not like 'foul%' then 1.0 else 0 end),0) as float),3)as 'TM BP LF Rate'
, round(cast(sum(case when f.pitch_result not like 'foul%' and h.hit_horizontal_angle between 75 and 105 then 1 else 0 end) / nullif(sum(case when h.hit_horizontal_angle is not null and f.pitch_result not like 'foul%' then 1.0 else 0 end),0) as float),3)as 'TM BP CF Rate'
, round(cast(sum(case when f.pitch_result not like 'foul%' and h.hit_horizontal_angle < 75 then 1 else 0 end) / nullif(sum(case when h.hit_horizontal_angle is not null and f.pitch_result not like 'foul%' then 1.0 else 0 end),0) as float),3)as 'TM BP RF Rate'




from
astros.players a
left join astros.pitches_view f
	on f.batter_id = a.groundcontrol_id 
left join astros.Schedule_View s
	on s.sched_id = f.sched_id and s.sched_type = 'b' and s.description not like ('%machine%')
join astros.Hits h 
	on f.sched_id = h.sched_id and f.pitch_id = h.pitch_id and f.release_speed < 67
left join astros.events e
	on f.cur_event_id = e.event_id and f.sched_id = e.sched_id
join scout.Intl_Bios b
on b.player_guid = a.intl_guid

where 
s.level_code in ('int')
-- a.groundcontrol_id = -777116


group by 
a.groundcontrol_id,
a.first_name , a.last_name
, a.birthdate
)

select * from BP
where BP.[TM BP Contacts] > 5

 
 union 
 select * from BP2"
TM_BP_data <- dbGetQuery(con, TM_BP_query)
TM_BP_data <- mutate_if( TM_BP_data, is.character, str_replace_all, pattern = "ñ", replacement = "n")
TM_BP_data <- mutate_if( TM_BP_data, is.character, str_replace_all, pattern = "É", replacement = "e")
TM_BP_data <- mutate_if( TM_BP_data, is.character, str_replace_all, pattern = "Ó", replacement = "o")
TM_BP_data <- mutate_if( TM_BP_data, is.character, str_replace_all, pattern = "í", replacement = "i")
TM_BP_data <- mutate_if( TM_BP_data, is.character, str_replace_all, pattern = "é", replacement = "e")
TM_BP_data <- mutate_if( TM_BP_data, is.character, str_replace_all, pattern = "Ñ", replacement = "n")
TM_BP_data <- mutate_if( TM_BP_data, is.character, str_replace_all, pattern = "Í", replacement = "i")
TM_BP_data <- mutate_if( TM_BP_data, is.character, str_replace_all, pattern = "Ó", replacement = "o")
TM_BP_data <- mutate_if( TM_BP_data, is.character, str_replace_all, pattern = "Ú", replacement = "u")
TM_BP_data <- mutate_if( TM_BP_data, is.character, str_replace_all, pattern = "Á", replacement = "a")
TM_BP_data <- TM_BP_data %>% mutate_at(vars(Player, `GC ID`, `Date Recorded`), ~as.factor(.))

write_feather(TM_BP_data, "TM_BP_data.feather")

TM_BP_columns <- c("TM BP Contacts",	"TM BP Avg UEV",	"TM BP Max UEV",	"TM BP Avg UEV vs 90mph",	"TM BP Max UEV vs 90mph",	"TM BP Avg EV",	"TM BP Max EV",	"TM BP Avg EV vs 90mph",	"TM BP Max EV vs 90mph",	"TM BP Launch Rate",	"TM BP Barrel Rate",	"TM BP Avg Launch Angle",	"TM BP Avg Horz Angle",	"TM BP GB Rate",	"TM BP LD Rate",	"TM BP Fly Ball Rate",	"TM BP Pop Up Rate",	"TM BP LF Rate",	"TM BP CF Rate",	"TM BP RF Rate")

BM_query <- "with BM as (
select 
concat(a.last_name + ', ' + a.first_name,' ', '(',a.groundcontrol_id,')') as Player,
 a.groundcontrol_id as 'GC ID',
 dbo.GetAge_Decimal(a.birthdate, swingdate) as 'Age Recorded'
, convert(varchar, swingdate, 101) as 'Date Recorded',

round(count(actionid),2) as 'BM Swings'
, round(avg(plane_ratio),2) as 'BM Plane'
, round(avg(accel_ratio),2) as 'BM Connection'
, round(avg(peak_power_eff),2) as 'BM Peak Power'
, round(avg(max_gy),2) as 'BM Max Plane RPMs'
, round(avg(max_axy),1) as 'Total Initial Hand Accel'
, round(avg(time_to_contact),2) as 'BM Swing Time' 
, round(avg(min_gx),2) as 'BM DIP'


from blastmotion.Metrics_View_RPM b
 join astros.players a 
on b.player_guid = a.intl_guid
left join scout.Intl_Bios bb
on bb.player_guid = a.intl_guid

where 

 swingplane_cs = 1 
 and b.bad_data = 0 
 and b.bad_accel_data = 0 
 and b.weak_swing = 0
 and bb.signing_elig_year > 2017
 


group by 
a.last_name
, a.first_name
, a.groundcontrol_id,
a.birthdate
, b.swingdate),

BM2 as (
select 
concat(a.last_name + ', ' + a.first_name,' ', '(',a.groundcontrol_id,')') as Player,
 a.groundcontrol_id as 'GC ID',
dbo.GetAge_Decimal(a.birthdate, getdate()) as 'Age Recorded'
---'Cur Age' as 'BM Age Recorded'
,'Total' as 'Date Recorded',

round(count(actionid),2) as 'BM Swings'
, round(avg(plane_ratio),2) as 'BM Plane'
, round(avg(accel_ratio),2) as 'BM Connection'
, round(avg(peak_power_eff),2) as 'BM Peak Power'
, round(avg(max_gy),2) as 'BM Max Plane RPMs'
, round(avg(max_axy),1) as 'Total Initial Hand Accel'
, round(avg(time_to_contact),2) as 'BM Swing Time' 
, round(avg(min_gx),2) as 'BM DIP'


from blastmotion.Metrics_View_RPM b
 join astros.players a 
on b.player_guid = a.intl_guid
left join scout.Intl_Bios bb
on bb.player_guid  = a.intl_guid

where 

 swingplane_cs = 1 
 and b.bad_data = 0 
 and b.bad_accel_data = 0 
 and b.weak_swing = 0
 and bb.signing_elig_year > 2017

group by 
a.last_name
, a.first_name
, a.groundcontrol_id
, a.birthdate)

select * from BM
union
select * from BM2" 



BM_data <- dbGetQuery(con, BM_query)
BM_data <- mutate_if( BM_data, is.character, str_replace_all, pattern = "ñ", replacement = "n")
BM_data <- mutate_if( BM_data, is.character, str_replace_all, pattern = "É", replacement = "e")
BM_data <- mutate_if( BM_data, is.character, str_replace_all, pattern = "Ó", replacement = "o")
BM_data <- mutate_if( BM_data, is.character, str_replace_all, pattern = "í", replacement = "i")
BM_data <- mutate_if( BM_data, is.character, str_replace_all, pattern = "é", replacement = "e")
BM_data <- mutate_if( BM_data, is.character, str_replace_all, pattern = "Ñ", replacement = "n")
BM_data <- mutate_if( BM_data, is.character, str_replace_all, pattern = "Í", replacement = "i")
BM_data <- mutate_if( BM_data, is.character, str_replace_all, pattern = "Ó", replacement = "o")
BM_data <- mutate_if( BM_data, is.character, str_replace_all, pattern = "Ú", replacement = "u")
BM_data <- mutate_if( BM_data, is.character, str_replace_all, pattern = "Á", replacement = "a")
BM_data <- BM_data %>% mutate_at(vars(Player, `GC ID`, `Date Recorded`), ~as.factor(.))

write_feather(BM_data, "BM_data.feather")

BM_columns <- c("BM Swings",	"BM Plane",	"BM Connection",	"BM Peak Power",	"BM Max Plane RPMs",	"Total Initial Hand Accel",	"BM Swing Time",	"BM DIP")

Pitcher_TM_query <- "with TM as (select
concat(a.last_name + ', ' + a.first_name,' ', '(',p.pitcher_id,')') as Player,
--convert(varchar, a.birthdate, 101) as  'Birth',
p.pitcher_id as 'GC ID',
convert(varchar, s.sched_date, 101) as 'Date Recorded',
max(cast(round(DATEDIFF(day, a.birthdate, s.sched_date)/365.25, 1) as decimal(10,1))) as 'Age Recorded', 


count(iif(p.pitch_type = 'FF', p.pitch_type, null)) as 'FF Thrown',
round(avg(iif(p.pitch_type = 'FF', p.release_speed, null)),1)  as 'Avg FF Velo',
round(avg(iif(p.pitch_type = 'FF', p.spin_rate, NULL)), 1) as 'Avg FF Spin Rate',
round(avg(iif(p.pitch_type = 'FF', p.spin_direction, NULL)), 1) as 'Avg FF Spin Direction',
round(avg(iif(p.pitch_type = 'FF', p.inducedvertbreak, null)),1)  as 'Avg FF Induced Vert. Break',
round(avg(iif(p.pitch_type = 'FF', p.horzbreak, null)),1)  as 'Avg FF Horz. Break',
round(avg(iif(p.pitch_type = 'FF', g.stuffrelvel_grade_2080, null)),1)  as 'Avg FF RelVel Grade',
round(avg(iif(p.pitch_type = 'FF', g.stuffrelvelloc_grade_2080, null)),1)  as 'Avg FF RelVelLoc Grade',
round(max(iif(p.pitch_type = 'FF', p.release_z, null)),1)  as 'Max FF Release Z',
round(avg(iif(p.pitch_type = 'FF', p.release_z, null)),1)  as 'Avg FF Release Z',
round(max(iif(p.pitch_type = 'FF', p.release_x, null)),1)  as 'Max FF Release X',
round(avg(iif(p.pitch_type = 'FF', p.release_x, null)),1)  as 'Avg FF Release X',
round(avg(iif(p.pitch_type = 'FF', p.extension, null)),1)  as 'Avg FF Extension',
round(cast(sum(case when pitch_type in ('FF') and (plate_x between -1 and 1) and (plate_z between 1.59 and 3.38) then 1 else 0 end) / nullif(sum(case when pitch_type in ('FF') and plate_x is not null then 1.0 else 0.0 end),0) as float), 2) as 'FF Zone Pct',
round(cast(sum(case when pitch_type in ('FF') and pitch_result in ('swinging_strike','swinging_strike_blocked', 'foul_tip') then 1.0 else 0.0 end) / nullif(sum(case when pitch_type in ('FF') then 1.0 else 0.0 end),0) as float), 2) as 'FF Whiff Rate'
, round(cast(sum(case when pitch_type in ('FF') and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'foul_tip') then 1.0 else 0.0 end) / nullif((sum(case when pitch_type in ('FF') and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'hit_into_play', 'hit_into_play_no_out', 'hit_into_play_score', 'foul', 'foul tip') then 1.0 else 0.0 end)),0) as float), 2) as 'FF ISO Whiff Rate'
, round(cast(sum(case when pitch_type in ('FF') and (plate_x between -1 and 1) and (plate_z between 1.59 and 3.38) and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'foul_tip') then 1.0 else 0.0 end) / nullif((sum(case when pitch_type in ('FF') and (plate_x between -1 and 1) and (plate_z between 1.59 and 3.38) and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'hit_into_play', 'hit_into_play_no_out', 'hit_into_play_score', 'foul', 'foul tip') then 1.0 else 0.0 end)),0) as float), 2) as 'FF Zone ISO Whiff Rate'


,count(iif(p.pitch_type = 'FT', p.pitch_type, null)) as 'FT Thrown',
round(avg(iif(p.pitch_type = 'FT', p.release_speed, null)),1)  as 'Avg FT Velo',
round(avg(iif(p.pitch_type = 'FT', p.spin_rate, NULL)), 1) as 'Avg FT Spin Rate',
round(avg(iif(p.pitch_type = 'FT', p.spin_direction, NULL)), 1) as 'Avg FT Spin Direction',
round(avg(iif(p.pitch_type = 'FT', p.inducedvertbreak, null)),1)  as 'Avg FT Induced Vert. Break',
round(avg(iif(p.pitch_type = 'FT', p.horzbreak, null)),1)  as 'Avg FT Horz. Break',
round(avg(iif(p.pitch_type = 'FT', g.stuffrelvel_grade_2080, null)),1)  as 'Avg FT RelVel Grade',
round(avg(iif(p.pitch_type = 'FT', g.stuffrelvelloc_grade_2080, null)),1)  as 'Avg FT RelVelLoc Grade',
round(max(iif(p.pitch_type = 'FT', p.release_z, null)),1)  as 'Max FT Release Z',
round(avg(iif(p.pitch_type = 'FT', p.release_z, null)),1)  as 'Avg FT Release Z',
round(max(iif(p.pitch_type = 'FT', p.release_x, null)),1)  as 'Max FT Release X',
round(avg(iif(p.pitch_type = 'FT', p.release_x, null)),1)  as 'Avg FT Release X',
round(avg(iif(p.pitch_type = 'FT', p.extension, null)),1)  as 'Avg FT Extension',
round(cast(sum(case when pitch_type in ('FT') and (plate_x between -1 and 1) and (plate_z between 1.59 and 3.38) then 1 else 0 end) / nullif(sum(case when pitch_type in ('FT') and plate_x is not null then 1.0 else 0.0 end),0) as float), 2) as 'FT Zone Pct',
round(cast(sum(case when pitch_type in ('FT') and pitch_result in ('swinging_strike','swinging_strike_blocked', 'foul_tip') then 1.0 else 0.0 end) / nullif(sum(case when pitch_type in ('FT') then 1.0 else 0.0 end),0) as float), 2) as 'FT Whiff Rate'
, round(cast(sum(case when pitch_type in ('FT') and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'foul_tip') then 1.0 else 0.0 end) / nullif((sum(case when pitch_type in ('FT') and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'hit_into_play', 'hit_into_play_no_out', 'hit_into_play_score', 'foul', 'foul tip') then 1.0 else 0.0 end)),0) as float), 2) as 'FT ISO Whiff Rate'
, round(cast(sum(case when pitch_type in ('FT') and (plate_x between -1 and 1) and (plate_z between 1.59 and 3.38) and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'foul_tip') then 1.0 else 0.0 end) / nullif((sum(case when pitch_type in ('FT') and (plate_x between -1 and 1) and (plate_z between 1.59 and 3.38) and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'hit_into_play', 'hit_into_play_no_out', 'hit_into_play_score', 'foul', 'foul tip') then 1.0 else 0.0 end)),0) as float), 2) as 'FT Zone ISO Whiff Rate'


,count(iif(p.pitch_type = 'FC', p.pitch_type, null)) as 'FC Thrown',
round(avg(iif(p.pitch_type = 'FC', p.release_speed, null)),1)  as 'Avg FC Velo',
round(avg(iif(p.pitch_type = 'FC', p.spin_rate, NULL)), 1) as 'Avg FC Spin Rate',
round(avg(iif(p.pitch_type = 'FC', p.spin_direction, NULL)), 1) as 'Avg FC Spin Direction',
round(avg(iif(p.pitch_type = 'FC', p.inducedvertbreak, null)),1)  as 'Avg FC Induced Vert. Break',
round(avg(iif(p.pitch_type = 'FC', p.horzbreak, null)),1)  as 'Avg FC Horz. Break',
round(avg(iif(p.pitch_type = 'FC', g.stuffrelvel_grade_2080, null)),1)  as 'Avg FC RelVel Grade',
round(avg(iif(p.pitch_type = 'FC', g.stuffrelvelloc_grade_2080, null)),1)  as 'Avg FC RelVelLoc Grade',
round(max(iif(p.pitch_type = 'FC', p.release_z, null)),1)  as 'Max FC Release Z',
round(avg(iif(p.pitch_type = 'FC', p.release_z, null)),1)  as 'Avg FC Release Z',
round(max(iif(p.pitch_type = 'FC', p.release_x, null)),1)  as 'Max FC Release X',
round(avg(iif(p.pitch_type = 'FC', p.release_x, null)),1)  as 'Avg FC Release X',
round(avg(iif(p.pitch_type = 'FC', p.extension, null)),1)  as 'Avg FC Extension',
round(cast(sum(case when pitch_type in ('FC') and (plate_x between -1 and 1) and (plate_z between 1.59 and 3.38) then 1 else 0 end) / nullif(sum(case when pitch_type in ('FC') and plate_x is not null then 1.0 else 0.0 end),0) as float), 2) as 'FC Zone Pct',
round(cast(sum(case when pitch_type in ('FC') and pitch_result in ('swinging_strike','swinging_strike_blocked', 'foul_tip') then 1.0 else 0.0 end) / nullif(sum(case when pitch_type in ('FC') then 1.0 else 0.0 end),0) as float), 2) as 'FC Whiff Rate'
, round(cast(sum(case when pitch_type in ('FC') and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'foul_tip') then 1.0 else 0.0 end) / nullif((sum(case when pitch_type in ('FC') and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'hit_into_play', 'hit_into_play_no_out', 'hit_into_play_score', 'foul', 'foul tip') then 1.0 else 0.0 end)),0) as float), 2) as 'FC ISO Whiff Rate'
, round(cast(sum(case when pitch_type in ('FC') and (plate_x between -1 and 1) and (plate_z between 1.59 and 3.38) and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'foul_tip') then 1.0 else 0.0 end) / nullif((sum(case when pitch_type in ('FC') and (plate_x between -1 and 1) and (plate_z between 1.59 and 3.38) and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'hit_into_play', 'hit_into_play_no_out', 'hit_into_play_score', 'foul', 'foul tip') then 1.0 else 0.0 end)),0) as float), 2) as 'FC Zone ISO Whiff Rate'


,count(iif(p.pitch_type = 'CU', p.pitch_type, null)) as 'CU Thrown',
round(avg(iif(p.pitch_type = 'CU', p.release_speed, null)),1)  as 'Avg CU Velo',
round(avg(iif(p.pitch_type = 'CU', p.spin_rate, NULL)), 1) as 'Avg CU Spin Rate',
round(avg(iif(p.pitch_type = 'CU', p.spin_direction, NULL)), 1) as 'Avg CU Spin Direction',
round(avg(iif(p.pitch_type = 'CU', p.inducedvertbreak, null)),1)  as 'Avg CU Induced Vert. Break',
round(avg(iif(p.pitch_type = 'CU', p.horzbreak, null)),1)  as 'Avg CU Horz. Break',
round(avg(iif(p.pitch_type = 'CU', g.stuffrelvel_grade_2080, null)),1)  as 'Avg CU RelVel Grade',
round(avg(iif(p.pitch_type = 'CU', g.stuffrelvelloc_grade_2080, null)),1)  as 'Avg CU RelVelLoc Grade',
round(max(iif(p.pitch_type = 'CU', p.release_z, null)),1)  as 'Max CU Release Z',
round(avg(iif(p.pitch_type = 'CU', p.release_z, null)),1)  as 'Avg CU Release Z',
round(max(iif(p.pitch_type = 'CU', p.release_x, null)),1)  as 'Max CU Release X',
round(avg(iif(p.pitch_type = 'CU', p.release_x, null)),1)  as 'Avg CU Release X',
round(avg(iif(p.pitch_type = 'CU', p.extension, null)),1)  as 'Avg CU Extension',
round(cast(sum(case when pitch_type in ('CU') and (plate_x between -1 and 1) and (plate_z between 1.59 and 3.38) then 1 else 0 end) / nullif(sum(case when pitch_type in ('CU') and plate_x is not null then 1.0 else 0.0 end),0) as float), 2) as 'CU Zone Pct',
round(cast(sum(case when pitch_type in ('CU') and pitch_result in ('swinging_strike','swinging_strike_blocked', 'foul_tip') then 1.0 else 0.0 end) / nullif(sum(case when pitch_type in ('CU') then 1.0 else 0.0 end),0) as float), 2) as 'CU Whiff Rate'
, round(cast(sum(case when pitch_type in ('CU') and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'foul_tip') then 1.0 else 0.0 end) / nullif((sum(case when pitch_type in ('CU') and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'hit_into_play', 'hit_into_play_no_out', 'hit_into_play_score', 'foul', 'foul tip') then 1.0 else 0.0 end)),0) as float), 2) as 'CU ISO Whiff Rate'
, round(cast(sum(case when pitch_type in ('CU') and (plate_x between -1 and 1) and (plate_z between 1.59 and 3.38) and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'foul_tip') then 1.0 else 0.0 end) / nullif((sum(case when pitch_type in ('CU') and (plate_x between -1 and 1) and (plate_z between 1.59 and 3.38) and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'hit_into_play', 'hit_into_play_no_out', 'hit_into_play_score', 'foul', 'foul tip') then 1.0 else 0.0 end)),0) as float), 2) as 'CU Zone ISO Whiff Rate'



,count(iif(p.pitch_type = 'SL', p.pitch_type, null)) as 'SL Thrown',
round(avg(iif(p.pitch_type = 'SL', p.release_speed, null)),1)  as 'Avg SL Velo',
round(avg(iif(p.pitch_type = 'SL', p.spin_rate, NULL)), 1) as 'Avg SL Spin Rate',
round(avg(iif(p.pitch_type = 'SL', p.spin_direction, NULL)), 1) as 'Avg SL Spin Direction',
round(avg(iif(p.pitch_type = 'SL', p.inducedvertbreak, null)),1)  as 'Avg SL Induced Vert. Break',
round(avg(iif(p.pitch_type = 'SL', p.horzbreak, null)),1)  as 'Avg SL Horz. Break',
round(avg(iif(p.pitch_type = 'SL', g.stuffrelvel_grade_2080, null)),1)  as 'Avg SL RelVel Grade',
round(avg(iif(p.pitch_type = 'SL', g.stuffrelvelloc_grade_2080, null)),1)  as 'Avg SL RelVelLoc Grade',
round(max(iif(p.pitch_type = 'SL', p.release_z, null)),1)  as 'Max SL Release Z',
round(avg(iif(p.pitch_type = 'SL', p.release_z, null)),1)  as 'Avg SL Release Z',
round(max(iif(p.pitch_type = 'SL', p.release_x, null)),1)  as 'Max SL Release X',
round(avg(iif(p.pitch_type = 'SL', p.release_x, null)),1)  as 'Avg SL Release X',
round(avg(iif(p.pitch_type = 'SL', p.extension, null)),1)  as 'Avg SL Extension',
round(cast(sum(case when pitch_type in ('SL') and (plate_x between -1 and 1) and (plate_z between 1.59 and 3.38) then 1 else 0 end) / nullif(sum(case when pitch_type in ('SL') and plate_x is not null then 1.0 else 0.0 end),0) as float), 2) as 'SL Zone Pct',
round(cast(sum(case when pitch_type in ('SL') and pitch_result in ('swinging_strike','swinging_strike_blocked', 'foul_tip') then 1.0 else 0.0 end) / nullif(sum(case when pitch_type in ('SL') then 1.0 else 0.0 end),0) as float), 2) as 'SL Whiff Rate'
, round(cast(sum(case when pitch_type in ('SL') and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'foul_tip') then 1.0 else 0.0 end) / nullif((sum(case when pitch_type in ('SL') and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'hit_into_play', 'hit_into_play_no_out', 'hit_into_play_score', 'foul', 'foul tip') then 1.0 else 0.0 end)),0) as float), 2) as 'SL ISO Whiff Rate'
, round(cast(sum(case when pitch_type in ('SL') and (plate_x between -1 and 1) and (plate_z between 1.59 and 3.38) and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'foul_tip') then 1.0 else 0.0 end) / nullif((sum(case when pitch_type in ('SL') and (plate_x between -1 and 1) and (plate_z between 1.59 and 3.38) and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'hit_into_play', 'hit_into_play_no_out', 'hit_into_play_score', 'foul', 'foul tip') then 1.0 else 0.0 end)),0) as float), 2) as 'SL Zone ISO Whiff Rate'


,count(iif(p.pitch_type = 'CH', p.pitch_type, null)) as 'CH Thrown',
round(avg(iif(p.pitch_type = 'CH', p.release_speed, null)),1)  as 'Avg CH Velo',
round(avg(iif(p.pitch_type = 'CH', p.spin_rate, NULL)), 1) as 'Avg CH Spin Rate',
round(avg(iif(p.pitch_type = 'CH', p.spin_direction, NULL)), 1) as 'Avg CH Spin Direction',
round(avg(iif(p.pitch_type = 'CH', p.inducedvertbreak, null)),1)  as 'Avg CH Induced Vert. Break',
round(avg(iif(p.pitch_type = 'CH', p.horzbreak, null)),1)  as 'Avg CH Horz. Break',
round(avg(iif(p.pitch_type = 'CH', g.stuffrelvel_grade_2080, null)),1)  as 'Avg CH RelVel Grade',
round(avg(iif(p.pitch_type = 'CH', g.stuffrelvelloc_grade_2080, null)),1)  as 'Avg CH RelVelLoc Grade',
round(max(iif(p.pitch_type = 'CH', p.release_z, null)),1)  as 'Max CH Release Z',
round(avg(iif(p.pitch_type = 'CH', p.release_z, null)),1)  as 'Avg CH Release Z',
round(max(iif(p.pitch_type = 'CH', p.release_x, null)),1)  as 'Max CH Release X',
round(avg(iif(p.pitch_type = 'CH', p.release_x, null)),1)  as 'Avg CH Release X',
round(avg(iif(p.pitch_type = 'CH', p.extension, null)),1)  as 'Avg CH Extension',
round(cast(sum(case when pitch_type in ('CH') and (plate_x between -1 and 1) and (plate_z between 1.59 and 3.38) then 1 else 0 end) / nullif(sum(case when pitch_type in ('CH') and plate_x is not null then 1.0 else 0.0 end),0) as float), 2) as 'CH Zone Pct',
round(cast(sum(case when pitch_type in ('CH') and pitch_result in ('swinging_strike','swinging_strike_blocked', 'foul_tip') then 1.0 else 0.0 end) / nullif(sum(case when pitch_type in ('CH') then 1.0 else 0.0 end),0) as float), 2) as 'CH Whiff Rate'
, round(cast(sum(case when pitch_type in ('CH') and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'foul_tip') then 1.0 else 0.0 end) / nullif((sum(case when pitch_type in ('CH') and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'hit_into_play', 'hit_into_play_no_out', 'hit_into_play_score', 'foul', 'foul tip') then 1.0 else 0.0 end)),0) as float), 2) as 'CH ISO Whiff Rate'
, round(cast(sum(case when pitch_type in ('CH') and (plate_x between -1 and 1) and (plate_z between 1.59 and 3.38) and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'foul_tip') then 1.0 else 0.0 end) / nullif((sum(case when pitch_type in ('CH') and (plate_x between -1 and 1) and (plate_z between 1.59 and 3.38) and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'hit_into_play', 'hit_into_play_no_out', 'hit_into_play_score', 'foul', 'foul tip') then 1.0 else 0.0 end)),0) as float), 2) as 'CH Zone ISO Whiff Rate'




,count(iif(p.pitch_type = 'FS', p.pitch_type, null)) as 'FS Thrown',
round(avg(iif(p.pitch_type = 'FS', p.release_speed, null)),1)  as 'Avg FS Velo',
round(avg(iif(p.pitch_type = 'FS', p.spin_rate, NULL)), 1) as 'Avg FS Spin Rate',
round(avg(iif(p.pitch_type = 'FS', p.spin_direction, NULL)), 1) as 'Avg FS Spin Direction',
round(avg(iif(p.pitch_type = 'FS', p.inducedvertbreak, null)),1)  as 'Avg FS Induced Vert. Break',
round(avg(iif(p.pitch_type = 'FS', p.horzbreak, null)),1)  as 'Avg FS Horz. Break',
round(avg(iif(p.pitch_type = 'FS', g.stuffrelvel_grade_2080, null)),1)  as 'Avg FS RelVel Grade',
round(avg(iif(p.pitch_type = 'FS', g.stuffrelvelloc_grade_2080, null)),1)  as 'Avg FS RelVelLoc Grade',
round(max(iif(p.pitch_type = 'FS', p.release_z, null)),1)  as 'Max FS Release Z',
round(avg(iif(p.pitch_type = 'FS', p.release_z, null)),1)  as 'Avg FS Release Z',
round(max(iif(p.pitch_type = 'FS', p.release_x, null)),1)  as 'Max FS Release X',
round(avg(iif(p.pitch_type = 'FS', p.release_x, null)),1)  as 'Avg FS Release X',
round(avg(iif(p.pitch_type = 'FS', p.extension, null)),1)  as 'Avg FS Extension',
round(cast(sum(case when pitch_type in ('FS') and (plate_x between -1 and 1) and (plate_z between 1.59 and 3.38) then 1 else 0 end) / nullif(sum(case when pitch_type in ('FS') and plate_x is not null then 1.0 else 0.0 end),0) as float), 2) as 'FS Zone Pct',
round(cast(sum(case when pitch_type in ('FS') and pitch_result in ('swinging_strike','swinging_strike_blocked', 'foul_tip') then 1.0 else 0.0 end) / nullif(sum(case when pitch_type in ('FS') then 1.0 else 0.0 end),0) as float), 2) as 'FS Whiff Rate'
, round(cast(sum(case when pitch_type in ('FS') and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'foul_tip') then 1.0 else 0.0 end) / nullif((sum(case when pitch_type in ('FS') and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'hit_into_play', 'hit_into_play_no_out', 'hit_into_play_score', 'foul', 'foul tip') then 1.0 else 0.0 end)),0) as float), 2) as 'FS ISO Whiff Rate'
, round(cast(sum(case when pitch_type in ('FS') and (plate_x between -1 and 1) and (plate_z between 1.59 and 3.38) and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'foul_tip') then 1.0 else 0.0 end) / nullif((sum(case when pitch_type in ('FS') and (plate_x between -1 and 1) and (plate_z between 1.59 and 3.38) and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'hit_into_play', 'hit_into_play_no_out', 'hit_into_play_score', 'foul', 'foul tip') then 1.0 else 0.0 end)),0) as float), 2) as 'FS Zone ISO Whiff Rate'


from Astros.Schedule_View s
left join Astros.Pitches_View p
on p.sched_id = s.sched_id
and s.sched_type = 'b'
left join Astros.Pitches_Grades g
on g.sched_id = p.sched_id and g.pitch_id = p.pitch_id
left join Astros.Players a
on a.groundcontrol_id = p.pitcher_id
left join Trackman.Game_Data t
		on t.gameid = s.description and t.date = s.sched_date and s.sched_type = 'b'
		--and t.stadium in ('Dominican Republic - Houston', 'DRAstros', 'DRHouston', 'DRHouston1NN', 'DRHouston2NN')
		left join scout.Intl_Bios bios
		on bios.player_guid = a.intl_guid
		
where s.sched_type = 'b'
and s.year >= 2017
and p.pitcher_id < 0 
and p.pitcher_id is not null
and p.release_speed >= 65
and p.pitch_type is not null
and bios.signing_elig_year > 2017


---and s.level_code in ('mlb','aaa','aax','afa','afx','asx','rok')
group by s.year, s.level_code, p.pitcher_id, a.last_name, a.first_name, s.sched_date

),
TM2 as (
select
concat(a.last_name + ', ' + a.first_name,' ', '(',p.pitcher_id,')') as Player,

p.pitcher_id as 'GC ID',
'Total' as 'Date Recorded',
dbo.GetAge_Decimal(a.birthdate, getdate()) as 'Age Recorded',
count(iif(p.pitch_type = 'FF', p.pitch_type, null)) as 'FF Thrown',
round(avg(iif(p.pitch_type = 'FF', p.release_speed, null)),1)  as 'Avg FF Velo',
round(avg(iif(p.pitch_type = 'FF', p.spin_rate, NULL)), 1) as 'Avg FF Spin Rate',



round(avg(iif(p.pitch_type = 'FF', p.spin_direction, NULL)), 1) as 'Avg FF Spin Direction',
round(avg(iif(p.pitch_type = 'FF', p.inducedvertbreak, null)),1)  as 'Avg FF Induced Vert. Break',
round(avg(iif(p.pitch_type = 'FF', p.horzbreak, null)),1)  as 'Avg FF Horz. Break',
round(avg(iif(p.pitch_type = 'FF', g.stuffrelvel_grade_2080, null)),1)  as 'Avg FF RelVel Grade',
round(avg(iif(p.pitch_type = 'FF', g.stuffrelvelloc_grade_2080, null)),1)  as 'Avg FF RelVelLoc Grade',
round(max(iif(p.pitch_type = 'FF', p.release_z, null)),1)  as 'Max FF Release Z',
round(avg(iif(p.pitch_type = 'FF', p.release_z, null)),1)  as 'Avg FF Release Z',
round(max(iif(p.pitch_type = 'FF', p.release_x, null)),1)  as 'Max FF Release X',
round(avg(iif(p.pitch_type = 'FF', p.release_x, null)),1)  as 'Avg FF Release X',
round(avg(iif(p.pitch_type = 'FF', p.extension, null)),1)  as 'Avg FF Extension',
round(cast(sum(case when pitch_type in ('FF') and (plate_x between -1 and 1) and (plate_z between 1.59 and 3.38) then 1 else 0 end) / nullif(sum(case when pitch_type in ('FF') and plate_x is not null then 1.0 else 0.0 end),0) as float), 2) as 'FF Zone Pct',
round(cast(sum(case when pitch_type in ('FF') and pitch_result in ('swinging_strike','swinging_strike_blocked', 'foul_tip') then 1.0 else 0.0 end) / nullif(sum(case when pitch_type in ('FF') then 1.0 else 0.0 end),0) as float), 2) as 'FF Whiff Rate'
, round(cast(sum(case when pitch_type in ('FF') and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'foul_tip') then 1.0 else 0.0 end) / nullif((sum(case when pitch_type in ('FF') and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'hit_into_play', 'hit_into_play_no_out', 'hit_into_play_score', 'foul', 'foul tip') then 1.0 else 0.0 end)),0) as float), 2) as 'FF ISO Whiff Rate'
, round(cast(sum(case when pitch_type in ('FF') and (plate_x between -1 and 1) and (plate_z between 1.59 and 3.38) and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'foul_tip') then 1.0 else 0.0 end) / nullif((sum(case when pitch_type in ('FF') and (plate_x between -1 and 1) and (plate_z between 1.59 and 3.38) and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'hit_into_play', 'hit_into_play_no_out', 'hit_into_play_score', 'foul', 'foul tip') then 1.0 else 0.0 end)),0) as float), 2) as 'FF Zone ISO Whiff Rate'


,count(iif(p.pitch_type = 'FT', p.pitch_type, null)) as 'FT Thrown',
round(avg(iif(p.pitch_type = 'FT', p.release_speed, null)),1)  as 'Avg FT Velo',
round(avg(iif(p.pitch_type = 'FT', p.spin_rate, NULL)), 1) as 'Avg FT Spin Rate',
round(avg(iif(p.pitch_type = 'FT', p.spin_direction, NULL)), 1) as 'Avg FT Spin Direction',
round(avg(iif(p.pitch_type = 'FT', p.inducedvertbreak, null)),1)  as 'Avg FT Induced Vert. Break',
round(avg(iif(p.pitch_type = 'FT', p.horzbreak, null)),1)  as 'Avg FT Horz. Break',
round(avg(iif(p.pitch_type = 'FT', g.stuffrelvel_grade_2080, null)),1)  as 'Avg FT RelVel Grade',
round(avg(iif(p.pitch_type = 'FT', g.stuffrelvelloc_grade_2080, null)),1)  as 'Avg FT RelVelLoc Grade',
round(max(iif(p.pitch_type = 'FT', p.release_z, null)),1)  as 'Max FT Release Z',
round(avg(iif(p.pitch_type = 'FT', p.release_z, null)),1)  as 'Avg FT Release Z',
round(max(iif(p.pitch_type = 'FT', p.release_x, null)),1)  as 'Max FT Release X',
round(avg(iif(p.pitch_type = 'FT', p.release_x, null)),1)  as 'Avg FT Release X',
round(avg(iif(p.pitch_type = 'FT', p.extension, null)),1)  as 'Avg FT Extension',
round(cast(sum(case when pitch_type in ('FT') and (plate_x between -1 and 1) and (plate_z between 1.59 and 3.38) then 1 else 0 end) / nullif(sum(case when pitch_type in ('FT') and plate_x is not null then 1.0 else 0.0 end),0) as float), 2) as 'FT Zone Pct',
round(cast(sum(case when pitch_type in ('FT') and pitch_result in ('swinging_strike','swinging_strike_blocked', 'foul_tip') then 1.0 else 0.0 end) / nullif(sum(case when pitch_type in ('FT') then 1.0 else 0.0 end),0) as float), 2) as 'FT Whiff Rate'
, round(cast(sum(case when pitch_type in ('FT') and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'foul_tip') then 1.0 else 0.0 end) / nullif((sum(case when pitch_type in ('FT') and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'hit_into_play', 'hit_into_play_no_out', 'hit_into_play_score', 'foul', 'foul tip') then 1.0 else 0.0 end)),0) as float), 2) as 'FT ISO Whiff Rate'
, round(cast(sum(case when pitch_type in ('FT') and (plate_x between -1 and 1) and (plate_z between 1.59 and 3.38) and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'foul_tip') then 1.0 else 0.0 end) / nullif((sum(case when pitch_type in ('FT') and (plate_x between -1 and 1) and (plate_z between 1.59 and 3.38) and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'hit_into_play', 'hit_into_play_no_out', 'hit_into_play_score', 'foul', 'foul tip') then 1.0 else 0.0 end)),0) as float), 2) as 'FT Zone ISO Whiff Rate'


,count(iif(p.pitch_type = 'FC', p.pitch_type, null)) as 'FC Thrown',
round(avg(iif(p.pitch_type = 'FC', p.release_speed, null)),1)  as 'Avg FC Velo',
round(avg(iif(p.pitch_type = 'FC', p.spin_rate, NULL)), 1) as 'Avg FC Spin Rate',
round(avg(iif(p.pitch_type = 'FC', p.spin_direction, NULL)), 1) as 'Avg FC Spin Direction',
round(avg(iif(p.pitch_type = 'FC', p.inducedvertbreak, null)),1)  as 'Avg FC Induced Vert. Break',
round(avg(iif(p.pitch_type = 'FC', p.horzbreak, null)),1)  as 'Avg FC Horz. Break',
round(avg(iif(p.pitch_type = 'FC', g.stuffrelvel_grade_2080, null)),1)  as 'Avg FC RelVel Grade',
round(avg(iif(p.pitch_type = 'FC', g.stuffrelvelloc_grade_2080, null)),1)  as 'Avg FC RelVelLoc Grade',
round(max(iif(p.pitch_type = 'FC', p.release_z, null)),1)  as 'Max FC Release Z',
round(avg(iif(p.pitch_type = 'FC', p.release_z, null)),1)  as 'Avg FC Release Z',
round(max(iif(p.pitch_type = 'FC', p.release_x, null)),1)  as 'Max FC Release X',
round(avg(iif(p.pitch_type = 'FC', p.release_x, null)),1)  as 'Avg FC Release X',
round(avg(iif(p.pitch_type = 'FC', p.extension, null)),1)  as 'Avg FC Extension',
round(cast(sum(case when pitch_type in ('FC') and (plate_x between -1 and 1) and (plate_z between 1.59 and 3.38) then 1 else 0 end) / nullif(sum(case when pitch_type in ('FC') and plate_x is not null then 1.0 else 0.0 end),0) as float), 2) as 'FC Zone Pct',
round(cast(sum(case when pitch_type in ('FC') and pitch_result in ('swinging_strike','swinging_strike_blocked', 'foul_tip') then 1.0 else 0.0 end) / nullif(sum(case when pitch_type in ('FC') then 1.0 else 0.0 end),0) as float), 2) as 'FC Whiff Rate'
, round(cast(sum(case when pitch_type in ('FC') and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'foul_tip') then 1.0 else 0.0 end) / nullif((sum(case when pitch_type in ('FC') and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'hit_into_play', 'hit_into_play_no_out', 'hit_into_play_score', 'foul', 'foul tip') then 1.0 else 0.0 end)),0) as float), 2) as 'FC ISO Whiff Rate'
, round(cast(sum(case when pitch_type in ('FC') and (plate_x between -1 and 1) and (plate_z between 1.59 and 3.38) and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'foul_tip') then 1.0 else 0.0 end) / nullif((sum(case when pitch_type in ('FC') and (plate_x between -1 and 1) and (plate_z between 1.59 and 3.38) and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'hit_into_play', 'hit_into_play_no_out', 'hit_into_play_score', 'foul', 'foul tip') then 1.0 else 0.0 end)),0) as float), 2) as 'FC Zone ISO Whiff Rate'


,count(iif(p.pitch_type = 'CU', p.pitch_type, null)) as 'CU Thrown',
round(avg(iif(p.pitch_type = 'CU', p.release_speed, null)),1)  as 'Avg CU Velo',
round(avg(iif(p.pitch_type = 'CU', p.spin_rate, NULL)), 1) as 'Avg CU Spin Rate',
round(avg(iif(p.pitch_type = 'CU', p.spin_direction, NULL)), 1) as 'Avg CU Spin Direction',
round(avg(iif(p.pitch_type = 'CU', p.inducedvertbreak, null)),1)  as 'Avg CU Induced Vert. Break',
round(avg(iif(p.pitch_type = 'CU', p.horzbreak, null)),1)  as 'Avg CU Horz. Break',
round(avg(iif(p.pitch_type = 'CU', g.stuffrelvel_grade_2080, null)),1)  as 'Avg CU RelVel Grade',
round(avg(iif(p.pitch_type = 'CU', g.stuffrelvelloc_grade_2080, null)),1)  as 'Avg CU RelVelLoc Grade',
round(max(iif(p.pitch_type = 'CU', p.release_z, null)),1)  as 'Max CU Release Z',
round(avg(iif(p.pitch_type = 'CU', p.release_z, null)),1)  as 'Avg CU Release Z',
round(max(iif(p.pitch_type = 'CU', p.release_x, null)),1)  as 'Max CU Release X',
round(avg(iif(p.pitch_type = 'CU', p.release_x, null)),1)  as 'Avg CU Release X',
round(avg(iif(p.pitch_type = 'CU', p.extension, null)),1)  as 'Avg CU Extension',
round(cast(sum(case when pitch_type in ('CU') and (plate_x between -1 and 1) and (plate_z between 1.59 and 3.38) then 1 else 0 end) / nullif(sum(case when pitch_type in ('CU') and plate_x is not null then 1.0 else 0.0 end),0) as float), 2) as 'CU Zone Pct',
round(cast(sum(case when pitch_type in ('CU') and pitch_result in ('swinging_strike','swinging_strike_blocked', 'foul_tip') then 1.0 else 0.0 end) / nullif(sum(case when pitch_type in ('CU') then 1.0 else 0.0 end),0) as float), 2) as 'CU Whiff Rate'
, round(cast(sum(case when pitch_type in ('CU') and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'foul_tip') then 1.0 else 0.0 end) / nullif((sum(case when pitch_type in ('CU') and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'hit_into_play', 'hit_into_play_no_out', 'hit_into_play_score', 'foul', 'foul tip') then 1.0 else 0.0 end)),0) as float), 2) as 'CU ISO Whiff Rate'
, round(cast(sum(case when pitch_type in ('CU') and (plate_x between -1 and 1) and (plate_z between 1.59 and 3.38) and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'foul_tip') then 1.0 else 0.0 end) / nullif((sum(case when pitch_type in ('CU') and (plate_x between -1 and 1) and (plate_z between 1.59 and 3.38) and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'hit_into_play', 'hit_into_play_no_out', 'hit_into_play_score', 'foul', 'foul tip') then 1.0 else 0.0 end)),0) as float), 2) as 'CU Zone ISO Whiff Rate'



,count(iif(p.pitch_type = 'SL', p.pitch_type, null)) as 'SL Thrown',
round(avg(iif(p.pitch_type = 'SL', p.release_speed, null)),1)  as 'Avg SL Velo',
round(avg(iif(p.pitch_type = 'SL', p.spin_rate, NULL)), 1) as 'Avg SL Spin Rate',
round(avg(iif(p.pitch_type = 'SL', p.spin_direction, NULL)), 1) as 'Avg SL Spin Direction',
round(avg(iif(p.pitch_type = 'SL', p.inducedvertbreak, null)),1)  as 'Avg SL Induced Vert. Break',
round(avg(iif(p.pitch_type = 'SL', p.horzbreak, null)),1)  as 'Avg SL Horz. Break',
round(avg(iif(p.pitch_type = 'SL', g.stuffrelvel_grade_2080, null)),1)  as 'Avg SL RelVel Grade',
round(avg(iif(p.pitch_type = 'SL', g.stuffrelvelloc_grade_2080, null)),1)  as 'Avg SL RelVelLoc Grade',
round(max(iif(p.pitch_type = 'SL', p.release_z, null)),1)  as 'Max SL Release Z',
round(avg(iif(p.pitch_type = 'SL', p.release_z, null)),1)  as 'Avg SL Release Z',
round(max(iif(p.pitch_type = 'SL', p.release_x, null)),1)  as 'Max SL Release X',
round(avg(iif(p.pitch_type = 'SL', p.release_x, null)),1)  as 'Avg SL Release X',
round(avg(iif(p.pitch_type = 'SL', p.extension, null)),1)  as 'Avg SL Extension',
round(cast(sum(case when pitch_type in ('SL') and (plate_x between -1 and 1) and (plate_z between 1.59 and 3.38) then 1 else 0 end) / nullif(sum(case when pitch_type in ('SL') and plate_x is not null then 1.0 else 0.0 end),0) as float), 2) as 'SL Zone Pct',
round(cast(sum(case when pitch_type in ('SL') and pitch_result in ('swinging_strike','swinging_strike_blocked', 'foul_tip') then 1.0 else 0.0 end) / nullif(sum(case when pitch_type in ('SL') then 1.0 else 0.0 end),0) as float), 2) as 'SL Whiff Rate'
, round(cast(sum(case when pitch_type in ('SL') and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'foul_tip') then 1.0 else 0.0 end) / nullif((sum(case when pitch_type in ('SL') and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'hit_into_play', 'hit_into_play_no_out', 'hit_into_play_score', 'foul', 'foul tip') then 1.0 else 0.0 end)),0) as float), 2) as 'SL ISO Whiff Rate'
, round(cast(sum(case when pitch_type in ('SL') and (plate_x between -1 and 1) and (plate_z between 1.59 and 3.38) and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'foul_tip') then 1.0 else 0.0 end) / nullif((sum(case when pitch_type in ('SL') and (plate_x between -1 and 1) and (plate_z between 1.59 and 3.38) and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'hit_into_play', 'hit_into_play_no_out', 'hit_into_play_score', 'foul', 'foul tip') then 1.0 else 0.0 end)),0) as float), 2) as 'SL Zone ISO Whiff Rate'


,count(iif(p.pitch_type = 'CH', p.pitch_type, null)) as 'CH Thrown',
round(avg(iif(p.pitch_type = 'CH', p.release_speed, null)),1)  as 'Avg CH Velo',
round(avg(iif(p.pitch_type = 'CH', p.spin_rate, NULL)), 1) as 'Avg CH Spin Rate',
round(avg(iif(p.pitch_type = 'CH', p.spin_direction, NULL)), 1) as 'Avg CH Spin Direction',
round(avg(iif(p.pitch_type = 'CH', p.inducedvertbreak, null)),1)  as 'Avg CH Induced Vert. Break',
round(avg(iif(p.pitch_type = 'CH', p.horzbreak, null)),1)  as 'Avg CH Horz. Break',
round(avg(iif(p.pitch_type = 'CH', g.stuffrelvel_grade_2080, null)),1)  as 'Avg CH RelVel Grade',
round(avg(iif(p.pitch_type = 'CH', g.stuffrelvelloc_grade_2080, null)),1)  as 'Avg CH RelVelLoc Grade',
round(max(iif(p.pitch_type = 'CH', p.release_z, null)),1)  as 'Max CH Release Z',
round(avg(iif(p.pitch_type = 'CH', p.release_z, null)),1)  as 'Avg CH Release Z',
round(max(iif(p.pitch_type = 'CH', p.release_x, null)),1)  as 'Max CH Release X',
round(avg(iif(p.pitch_type = 'CH', p.release_x, null)),1)  as 'Avg CH Release X',
round(avg(iif(p.pitch_type = 'CH', p.extension, null)),1)  as 'Avg CH Extension',
round(cast(sum(case when pitch_type in ('CH') and (plate_x between -1 and 1) and (plate_z between 1.59 and 3.38) then 1 else 0 end) / nullif(sum(case when pitch_type in ('CH') and plate_x is not null then 1.0 else 0.0 end),0) as float), 2) as 'CH Zone Pct',
round(cast(sum(case when pitch_type in ('CH') and pitch_result in ('swinging_strike','swinging_strike_blocked', 'foul_tip') then 1.0 else 0.0 end) / nullif(sum(case when pitch_type in ('CH') then 1.0 else 0.0 end),0) as float), 2) as 'CH Whiff Rate'
, round(cast(sum(case when pitch_type in ('CH') and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'foul_tip') then 1.0 else 0.0 end) / nullif((sum(case when pitch_type in ('CH') and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'hit_into_play', 'hit_into_play_no_out', 'hit_into_play_score', 'foul', 'foul tip') then 1.0 else 0.0 end)),0) as float), 2) as 'CH ISO Whiff Rate'
, round(cast(sum(case when pitch_type in ('CH') and (plate_x between -1 and 1) and (plate_z between 1.59 and 3.38) and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'foul_tip') then 1.0 else 0.0 end) / nullif((sum(case when pitch_type in ('CH') and (plate_x between -1 and 1) and (plate_z between 1.59 and 3.38) and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'hit_into_play', 'hit_into_play_no_out', 'hit_into_play_score', 'foul', 'foul tip') then 1.0 else 0.0 end)),0) as float), 2) as 'CH Zone ISO Whiff Rate'




,count(iif(p.pitch_type = 'FS', p.pitch_type, null)) as 'FS Thrown',
round(avg(iif(p.pitch_type = 'FS', p.release_speed, null)),1)  as 'Avg FS Velo',
round(avg(iif(p.pitch_type = 'FS', p.spin_rate, NULL)), 1) as 'Avg FS Spin Rate',
round(avg(iif(p.pitch_type = 'FS', p.spin_direction, NULL)), 1) as 'Avg FS Spin Direction',
round(avg(iif(p.pitch_type = 'FS', p.inducedvertbreak, null)),1)  as 'Avg FS Induced Vert. Break',
round(avg(iif(p.pitch_type = 'FS', p.horzbreak, null)),1)  as 'Avg FS Horz. Break',
round(avg(iif(p.pitch_type = 'FS', g.stuffrelvel_grade_2080, null)),1)  as 'Avg FS RelVel Grade',
round(avg(iif(p.pitch_type = 'FS', g.stuffrelvelloc_grade_2080, null)),1)  as 'Avg FS RelVelLoc Grade',
round(max(iif(p.pitch_type = 'FS', p.release_z, null)),1)  as 'Max FS Release Z',
round(avg(iif(p.pitch_type = 'FS', p.release_z, null)),1)  as 'Avg FS Release Z',
round(max(iif(p.pitch_type = 'FS', p.release_x, null)),1)  as 'Max FS Release X',
round(avg(iif(p.pitch_type = 'FS', p.release_x, null)),1)  as 'Avg FS Release X',
round(avg(iif(p.pitch_type = 'FS', p.extension, null)),1)  as 'Avg FS Extension',
round(cast(sum(case when pitch_type in ('FS') and (plate_x between -1 and 1) and (plate_z between 1.59 and 3.38) then 1 else 0 end) / nullif(sum(case when pitch_type in ('FS') and plate_x is not null then 1.0 else 0.0 end),0) as float), 2) as 'FS Zone Pct',
round(cast(sum(case when pitch_type in ('FS') and pitch_result in ('swinging_strike','swinging_strike_blocked', 'foul_tip') then 1.0 else 0.0 end) / nullif(sum(case when pitch_type in ('FS') then 1.0 else 0.0 end),0) as float), 2) as 'FS Whiff Rate'
, round(cast(sum(case when pitch_type in ('FS') and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'foul_tip') then 1.0 else 0.0 end) / nullif((sum(case when pitch_type in ('FS') and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'hit_into_play', 'hit_into_play_no_out', 'hit_into_play_score', 'foul', 'foul tip') then 1.0 else 0.0 end)),0) as float), 2) as 'FS ISO Whiff Rate'
, round(cast(sum(case when pitch_type in ('FS') and (plate_x between -1 and 1) and (plate_z between 1.59 and 3.38) and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'foul_tip') then 1.0 else 0.0 end) / nullif((sum(case when pitch_type in ('FS') and (plate_x between -1 and 1) and (plate_z between 1.59 and 3.38) and pitch_result in ('swinging_strike', 'swinging_strike_blocked', 'hit_into_play', 'hit_into_play_no_out', 'hit_into_play_score', 'foul', 'foul tip') then 1.0 else 0.0 end)),0) as float), 2) as 'FS Zone ISO Whiff Rate'


from Astros.Schedule_View s
left join Astros.Pitches_View p
on p.sched_id = s.sched_id
and s.sched_type = 'b'
left join Astros.Pitches_Grades g
on g.sched_id = p.sched_id and g.pitch_id = p.pitch_id
left join Astros.Players a
on a.groundcontrol_id = p.pitcher_id
left join Trackman.Game_Data t
		on t.gameid = s.description and t.date = s.sched_date and s.sched_type = 'b'
		--and t.stadium in ('Dominican Republic - Houston', 'DRAstros', 'DRHouston', 'DRHouston1NN', 'DRHouston2NN')
		left join scout.Intl_Bios bios
		on bios.player_guid = a.intl_guid
		
where s.sched_type = 'b'
and s.year >= 2017
and p.pitcher_id < 0 
and p.pitcher_id is not null
and p.release_speed >= 65
and p.pitch_type is not null
and bios.signing_elig_year > 2017


---and s.level_code in ('mlb','aaa','aax','afa','afx','asx','rok')
group by s.level_code, p.pitcher_id, a.last_name, a.first_name, a.birthdate

)

select * from TM
union
select * from TM2"

TM_Pitcher_data <- dbGetQuery(con, Pitcher_TM_query)
TM_Pitcher_data <- mutate_if( TM_Pitcher_data, is.character, str_replace_all, pattern = "ñ", replacement = "n")
TM_Pitcher_data <- mutate_if( TM_Pitcher_data, is.character, str_replace_all, pattern = "É", replacement = "e")
TM_Pitcher_data <- mutate_if( TM_Pitcher_data, is.character, str_replace_all, pattern = "Ó", replacement = "o")
TM_Pitcher_data <- mutate_if( TM_Pitcher_data, is.character, str_replace_all, pattern = "í", replacement = "i")
TM_Pitcher_data <- mutate_if( TM_Pitcher_data, is.character, str_replace_all, pattern = "é", replacement = "e")
TM_Pitcher_data <- mutate_if( TM_Pitcher_data, is.character, str_replace_all, pattern = "Ñ", replacement = "n")
TM_Pitcher_data <- mutate_if( TM_Pitcher_data, is.character, str_replace_all, pattern = "Í", replacement = "i")
TM_Pitcher_data <- mutate_if( TM_Pitcher_data, is.character, str_replace_all, pattern = "Ó", replacement = "o")
TM_Pitcher_data <- mutate_if( TM_Pitcher_data, is.character, str_replace_all, pattern = "Ú", replacement = "u")
TM_Pitcher_data <- mutate_if( TM_Pitcher_data, is.character, str_replace_all, pattern = "Á", replacement = "a")
TM_Pitcher_data <- TM_Pitcher_data %>% mutate_at(vars(Player, `GC ID`, `Date Recorded`), ~as.factor(.))

write_feather(TM_Pitcher_data, "TM_Pitcher_data.feather")

mergefirst <- Reduce(function(x,y) merge(x, y, by = c("Player", "GC ID", "Date Recorded", "Age Recorded"), all = TRUE), list(BM_data, Hitter_Workout_data, Pitcher_Workout_data, Rap_data,  SG_data, TM_BP_data, TM_Live_data, TM_Pitcher_data))
intl_df <- Reduce(function(x,y) merge(x, y, by = c("Player", "GC ID"), all = TRUE), list(mergefirst, Player_data, Scout_Follows))





# percentilerank_low <- function(x) {
#   rx <- rle(sort(x))
#   smaller <- cumsum(c(0, rx$lengths))[seq(length(rx$lengths))]
#   larger <- rev(cumsum(c(0, rev(rx$lengths))))[-1]
#   rxpr <- larger / (smaller + larger) # larger over total as smaller indicates a better
#   rxpr[match(x, rx$values)]
# }




# percentilerank_low <- function(x) {
#   rx <- rle(sort(x))
#   smaller <- cumsum(c(0, rx$lengths))[seq(length(rx$lengths))]
#   larger <- rev(cumsum(c(0, rev(rx$lengths))))[-1]
#   rxpr <- larger / (smaller + larger) # larger over total as smaller indicates a better
#   rxpr[match(x, rx$values)]
# }

percentilerank_high <- function(x) {
  rx <- rle(sort(x))
  smaller <- cumsum(c(0, rx$lengths))[seq(length(rx$lengths))]
  larger <- rev(cumsum(c(0, rev(rx$lengths))))[-1]
  rxpr <- smaller / (smaller + larger) # smaller over total as latger indicates a better
  rxpr[match(x, rx$values)]
}




intl_df1<- intl_df %>% group_by(J2)%>% dplyr::mutate(`BM Plane Per%`=round(percentilerank_high(`BM Plane`), digits = 2))
intl_df2<- intl_df %>% group_by(J2)%>% dplyr::mutate(`BM Connection Per%`=round(percentilerank_high(`BM Connection`), digits = 2))
intl_df3<- intl_df %>% group_by(J2)%>% dplyr::mutate(`BM Peak Power Per%`=round(percentilerank_high(`BM Peak Power`), digits = 2))
intl_df4<- intl_df %>% group_by(J2)%>% dplyr::mutate(`BM Max Plane RPMs Per%`=round(percentilerank_high(`BM Max Plane RPMs`), digits = 2))
intl_df5<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Total Initial Hand Accel Per%`=round(percentilerank_high(`Total Initial Hand Accel`), digits = 2))
intl_df6<- intl_df %>% group_by(J2)%>% dplyr::mutate(`BM Swing Time Per%`=round(percentilerank_high(`BM Swing Time`), digits = 2))
intl_df7<- intl_df %>% group_by(J2)%>% dplyr::mutate(`BM DIP Per%`=round(percentilerank_high(`BM DIP`), digits = 2))
intl_df8<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg Bat Speed Per%`=round(percentilerank_high(`Avg Bat Speed`), digits = 2))
intl_df9<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Max Bat Speed Per%`=round(percentilerank_high(`Max Bat Speed`), digits = 2))
intl_df10<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Max OF Throw Per%`=round(percentilerank_high(`Max OF Throw`), digits = 2))
intl_df11<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Max IF Throw Per%`=round(percentilerank_high(`Max IF Throw`), digits = 2))
intl_df12<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Max C Throw Per%`=round(percentilerank_high(`Max C Throw`), digits = 2))
intl_df13<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg C POP Per%`=round(percentilerank_high(`Avg C POP`), digits = 2))
intl_df14<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Max Run and Gun Per%`=round(percentilerank_high(`Max Run and Gun`), digits = 2))
intl_df15<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg Run and Gun Per%`=round(percentilerank_high(`Avg Run and Gun`), digits = 2))
intl_df16<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Max Step and Throw Per%`=round(percentilerank_high(`Max Step and Throw`), digits = 2))
intl_df17<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg Step and Throw Per%`=round(percentilerank_high(`Avg Step and Throw`), digits = 2))
intl_df18<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Max Broad Jump Per%`=round(percentilerank_high(`Max Broad Jump`), digits = 2))
intl_df19<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg Broad Jump Per%`=round(percentilerank_high(`Avg Broad Jump`), digits = 2))
intl_df20<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Max FB Per%`=round(percentilerank_high(`Max FB`), digits = 2))
intl_df21<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg FB Per%`=round(percentilerank_high(`Avg FB`), digits = 2))
intl_df22<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Min FB Per%`=round(percentilerank_high(`Min FB`), digits = 2))
intl_df23<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Max CB Per%`=round(percentilerank_high(`Max CB`), digits = 2))
intl_df24<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg CB Per%`=round(percentilerank_high(`Avg CB`), digits = 2))
intl_df25<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Min CB Per%`=round(percentilerank_high(`Min CB`), digits = 2))
intl_df26<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Max SL Per%`=round(percentilerank_high(`Max SL`), digits = 2))
intl_df27<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg SL Per%`=round(percentilerank_high(`Avg SL`), digits = 2))
intl_df28<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Min SL Per%`=round(percentilerank_high(`Min SL`), digits = 2))
intl_df29<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Max CH Per%`=round(percentilerank_high(`Max CH`), digits = 2))
intl_df30<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg CH Per%`=round(percentilerank_high(`Avg CH`), digits = 2))
intl_df31<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Min CH Per%`=round(percentilerank_high(`Min CH`), digits = 2))
intl_df32<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Max SP Per%`=round(percentilerank_high(`Max SP`), digits = 2))
intl_df33<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg SP Per%`=round(percentilerank_high(`Avg SP`), digits = 2))
intl_df34<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Min SP Per%`=round(percentilerank_high(`Min SP`), digits = 2))
intl_df35<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Max OT Per%`=round(percentilerank_high(`Max OT`), digits = 2))
intl_df36<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg OT Per%`=round(percentilerank_high(`Avg OT`), digits = 2))
intl_df37<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Min OT Per%`=round(percentilerank_high(`Min OT`), digits = 2))
intl_df38<- intl_df %>% group_by(J2)%>% dplyr::mutate(`RAP BP Barrel Rate Per%`=round(percentilerank_high(`RAP BP Barrel Rate`), digits = 2))
intl_df39<- intl_df %>% group_by(J2)%>% dplyr::mutate(`RAP 90 EV % BIP Per%`=round(percentilerank_high(`RAP 90 EV % BIP`), digits = 2))
intl_df40<- intl_df %>% group_by(J2)%>% dplyr::mutate(`RAP Weak EV % BIP Per%`=round(percentilerank_high(`RAP Weak EV % BIP`), digits = 2))
intl_df41<- intl_df %>% group_by(J2)%>% dplyr::mutate(`RAP Max EV Per%`=round(percentilerank_high(`RAP Max EV`), digits = 2))
intl_df42<- intl_df %>% group_by(J2)%>% dplyr::mutate(`RAP 90MPH Equiv EV Per%`=round(percentilerank_high(`RAP 90MPH Equiv EV`), digits = 2))
intl_df43<- intl_df %>% group_by(J2)%>% dplyr::mutate(`RAP Max Distance Per%`=round(percentilerank_high(`RAP Max Distance`), digits = 2))
intl_df44<- intl_df %>% group_by(J2)%>% dplyr::mutate(`RAP Avg Distance Per%`=round(percentilerank_high(`RAP Avg Distance`), digits = 2))
intl_df45<- intl_df %>% group_by(J2)%>% dplyr::mutate(`RAP Ground Ball % Per%`=round(percentilerank_high(`RAP Ground Ball %`), digits = 2))
intl_df46<- intl_df %>% group_by(J2)%>% dplyr::mutate(`RAP Line Drive % Per%`=round(percentilerank_high(`RAP Line Drive %`), digits = 2))
intl_df47<- intl_df %>% group_by(J2)%>% dplyr::mutate(`RAP Fly Ball % Per%`=round(percentilerank_high(`RAP Fly Ball %`), digits = 2))
intl_df48<- intl_df %>% group_by(J2)%>% dplyr::mutate(`RAP Pop Up % Per%`=round(percentilerank_high(`RAP Pop Up %`), digits = 2))
intl_df49<- intl_df %>% group_by(J2)%>% dplyr::mutate(`SG Max 10yd Per%`=round(percentilerank_high(`SG Max 10yd`), digits = 2))
intl_df50<- intl_df %>% group_by(J2)%>% dplyr::mutate(`SG Avg 10yd Per%`=round(percentilerank_high(`SG Avg 10yd`), digits = 2))
intl_df51<- intl_df %>% group_by(J2)%>% dplyr::mutate(`SG Max 30yd Per%`=round(percentilerank_high(`SG Max 30yd`), digits = 2))
intl_df52<- intl_df %>% group_by(J2)%>% dplyr::mutate(`SG Avg 30yd Per%`=round(percentilerank_high(`SG Avg 30yd`), digits = 2))
intl_df53<- intl_df %>% group_by(J2)%>% dplyr::mutate(`TM BP Contacts Per%`=round(percentilerank_high(`TM BP Contacts`), digits = 2))
intl_df54<- intl_df %>% group_by(J2)%>% dplyr::mutate(`TM BP Avg UEV Per%`=round(percentilerank_high(`TM BP Avg UEV`), digits = 2))
intl_df55<- intl_df %>% group_by(J2)%>% dplyr::mutate(`TM BP Max UEV Per%`=round(percentilerank_high(`TM BP Max UEV`), digits = 2))
intl_df56<- intl_df %>% group_by(J2)%>% dplyr::mutate(`TM BP Avg UEV vs 90mph Per%`=round(percentilerank_high(`TM BP Avg UEV vs 90mph`), digits = 2))
intl_df57<- intl_df %>% group_by(J2)%>% dplyr::mutate(`TM BP Max UEV vs 90mph Per%`=round(percentilerank_high(`TM BP Max UEV vs 90mph`), digits = 2))
intl_df58<- intl_df %>% group_by(J2)%>% dplyr::mutate(`TM BP Avg EV Per%`=round(percentilerank_high(`TM BP Avg EV`), digits = 2))
intl_df59<- intl_df %>% group_by(J2)%>% dplyr::mutate(`TM BP Max EV Per%`=round(percentilerank_high(`TM BP Max EV`), digits = 2))
intl_df60<- intl_df %>% group_by(J2)%>% dplyr::mutate(`TM BP Avg EV vs 90mph Per%`=round(percentilerank_high(`TM BP Avg EV vs 90mph`), digits = 2))
intl_df61<- intl_df %>% group_by(J2)%>% dplyr::mutate(`TM BP Max EV vs 90mph Per%`=round(percentilerank_high(`TM BP Max EV vs 90mph`), digits = 2))
intl_df62<- intl_df %>% group_by(J2)%>% dplyr::mutate(`TM BP Avg Launch Angle Per%`=round(percentilerank_high(`TM BP Avg Launch Angle`), digits = 2))
intl_df63<- intl_df %>% group_by(J2)%>% dplyr::mutate(`TM BP Avg Horz Angle Per%`=round(percentilerank_high(`TM BP Avg Horz Angle`), digits = 2))
intl_df64<- intl_df %>% group_by(J2)%>% dplyr::mutate(`TM BP GB Rate Per%`=round(percentilerank_high(`TM BP GB Rate`), digits = 2))
intl_df65<- intl_df %>% group_by(J2)%>% dplyr::mutate(`TM BP LD Rate Per%`=round(percentilerank_high(`TM BP LD Rate`), digits = 2))
intl_df66<- intl_df %>% group_by(J2)%>% dplyr::mutate(`TM BP Fly Ball Rate Per%`=round(percentilerank_high(`TM BP Fly Ball Rate`), digits = 2))
intl_df67<- intl_df %>% group_by(J2)%>% dplyr::mutate(`TM BP Pop Up Rate Per%`=round(percentilerank_high(`TM BP Pop Up Rate`), digits = 2))
intl_df68<- intl_df %>% group_by(J2)%>% dplyr::mutate(`TM BP LF Rate Per%`=round(percentilerank_high(`TM BP LF Rate`), digits = 2))
intl_df69<- intl_df %>% group_by(J2)%>% dplyr::mutate(`TM BP CF Rate Per%`=round(percentilerank_high(`TM BP CF Rate`), digits = 2))
intl_df70<- intl_df %>% group_by(J2)%>% dplyr::mutate(`TM BP RF Rate Per%`=round(percentilerank_high(`TM BP RF Rate`), digits = 2))
intl_df71<- intl_df %>% group_by(J2)%>% dplyr::mutate(`PA Per%`=round(percentilerank_high(`PA`), digits = 2))
intl_df72<- intl_df %>% group_by(J2)%>% dplyr::mutate(`TM Live Contacts Per%`=round(percentilerank_high(`TM Live Contacts`), digits = 2))
intl_df73<- intl_df %>% group_by(J2)%>% dplyr::mutate(`TM Live BIP Per%`=round(percentilerank_high(`TM Live BIP`), digits = 2))
intl_df74<- intl_df %>% group_by(J2)%>% dplyr::mutate(`TM Live Swing Decs Per%`=round(percentilerank_high(`TM Live Swing Decs`), digits = 2))
intl_df75<- intl_df %>% group_by(J2)%>% dplyr::mutate(`TM Live Swing % Per%`=round(percentilerank_high(`TM Live Swing %`), digits = 2))
intl_df76<- intl_df %>% group_by(J2)%>% dplyr::mutate(`TM Live Chase % Per%`=round(percentilerank_high(`TM Live Chase %`), digits = 2))
intl_df77<- intl_df %>% group_by(J2)%>% dplyr::mutate(`TM Live inZ Swing % Per%`=round(percentilerank_high(`TM Live inZ Swing %`), digits = 2))
intl_df78<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Contact % Per%`=round(percentilerank_high(`Contact %`), digits = 2))
intl_df79<- intl_df %>% group_by(J2)%>% dplyr::mutate(`SwStr % Per%`=round(percentilerank_high(`SwStr %`), digits = 2))
intl_df80<- intl_df %>% group_by(J2)%>% dplyr::mutate(`InZ Contact % Per%`=round(percentilerank_high(`InZ Contact %`), digits = 2))
intl_df81<- intl_df %>% group_by(J2)%>% dplyr::mutate(`FF Thrown Per%`=round(percentilerank_high(`FF Thrown`), digits = 2))
intl_df82<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg FF Velo Per%`=round(percentilerank_high(`Avg FF Velo`), digits = 2))
intl_df83<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg FF Spin Rate Per%`=round(percentilerank_high(`Avg FF Spin Rate`), digits = 2))
intl_df84<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg FF Spin Direction Per%`=round(percentilerank_high(`Avg FF Spin Direction`), digits = 2))
intl_df85<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg FF Induced Vert. Break Per%`=round(percentilerank_high(`Avg FF Induced Vert. Break`), digits = 2))
intl_df86<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg FF Horz. Break Per%`=round(percentilerank_high(`Avg FF Horz. Break`), digits = 2))
intl_df87<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg FF RelVel Grade Per%`=round(percentilerank_high(`Avg FF RelVel Grade`), digits = 2))
intl_df88<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg FF RelVelLoc Grade Per%`=round(percentilerank_high(`Avg FF RelVelLoc Grade`), digits = 2))
intl_df89<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Max FF Release Z Per%`=round(percentilerank_high(`Max FF Release Z`), digits = 2))
intl_df90<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg FF Release Z Per%`=round(percentilerank_high(`Avg FF Release Z`), digits = 2))
intl_df91<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Max FF Release X Per%`=round(percentilerank_high(`Max FF Release X`), digits = 2))
intl_df92<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg FF Release X Per%`=round(percentilerank_high(`Avg FF Release X`), digits = 2))
intl_df93<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg FF Extension Per%`=round(percentilerank_high(`Avg FF Extension`), digits = 2))
intl_df94<- intl_df %>% group_by(J2)%>% dplyr::mutate(`FF Zone Pct Per%`=round(percentilerank_high(`FF Zone Pct`), digits = 2))
intl_df95<- intl_df %>% group_by(J2)%>% dplyr::mutate(`FF Whiff Rate Per%`=round(percentilerank_high(`FF Whiff Rate`), digits = 2))
intl_df96<- intl_df %>% group_by(J2)%>% dplyr::mutate(`FF ISO Whiff Rate Per%`=round(percentilerank_high(`FF ISO Whiff Rate`), digits = 2))
intl_df97<- intl_df %>% group_by(J2)%>% dplyr::mutate(`FF Zone ISO Whiff Rate Per%`=round(percentilerank_high(`FF Zone ISO Whiff Rate`), digits = 2))
intl_df98<- intl_df %>% group_by(J2)%>% dplyr::mutate(`FT Thrown Per%`=round(percentilerank_high(`FT Thrown`), digits = 2))
intl_df99<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg FT Velo Per%`=round(percentilerank_high(`Avg FT Velo`), digits = 2))
intl_df100<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg FT Spin Rate Per%`=round(percentilerank_high(`Avg FT Spin Rate`), digits = 2))
intl_df101<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg FT Spin Direction Per%`=round(percentilerank_high(`Avg FT Spin Direction`), digits = 2))
intl_df102<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg FT Induced Vert. Break Per%`=round(percentilerank_high(`Avg FT Induced Vert. Break`), digits = 2))
intl_df103<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg FT Horz. Break Per%`=round(percentilerank_high(`Avg FT Horz. Break`), digits = 2))
intl_df104<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg FT RelVel Grade Per%`=round(percentilerank_high(`Avg FT RelVel Grade`), digits = 2))
intl_df105<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg FT RelVelLoc Grade Per%`=round(percentilerank_high(`Avg FT RelVelLoc Grade`), digits = 2))
intl_df106<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Max FT Release Z Per%`=round(percentilerank_high(`Max FT Release Z`), digits = 2))
intl_df107<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg FT Release Z Per%`=round(percentilerank_high(`Avg FT Release Z`), digits = 2))
intl_df108<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Max FT Release X Per%`=round(percentilerank_high(`Max FT Release X`), digits = 2))
intl_df109<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg FT Release X Per%`=round(percentilerank_high(`Avg FT Release X`), digits = 2))
intl_df110<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg FT Extension Per%`=round(percentilerank_high(`Avg FT Extension`), digits = 2))
intl_df111<- intl_df %>% group_by(J2)%>% dplyr::mutate(`FT Zone Pct Per%`=round(percentilerank_high(`FT Zone Pct`), digits = 2))
intl_df112<- intl_df %>% group_by(J2)%>% dplyr::mutate(`FT Whiff Rate Per%`=round(percentilerank_high(`FT Whiff Rate`), digits = 2))
intl_df113<- intl_df %>% group_by(J2)%>% dplyr::mutate(`FT ISO Whiff Rate Per%`=round(percentilerank_high(`FT ISO Whiff Rate`), digits = 2))
intl_df114<- intl_df %>% group_by(J2)%>% dplyr::mutate(`FT Zone ISO Whiff Rate Per%`=round(percentilerank_high(`FT Zone ISO Whiff Rate`), digits = 2))
intl_df115<- intl_df %>% group_by(J2)%>% dplyr::mutate(`FC Thrown Per%`=round(percentilerank_high(`FC Thrown`), digits = 2))
intl_df116<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg FC Velo Per%`=round(percentilerank_high(`Avg FC Velo`), digits = 2))
intl_df117<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg FC Spin Rate Per%`=round(percentilerank_high(`Avg FC Spin Rate`), digits = 2))
intl_df118<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg FC Spin Direction Per%`=round(percentilerank_high(`Avg FC Spin Direction`), digits = 2))
intl_df119<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg FC Induced Vert. Break Per%`=round(percentilerank_high(`Avg FC Induced Vert. Break`), digits = 2))
intl_df120<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg FC Horz. Break Per%`=round(percentilerank_high(`Avg FC Horz. Break`), digits = 2))
intl_df121<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg FC RelVel Grade Per%`=round(percentilerank_high(`Avg FC RelVel Grade`), digits = 2))
intl_df122<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg FC RelVelLoc Grade Per%`=round(percentilerank_high(`Avg FC RelVelLoc Grade`), digits = 2))
intl_df123<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Max FC Release Z Per%`=round(percentilerank_high(`Max FC Release Z`), digits = 2))
intl_df124<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg FC Release Z Per%`=round(percentilerank_high(`Avg FC Release Z`), digits = 2))
intl_df125<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Max FC Release X Per%`=round(percentilerank_high(`Max FC Release X`), digits = 2))
intl_df126<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg FC Release X Per%`=round(percentilerank_high(`Avg FC Release X`), digits = 2))
intl_df127<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg FC Extension Per%`=round(percentilerank_high(`Avg FC Extension`), digits = 2))
intl_df128<- intl_df %>% group_by(J2)%>% dplyr::mutate(`FC Zone Pct Per%`=round(percentilerank_high(`FC Zone Pct`), digits = 2))
intl_df129<- intl_df %>% group_by(J2)%>% dplyr::mutate(`FC Whiff Rate Per%`=round(percentilerank_high(`FC Whiff Rate`), digits = 2))
intl_df130<- intl_df %>% group_by(J2)%>% dplyr::mutate(`FC ISO Whiff Rate Per%`=round(percentilerank_high(`FC ISO Whiff Rate`), digits = 2))
intl_df131<- intl_df %>% group_by(J2)%>% dplyr::mutate(`FC Zone ISO Whiff Rate Per%`=round(percentilerank_high(`FC Zone ISO Whiff Rate`), digits = 2))
intl_df132<- intl_df %>% group_by(J2)%>% dplyr::mutate(`CU Thrown Per%`=round(percentilerank_high(`CU Thrown`), digits = 2))
intl_df133<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg CU Velo Per%`=round(percentilerank_high(`Avg CU Velo`), digits = 2))
intl_df134<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg CU Spin Rate Per%`=round(percentilerank_high(`Avg CU Spin Rate`), digits = 2))
intl_df135<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg CU Spin Direction Per%`=round(percentilerank_high(`Avg CU Spin Direction`), digits = 2))
intl_df136<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg CU Induced Vert. Break Per%`=round(percentilerank_high(`Avg CU Induced Vert. Break`), digits = 2))
intl_df137<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg CU Horz. Break Per%`=round(percentilerank_high(`Avg CU Horz. Break`), digits = 2))
intl_df138<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg CU RelVel Grade Per%`=round(percentilerank_high(`Avg CU RelVel Grade`), digits = 2))
intl_df139<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg CU RelVelLoc Grade Per%`=round(percentilerank_high(`Avg CU RelVelLoc Grade`), digits = 2))
intl_df140<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Max CU Release Z Per%`=round(percentilerank_high(`Max CU Release Z`), digits = 2))
intl_df141<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg CU Release Z Per%`=round(percentilerank_high(`Avg CU Release Z`), digits = 2))
intl_df142<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Max CU Release X Per%`=round(percentilerank_high(`Max CU Release X`), digits = 2))
intl_df143<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg CU Release X Per%`=round(percentilerank_high(`Avg CU Release X`), digits = 2))
intl_df144<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg CU Extension Per%`=round(percentilerank_high(`Avg CU Extension`), digits = 2))
intl_df145<- intl_df %>% group_by(J2)%>% dplyr::mutate(`CU Zone Pct Per%`=round(percentilerank_high(`CU Zone Pct`), digits = 2))
intl_df146<- intl_df %>% group_by(J2)%>% dplyr::mutate(`CU Whiff Rate Per%`=round(percentilerank_high(`CU Whiff Rate`), digits = 2))
intl_df147<- intl_df %>% group_by(J2)%>% dplyr::mutate(`CU ISO Whiff Rate Per%`=round(percentilerank_high(`CU ISO Whiff Rate`), digits = 2))
intl_df148<- intl_df %>% group_by(J2)%>% dplyr::mutate(`CU Zone ISO Whiff Rate Per%`=round(percentilerank_high(`CU Zone ISO Whiff Rate`), digits = 2))
intl_df149<- intl_df %>% group_by(J2)%>% dplyr::mutate(`SL Thrown Per%`=round(percentilerank_high(`SL Thrown`), digits = 2))
intl_df150<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg SL Velo Per%`=round(percentilerank_high(`Avg SL Velo`), digits = 2))
intl_df151<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg SL Spin Rate Per%`=round(percentilerank_high(`Avg SL Spin Rate`), digits = 2))
intl_df152<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg SL Spin Direction Per%`=round(percentilerank_high(`Avg SL Spin Direction`), digits = 2))
intl_df153<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg SL Induced Vert. Break Per%`=round(percentilerank_high(`Avg SL Induced Vert. Break`), digits = 2))
intl_df154<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg SL Horz. Break Per%`=round(percentilerank_high(`Avg SL Horz. Break`), digits = 2))
intl_df155<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg SL RelVel Grade Per%`=round(percentilerank_high(`Avg SL RelVel Grade`), digits = 2))
intl_df156<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg SL RelVelLoc Grade Per%`=round(percentilerank_high(`Avg SL RelVelLoc Grade`), digits = 2))
intl_df157<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Max SL Release Z Per%`=round(percentilerank_high(`Max SL Release Z`), digits = 2))
intl_df158<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg SL Release Z Per%`=round(percentilerank_high(`Avg SL Release Z`), digits = 2))
intl_df159<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Max SL Release X Per%`=round(percentilerank_high(`Max SL Release X`), digits = 2))
intl_df160<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg SL Release X Per%`=round(percentilerank_high(`Avg SL Release X`), digits = 2))
intl_df161<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg SL Extension Per%`=round(percentilerank_high(`Avg SL Extension`), digits = 2))
intl_df162<- intl_df %>% group_by(J2)%>% dplyr::mutate(`SL Zone Pct Per%`=round(percentilerank_high(`SL Zone Pct`), digits = 2))
intl_df163<- intl_df %>% group_by(J2)%>% dplyr::mutate(`SL Whiff Rate Per%`=round(percentilerank_high(`SL Whiff Rate`), digits = 2))
intl_df164<- intl_df %>% group_by(J2)%>% dplyr::mutate(`SL ISO Whiff Rate Per%`=round(percentilerank_high(`SL ISO Whiff Rate`), digits = 2))
intl_df165<- intl_df %>% group_by(J2)%>% dplyr::mutate(`SL Zone ISO Whiff Rate Per%`=round(percentilerank_high(`SL Zone ISO Whiff Rate`), digits = 2))
intl_df166<- intl_df %>% group_by(J2)%>% dplyr::mutate(`CH Thrown Per%`=round(percentilerank_high(`CH Thrown`), digits = 2))
intl_df167<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg CH Velo Per%`=round(percentilerank_high(`Avg CH Velo`), digits = 2))
intl_df168<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg CH Spin Rate Per%`=round(percentilerank_high(`Avg CH Spin Rate`), digits = 2))
intl_df169<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg CH Spin Direction Per%`=round(percentilerank_high(`Avg CH Spin Direction`), digits = 2))
intl_df170<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg CH Induced Vert. Break Per%`=round(percentilerank_high(`Avg CH Induced Vert. Break`), digits = 2))
intl_df171<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg CH Horz. Break Per%`=round(percentilerank_high(`Avg CH Horz. Break`), digits = 2))
intl_df172<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg CH RelVel Grade Per%`=round(percentilerank_high(`Avg CH RelVel Grade`), digits = 2))
intl_df173<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg CH RelVelLoc Grade Per%`=round(percentilerank_high(`Avg CH RelVelLoc Grade`), digits = 2))
intl_df174<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Max CH Release Z Per%`=round(percentilerank_high(`Max CH Release Z`), digits = 2))
intl_df175<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg CH Release Z Per%`=round(percentilerank_high(`Avg CH Release Z`), digits = 2))
intl_df176<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Max CH Release X Per%`=round(percentilerank_high(`Max CH Release X`), digits = 2))
intl_df177<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg CH Release X Per%`=round(percentilerank_high(`Avg CH Release X`), digits = 2))
intl_df178<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg CH Extension Per%`=round(percentilerank_high(`Avg CH Extension`), digits = 2))
intl_df179<- intl_df %>% group_by(J2)%>% dplyr::mutate(`CH Zone Pct Per%`=round(percentilerank_high(`CH Zone Pct`), digits = 2))
intl_df180<- intl_df %>% group_by(J2)%>% dplyr::mutate(`CH Whiff Rate Per%`=round(percentilerank_high(`CH Whiff Rate`), digits = 2))
intl_df181<- intl_df %>% group_by(J2)%>% dplyr::mutate(`CH ISO Whiff Rate Per%`=round(percentilerank_high(`CH ISO Whiff Rate`), digits = 2))
intl_df182<- intl_df %>% group_by(J2)%>% dplyr::mutate(`CH Zone ISO Whiff Rate Per%`=round(percentilerank_high(`CH Zone ISO Whiff Rate`), digits = 2))
intl_df183<- intl_df %>% group_by(J2)%>% dplyr::mutate(`FS Thrown Per%`=round(percentilerank_high(`FS Thrown`), digits = 2))
intl_df184<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg FS Velo Per%`=round(percentilerank_high(`Avg FS Velo`), digits = 2))
intl_df185<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg FS Spin Rate Per%`=round(percentilerank_high(`Avg FS Spin Rate`), digits = 2))
intl_df186<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg FS Spin Direction Per%`=round(percentilerank_high(`Avg FS Spin Direction`), digits = 2))
intl_df187<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg FS Induced Vert. Break Per%`=round(percentilerank_high(`Avg FS Induced Vert. Break`), digits = 2))
intl_df188<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg FS Horz. Break Per%`=round(percentilerank_high(`Avg FS Horz. Break`), digits = 2))
intl_df189<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg FS RelVel Grade Per%`=round(percentilerank_high(`Avg FS RelVel Grade`), digits = 2))
intl_df190<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg FS RelVelLoc Grade Per%`=round(percentilerank_high(`Avg FS RelVelLoc Grade`), digits = 2))
intl_df191<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Max FS Release Z Per%`=round(percentilerank_high(`Max FS Release Z`), digits = 2))
intl_df192<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg FS Release Z Per%`=round(percentilerank_high(`Avg FS Release Z`), digits = 2))
intl_df193<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Max FS Release X Per%`=round(percentilerank_high(`Max FS Release X`), digits = 2))
intl_df194<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg FS Release X Per%`=round(percentilerank_high(`Avg FS Release X`), digits = 2))
intl_df195<- intl_df %>% group_by(J2)%>% dplyr::mutate(`Avg FS Extension Per%`=round(percentilerank_high(`Avg FS Extension`), digits = 2))
intl_df196<- intl_df %>% group_by(J2)%>% dplyr::mutate(`FS Zone Pct Per%`=round(percentilerank_high(`FS Zone Pct`), digits = 2))
intl_df197<- intl_df %>% group_by(J2)%>% dplyr::mutate(`FS Whiff Rate Per%`=round(percentilerank_high(`FS Whiff Rate`), digits = 2))
intl_df198<- intl_df %>% group_by(J2)%>% dplyr::mutate(`FS ISO Whiff Rate Per%`=round(percentilerank_high(`FS ISO Whiff Rate`), digits = 2))
intl_df199<- intl_df %>% group_by(J2)%>% dplyr::mutate(`FS Zone ISO Whiff Rate Per%`=round(percentilerank_high(`FS Zone ISO Whiff Rate`), digits = 2))

intl_df1 <- filter(intl_df1, `Date Recorded` == "Total")
intl_df2<- filter(intl_df2, `Date Recorded` == "Total")
intl_df3<- filter(intl_df3, `Date Recorded` == "Total")
intl_df4<- filter(intl_df4, `Date Recorded` == "Total")
intl_df5<- filter(intl_df5, `Date Recorded` == "Total")
intl_df6<- filter(intl_df6, `Date Recorded` == "Total")
intl_df7<- filter(intl_df7, `Date Recorded` == "Total")
intl_df8<- filter(intl_df8, `Date Recorded` == "Total")
intl_df9<- filter(intl_df9, `Date Recorded` == "Total")
intl_df10<- filter(intl_df10, `Date Recorded` == "Total")
intl_df11<- filter(intl_df11, `Date Recorded` == "Total")
intl_df12<- filter(intl_df12, `Date Recorded` == "Total")
intl_df13<- filter(intl_df13, `Date Recorded` == "Total")
intl_df14<- filter(intl_df14, `Date Recorded` == "Total")
intl_df15<- filter(intl_df15, `Date Recorded` == "Total")
intl_df16<- filter(intl_df16, `Date Recorded` == "Total")
intl_df17<- filter(intl_df17, `Date Recorded` == "Total")
intl_df18<- filter(intl_df18, `Date Recorded` == "Total")
intl_df19<- filter(intl_df19, `Date Recorded` == "Total")
intl_df20<- filter(intl_df20, `Date Recorded` == "Total")
intl_df21<- filter(intl_df21, `Date Recorded` == "Total")
intl_df22<- filter(intl_df22, `Date Recorded` == "Total")
intl_df23<- filter(intl_df23, `Date Recorded` == "Total")
intl_df24<- filter(intl_df24, `Date Recorded` == "Total")
intl_df25<- filter(intl_df25, `Date Recorded` == "Total")
intl_df26<- filter(intl_df26, `Date Recorded` == "Total")
intl_df27<- filter(intl_df27, `Date Recorded` == "Total")
intl_df28<- filter(intl_df28, `Date Recorded` == "Total")
intl_df29<- filter(intl_df29, `Date Recorded` == "Total")
intl_df30<- filter(intl_df30, `Date Recorded` == "Total")
intl_df31<- filter(intl_df31, `Date Recorded` == "Total")
intl_df32<- filter(intl_df32, `Date Recorded` == "Total")
intl_df33<- filter(intl_df33, `Date Recorded` == "Total")
intl_df34<- filter(intl_df34, `Date Recorded` == "Total")
intl_df35<- filter(intl_df35, `Date Recorded` == "Total")
intl_df36<- filter(intl_df36, `Date Recorded` == "Total")
intl_df37<- filter(intl_df37, `Date Recorded` == "Total")
intl_df38<- filter(intl_df38, `Date Recorded` == "Total")
intl_df39<- filter(intl_df39, `Date Recorded` == "Total")
intl_df40<- filter(intl_df40, `Date Recorded` == "Total")
intl_df41<- filter(intl_df41, `Date Recorded` == "Total")
intl_df42<- filter(intl_df42, `Date Recorded` == "Total")
intl_df43<- filter(intl_df43, `Date Recorded` == "Total")
intl_df44<- filter(intl_df44, `Date Recorded` == "Total")
intl_df45<- filter(intl_df45, `Date Recorded` == "Total")
intl_df46<- filter(intl_df46, `Date Recorded` == "Total")
intl_df47<- filter(intl_df47, `Date Recorded` == "Total")
intl_df48<- filter(intl_df48, `Date Recorded` == "Total")
intl_df49<- filter(intl_df49, `Date Recorded` == "Total")
intl_df50<- filter(intl_df50, `Date Recorded` == "Total")
intl_df51<- filter(intl_df51, `Date Recorded` == "Total")
intl_df52<- filter(intl_df52, `Date Recorded` == "Total")
intl_df53<- filter(intl_df53, `Date Recorded` == "Total")
intl_df54<- filter(intl_df54, `Date Recorded` == "Total")
intl_df55<- filter(intl_df55, `Date Recorded` == "Total")
intl_df56<- filter(intl_df56, `Date Recorded` == "Total")
intl_df57<- filter(intl_df57, `Date Recorded` == "Total")
intl_df58<- filter(intl_df58, `Date Recorded` == "Total")
intl_df59<- filter(intl_df59, `Date Recorded` == "Total")
intl_df60<- filter(intl_df60, `Date Recorded` == "Total")
intl_df61<- filter(intl_df61, `Date Recorded` == "Total")
intl_df62<- filter(intl_df62, `Date Recorded` == "Total")
intl_df63<- filter(intl_df63, `Date Recorded` == "Total")
intl_df64<- filter(intl_df64, `Date Recorded` == "Total")
intl_df65<- filter(intl_df65, `Date Recorded` == "Total")
intl_df66<- filter(intl_df66, `Date Recorded` == "Total")
intl_df67<- filter(intl_df67, `Date Recorded` == "Total")
intl_df68<- filter(intl_df68, `Date Recorded` == "Total")
intl_df69<- filter(intl_df69, `Date Recorded` == "Total")
intl_df70<- filter(intl_df70, `Date Recorded` == "Total")
intl_df71<- filter(intl_df71, `Date Recorded` == "Total")
intl_df72<- filter(intl_df72, `Date Recorded` == "Total")
intl_df73<- filter(intl_df73, `Date Recorded` == "Total")
intl_df74<- filter(intl_df74, `Date Recorded` == "Total")
intl_df75<- filter(intl_df75, `Date Recorded` == "Total")
intl_df76<- filter(intl_df76, `Date Recorded` == "Total")
intl_df77<- filter(intl_df77, `Date Recorded` == "Total")
intl_df78<- filter(intl_df78, `Date Recorded` == "Total")
intl_df79<- filter(intl_df79, `Date Recorded` == "Total")
intl_df80<- filter(intl_df80, `Date Recorded` == "Total")
intl_df81<- filter(intl_df81, `Date Recorded` == "Total")
intl_df82<- filter(intl_df82, `Date Recorded` == "Total")
intl_df83<- filter(intl_df83, `Date Recorded` == "Total")
intl_df84<- filter(intl_df84, `Date Recorded` == "Total")
intl_df85<- filter(intl_df85, `Date Recorded` == "Total")
intl_df86<- filter(intl_df86, `Date Recorded` == "Total")
intl_df87<- filter(intl_df87, `Date Recorded` == "Total")
intl_df88<- filter(intl_df88, `Date Recorded` == "Total")
intl_df89<- filter(intl_df89, `Date Recorded` == "Total")
intl_df90<- filter(intl_df90, `Date Recorded` == "Total")
intl_df91<- filter(intl_df91, `Date Recorded` == "Total")
intl_df92<- filter(intl_df92, `Date Recorded` == "Total")
intl_df93<- filter(intl_df93, `Date Recorded` == "Total")
intl_df94<- filter(intl_df94, `Date Recorded` == "Total")
intl_df95<- filter(intl_df95, `Date Recorded` == "Total")
intl_df96<- filter(intl_df96, `Date Recorded` == "Total")
intl_df97<- filter(intl_df97, `Date Recorded` == "Total")
intl_df98<- filter(intl_df98, `Date Recorded` == "Total")
intl_df99<- filter(intl_df99, `Date Recorded` == "Total")
intl_df100<- filter(intl_df100, `Date Recorded` == "Total")
intl_df101<- filter(intl_df101, `Date Recorded` == "Total")
intl_df102<- filter(intl_df102, `Date Recorded` == "Total")
intl_df103<- filter(intl_df103, `Date Recorded` == "Total")
intl_df104<- filter(intl_df104, `Date Recorded` == "Total")
intl_df105<- filter(intl_df105, `Date Recorded` == "Total")
intl_df106<- filter(intl_df106, `Date Recorded` == "Total")
intl_df107<- filter(intl_df107, `Date Recorded` == "Total")
intl_df108<- filter(intl_df108, `Date Recorded` == "Total")
intl_df109<- filter(intl_df109, `Date Recorded` == "Total")
intl_df110<- filter(intl_df110, `Date Recorded` == "Total")
intl_df111<- filter(intl_df111, `Date Recorded` == "Total")
intl_df112<- filter(intl_df112, `Date Recorded` == "Total")
intl_df113<- filter(intl_df113, `Date Recorded` == "Total")
intl_df114<- filter(intl_df114, `Date Recorded` == "Total")
intl_df115<- filter(intl_df115, `Date Recorded` == "Total")
intl_df116<- filter(intl_df116, `Date Recorded` == "Total")
intl_df117<- filter(intl_df117, `Date Recorded` == "Total")
intl_df118<- filter(intl_df118, `Date Recorded` == "Total")
intl_df119<- filter(intl_df119, `Date Recorded` == "Total")
intl_df120<- filter(intl_df120, `Date Recorded` == "Total")
intl_df121<- filter(intl_df121, `Date Recorded` == "Total")
intl_df122<- filter(intl_df122, `Date Recorded` == "Total")
intl_df123<- filter(intl_df123, `Date Recorded` == "Total")
intl_df124<- filter(intl_df124, `Date Recorded` == "Total")
intl_df125<- filter(intl_df125, `Date Recorded` == "Total")
intl_df126<- filter(intl_df126, `Date Recorded` == "Total")
intl_df127<- filter(intl_df127, `Date Recorded` == "Total")
intl_df128<- filter(intl_df128, `Date Recorded` == "Total")
intl_df129<- filter(intl_df129, `Date Recorded` == "Total")
intl_df130<- filter(intl_df130, `Date Recorded` == "Total")
intl_df131<- filter(intl_df131, `Date Recorded` == "Total")
intl_df132<- filter(intl_df132, `Date Recorded` == "Total")
intl_df133<- filter(intl_df133, `Date Recorded` == "Total")
intl_df134<- filter(intl_df134, `Date Recorded` == "Total")
intl_df135<- filter(intl_df135, `Date Recorded` == "Total")
intl_df136<- filter(intl_df136, `Date Recorded` == "Total")
intl_df137<- filter(intl_df137, `Date Recorded` == "Total")
intl_df138<- filter(intl_df138, `Date Recorded` == "Total")
intl_df139<- filter(intl_df139, `Date Recorded` == "Total")
intl_df140<- filter(intl_df140, `Date Recorded` == "Total")
intl_df141<- filter(intl_df141, `Date Recorded` == "Total")
intl_df142<- filter(intl_df142, `Date Recorded` == "Total")
intl_df143<- filter(intl_df143, `Date Recorded` == "Total")
intl_df144<- filter(intl_df144, `Date Recorded` == "Total")
intl_df145<- filter(intl_df145, `Date Recorded` == "Total")
intl_df146<- filter(intl_df146, `Date Recorded` == "Total")
intl_df147<- filter(intl_df147, `Date Recorded` == "Total")
intl_df148<- filter(intl_df148, `Date Recorded` == "Total")
intl_df149<- filter(intl_df149, `Date Recorded` == "Total")
intl_df150<- filter(intl_df150, `Date Recorded` == "Total")
intl_df151<- filter(intl_df151, `Date Recorded` == "Total")
intl_df152<- filter(intl_df152, `Date Recorded` == "Total")
intl_df153<- filter(intl_df153, `Date Recorded` == "Total")
intl_df154<- filter(intl_df154, `Date Recorded` == "Total")
intl_df155<- filter(intl_df155, `Date Recorded` == "Total")
intl_df156<- filter(intl_df156, `Date Recorded` == "Total")
intl_df157<- filter(intl_df157, `Date Recorded` == "Total")
intl_df158<- filter(intl_df158, `Date Recorded` == "Total")
intl_df159<- filter(intl_df159, `Date Recorded` == "Total")
intl_df160<- filter(intl_df160, `Date Recorded` == "Total")
intl_df161<- filter(intl_df161, `Date Recorded` == "Total")
intl_df162<- filter(intl_df162, `Date Recorded` == "Total")
intl_df163<- filter(intl_df163, `Date Recorded` == "Total")
intl_df164<- filter(intl_df164, `Date Recorded` == "Total")
intl_df165<- filter(intl_df165, `Date Recorded` == "Total")
intl_df166<- filter(intl_df166, `Date Recorded` == "Total")
intl_df167<- filter(intl_df167, `Date Recorded` == "Total")
intl_df168<- filter(intl_df168, `Date Recorded` == "Total")
intl_df169<- filter(intl_df169, `Date Recorded` == "Total")
intl_df170<- filter(intl_df170, `Date Recorded` == "Total")
intl_df171<- filter(intl_df171, `Date Recorded` == "Total")
intl_df172<- filter(intl_df172, `Date Recorded` == "Total")
intl_df173<- filter(intl_df173, `Date Recorded` == "Total")
intl_df174<- filter(intl_df174, `Date Recorded` == "Total")
intl_df175<- filter(intl_df175, `Date Recorded` == "Total")
intl_df176<- filter(intl_df176, `Date Recorded` == "Total")
intl_df177<- filter(intl_df177, `Date Recorded` == "Total")
intl_df178<- filter(intl_df178, `Date Recorded` == "Total")
intl_df179<- filter(intl_df179, `Date Recorded` == "Total")
intl_df180<- filter(intl_df180, `Date Recorded` == "Total")
intl_df181<- filter(intl_df181, `Date Recorded` == "Total")
intl_df182<- filter(intl_df182, `Date Recorded` == "Total")
intl_df183<- filter(intl_df183, `Date Recorded` == "Total")
intl_df184<- filter(intl_df184, `Date Recorded` == "Total")
intl_df185<- filter(intl_df185, `Date Recorded` == "Total")
intl_df186<- filter(intl_df186, `Date Recorded` == "Total")
intl_df187<- filter(intl_df187, `Date Recorded` == "Total")
intl_df188<- filter(intl_df188, `Date Recorded` == "Total")
intl_df189<- filter(intl_df189, `Date Recorded` == "Total")
intl_df190<- filter(intl_df190, `Date Recorded` == "Total")
intl_df191<- filter(intl_df191, `Date Recorded` == "Total")
intl_df192<- filter(intl_df192, `Date Recorded` == "Total")
intl_df193<- filter(intl_df193, `Date Recorded` == "Total")
intl_df194<- filter(intl_df194, `Date Recorded` == "Total")
intl_df195<- filter(intl_df195, `Date Recorded` == "Total")
intl_df196<- filter(intl_df196, `Date Recorded` == "Total")
intl_df197<- filter(intl_df197, `Date Recorded` == "Total")
intl_df198<- filter(intl_df198, `Date Recorded` == "Total")
intl_df199<- filter(intl_df199, `Date Recorded` == "Total")



intl_df1<- intl_df1%>% select(c(1:3, 223:228, 234))
intl_df2<- intl_df2%>% select(c(1:3, 223:228, 234))
intl_df3<- intl_df3%>% select(c(1:3, 223:228, 234))
intl_df4<- intl_df4%>% select(c(1:3, 223:228, 234))
intl_df5<- intl_df5%>% select(c(1:3, 223:228, 234))
intl_df6<- intl_df6%>% select(c(1:3, 223:228, 234))
intl_df7<- intl_df7%>% select(c(1:3, 223:228, 234))
intl_df8<- intl_df8%>% select(c(1:3, 223:228, 234))
intl_df9<- intl_df9%>% select(c(1:3, 223:228, 234))
intl_df10<- intl_df10%>% select(c(1:3, 223:228, 234))
intl_df11<- intl_df11%>% select(c(1:3, 223:228, 234))
intl_df12<- intl_df12%>% select(c(1:3, 223:228, 234))
intl_df13<- intl_df13%>% select(c(1:3, 223:228, 234))
intl_df14<- intl_df14%>% select(c(1:3, 223:228, 234))
intl_df15<- intl_df15%>% select(c(1:3, 223:228, 234))
intl_df16<- intl_df16%>% select(c(1:3, 223:228, 234))
intl_df17<- intl_df17%>% select(c(1:3, 223:228, 234))
intl_df18<- intl_df18%>% select(c(1:3, 223:228, 234))
intl_df19<- intl_df19%>% select(c(1:3, 223:228, 234))
intl_df20<- intl_df20%>% select(c(1:3, 223:228, 234))
intl_df21<- intl_df21%>% select(c(1:3, 223:228, 234))
intl_df22<- intl_df22%>% select(c(1:3, 223:228, 234))
intl_df23<- intl_df23%>% select(c(1:3, 223:228, 234))
intl_df24<- intl_df24%>% select(c(1:3, 223:228, 234))
intl_df25<- intl_df25%>% select(c(1:3, 223:228, 234))
intl_df26<- intl_df26%>% select(c(1:3, 223:228, 234))
intl_df27<- intl_df27%>% select(c(1:3, 223:228, 234))
intl_df28<- intl_df28%>% select(c(1:3, 223:228, 234))
intl_df29<- intl_df29%>% select(c(1:3, 223:228, 234))
intl_df30<- intl_df30%>% select(c(1:3, 223:228, 234))
intl_df31<- intl_df31%>% select(c(1:3, 223:228, 234))
intl_df32<- intl_df32%>% select(c(1:3, 223:228, 234))
intl_df33<- intl_df33%>% select(c(1:3, 223:228, 234))
intl_df34<- intl_df34%>% select(c(1:3, 223:228, 234))
intl_df35<- intl_df35%>% select(c(1:3, 223:228, 234))
intl_df36<- intl_df36%>% select(c(1:3, 223:228, 234))
intl_df37<- intl_df37%>% select(c(1:3, 223:228, 234))
intl_df38<- intl_df38%>% select(c(1:3, 223:228, 234))
intl_df39<- intl_df39%>% select(c(1:3, 223:228, 234))
intl_df40<- intl_df40%>% select(c(1:3, 223:228, 234))
intl_df41<- intl_df41%>% select(c(1:3, 223:228, 234))
intl_df42<- intl_df42%>% select(c(1:3, 223:228, 234))
intl_df43<- intl_df43%>% select(c(1:3, 223:228, 234))
intl_df44<- intl_df44%>% select(c(1:3, 223:228, 234))
intl_df45<- intl_df45%>% select(c(1:3, 223:228, 234))
intl_df46<- intl_df46%>% select(c(1:3, 223:228, 234))
intl_df47<- intl_df47%>% select(c(1:3, 223:228, 234))
intl_df48<- intl_df48%>% select(c(1:3, 223:228, 234))
intl_df49<- intl_df49%>% select(c(1:3, 223:228, 234))
intl_df50<- intl_df50%>% select(c(1:3, 223:228, 234))
intl_df51<- intl_df51%>% select(c(1:3, 223:228, 234))
intl_df52<- intl_df52%>% select(c(1:3, 223:228, 234))
intl_df53<- intl_df53%>% select(c(1:3, 223:228, 234))
intl_df54<- intl_df54%>% select(c(1:3, 223:228, 234))
intl_df55<- intl_df55%>% select(c(1:3, 223:228, 234))
intl_df56<- intl_df56%>% select(c(1:3, 223:228, 234))
intl_df57<- intl_df57%>% select(c(1:3, 223:228, 234))
intl_df58<- intl_df58%>% select(c(1:3, 223:228, 234))
intl_df59<- intl_df59%>% select(c(1:3, 223:228, 234))
intl_df60<- intl_df60%>% select(c(1:3, 223:228, 234))
intl_df61<- intl_df61%>% select(c(1:3, 223:228, 234))
intl_df62<- intl_df62%>% select(c(1:3, 223:228, 234))
intl_df63<- intl_df63%>% select(c(1:3, 223:228, 234))
intl_df64<- intl_df64%>% select(c(1:3, 223:228, 234))
intl_df65<- intl_df65%>% select(c(1:3, 223:228, 234))
intl_df66<- intl_df66%>% select(c(1:3, 223:228, 234))
intl_df67<- intl_df67%>% select(c(1:3, 223:228, 234))
intl_df68<- intl_df68%>% select(c(1:3, 223:228, 234))
intl_df69<- intl_df69%>% select(c(1:3, 223:228, 234))
intl_df70<- intl_df70%>% select(c(1:3, 223:228, 234))
intl_df71<- intl_df71%>% select(c(1:3, 223:228, 234))
intl_df72<- intl_df72%>% select(c(1:3, 223:228, 234))
intl_df73<- intl_df73%>% select(c(1:3, 223:228, 234))
intl_df74<- intl_df74%>% select(c(1:3, 223:228, 234))
intl_df75<- intl_df75%>% select(c(1:3, 223:228, 234))
intl_df76<- intl_df76%>% select(c(1:3, 223:228, 234))
intl_df77<- intl_df77%>% select(c(1:3, 223:228, 234))
intl_df78<- intl_df78%>% select(c(1:3, 223:228, 234))
intl_df79<- intl_df79%>% select(c(1:3, 223:228, 234))
intl_df80<- intl_df80%>% select(c(1:3, 223:228, 234))
intl_df81<- intl_df81%>% select(c(1:3, 223:228, 234))
intl_df82<- intl_df82%>% select(c(1:3, 223:228, 234))
intl_df83<- intl_df83%>% select(c(1:3, 223:228, 234))
intl_df84<- intl_df84%>% select(c(1:3, 223:228, 234))
intl_df85<- intl_df85%>% select(c(1:3, 223:228, 234))
intl_df86<- intl_df86%>% select(c(1:3, 223:228, 234))
intl_df87<- intl_df87%>% select(c(1:3, 223:228, 234))
intl_df88<- intl_df88%>% select(c(1:3, 223:228, 234))
intl_df89<- intl_df89%>% select(c(1:3, 223:228, 234))
intl_df90<- intl_df90%>% select(c(1:3, 223:228, 234))
intl_df91<- intl_df91%>% select(c(1:3, 223:228, 234))
intl_df92<- intl_df92%>% select(c(1:3, 223:228, 234))
intl_df93<- intl_df93%>% select(c(1:3, 223:228, 234))
intl_df94<- intl_df94%>% select(c(1:3, 223:228, 234))
intl_df95<- intl_df95%>% select(c(1:3, 223:228, 234))
intl_df96<- intl_df96%>% select(c(1:3, 223:228, 234))
intl_df97<- intl_df97%>% select(c(1:3, 223:228, 234))
intl_df98<- intl_df98%>% select(c(1:3, 223:228, 234))
intl_df99<- intl_df99%>% select(c(1:3, 223:228, 234))
intl_df100<- intl_df100%>% select(c(1:3, 223:228, 234))
intl_df101<- intl_df101%>% select(c(1:3, 223:228, 234))
intl_df102<- intl_df102%>% select(c(1:3, 223:228, 234))
intl_df103<- intl_df103%>% select(c(1:3, 223:228, 234))
intl_df104<- intl_df104%>% select(c(1:3, 223:228, 234))
intl_df105<- intl_df105%>% select(c(1:3, 223:228, 234))
intl_df106<- intl_df106%>% select(c(1:3, 223:228, 234))
intl_df107<- intl_df107%>% select(c(1:3, 223:228, 234))
intl_df108<- intl_df108%>% select(c(1:3, 223:228, 234))
intl_df109<- intl_df109%>% select(c(1:3, 223:228, 234))
intl_df110<- intl_df110%>% select(c(1:3, 223:228, 234))
intl_df111<- intl_df111%>% select(c(1:3, 223:228, 234))
intl_df112<- intl_df112%>% select(c(1:3, 223:228, 234))
intl_df113<- intl_df113%>% select(c(1:3, 223:228, 234))
intl_df114<- intl_df114%>% select(c(1:3, 223:228, 234))
intl_df115<- intl_df115%>% select(c(1:3, 223:228, 234))
intl_df116<- intl_df116%>% select(c(1:3, 223:228, 234))
intl_df117<- intl_df117%>% select(c(1:3, 223:228, 234))
intl_df118<- intl_df118%>% select(c(1:3, 223:228, 234))
intl_df119<- intl_df119%>% select(c(1:3, 223:228, 234))
intl_df120<- intl_df120%>% select(c(1:3, 223:228, 234))
intl_df121<- intl_df121%>% select(c(1:3, 223:228, 234))
intl_df122<- intl_df122%>% select(c(1:3, 223:228, 234))
intl_df123<- intl_df123%>% select(c(1:3, 223:228, 234))
intl_df124<- intl_df124%>% select(c(1:3, 223:228, 234))
intl_df125<- intl_df125%>% select(c(1:3, 223:228, 234))
intl_df126<- intl_df126%>% select(c(1:3, 223:228, 234))
intl_df127<- intl_df127%>% select(c(1:3, 223:228, 234))
intl_df128<- intl_df128%>% select(c(1:3, 223:228, 234))
intl_df129<- intl_df129%>% select(c(1:3, 223:228, 234))
intl_df130<- intl_df130%>% select(c(1:3, 223:228, 234))
intl_df131<- intl_df131%>% select(c(1:3, 223:228, 234))
intl_df132<- intl_df132%>% select(c(1:3, 223:228, 234))
intl_df133<- intl_df133%>% select(c(1:3, 223:228, 234))
intl_df134<- intl_df134%>% select(c(1:3, 223:228, 234))
intl_df135<- intl_df135%>% select(c(1:3, 223:228, 234))
intl_df136<- intl_df136%>% select(c(1:3, 223:228, 234))
intl_df137<- intl_df137%>% select(c(1:3, 223:228, 234))
intl_df138<- intl_df138%>% select(c(1:3, 223:228, 234))
intl_df139<- intl_df139%>% select(c(1:3, 223:228, 234))
intl_df140<- intl_df140%>% select(c(1:3, 223:228, 234))
intl_df141<- intl_df141%>% select(c(1:3, 223:228, 234))
intl_df142<- intl_df142%>% select(c(1:3, 223:228, 234))
intl_df143<- intl_df143%>% select(c(1:3, 223:228, 234))
intl_df144<- intl_df144%>% select(c(1:3, 223:228, 234))
intl_df145<- intl_df145%>% select(c(1:3, 223:228, 234))
intl_df146<- intl_df146%>% select(c(1:3, 223:228, 234))
intl_df147<- intl_df147%>% select(c(1:3, 223:228, 234))
intl_df148<- intl_df148%>% select(c(1:3, 223:228, 234))
intl_df149<- intl_df149%>% select(c(1:3, 223:228, 234))
intl_df150<- intl_df150%>% select(c(1:3, 223:228, 234))
intl_df151<- intl_df151%>% select(c(1:3, 223:228, 234))
intl_df152<- intl_df152%>% select(c(1:3, 223:228, 234))
intl_df153<- intl_df153%>% select(c(1:3, 223:228, 234))
intl_df154<- intl_df154%>% select(c(1:3, 223:228, 234))
intl_df155<- intl_df155%>% select(c(1:3, 223:228, 234))
intl_df156<- intl_df156%>% select(c(1:3, 223:228, 234))
intl_df157<- intl_df157%>% select(c(1:3, 223:228, 234))
intl_df158<- intl_df158%>% select(c(1:3, 223:228, 234))
intl_df159<- intl_df159%>% select(c(1:3, 223:228, 234))
intl_df160<- intl_df160%>% select(c(1:3, 223:228, 234))
intl_df161<- intl_df161%>% select(c(1:3, 223:228, 234))
intl_df162<- intl_df162%>% select(c(1:3, 223:228, 234))
intl_df163<- intl_df163%>% select(c(1:3, 223:228, 234))
intl_df164<- intl_df164%>% select(c(1:3, 223:228, 234))
intl_df165<- intl_df165%>% select(c(1:3, 223:228, 234))
intl_df166<- intl_df166%>% select(c(1:3, 223:228, 234))
intl_df167<- intl_df167%>% select(c(1:3, 223:228, 234))
intl_df168<- intl_df168%>% select(c(1:3, 223:228, 234))
intl_df169<- intl_df169%>% select(c(1:3, 223:228, 234))
intl_df170<- intl_df170%>% select(c(1:3, 223:228, 234))
intl_df171<- intl_df171%>% select(c(1:3, 223:228, 234))
intl_df172<- intl_df172%>% select(c(1:3, 223:228, 234))
intl_df173<- intl_df173%>% select(c(1:3, 223:228, 234))
intl_df174<- intl_df174%>% select(c(1:3, 223:228, 234))
intl_df175<- intl_df175%>% select(c(1:3, 223:228, 234))
intl_df176<- intl_df176%>% select(c(1:3, 223:228, 234))
intl_df177<- intl_df177%>% select(c(1:3, 223:228, 234))
intl_df178<- intl_df178%>% select(c(1:3, 223:228, 234))
intl_df179<- intl_df179%>% select(c(1:3, 223:228, 234))
intl_df180<- intl_df180%>% select(c(1:3, 223:228, 234))
intl_df181<- intl_df181%>% select(c(1:3, 223:228, 234))
intl_df182<- intl_df182%>% select(c(1:3, 223:228, 234))
intl_df183<- intl_df183%>% select(c(1:3, 223:228, 234))
intl_df184<- intl_df184%>% select(c(1:3, 223:228, 234))
intl_df185<- intl_df185%>% select(c(1:3, 223:228, 234))
intl_df186<- intl_df186%>% select(c(1:3, 223:228, 234))
intl_df187<- intl_df187%>% select(c(1:3, 223:228, 234))
intl_df188<- intl_df188%>% select(c(1:3, 223:228, 234))
intl_df189<- intl_df189%>% select(c(1:3, 223:228, 234))
intl_df190<- intl_df190%>% select(c(1:3, 223:228, 234))
intl_df191<- intl_df191%>% select(c(1:3, 223:228, 234))
intl_df192<- intl_df192%>% select(c(1:3, 223:228, 234))
intl_df193<- intl_df193%>% select(c(1:3, 223:228, 234))
intl_df194<- intl_df194%>% select(c(1:3, 223:228, 234))
intl_df195<- intl_df195%>% select(c(1:3, 223:228, 234))
intl_df196<- intl_df196%>% select(c(1:3, 223:228, 234))
intl_df197<- intl_df197%>% select(c(1:3, 223:228, 234))
intl_df198<- intl_df198%>% select(c(1:3, 223:228, 234))
intl_df199<- intl_df199%>% select(c(1:3, 223:228, 234))
intl_df1<- na.omit(intl_df1)
intl_df2<- na.omit(intl_df2)
intl_df3<- na.omit(intl_df3)
intl_df4<- na.omit(intl_df4)
intl_df5<- na.omit(intl_df5)
intl_df6<- na.omit(intl_df6)
intl_df7<- na.omit(intl_df7)
intl_df8<- na.omit(intl_df8)
intl_df9<- na.omit(intl_df9)
intl_df10<- na.omit(intl_df10)
intl_df11<- na.omit(intl_df11)
intl_df12<- na.omit(intl_df12)
intl_df13<- na.omit(intl_df13)
intl_df14<- na.omit(intl_df14)
intl_df15<- na.omit(intl_df15)
intl_df16<- na.omit(intl_df16)
intl_df17<- na.omit(intl_df17)
intl_df18<- na.omit(intl_df18)
intl_df19<- na.omit(intl_df19)
intl_df20<- na.omit(intl_df20)
intl_df21<- na.omit(intl_df21)
intl_df22<- na.omit(intl_df22)
intl_df23<- na.omit(intl_df23)
intl_df24<- na.omit(intl_df24)
intl_df25<- na.omit(intl_df25)
intl_df26<- na.omit(intl_df26)
intl_df27<- na.omit(intl_df27)
intl_df28<- na.omit(intl_df28)
intl_df29<- na.omit(intl_df29)
intl_df30<- na.omit(intl_df30)
intl_df31<- na.omit(intl_df31)
intl_df32<- na.omit(intl_df32)
intl_df33<- na.omit(intl_df33)
intl_df34<- na.omit(intl_df34)
intl_df35<- na.omit(intl_df35)
intl_df36<- na.omit(intl_df36)
intl_df37<- na.omit(intl_df37)
intl_df38<- na.omit(intl_df38)
intl_df39<- na.omit(intl_df39)
intl_df40<- na.omit(intl_df40)
intl_df41<- na.omit(intl_df41)
intl_df42<- na.omit(intl_df42)
intl_df43<- na.omit(intl_df43)
intl_df44<- na.omit(intl_df44)
intl_df45<- na.omit(intl_df45)
intl_df46<- na.omit(intl_df46)
intl_df47<- na.omit(intl_df47)
intl_df48<- na.omit(intl_df48)
intl_df49<- na.omit(intl_df49)
intl_df50<- na.omit(intl_df50)
intl_df51<- na.omit(intl_df51)
intl_df52<- na.omit(intl_df52)
intl_df53<- na.omit(intl_df53)
intl_df54<- na.omit(intl_df54)
intl_df55<- na.omit(intl_df55)
intl_df56<- na.omit(intl_df56)
intl_df57<- na.omit(intl_df57)
intl_df58<- na.omit(intl_df58)
intl_df59<- na.omit(intl_df59)
intl_df60<- na.omit(intl_df60)
intl_df61<- na.omit(intl_df61)
intl_df62<- na.omit(intl_df62)
intl_df63<- na.omit(intl_df63)
intl_df64<- na.omit(intl_df64)
intl_df65<- na.omit(intl_df65)
intl_df66<- na.omit(intl_df66)
intl_df67<- na.omit(intl_df67)
intl_df68<- na.omit(intl_df68)
intl_df69<- na.omit(intl_df69)
intl_df70<- na.omit(intl_df70)
intl_df71<- na.omit(intl_df71)
intl_df72<- na.omit(intl_df72)
intl_df73<- na.omit(intl_df73)
intl_df74<- na.omit(intl_df74)
intl_df75<- na.omit(intl_df75)
intl_df76<- na.omit(intl_df76)
intl_df77<- na.omit(intl_df77)
intl_df78<- na.omit(intl_df78)
intl_df79<- na.omit(intl_df79)
intl_df80<- na.omit(intl_df80)
intl_df81<- na.omit(intl_df81)
intl_df82<- na.omit(intl_df82)
intl_df83<- na.omit(intl_df83)
intl_df84<- na.omit(intl_df84)
intl_df85<- na.omit(intl_df85)
intl_df86<- na.omit(intl_df86)
intl_df87<- na.omit(intl_df87)
intl_df88<- na.omit(intl_df88)
intl_df89<- na.omit(intl_df89)
intl_df90<- na.omit(intl_df90)
intl_df91<- na.omit(intl_df91)
intl_df92<- na.omit(intl_df92)
intl_df93<- na.omit(intl_df93)
intl_df94<- na.omit(intl_df94)
intl_df95<- na.omit(intl_df95)
intl_df96<- na.omit(intl_df96)
intl_df97<- na.omit(intl_df97)
intl_df98<- na.omit(intl_df98)
intl_df99<- na.omit(intl_df99)
intl_df100<- na.omit(intl_df100)
intl_df101<- na.omit(intl_df101)
intl_df102<- na.omit(intl_df102)
intl_df103<- na.omit(intl_df103)
intl_df104<- na.omit(intl_df104)
intl_df105<- na.omit(intl_df105)
intl_df106<- na.omit(intl_df106)
intl_df107<- na.omit(intl_df107)
intl_df108<- na.omit(intl_df108)
intl_df109<- na.omit(intl_df109)
intl_df110<- na.omit(intl_df110)
intl_df111<- na.omit(intl_df111)
intl_df112<- na.omit(intl_df112)
intl_df113<- na.omit(intl_df113)
intl_df114<- na.omit(intl_df114)
intl_df115<- na.omit(intl_df115)
intl_df116<- na.omit(intl_df116)
intl_df117<- na.omit(intl_df117)
intl_df118<- na.omit(intl_df118)
intl_df119<- na.omit(intl_df119)
intl_df120<- na.omit(intl_df120)
intl_df121<- na.omit(intl_df121)
intl_df122<- na.omit(intl_df122)
intl_df123<- na.omit(intl_df123)
intl_df124<- na.omit(intl_df124)
intl_df125<- na.omit(intl_df125)
intl_df126<- na.omit(intl_df126)
intl_df127<- na.omit(intl_df127)
intl_df128<- na.omit(intl_df128)
intl_df129<- na.omit(intl_df129)
intl_df130<- na.omit(intl_df130)
intl_df131<- na.omit(intl_df131)
intl_df132<- na.omit(intl_df132)
intl_df133<- na.omit(intl_df133)
intl_df134<- na.omit(intl_df134)
intl_df135<- na.omit(intl_df135)
intl_df136<- na.omit(intl_df136)
intl_df137<- na.omit(intl_df137)
intl_df138<- na.omit(intl_df138)
intl_df139<- na.omit(intl_df139)
intl_df140<- na.omit(intl_df140)
intl_df141<- na.omit(intl_df141)
intl_df142<- na.omit(intl_df142)
intl_df143<- na.omit(intl_df143)
intl_df144<- na.omit(intl_df144)
intl_df145<- na.omit(intl_df145)
intl_df146<- na.omit(intl_df146)
intl_df147<- na.omit(intl_df147)
intl_df148<- na.omit(intl_df148)
intl_df149<- na.omit(intl_df149)
intl_df150<- na.omit(intl_df150)
intl_df151<- na.omit(intl_df151)
intl_df152<- na.omit(intl_df152)
intl_df153<- na.omit(intl_df153)
intl_df154<- na.omit(intl_df154)
intl_df155<- na.omit(intl_df155)
intl_df156<- na.omit(intl_df156)
intl_df157<- na.omit(intl_df157)
intl_df158<- na.omit(intl_df158)
intl_df159<- na.omit(intl_df159)
intl_df160<- na.omit(intl_df160)
intl_df161<- na.omit(intl_df161)
intl_df162<- na.omit(intl_df162)
intl_df163<- na.omit(intl_df163)
intl_df164<- na.omit(intl_df164)
intl_df165<- na.omit(intl_df165)
intl_df166<- na.omit(intl_df166)
intl_df167<- na.omit(intl_df167)
intl_df168<- na.omit(intl_df168)
intl_df169<- na.omit(intl_df169)
intl_df170<- na.omit(intl_df170)
intl_df171<- na.omit(intl_df171)
intl_df172<- na.omit(intl_df172)
intl_df173<- na.omit(intl_df173)
intl_df174<- na.omit(intl_df174)
intl_df175<- na.omit(intl_df175)
intl_df176<- na.omit(intl_df176)
intl_df177<- na.omit(intl_df177)
intl_df178<- na.omit(intl_df178)
intl_df179<- na.omit(intl_df179)
intl_df180<- na.omit(intl_df180)
intl_df181<- na.omit(intl_df181)
intl_df182<- na.omit(intl_df182)
intl_df183<- na.omit(intl_df183)
intl_df184<- na.omit(intl_df184)
intl_df185<- na.omit(intl_df185)
intl_df186<- na.omit(intl_df186)
intl_df187<- na.omit(intl_df187)
intl_df188<- na.omit(intl_df188)
intl_df189<- na.omit(intl_df189)
intl_df190<- na.omit(intl_df190)
intl_df191<- na.omit(intl_df191)
intl_df192<- na.omit(intl_df192)
intl_df193<- na.omit(intl_df193)
intl_df194<- na.omit(intl_df194)
intl_df195<- na.omit(intl_df195)
intl_df196<- na.omit(intl_df196)
intl_df197<- na.omit(intl_df197)
intl_df198<- na.omit(intl_df198)
intl_df199<- na.omit(intl_df199)




Percentile_Count <- Reduce(function(x,y) merge(x, y, by = c("Player", "GC ID", "J2", "Date Recorded", "Player Type", "GC Profile", "Country", "Cur Age", "Pos"), all = TRUE), list(intl_df1,	intl_df2,	intl_df3,	intl_df4,	intl_df5,	intl_df6,	intl_df7,	intl_df8,	intl_df9,	intl_df10,	intl_df11,	intl_df12,	intl_df13,	intl_df14,	intl_df15,	intl_df16,	intl_df17,	intl_df18,	intl_df19,	intl_df20,	intl_df21,	intl_df22,	intl_df23,	intl_df24,	intl_df25,	intl_df26,	intl_df27,	intl_df28,	intl_df29,	intl_df30,	intl_df31,	intl_df32,	intl_df33,	intl_df34,	intl_df35,	intl_df36,	intl_df37,	intl_df38,	intl_df39,	intl_df40,	intl_df41,	intl_df42,	intl_df43,	intl_df44,	intl_df45,	intl_df46,	intl_df47,	intl_df48,	intl_df49,	intl_df50,	intl_df51,	intl_df52,	intl_df53,	intl_df54,	intl_df55,	intl_df56,	intl_df57,	intl_df58,	intl_df59,	intl_df60,	intl_df61,	intl_df62,	intl_df63,	intl_df64,	intl_df65,	intl_df66,	intl_df67,	intl_df68,	intl_df69,	intl_df70,	intl_df71,	intl_df72,	intl_df73,	intl_df74,	intl_df75,	intl_df76,	intl_df77,	intl_df78,	intl_df79,	intl_df80,	intl_df81,	intl_df82,	intl_df83,	intl_df84,	intl_df85,	intl_df86,	intl_df87,	intl_df88,	intl_df89,	intl_df90,	intl_df91,	intl_df92,	intl_df93,	intl_df94,	intl_df95,	intl_df96,	intl_df97,	intl_df98,	intl_df99,	intl_df100,	intl_df101,	intl_df102,	intl_df103,	intl_df104,	intl_df105,	intl_df106,	intl_df107,	intl_df108,	intl_df109,	intl_df110,	intl_df111,	intl_df112,	intl_df113,	intl_df114,	intl_df115,	intl_df116,	intl_df117,	intl_df118,	intl_df119,	intl_df120,	intl_df121,	intl_df122,	intl_df123,	intl_df124,	intl_df125,	intl_df126,	intl_df127,	intl_df128,	intl_df129,	intl_df130,	intl_df131,	intl_df132,	intl_df133,	intl_df134,	intl_df135,	intl_df136,	intl_df137,	intl_df138,	intl_df139,	intl_df140,	intl_df141,	intl_df142,	intl_df143,	intl_df144,	intl_df145,	intl_df146,	intl_df147,	intl_df148,	intl_df149,	intl_df150,	intl_df151,	intl_df152,	intl_df153,	intl_df154,	intl_df155,	intl_df156,	intl_df157,	intl_df158,	intl_df159,	intl_df160,	intl_df161,	intl_df162,	intl_df163,	intl_df164,	intl_df165,	intl_df166,	intl_df167,	intl_df168,	intl_df169,	intl_df170,	intl_df171,	intl_df172,	intl_df173,	intl_df174,	intl_df175,	intl_df176,	intl_df177,	intl_df178,	intl_df179,	intl_df180,	intl_df181,	intl_df182,	intl_df183,	intl_df184,	intl_df185,	intl_df186,	intl_df187,	intl_df188,	intl_df189,	intl_df190,	intl_df191,	intl_df192,	intl_df193,	intl_df194,	intl_df195,	intl_df196,	intl_df197,	intl_df198,	intl_df199))
#Percentile_Count <- Percentile_Count %>% select(c(1:3, 5:203))
rm(intl_df1,	intl_df2,	intl_df3,	intl_df4,	intl_df5,	intl_df6,	intl_df7,	intl_df8,	intl_df9,	intl_df10,	intl_df11,	intl_df12,	intl_df13,	intl_df14,	intl_df15,	intl_df16,	intl_df17,	intl_df18,	intl_df19,	intl_df20,	intl_df21,	intl_df22,	intl_df23,	intl_df24,	intl_df25,	intl_df26,	intl_df27,	intl_df28,	intl_df29,	intl_df30,	intl_df31,	intl_df32,	intl_df33,	intl_df34,	intl_df35,	intl_df36,	intl_df37,	intl_df38,	intl_df39,	intl_df40,	intl_df41,	intl_df42,	intl_df43,	intl_df44,	intl_df45,	intl_df46,	intl_df47,	intl_df48,	intl_df49,	intl_df50,	intl_df51,	intl_df52,	intl_df53,	intl_df54,	intl_df55,	intl_df56,	intl_df57,	intl_df58,	intl_df59,	intl_df60,	intl_df61,	intl_df62,	intl_df63,	intl_df64,	intl_df65,	intl_df66,	intl_df67,	intl_df68,	intl_df69,	intl_df70,	intl_df71,	intl_df72,	intl_df73,	intl_df74,	intl_df75,	intl_df76,	intl_df77,	intl_df78,	intl_df79,	intl_df80,	intl_df81,	intl_df82,	intl_df83,	intl_df84,	intl_df85,	intl_df86,	intl_df87,	intl_df88,	intl_df89,	intl_df90,	intl_df91,	intl_df92,	intl_df93,	intl_df94,	intl_df95,	intl_df96,	intl_df97,	intl_df98,	intl_df99,	intl_df100,	intl_df101,	intl_df102,	intl_df103,	intl_df104,	intl_df105,	intl_df106,	intl_df107,	intl_df108,	intl_df109,	intl_df110,	intl_df111,	intl_df112,	intl_df113,	intl_df114,	intl_df115,	intl_df116,	intl_df117,	intl_df118,	intl_df119,	intl_df120,	intl_df121,	intl_df122,	intl_df123,	intl_df124,	intl_df125,	intl_df126,	intl_df127,	intl_df128,	intl_df129,	intl_df130,	intl_df131,	intl_df132,	intl_df133,	intl_df134,	intl_df135,	intl_df136,	intl_df137,	intl_df138,	intl_df139,	intl_df140,	intl_df141,	intl_df142,	intl_df143,	intl_df144,	intl_df145,	intl_df146,	intl_df147,	intl_df148,	intl_df149,	intl_df150,	intl_df151,	intl_df152,	intl_df153,	intl_df154,	intl_df155,	intl_df156,	intl_df157,	intl_df158,	intl_df159,	intl_df160,	intl_df161,	intl_df162,	intl_df163,	intl_df164,	intl_df165,	intl_df166,	intl_df167,	intl_df168,	intl_df169,	intl_df170,	intl_df171,	intl_df172,	intl_df173,	intl_df174,	intl_df175,	intl_df176,	intl_df177,	intl_df178,	intl_df179,	intl_df180,	intl_df181,	intl_df182,	intl_df183,	intl_df184,	intl_df185,	intl_df186,	intl_df187,	intl_df188,	intl_df189,	intl_df190,	intl_df191,	intl_df192,	intl_df193,	intl_df194,	intl_df195,	intl_df196,	intl_df197,	intl_df198,	intl_df199)

write_feather(Percentile_Count, "Percentile_Count.feather")








