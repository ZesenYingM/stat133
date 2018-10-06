"Title: nba2018
Description: preparation of the data
Input: nba2018.csv
Output: efficiency-summary.txt teams-summary.txt nba2018-teams.csv"




### Data Preparation

nba2018<-read_csv('../data/nba2018.csv',
                  col_types= cols(
                    .default = col_integer(),
                    player = col_character(),
                    number = col_character(),
                    team = col_character(),
                    position = col_factor(c("C","PF","PG","SF","SG")),
                    height = col_character(),
                    birth_date = col_character(),
                    country = col_character(),
                    experience = col_character(),
                    college = col_character(),
                    salary = col_double(),
                    field_goals_perc = col_double(),
                    points3_perc = col_double(),
                    points2_perc = col_double(),
                    effective_field_goal_perc = col_double(),
                    points1_perc = col_double()
                  ))
## experience:
  
factor(nba2018$experience)
nba2018$experience[nba2018$experience == "R"] <- 0
as.integer(nba2018$experience)

## salary:
  
nba2018$salary<- nba2018$salary/1000000

## Position:

nba2018$position<-factor(nba2018$position, labels = c("center","power_fwd","point_guard","small_fwd",'shoot_guard'))


nba2018new<- mutate(nba2018,
                    missed_fg = points3_atts - points3 + points2_atts - points2,
                    missed_ft = points1_atts - points1,
                    rebounds = off_rebounds + def_rebounds,
                    efficiency = (points + rebounds + assists + steals + blocks
                                  - missed_fg - missed_ft - turnovers)/ games )

## sink

sink('../output/efficiency-summary.txt')
summary(nba2018new$efficiency)

## creating nba2018.csv

teamstats<-summarise( group_by(nba2018new, team),
                      experience = sum(as.integer(experience)),
                      salary= sum(salary),
                      points3byteam =  sum(points3),
                      points2byteam = sum(points2),
                      freebyteam =  sum(points1),
                      pointsbyteam =  sum(points),
                      offrebounds= sum(off_rebounds),
                      defrebounds= sum(def_rebounds),
                      assistteam = sum(assists),
                      stealsteam = sum(steals), 
                      blocksteam=  sum( blocks),
                      turnovers = sum(turnovers),
                      foulsteam = sum(fouls),
                      efficiency = sum(efficiency))

## sink

sink('../data/teams-summary.txt')
summary(teamstats)

write_csv(teamstats, '../data/nba2018-teams.csv')

