Workout1
================

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
library(readr)
```

### Data Preparation

``` r
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
```

experience:
-----------

``` r
factor(nba2018$experience)
```

    ##   [1] 9  11 6  R  9  5  4  2  R  6  1  3  2  1  4  11 10 12 11 5  1  5  12
    ##  [24] 13 1  R  8  13 5  5  13 15 5  2  5  1  7  7  R  R  4  10 2  1  5  R 
    ##  [47] 6  7  2  4  7  1  R  R  8  8  6  1  9  5  3  R  3  R  3  12 8  6  11
    ##  [70] 4  12 1  R  14 3  10 3  R  10 3  1  R  3  6  2  17 4  4  R  3  8  4 
    ##  [93] 4  1  9  10 4  R  3  8  12 11 R  2  7  1  6  6  5  11 1  6  1  9  9 
    ## [116] 8  1  1  1  R  13 3  1  5  2  3  2  R  1  10 8  4  8  4  7  9  1  1 
    ## [139] 6  R  R  2  13 7  1  4  4  12 1  1  R  6  5  3  5  R  3  5  1  5  1 
    ## [162] 4  1  1  3  1  4  2  5  9  11 4  R  8  9  R  13 R  8  7  9  3  1  4 
    ## [185] 5  R  R  R  R  9  R  2  1  2  5  9  2  8  2  2  4  8  7  R  1  5  R 
    ## [208] R  4  R  R  7  3  1  8  R  1  2  2  1  3  4  R  1  6  R  4  3  3  8 
    ## [231] R  6  R  6  2  2  2  9  4  10 1  2  2  6  R  12 12 R  13 4  3  2  8 
    ## [254] 9  1  5  13 R  11 7  13 R  7  11 R  R  3  9  1  5  2  10 14 R  7  15
    ## [277] 15 2  R  2  8  R  7  11 1  14 4  8  1  12 R  7  4  6  11 R  11 8  R 
    ## [300] 10 16 8  8  18 11 6  5  13 1  6  8  6  3  2  15 R  1  2  3  5  1  R 
    ## [323] 3  R  2  5  2  1  4  12 5  8  R  3  7  3  R  8  5  R  2  2  1  8  9 
    ## [346] 7  12 3  18 R  R  15 6  3  3  4  6  6  R  2  4  4  2  1  2  7  7  7 
    ## [369] 1  2  R  12 2  R  5  R  3  16 1  8  4  8  6  4  R  7  6  4  5  11 4 
    ## [392] 7  6  7  R  6  3  2  R  R  3  12 18 R  2  4  10 R  R  3  2  R  R  1 
    ## [415] 3  7  8  9  3  R  7  6  R  9  8  2  R  10 R  7  7  1  2  2  8  6  3 
    ## [438] 7  7  1  R  1  5  3  1  2  R  9  1  R  R  2  2  12 16 9  2  4  6  2 
    ## [461] 1  3  5  R  1  R  2  6  9  1  4  13 R  11 2  R  15
    ## Levels: 1 10 11 12 13 14 15 16 17 18 2 3 4 5 6 7 8 9 R

``` r
nba2018$experience[nba2018$experience == "R"] <- 0
as.integer(nba2018$experience)
```

    ##   [1]  9 11  6  0  9  5  4  2  0  6  1  3  2  1  4 11 10 12 11  5  1  5 12
    ##  [24] 13  1  0  8 13  5  5 13 15  5  2  5  1  7  7  0  0  4 10  2  1  5  0
    ##  [47]  6  7  2  4  7  1  0  0  8  8  6  1  9  5  3  0  3  0  3 12  8  6 11
    ##  [70]  4 12  1  0 14  3 10  3  0 10  3  1  0  3  6  2 17  4  4  0  3  8  4
    ##  [93]  4  1  9 10  4  0  3  8 12 11  0  2  7  1  6  6  5 11  1  6  1  9  9
    ## [116]  8  1  1  1  0 13  3  1  5  2  3  2  0  1 10  8  4  8  4  7  9  1  1
    ## [139]  6  0  0  2 13  7  1  4  4 12  1  1  0  6  5  3  5  0  3  5  1  5  1
    ## [162]  4  1  1  3  1  4  2  5  9 11  4  0  8  9  0 13  0  8  7  9  3  1  4
    ## [185]  5  0  0  0  0  9  0  2  1  2  5  9  2  8  2  2  4  8  7  0  1  5  0
    ## [208]  0  4  0  0  7  3  1  8  0  1  2  2  1  3  4  0  1  6  0  4  3  3  8
    ## [231]  0  6  0  6  2  2  2  9  4 10  1  2  2  6  0 12 12  0 13  4  3  2  8
    ## [254]  9  1  5 13  0 11  7 13  0  7 11  0  0  3  9  1  5  2 10 14  0  7 15
    ## [277] 15  2  0  2  8  0  7 11  1 14  4  8  1 12  0  7  4  6 11  0 11  8  0
    ## [300] 10 16  8  8 18 11  6  5 13  1  6  8  6  3  2 15  0  1  2  3  5  1  0
    ## [323]  3  0  2  5  2  1  4 12  5  8  0  3  7  3  0  8  5  0  2  2  1  8  9
    ## [346]  7 12  3 18  0  0 15  6  3  3  4  6  6  0  2  4  4  2  1  2  7  7  7
    ## [369]  1  2  0 12  2  0  5  0  3 16  1  8  4  8  6  4  0  7  6  4  5 11  4
    ## [392]  7  6  7  0  6  3  2  0  0  3 12 18  0  2  4 10  0  0  3  2  0  0  1
    ## [415]  3  7  8  9  3  0  7  6  0  9  8  2  0 10  0  7  7  1  2  2  8  6  3
    ## [438]  7  7  1  0  1  5  3  1  2  0  9  1  0  0  2  2 12 16  9  2  4  6  2
    ## [461]  1  3  5  0  1  0  2  6  9  1  4 13  0 11  2  0 15

salary:
-------

``` r
nba2018$salary<- nba2018$salary/1000000
```

Position:
---------

``` r
nba2018$position<-factor(nba2018$position, labels = c("center","power_fwd","point_guard","small_fwd",'shoot_guard'))
```

``` r
nba2018new<- mutate(nba2018,
                    missed_fg = points3_atts - points3 + points2_atts - points2,
                    missed_ft = points1_atts - points1,
                    rebounds = off_rebounds + def_rebounds,
                    efficiency = (points + rebounds + assists + steals + blocks
                                  - missed_fg - missed_ft - turnovers)/ games )
```

sink
----

``` r
sink('../output/efficiency-summary.txt')
summary(nba2018new$efficiency)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## -0.6667  5.0000  8.3470  9.5790 12.6100 33.8300

creating nba2018.csv
--------------------

``` r
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
```

sink
----

``` r
sink('../data/teams-summary.txt')
summary(teamstats)
```

    ##      team             experience         salary       points3byteam   
    ##  Length:30          Min.   : 39.00   Min.   : 56.29   Min.   : 519.0  
    ##  Class :character   1st Qu.: 59.00   1st Qu.: 85.59   1st Qu.: 620.0  
    ##  Mode  :character   Median : 67.50   Median : 92.72   Median : 718.0  
    ##                     Mean   : 74.13   Mean   : 92.29   Mean   : 737.8  
    ##                     3rd Qu.: 83.00   3rd Qu.:102.93   3rd Qu.: 805.8  
    ##                     Max.   :145.00   Max.   :127.25   Max.   :1140.0  
    ##  points2byteam    freebyteam    pointsbyteam   offrebounds   
    ##  Min.   :1754   Min.   : 998   Min.   :6360   Min.   :521.0  
    ##  1st Qu.:2104   1st Qu.:1240   1st Qu.:7906   1st Qu.:711.0  
    ##  Median :2290   Median :1426   Median :8240   Median :780.0  
    ##  Mean   :2263   Mean   :1375   Mean   :8114   Mean   :776.9  
    ##  3rd Qu.:2458   3rd Qu.:1495   3rd Qu.:8611   3rd Qu.:863.0  
    ##  Max.   :2638   Max.   :1623   Max.   :9491   Max.   :962.0  
    ##   defrebounds     assistteam     stealsteam      blocksteam   
    ##  Min.   :1876   Min.   :1183   Min.   :483.0   Min.   :232.0  
    ##  1st Qu.:2455   1st Qu.:1551   1st Qu.:543.0   1st Qu.:318.2  
    ##  Median :2600   Median :1753   Median :591.5   Median :357.5  
    ##  Mean   :2552   Mean   :1744   Mean   :588.9   Mean   :363.0  
    ##  3rd Qu.:2692   3rd Qu.:1882   3rd Qu.:626.8   3rd Qu.:392.5  
    ##  Max.   :2869   Max.   :2486   Max.   :782.0   Max.   :554.0  
    ##    turnovers        foulsteam      efficiency   
    ##  Min.   : 674.0   Min.   :1168   Min.   :131.5  
    ##  1st Qu.: 972.8   1st Qu.:1444   1st Qu.:144.1  
    ##  Median :1029.5   Median :1567   Median :148.0  
    ##  Mean   :1025.0   Mean   :1519   Mean   :152.3  
    ##  3rd Qu.:1100.5   3rd Qu.:1620   3rd Qu.:160.6  
    ##  Max.   :1212.0   Max.   :1887   Max.   :182.1

``` r
write_csv(teamstats, '../data/nba2018-teams.csv')
```

``` r
teamsalary<-teamstats %>% 
  arrange(salary)

teamsalary$num<- 1:nrow(teamstats)
```

``` r
ggplot(data= teamsalary, 
       aes(x= reorder(team,num),
           y = salary)
       )+
  geom_bar(stat = 'identity')+
  coord_flip()+
  geom_hline(yintercept= mean(teamsalary$salary), col= 'red')
```

![](workout01_files/figure-markdown_github/unnamed-chunk-12-1.png)

``` r
teamspoints<-teamstats %>% 
  arrange(pointsbyteam)

teamspoints$num<- 1:nrow(teamstats)
```

``` r
ggplot(data= teamspoints, 
       aes(x= reorder(team,num),
           y = pointsbyteam)
       )+
  geom_bar(stat = 'identity')+
  coord_flip()+
  geom_hline(yintercept= mean(teamspoints$pointsbyteam), col= 'red')
```

![](workout01_files/figure-markdown_github/unnamed-chunk-14-1.png)

``` r
teamefficiency<-teamstats %>% 
  arrange(efficiency)

teamefficiency$num<- 1:nrow(teamstats)
```

``` r
ggplot(data= teamefficiency, 
       aes(x= reorder(team,num),
           y = efficiency)
       )+
  geom_bar(stat = 'identity')+
  coord_flip()+
  geom_hline(yintercept= mean(teamefficiency$efficiency), col= 'red')
```

![](workout01_files/figure-markdown_github/unnamed-chunk-16-1.png)

I will use assists per turnover to rank a team which means the more assists per turnover by a team, the better coordinated that team is.

``` r
teamstatsnew<-mutate(teamstats, APT = assistteam/turnovers)
teamassistoverturn<-teamstatsnew %>% 
  arrange(APT)

teamassistoverturn$num<- 1:nrow(teamstatsnew)
```

``` r
ggplot(data= teamassistoverturn, 
       aes(x= reorder(team,num),
           y = APT)
       )+
  geom_bar(stat = 'identity')+
  coord_flip()+
  geom_hline(yintercept= mean(teamassistoverturn$APT), col= 'red')
```

![](workout01_files/figure-markdown_github/unnamed-chunk-18-1.png)

### comments and reflections

this was the first time working on a project with such file structures its really complicated Not the first time using relative path yes first time r script things were sooo hard everything was pretty hard except for the repetitive things No help spent 8 hours doing this except for the ggplot nothing was interesting
