library(skimr)
library(dplyr)
library(lubridate)
library(ggplot2)

library(readxl)
summary <- read_excel("F:/Project Datasets/all_tables.xlsx")
View(all_tables)

matches <- read_excel("F:/Project Datasets/matches.xlsx")

matches <- mutate(matches, time_converted=hm(time..utc.))

matches <- mutate(matches, total_goals = home_score + away_score)

matches <- filter(matches, total_goals <=10)
League_insights <-summarize(group_by(matches,League),count=n(), total_goals_mean=mean(total_goals))
View(League_insights)

matches <- mutate(matches, sun_time = time_converted <= hm('17:00'))

sun_time_sum <-summarize(group_by(matches,sun_time), goal_sum = sum(total_goals), count=n(),
                         goal_mean=mean(total_goals))
knitr::kable(sun_time_sum, format = "markdown")

ggplot(data=matches)+
  geom_bar(mapping=aes(x=total_goals),color="red")

ggplot(data=matches)+
  geom_histogram(mapping=aes(x=total_goals, y= after_stat(density)),binwidth=0.509, inherit.aes=TRUE, bins=10)+
  facet_wrap(~sun_time)

par(mfrow = c(1, 2))
barplot(prop.table(table(matches$home_score)), main= "Home score rate", ylab= "% of  matches", xlab = "goals")
barplot(prop.table(table(matches$away_score)), main= "Visit score rate", ylab= "% of matches", xlab= "goals")
par(mfrow = c(1, 1))

ggplot(data = matches)+
  geom_histogram(mapping = aes(x = total_goals, y= after_stat(density)))+
  facet_wrap(~League)

League <- arrange(summarize(group_by(matches,League), count = n(), total_goal_mean = mean(total_goals)), total_goal_mean)
knitr::kable(League, format = "markdown")

#Using Second Table

champions <- arrange(filter(summary, Place==1, Year>2000, Year != 2019), P, GP)

insights_champions <- summarize(group_by(champions,League),count=n(), total_points_mean=mean(P), game_played_mean=mean(GP), 
                                Win_mean=mean(W), Lost_mean=mean(L), Draw_mean=mean(D), max(P), min(P), number_champions=n_distinct(Team))
knitr::kable(insights_champions, format = "markdown")

insights_champions_team <- summarize(group_by(champions, Team, League),count=n(), total_points_mean=mean(P), game_played_mean=mean(GP), 
                                     Win_mean=mean(W), Lost_mean=mean(L), Draw_mean=mean(D), max(P), min(P))

insights_champions_winners <- filter(insights_champions_team, count >= 3)

ggplot(data=insights_champions_winners)+
  geom_bar(mapping=aes(x = reorder(Team, -count), y = count, fill = League), stat = "identity", position="dodge")+
  labs(
    x = "Teams",
    y = "Championships",
    title = "Most winning teams in Europe (2001-2021)"
  )

ggplot(data=insights_champions)+
  geom_bar(mapping=aes(x = reorder(League, -number_champions), y = number_champions, fill = League), stat = "identity", position="dodge")+
  labs(
    x = "League",
    y = "different champions",
    title = "Total of different champions in each League (2001-2021)"
  )+
  theme(axis.text.x = element_text(angle = 90))

Dutch <- (filter(insights_champions_team, League == "Dutch Eredivisie"))
Spanish <- (filter(insights_champions_team, League == "Spanish La Liga"))
English <- (filter(insights_champions_team, League == "English Premier League"))
German <- (filter(insights_champions_team, League == "German Bundesliga"))
Italian <- (filter(insights_champions_team, League == "Italian Serie A"))
French <- (filter(insights_champions_team, League == "French Ligue 1"))

par(mfrow = c(2, 3))
pie(Dutch$count,Dutch$Team, main= "Championship rate Dutch Eredivise", col = rainbow(length(Dutch$Team)))
pie(Spanish$count,Spanish$Team, main= "Championship rate Spain", col = rainbow(length(Spanish$Team)))
pie(English$count,English$Team, main= "Championship rate English", col = rainbow(length(English$Team)))
pie(German$count,German$Team, main= "Championship rate German", col = rainbow(length(Italian$Team)))
pie(Italian$count,Italian$Team, main= "Championship rate Italian", col = rainbow(length(French$Team)))
pie(French$count,French$Team, main= "Championship rate French", col = rainbow(length(French$Team)))
par(mfrow = c(1, 1))



champions <- mutate(champions, win_rate= W/GP)

insights_wr <- arrange(summarize(group_by(champions, League), WR_mean=mean(win_rate), WR_max=max(win_rate), WR_min=min(win_rate)), WR_mean)

knitr::kable(insights_wr, format = "markdown")

ggplot(data=insights_wr)+
  geom_bar(mapping=aes(x = reorder(League, -WR_mean), y = WR_mean, fill = League), stat = "identity", position="dodge")+
  labs(
    x = "League",
    y = " Win rate ",
    title = "Champion's Average Win rate "
  )+
  theme(axis.text.x = element_text(angle = 90))

ggplot(data=insights_wr)+
  geom_bar(mapping=aes(x = reorder(League, -WR_min), y = WR_min, fill = League), stat = "identity", position="dodge")+
  labs(
    x = "League",
    y = "Win rate ",
    title = "Champion's Historical min win rate  (2000-2021)"
  )+
  theme(axis.text.x = element_text(angle = 90))

