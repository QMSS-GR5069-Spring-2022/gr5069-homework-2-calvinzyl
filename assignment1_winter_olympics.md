---
title: 'Assignment 1: Using ggplot2 for visualization'
author: "Thomas Brambor"
always_allow_html: yes
output:
  html_document:
    keep_md: yes
  pdf_document: default
---

Winter Olympics Medals over Time
================================



## Scenario

Imagine you are the data scientist at a respected media outlet -- say the "New York Times". For the Winter Olympics coverage, your editor-in-chief asks you to analyze some data on the history of `Winter Olympics Medals by Year, Country, Event and Gender` and prepare some data visualizations in which you outline the main patterns around which to base the story.

Since there is **no way that all features of the data can be represented** in such a memo, feel free to pick and choose some patterns that would make for a good story -- outlining important patterns and presenting them in a visually pleasing way. 

The full background and text of the story will be researched by a writer of the magazine -- your input should be based on the data and some common sense (i.e. no need to read up on this). 

Provide **polished plots** that are refined enough to include in the magazine with very little further manipulation (already include variable descriptions [if necessary for understanding], titles, source [e.g. "International Olympic Committee"], right color etc.) and are understandable to the average reader of the "New York Times". The design does not need to be NYTimes-like. Just be consistent.

## Data

The main data is provided as an excel sheet, containing the following variables on all participating athletes in all olympics from 1896 to 2016 (sadly, the original source of the data no longer updates beyond that year):

  - `ID`: a unique indentifier of the entry
  - `Name`: name of the athlete
  - `Sex`: sex of the athlete
  - `Age`: age of the athlete
  - `Height`: height of the athlete
  - `Weight`: weight of the athlete
  - `Team`: usually the country team of the athlete, with the exception of political accomodations, e.g. the "Refugee Olympic Athletes" team.
  - `NOC`: national olympic comittee abbreviation.
  - `Games`: year and season of games.
  - `Year`: year of games
  - `Season`: season of games.
  - `City`: host city
  - `Sport`: a grouping of disciplines
  - `Event`: the particular event / competition  
  - `Medal`: the particular event / competition  

For example, an `event` is a competition in a sport or discipline that gives rise to a ranking. Thus `Alpine Skiing` is the discipline, and `Alpine Skiing Women's Downhills` is a particular event.

In addition, you are provided with some additional information about the countries in a separate spreadsheet, including the `IOC Country	Code`, `Population`, and `GDP per capita`.

## Tasks

#### 1. Medal Counts over Time

a) Combine the information in the three spreadsheets `athletes_and_events.csv`, `noc_regions.csv`, and  `gdp_pop.csv`. Note, that the `noc_regions.csv` is the set all NOC regions, while `gdp_pop.csv` only contains a snapshot of the current set of countries. You have to decide what to do with some [countries that competed under different designations in the past (e.g. Germany and Russia)](https://en.wikipedia.org/wiki/All-time_Olympic_Games_medal_table) and some defunct countries and whether and how to combine their totals. Make sure to be clear about your decisions here, so that the editor (and potentially a user of your visualizations) understands what you did.


```r
library(ggplot2)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
```

```
## ✓ tibble  3.1.6     ✓ purrr   0.3.4
## ✓ tidyr   1.2.0     ✓ stringr 1.4.0
## ✓ readr   2.1.2     ✓ forcats 0.5.1
```

```
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
aaee <- read.csv("/Users/yanlinzhang/Desktop/Data Visualization/assignment-1---winter-olympics-calvinzyl/data/athletes_and_events.csv")
gdp <- read.csv("/Users/yanlinzhang/Desktop/Data Visualization/assignment-1---winter-olympics-calvinzyl/data/gdp_pop.csv")
noc <- read.csv("/Users/yanlinzhang/Desktop/Data Visualization/assignment-1---winter-olympics-calvinzyl/data/noc_regions.csv")
```


```r
library(caret)
```

```
## Loading required package: lattice
```

```
## 
## Attaching package: 'caret'
```

```
## The following object is masked from 'package:purrr':
## 
##     lift
```

```r
# One-hot encode gold, silver, and bronze 
dum <- aaee[ , c("ID", "Medal")]
dummy <- dummyVars(" ~ .", data=dum)
dummy <- data.frame(predict(dummy, newdata=dum))
dummy[is.na(dummy)] <- 0

# Replace strings in Medal with counts (1's)
aae2 <- aaee
aae2["Medal"][aae2["Medal"] == "Gold"] <- 1
aae2["Medal"][aae2["Medal"] == "Silver"] <- 1
aae2["Medal"][aae2["Medal"] == "Bronze"] <- 1
aae2[is.na(aae2)] <- 0

ouch <- data.frame(dummy, aae2)

# Drop all rows from Summer Olympics
aae3 <- ouch[!(ouch$Season=="Summer"),] 

# Ensure team events are counted as a single medal
aae <- distinct(aae3, NOC, Event, Year, Medal, MedalGold, MedalBronze, MedalSilver)

# Compute total winter games each country competed in
games <- aae3 %>% group_by(NOC) %>% 
  summarise(Winter_Games=n_distinct(Games))

# Compute total medals each country won
medals <- aae %>% group_by(NOC) %>%
            summarise(Medals=sum(as.numeric(Medal)),
                      .groups='drop')
golds <- aae %>% group_by(NOC) %>%
            summarise(Golds=sum(as.numeric(MedalGold)),
                      .groups='drop')
silvers <- aae %>% group_by(NOC) %>%
            summarise(Silvers=sum(as.numeric(MedalSilver)),
                      .groups='drop')
bronzes <- aae %>% group_by(NOC) %>%
            summarise(Bronzes=sum(as.numeric(MedalBronze)),
                      .groups='drop')

# Merge games, medals, and the noc dataframe
small_merge1 <- data.frame(games, medals, golds, silvers, bronzes)
small_merge2 <- inner_join(small_merge1, noc, by="NOC")
small_merge2 <- small_merge2[order(small_merge2$region, decreasing=FALSE),]

# Replace past or defunct countries with their current code, if applicable
small_merge2$NOC[small_merge2$NOC == "HKG"] <- "CHN"
small_merge2$NOC[small_merge2$NOC == "TCH"] <- "CZE"
small_merge2$NOC[small_merge2$NOC == "GDR"] <- "GER"
small_merge2$NOC[small_merge2$NOC == "FRG"] <- "GER"
small_merge2$NOC[small_merge2$NOC == "EUN"] <- "RUS"
small_merge2$NOC[small_merge2$NOC == "URS"] <- "RUS"
small_merge2$NOC[small_merge2$NOC == "SCG"] <- "SRB"
small_merge2$NOC[small_merge2$NOC == "YUG"] <- "SRB"

# Sum up total games and medals for countries with multiple designations
small_merge3 <- small_merge2 %>% 
  group_by(NOC) %>% 
  summarize(Winter_Games=sum(Winter_Games), Medals=sum(Medals), Golds=sum(Golds), Silver=sum(Silvers), Bronze=sum(Bronzes))

gdp <- gdp %>% 
  rename(NOC = Code)

Combined <- dplyr::inner_join(small_merge3, gdp, by="NOC") 
print(Combined)
```

```
## # A tibble: 106 × 9
##    NOC   Winter_Games Medals Golds Silver Bronze Country              Population
##    <chr>        <int>  <dbl> <dbl>  <dbl>  <dbl> <chr>                     <int>
##  1 AHO              2      0     0      0      0 Netherlands Antille…         NA
##  2 ALB              3      0     0      0      0 Albania                 2889167
##  3 ALG              3      0     0      0      0 Algeria                39666519
##  4 AND             11      0     0      0      0 Andorra                   70473
##  5 ARG             18      0     0      0      0 Argentina              43416755
##  6 ARM              6      0     0      0      0 Armenia                 3017712
##  7 ASA              1      0     0      0      0 American Samoa*           55538
##  8 AUS             19     13     6      3      4 Australia              23781169
##  9 AUT             22    218    59     78     81 Austria                 8611088
## 10 AZE              5      0     0      0      0 Azerbaijan              9651349
## # … with 96 more rows, and 1 more variable: GDP.per.Capita <dbl>
```

  _For countries with different designations in the past, I added up all the games and medals from those designations since I want align the NOCs with current set of countries but also take into account their past achievements under other designations. For example, I added medals and games from United Team and Soviet Union to those from Russia. Same rule applies to United Team of Germany and East Germany. Those governments of the same country are not strictly administratively identical, but I append their medal and game counts for the purpose of an up-to-date summary._
  
b) Calculate a summary of how many winter games each country competed in, and how many medals of each type the country won. Use that summary to provide a **visual comparison of medal count by country**. 


```r
# Data frame summary of how many winter games each country competed in and how many medals of total medal and each type the country won
summary_games_by_country <- Combined[,c("NOC","Winter_Games", "Medals","Golds", "Silver","Bronze")]
print(summary_games_by_country)
```

```
## # A tibble: 106 × 6
##    NOC   Winter_Games Medals Golds Silver Bronze
##    <chr>        <int>  <dbl> <dbl>  <dbl>  <dbl>
##  1 AHO              2      0     0      0      0
##  2 ALB              3      0     0      0      0
##  3 ALG              3      0     0      0      0
##  4 AND             11      0     0      0      0
##  5 ARG             18      0     0      0      0
##  6 ARM              6      0     0      0      0
##  7 ASA              1      0     0      0      0
##  8 AUS             19     13     6      3      4
##  9 AUT             22    218    59     78     81
## 10 AZE              5      0     0      0      0
## # … with 96 more rows
```

Feel free to focus on smaller set of countries (say the top 10), highlight the United States or another country of your choice, consider gender of the medal winners etc. to make the visualization interesting. 

Please provide (i) one visualization showing an over time comparison and (ii) one visualization in which a total medal count (across all Winter Olympics) is used. Briefly discuss which visualization you recommend to your editor and why.

**Note:** Currently, the medal data contains information on _each athlete_ competing, including for team events. For example, in 2014 Russia received _4 gold medals for their men's win in Bobsleigh Men's Four_ alone. Since this is usually not how it is done in official medal statistics, try to wrangle the data so that _team events are counted as a single medal_. 


```r
# Sort the combined data frame in an decreasing order based on medal counts
sorted_medal <- Combined[order(Combined$Medals, decreasing=TRUE),]
sorted_medal_graph <- head(sorted_medal, 10)

sorted_medal_graph <- sorted_medal_graph[order(sorted_medal_graph$Medals, decreasing=TRUE),]

# Create bar chart to show top 10 countries with most medals
medal_count_winter <- ggplot(sorted_medal_graph, aes(x=reorder(NOC, -Medals), y=Medals)) + 
  geom_bar(stat="identity",width=0.7) + 
  labs(y="Total Number of Medals", x="National Olympic Committee") + 
  ggtitle("Top 10 Countries with Most Winter Olympic Medals of All Time")

# Create bar chart to show top 10 countries with most golds
gold_count_winter <- ggplot(sorted_medal_graph, aes(x=reorder(NOC, -Golds), y=Golds)) + 
  geom_bar(stat="identity",width=0.7) + 
  labs(y="Total Number of Golds") +
  theme(axis.text.x = element_text(size=5),
        axis.title.x = element_blank()) +
  coord_cartesian(ylim = c(0, 140))

# Create bar chart to show top 10 countries with most silvers
silver_count_winter <- ggplot(sorted_medal_graph, aes(x=reorder(NOC, -Silver), y=Silver)) + 
  geom_bar(stat="identity",width=0.7) + 
  labs(y="Total Number of Silvers") +
  theme(axis.text.x = element_text(size=5),
        axis.title.x = element_blank()) +
  coord_cartesian(ylim = c(0, 140))

# Create bar chart to show top 10 countries with most bronzes
bronze_count_winter <- ggplot(sorted_medal_graph, aes(x=reorder(NOC, -Bronze), y=Bronze)) + 
  geom_bar(stat="identity",width=0.7) + 
  labs(y="Total Number of Bronzes") +
  theme(axis.text.x = element_text(size=5),
        axis.title.x = element_blank()) +
  coord_cartesian(ylim = c(0, 140))

library(patchwork)

# Align four graphs
medal_count_winter / (gold_count_winter + silver_count_winter + bronze_count_winter)
```

![](assignment1_winter_olympics_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


```r
# Remove the information that does not belong to selected top 10 countries
rus <- filter(aae, NOC %in% c("RUS"))
usa <- filter(aae, NOC %in% c("USA"))
ger <- filter(aae, NOC %in% c("GER"))
can <- filter(aae, NOC %in% c("CAN"))
nor <- filter(aae, NOC %in% c("NOR"))
swe <- filter(aae, NOC %in% c("SWE"))
fin <- filter(aae, NOC %in% c("FIN"))
aut <- filter(aae, NOC %in% c("AUT"))
sui <- filter(aae, NOC %in% c("SUI"))
cze <- filter(aae, NOC %in% c("CZE"))

# For each country, group by year and summarize medal counts to prepare for over time trend visualizations
rus_trend <- rus %>%
  group_by(Year) %>%
  summarise(Medals=sum(as.numeric(Medal)),
            .groups='drop') %>% 
  mutate(NOC="RUS",
         .after="Medals")

usa_trend <- usa %>%
  group_by(Year) %>%
  summarise(Medals=sum(as.numeric(Medal)),
            .groups='drop') %>% 
  mutate(NOC="USA",
         .after="Medals")

ger_trend <- ger %>%
  group_by(Year) %>%
  summarise(Medals=sum(as.numeric(Medal)),
            .groups='drop') %>% 
  mutate(NOC="GER",
         .after="Medals")

can_trend <- can %>%
  group_by(Year) %>%
  summarise(Medals=sum(as.numeric(Medal)),
            .groups='drop') %>% 
  mutate(NOC="CAN",
         .after="Medals")

nor_trend <- nor %>%
  group_by(Year) %>%
  summarise(Medals=sum(as.numeric(Medal)),
            .groups='drop') %>% 
  mutate(NOC="NOR",
         .after="Medals")

swe_trend <- swe %>%
  group_by(Year) %>%
  summarise(Medals=sum(as.numeric(Medal)),
            .groups='drop') %>% 
  mutate(NOC="SWE",
         .after="Medals")

fin_trend <- fin %>%
  group_by(Year) %>%
  summarise(Medals=sum(as.numeric(Medal)),
            .groups='drop') %>% 
  mutate(NOC="FIN",
         .after="Medals")

aut_trend <- aut %>%
  group_by(Year) %>%
  summarise(Medals=sum(as.numeric(Medal)),
            .groups='drop') %>% 
  mutate(NOC="AUT",
         .after="Medals")

sui_trend <- sui %>%
  group_by(Year) %>%
  summarise(Medals=sum(as.numeric(Medal)),
            .groups='drop') %>% 
  mutate(NOC="SUI",
             .after="Medals")

cze_trend <- cze %>%
  group_by(Year) %>%
  summarise(Medals=sum(as.numeric(Medal)),
            .groups='drop') %>% 
  mutate(NOC="CZE",
          .after="Medals")

plot <- rbind(rus_trend, usa_trend, ger_trend, can_trend, nor_trend,
              swe_trend, fin_trend, aut_trend, sui_trend, cze_trend)

library("ggthemes")

# Create a line plot with top 10 countries' medal counts over time
medal_trend <- ggplot(data = plot, aes(x=Year, y=Medals, group=NOC)) +
  theme_clean() +
  geom_line(aes(color=NOC)) +
  labs(x="National Olympic Committee") + 
  ggtitle("Top 10 Countries with Most Winter Olympic Medals over time")

print(medal_trend)
```

![](assignment1_winter_olympics_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
  
  _Between the over time medal comparison and medal count by countries, I prefer the latter (bar chart combination). This is because a basic bar chart is in my opinion the most suitable graphical representation of comparison. With comparisons of each type of medals in addition, viewers are able to see the medal breakdown of each country as well, although a stacked bar chart in this case would be a more concise choice. On the contrary, in terms of static graphics, the time-series plot above looks messy, making viewers hard to distinguish between different countries. Moreover, almost all countries were absent in some of the Winter Olympics, which makes the over time trends less accurate._

#### 2. Medal Counts adjusted by Population, GDP

There are different ways to calculate "success". Consider the following variants and choose one (and make sure your choice is clear in the visualization):  
  - Just consider gold medals.  
  - Simply add up the number of medals of different types.    
  - Create an index in which medals are valued differently. (gold=3, silver=2, bronze=1).   
  - A reasonable other way that you prefer.
  
Now, adjust the ranking of medal success by (a) GDP per capita and (b) population. You have now three rankings: unadjusted ranking, adjusted by GDP per capita, and adjusted by population.

Visualize how these rankings differ. Try to highlight a specific pattern (e.g. "South Korea -- specialization reaps benefits" or "The superpowers losing their grip").


```r
unadjusted_ranking <- ggplot(sorted_medal_graph, aes(x=reorder(NOC, -Medals), y=Medals)) + 
  geom_bar(stat="identity",width=0.7) + 
  theme(axis.title.y = element_text(size = 8)) +
  labs(y="Total Number of Medals", x="National Olympic Committee") + 
  ggtitle("Unadjusted Ranking")

# Compute adjusted rankings
Combined$MedalsbyPop <- Combined$Medals/Combined$Population
Combined$MedalsbyGDP <- Combined$Medals/Combined$GDP.per.Capita

sorted_pop<- Combined[order(Combined$MedalsbyPop, decreasing=TRUE),]
sorted_pop_graph <- head(sorted_pop, 10)

sorted_gdp<- Combined[order(Combined$MedalsbyGDP, decreasing=TRUE),]
sorted_gdp_graph <- head(sorted_gdp, 10)

adjusted_ranking1 <- ggplot(sorted_pop_graph, aes(x=reorder(NOC, -MedalsbyPop), y=MedalsbyPop)) + 
  geom_bar(stat="identity",width=0.7) + 
  theme(axis.title.y = element_text(size = 8)) +
  labs(y="Medal Counts adjusted by Population", x="National Olympic Committee") + 
  ggtitle("Population-Adjusted Ranking")

adjusted_ranking2 <- ggplot(sorted_gdp_graph, aes(x=reorder(NOC, -MedalsbyGDP), y=MedalsbyGDP)) + 
  geom_bar(stat="identity",width=0.7) +
  theme(axis.title.y = element_text(size = 8)) +
  labs(y="Medal Counts adjusted by GDP", x="National Olympic Committee") + 
  ggtitle("GDP-Adjusted Ranking")

library("cowplot")
```

```
## 
## Attaching package: 'cowplot'
```

```
## The following object is masked from 'package:ggthemes':
## 
##     theme_map
```

```
## The following object is masked from 'package:patchwork':
## 
##     align_plots
```

```r
# Align three bar charts
cowplot::plot_grid(unadjusted_ranking, adjusted_ranking1, adjusted_ranking2)
```

![](assignment1_winter_olympics_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

#### 3. Host Country Advantage

Until the 2014 Sochi Winter Olympics (our data for Winter Olympics end here), there were 19 host cities. Calculate whether the host nation had an advantage. That is calculate whether the host country did win more medals when the Winter Olympics was in their country compared to other times. 

Note, that the 19 host cities are noted in the data but not the countries they are located in. This happens commonly and often Wikipedia has the [kind of additional data you want for the task](https://en.wikipedia.org/wiki/Winter_Olympic_Games). To save you some time, here is a quick way to get this kind of table from Wikipedia into R:


```r
library(rvest)
library(stringr)
library(tidyverse)
wiki_hosts <- read_html("https://en.wikipedia.org/wiki/List_of_Olympic_Games_host_cities")
hosts <- html_table(html_nodes(wiki_hosts, "table")[[2]], fill=TRUE)[-1]
hosts %>% filter(Winter != "") %>%
  select(City, Country, Year)
```

Provide a visualization of the host country advantage (or absence thereof).


```r
# For each country, annotate specific years with host cities
usa_host <- usa_trend %>%
  mutate(Host="None") %>% 
  mutate(City="")
usa_host$Host[usa_host$Year == 1960 | usa_host$Year == 1932 | usa_host$Year == 1980 | usa_host$Year == 2002] <- "Host"
usa_host$City[usa_host$Year == 1932] <- "1932\nLake Placid"
usa_host$City[usa_host$Year == 1960] <- "1960\nSquaw Valley"
usa_host$City[usa_host$Year == 1980] <- "1980\nLake Placid"
usa_host$City[usa_host$Year == 2002] <- "2002\nSalt Lake City"

aut_host <- aut_trend %>%
  mutate(Host="None") %>% 
  mutate(City="")
aut_host$Host[aut_host$Year == 1964 | aut_host$Year == 1976 ] <- "Host"
aut_host$City[aut_host$Year == 1964] <- "1964\nInnsbruck"
aut_host$City[aut_host$Year == 1976] <- "1976\nInnsbruck"

sui_host <- sui_trend %>%
  mutate(Host="None") %>% 
  mutate(City="")
sui_host$Host[sui_host$Year == 1928 | sui_host$Year == 1948 ] <- "Host"
sui_host$City[sui_host$Year == 1928] <- "1928\nSt. Moritz"
sui_host$City[sui_host$Year == 1948] <- "1948\nSt. Moritz"

can_host <- can_trend %>%
  mutate(Host="None") %>% 
  mutate(City="")
can_host$Host[can_host$Year == 1988 | can_host$Year == 2010 ] <- "Host"
can_host$City[can_host$Year == 1988] <- "1988\nCalgary"
can_host$City[can_host$Year == 2010] <- "2010\nVancouver"
```


```r
# Use minimal bar plots to show four countries' over time medal counts and highlight the years of hosts
usa_host_graph <- ggplot(data=usa_host, aes(x=Year, y=Medals, color=Host)) +
  scale_color_grey() +
  theme_classic() +
  theme_tufte(base_size=13, ticks=TRUE) +
  geom_bar(width=1.7, fill="gray", stat = "identity") +
  geom_hline(yintercept=seq(0, 40, 10), col="white", lwd=1) +
  ggtitle("US Host Advantage?") +
  labs(y="Medal Counts") +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size = 11),
        legend.position="none") +
  geom_text(size=2.7, data=usa_host, aes(label=City, vjust= -0.2))

aut_host_graph <- ggplot(data=aut_host, aes(x=Year, y=Medals, color=Host)) +
  scale_color_grey() +
  theme_classic() +
  theme_tufte(base_size=13, ticks=TRUE) +
  geom_bar(width=1.7, fill="gray", stat = "identity") +
  geom_hline(yintercept=seq(0, 20, 5), col="white", lwd=1) +
  ggtitle("Austria Host Advantage?") +
  labs(y="Medal Counts") +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size = 11),
        legend.position="none") +
  geom_text(size=2.7, data=aut_host, aes(label=City, vjust= -0.2))

sui_host_graph <- ggplot(data=sui_host, aes(x=Year, y=Medals, color=Host)) +
  scale_color_grey() +
  theme_classic() +
  theme_tufte(base_size=13, ticks=TRUE) +
  geom_bar(width=1.7, fill="gray", stat = "identity") +
  geom_hline(yintercept=seq(0, 15, 5), col="white", lwd=1) +
  ggtitle("Switzerland Host Advantage?") +
  labs(y="Medal Counts") +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size = 11),
        legend.position="none") +
  geom_text(size=2.7, data=sui_host, aes(label=City, vjust= -0.2))

can_host_graph <- ggplot(data=can_host, aes(x=Year, y=Medals, color=Host)) +
  scale_color_grey() +
  theme_classic() +
  theme_tufte(base_size=13, ticks=TRUE) +
  geom_bar(width=1.7, fill="gray", stat = "identity") +
  geom_hline(yintercept=seq(0, 30, 5), col="white", lwd=1) +
  ggtitle("Canada Host Advantage?") +
  labs(y="Medal Counts") +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size = 11),
        legend.position="none") +
  geom_text(size=2.7, data=can_host, aes(label=City, vjust= -0.2))

library("patchwork")
(usa_host_graph + can_host_graph) / (sui_host_graph + aut_host_graph)
```

![](assignment1_winter_olympics_files/figure-html/unnamed-chunk-9-1.png)<!-- -->
  
  _I randomly picked four countries to explore the host country advantage. One certainly include a lot more to show more convincing patterns, but due to time limit I only did four. It seems like the host advantange is generally true, where I define as the host country obtaining conspicuously more medals than it did in adjacent Olympic games. There is apparently one exception which is Switzerland's 1928 St. Moritz Olympics, and I would say it's reasonable especially in the very early stage of Winter Olympics where everything was not well developed yet._

#### 4. Most successful athletes

a) Now, let's look at the most successful athletes. Provide a visual display of the most successful Winte Olympics athletes of all time.


```r
aae3$Sex[aae3$Sex == "M"] <- 1
aae3$Sex[aae3$Sex == "F"] <- 0

# Group total medals, events, and gender by athlete names
athlete <- aae3 %>%
  mutate(Event_ct=1) %>% 
  group_by(Name) %>%
  summarise(Medals=sum(as.numeric(Medal)),
            Events=sum(Event_ct),
            Gender=mean(as.numeric(Sex)),
            .groups='drop') %>% 
  arrange(desc(Medals))

athlete$Gender[athlete$Gender == 1] <- "Male"
athlete$Gender[athlete$Gender == 0] <- "Female"

sorted_athlete <- head(athlete, 300) 

pointsToLabel <- c("Ole Einar Bjrndalen", "Raisa Petrovna Smetanina",
                   "Stefania Belmondo", "Yang Yang","Lyubov Ivanovna Yegorova",
                   "Kjetil Andr Aamodt")

# Visualize the relationship between events and medals among the most successful athletes
suc_ath <- ggplot(data=sorted_athlete,
                  aes(x=Medals, y=Events, color=Gender)) +
  scale_color_grey() +
  theme_gdocs() +
  geom_point(alpha=0.8, size = 3) +
  labs(y="Events", x="Medals") + 
  ggtitle("Most Successful Athletes of All Time") +
  geom_text(aes(label=Name, hjust=0.9, vjust=1.5),
            color = "gray20",
            data = filter(sorted_athlete, Name %in% pointsToLabel)) +
  theme(plot.title = element_text(size = 18, hjust = 0.5),
        axis.title.x=element_text(size = 13),
        axis.title.y=element_text(size = 13))

print(suc_ath)
```

![](assignment1_winter_olympics_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
  
  _This scatter plot is trying to show a few athletes with most number of medals obtained and events attended. There are, not surprisingly, some big names appearing in the graph such as Ole Bjrndalen, Yang Yang, Raisa Semtanina, and so on. I also grouped the points by gender, which does not show anything outstanding. Note that the plot is static, so there isn't much room to illustrate whom each point corresponds to, and that's the reason why I used ggplotly in Part 5 to add interactive labels of a similar plot, which is way better than this one._

b) Chose of of the athlete specific dimensions (e.g. gender, height, weight) and visualize an interesting pattern in the data.


```r
library(gridExtra)
```

```
## 
## Attaching package: 'gridExtra'
```

```
## The following object is masked from 'package:dplyr':
## 
##     combine
```

```r
library(ggpubr)
```

```
## 
## Attaching package: 'ggpubr'
```

```
## The following object is masked from 'package:cowplot':
## 
##     get_legend
```

```r
aae3 <- aae3 %>% 
  filter(Height != "0") %>% 
  filter(Weight != "0")

# Filter out speed skating and ice hockey athlete dimensions for later comparison
sk <- aae3 %>% 
  filter(Sport == "Speed Skating") 

ih <- aae3 %>% 
  filter(Sport == "Ice Hockey")

# Visualize the relationship between weight and height in speed skating as well as ice hockey
sk_graph <- ggplot(data=sk,
                  aes(x=Weight, y=Height)) +
  geom_point(size=1, alpha=0.5) +
  scale_color_grey() +
  coord_cartesian(ylim = c(140, 210), xlim = c(40, 120)) +
  ggtitle("Speed Skating") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth() +
  stat_cor(label.y=205) +
  stat_regline_equation(label.y=200)

ih_graph <- ggplot(data=ih,
                  aes(x=Weight, y=Height)) +
  geom_point(size=1, alpha=0.5) +
  scale_color_grey() +
  coord_cartesian(ylim = c(140, 210), xlim = c(40, 120)) +
  ggtitle("Ice Hockey") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth() +
  stat_cor(label.y=205) +
  stat_regline_equation(label.y=200)

grid.arrange(sk_graph, ih_graph, ncol=2)
```

```
## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
```

```
## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
```

![](assignment1_winter_olympics_files/figure-html/unnamed-chunk-11-1.png)<!-- -->
  
  
  _In here I did a simple camparison of athlete dimensions between speed skating and ice hockey. The visualization shows that the range of height does not really differ, but athletes in Ice Hockey have generally higher weights, which is fairly intuitive._

### Interactivity

#### 5. Make two plots interactive

Choose 2 of the plots you created above and add interactivity. One of the plots needs to be written in `plotly` rather than just using the `ggplotly` automation. Briefly describe to the editor why interactivity in these visualization is particularly helpful for a reader.


```r
library(plotly)
```

```
## 
## Attaching package: 'plotly'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     last_plot
```

```
## The following object is masked from 'package:stats':
## 
##     filter
```

```
## The following object is masked from 'package:graphics':
## 
##     layout
```

```r
# Use plotly to create an interactive over time medal count comparison between countries
plot_ly(plot, x=~Year, y=~Medals, color=~NOC,
        type="scatter",
        mode="lines+markers",
        line =list(width=3, dash="dot")) %>% 
  layout(title="Top 10 Countries with Most Winter Olympic Medals over time",
         hovermode = "x unified")
```

```
## Warning in RColorBrewer::brewer.pal(N, "Set2"): n too large, allowed maximum for palette Set2 is 8
## Returning the palette you asked for with that many colors

## Warning in RColorBrewer::brewer.pal(N, "Set2"): n too large, allowed maximum for palette Set2 is 8
## Returning the palette you asked for with that many colors
```

```{=html}
<div id="htmlwidget-9f7816dd76b41cce1e46" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-9f7816dd76b41cce1e46">{"x":{"visdat":{"855e5e85dd18":["function () ","plotlyVisDat"]},"cur_data":"855e5e85dd18","attrs":{"855e5e85dd18":{"x":{},"y":{},"mode":"lines+markers","line":{"width":3,"dash":"dot"},"color":{},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatter"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":"Top 10 Countries with Most Winter Olympic Medals over time","hovermode":"x unified","xaxis":{"domain":[0,1],"automargin":true,"title":"Year"},"yaxis":{"domain":[0,1],"automargin":true,"title":"Medals"},"showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"x":[1924,1928,1932,1936,1948,1952,1956,1960,1964,1968,1972,1976,1980,1984,1988,1992,1994,1998,2002,2006,2010,2014],"y":[3,4,2,4,8,8,11,6,12,11,5,6,7,1,10,21,9,17,17,23,16,17],"mode":"lines+markers","line":{"color":"rgba(102,194,165,1)","width":3,"dash":"dot"},"type":"scatter","name":"AUT","marker":{"color":"rgba(102,194,165,1)","line":{"color":"rgba(102,194,165,1)"}},"textfont":{"color":"rgba(102,194,165,1)"},"error_y":{"color":"rgba(102,194,165,1)"},"error_x":{"color":"rgba(102,194,165,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[1924,1928,1932,1936,1948,1952,1956,1960,1964,1968,1972,1976,1980,1984,1988,1992,1994,1998,2002,2006,2010,2014],"y":[1,1,7,1,3,2,3,4,3,3,1,3,2,4,5,7,13,15,17,24,26,25],"mode":"lines+markers","line":{"color":"rgba(228,156,113,1)","width":3,"dash":"dot"},"type":"scatter","name":"CAN","marker":{"color":"rgba(228,156,113,1)","line":{"color":"rgba(228,156,113,1)"}},"textfont":{"color":"rgba(228,156,113,1)"},"error_y":{"color":"rgba(228,156,113,1)"},"error_x":{"color":"rgba(228,156,113,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[1994,1998,2002,2006,2010,2014],"y":[0,3,3,4,6,8],"mode":"lines+markers","line":{"color":"rgba(201,152,157,1)","width":3,"dash":"dot"},"type":"scatter","name":"CZE","marker":{"color":"rgba(201,152,157,1)","line":{"color":"rgba(201,152,157,1)"}},"textfont":{"color":"rgba(201,152,157,1)"},"error_y":{"color":"rgba(201,152,157,1)"},"error_x":{"color":"rgba(201,152,157,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[1924,1928,1932,1936,1948,1952,1956,1960,1964,1968,1972,1976,1980,1984,1988,1992,1994,1998,2002,2006,2010,2014],"y":[11,4,3,6,6,9,7,8,10,5,5,7,9,13,7,7,6,12,7,9,5,5],"mode":"lines+markers","line":{"color":"rgba(175,154,200,1)","width":3,"dash":"dot"},"type":"scatter","name":"FIN","marker":{"color":"rgba(175,154,200,1)","line":{"color":"rgba(175,154,200,1)"}},"textfont":{"color":"rgba(175,154,200,1)"},"error_y":{"color":"rgba(175,154,200,1)"},"error_x":{"color":"rgba(175,154,200,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[1928,1932,1936,1952,1956,1960,1964,1992,1994,1998,2002,2006,2010,2014],"y":[1,2,6,7,2,8,9,26,24,29,36,29,30,19],"mode":"lines+markers","line":{"color":"rgba(226,148,184,1)","width":3,"dash":"dot"},"type":"scatter","name":"GER","marker":{"color":"rgba(226,148,184,1)","line":{"color":"rgba(226,148,184,1)"}},"textfont":{"color":"rgba(226,148,184,1)"},"error_y":{"color":"rgba(226,148,184,1)"},"error_x":{"color":"rgba(226,148,184,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[1924,1928,1932,1936,1948,1952,1956,1960,1964,1968,1972,1976,1980,1984,1988,1992,1994,1998,2002,2006,2010,2014],"y":[17,15,10,15,10,16,4,6,15,14,12,7,10,9,5,17,23,22,24,19,23,26],"mode":"lines+markers","line":{"color":"rgba(176,208,99,1)","width":3,"dash":"dot"},"type":"scatter","name":"NOR","marker":{"color":"rgba(176,208,99,1)","line":{"color":"rgba(176,208,99,1)"}},"textfont":{"color":"rgba(176,208,99,1)"},"error_y":{"color":"rgba(176,208,99,1)"},"error_x":{"color":"rgba(176,208,99,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[1994,1998,2002,2006,2010,2014],"y":[23,18,13,22,15,33],"mode":"lines+markers","line":{"color":"rgba(227,217,62,1)","width":3,"dash":"dot"},"type":"scatter","name":"RUS","marker":{"color":"rgba(227,217,62,1)","line":{"color":"rgba(227,217,62,1)"}},"textfont":{"color":"rgba(227,217,62,1)"},"error_y":{"color":"rgba(227,217,62,1)"},"error_x":{"color":"rgba(227,217,62,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[1924,1928,1932,1936,1948,1952,1956,1960,1964,1968,1972,1976,1980,1984,1988,1992,1994,1998,2002,2006,2010,2014],"y":[3,1,1,3,9,2,6,2,0,6,10,5,5,5,15,3,9,7,11,14,9,11],"mode":"lines+markers","line":{"color":"rgba(245,207,100,1)","width":3,"dash":"dot"},"type":"scatter","name":"SUI","marker":{"color":"rgba(245,207,100,1)","line":{"color":"rgba(245,207,100,1)"}},"textfont":{"color":"rgba(245,207,100,1)"},"error_y":{"color":"rgba(245,207,100,1)"},"error_x":{"color":"rgba(245,207,100,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[1924,1928,1932,1936,1948,1952,1956,1960,1964,1968,1972,1976,1980,1984,1988,1992,1994,1998,2002,2006,2010,2014],"y":[2,5,3,7,10,4,10,7,7,8,4,2,4,8,6,4,3,3,7,14,11,15],"mode":"lines+markers","line":{"color":"rgba(219,192,155,1)","width":3,"dash":"dot"},"type":"scatter","name":"SWE","marker":{"color":"rgba(219,192,155,1)","line":{"color":"rgba(219,192,155,1)"}},"textfont":{"color":"rgba(219,192,155,1)"},"error_y":{"color":"rgba(219,192,155,1)"},"error_x":{"color":"rgba(219,192,155,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[1924,1928,1932,1936,1948,1952,1956,1960,1964,1968,1972,1976,1980,1984,1988,1992,1994,1998,2002,2006,2010,2014],"y":[4,6,12,4,8,11,7,10,7,5,8,10,12,8,6,11,13,13,34,25,37,28],"mode":"lines+markers","line":{"color":"rgba(179,179,179,1)","width":3,"dash":"dot"},"type":"scatter","name":"USA","marker":{"color":"rgba(179,179,179,1)","line":{"color":"rgba(179,179,179,1)"}},"textfont":{"color":"rgba(179,179,179,1)"},"error_y":{"color":"rgba(179,179,179,1)"},"error_x":{"color":"rgba(179,179,179,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

  
  _The first interactive plot is built upon the time-series plot of over time medal comparison between countries in Part 1. I used the line-marker combo to make the curves more eye-catching, if not more aesthetically appealing, and added the x-unified hover mode to enable comparisons in specific cross-sections of time. The idea of interactivity fits this plot well because it's hard to eyeball any detailed patterns in the static version but not in the dynamic one._


```r
sorted_athlete2 <- head(athlete, 500) 

# Use ggplotly to automate the successful athlete static scatter plot
suc_ath_inter <- ggplot(data=sorted_athlete2,
                  aes(x=Medals, y=Events, color=Gender, label=Name)) +
  scale_color_grey() +
  theme_gdocs() +
  geom_point(size = 2) +
  labs(y="Events", x="Medals") + 
  ggtitle("Most Successful Athletes of All Time") +
  theme(plot.title = element_text(size = 18, hjust = 0.5),
        axis.title.x=element_text(size = 13),
        axis.title.y=element_text(size = 13))

ggplotly(suc_ath_inter)
```

```{=html}
<div id="htmlwidget-d1d2a76341743e960bf8" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-d1d2a76341743e960bf8">{"x":{"data":[{"x":[10,10,10,10,9,9,9,8,8,8,8,7,7,7,7,7,7,6,6,6,6,6,6,6,6,6,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,2,2,2,2,2,2,2],"y":[19,15,22,19,15,12,17,12,11,13,10,12,17,17,19,19,11,12,10,15,12,12,20,9,8,13,15,10,9,8,13,13,11,11,9,18,25,5,5,7,14,12,9,7,7,9,16,4,6,4,6,7,8,6,7,10,7,6,6,9,4,4,13,4,8,9,13,13,4,9,6,6,8,8,11,16,4,4,9,7,9,5,6,16,8,11,10,5,11,6,8,7,6,6,5,4,6,4,9,6,8,8,3,10,3,3,6,9,3,3,10,20,14,4,6,3,13,6,3,8,7,4,4,3,4,3,3,16,7,7,12,6,4,9,6,6,8,3,10,3,17,3,3,9,6,9,3,3,5,6,7,11,10,3,15,4,9,9,5,4,4,5,3,4,5,5,5,9,4,3,3,7,6,13,5,3,7,5,5,4,8,6,6,5,2,4,2,4,9],"text":["Medals: 10<br />Events: 19<br />Gender: Female<br />Name: Marit Bjrgen","Medals: 10<br />Events: 15<br />Gender: Female<br />Name: Raisa Petrovna Smetanina","Medals: 10<br />Events: 22<br />Gender: Female<br />Name: Stefania Belmondo","Medals: 10<br />Events: 19<br />Gender: Female<br />Name: Yang Yang","Medals:  9<br />Events: 15<br />Gender: Female<br />Name: Claudia Pechstein","Medals:  9<br />Events: 12<br />Gender: Female<br />Name: Lyubov Ivanovna Yegorova","Medals:  9<br />Events: 17<br />Gender: Female<br />Name: Ursula \"Uschi\" Disl","Medals:  8<br />Events: 12<br />Gender: Female<br />Name: Galina Alekseyevna Kulakova","Medals:  8<br />Events: 11<br />Gender: Female<br />Name: Gunda Niemann-Stirnemann-Kleemann","Medals:  8<br />Events: 13<br />Gender: Female<br />Name: Irene Karlijn \"Ireen\" Wst","Medals:  8<br />Events: 10<br />Gender: Female<br />Name: Karin Enke-Kania (-Busch-, -Richter)","Medals:  7<br />Events: 12<br />Gender: Female<br />Name: Andrea Ehrig-Schne-Mitscherlich","Medals:  7<br />Events: 17<br />Gender: Female<br />Name: Katarina \"Kati\" Wilhelm","Medals:  7<br />Events: 17<br />Gender: Female<br />Name: Larisa Yevgenyevna Lazutina (Ptitsyna-)","Medals:  7<br />Events: 19<br />Gender: Female<br />Name: Manuela Di Centa","Medals:  7<br />Events: 19<br />Gender: Female<br />Name: Marja-Liisa Kirvesniemi-Hmlinen","Medals:  7<br />Events: 11<br />Gender: Female<br />Name: Yelena Valeryevna Vlbe (Trubitsyna-)","Medals:  6<br />Events: 12<br />Gender: Female<br />Name: Anja Sofia Tess Prson","Medals:  6<br />Events: 10<br />Gender: Female<br />Name: Bonnie Kathleen Blair (-Cruikshank)","Medals:  6<br />Events: 15<br />Gender: Female<br />Name: Claudia Knzel-Nystad","Medals:  6<br />Events: 12<br />Gender: Female<br />Name: Cynthia Nicole \"Cindy\" Klassen","Medals:  6<br />Events: 12<br />Gender: Female<br />Name: Janica Kosteli","Medals:  6<br />Events: 20<br />Gender: Female<br />Name: Kateina Neumannov","Medals:  6<br />Events:  9<br />Gender: Female<br />Name: Lidiya Pavlovna Skoblikova (-Polozkova)","Medals:  6<br />Events:  8<br />Gender: Female<br />Name: Wang Meng","Medals:  6<br />Events: 13<br />Gender: Female<br />Name: Yuliya Anatolyevna Chepalova","Medals:  5<br />Events: 15<br />Gender: Female<br />Name: Aino-Kaisa Saarinen","Medals:  5<br />Events: 10<br />Gender: Female<br />Name: Albina Khamitovna Akhatova","Medals:  5<br />Events:  9<br />Gender: Female<br />Name: Alevtina Pavlovna Kolchina (Leontyeva-)","Medals:  5<br />Events:  8<br />Gender: Female<br />Name: Anfisa Anatolyevna Reztsova (Romanova-)","Medals:  5<br />Events: 13<br />Gender: Female<br />Name: Anita Moen-Guidon (-Moen Bonden)","Medals:  5<br />Events: 13<br />Gender: Female<br />Name: Anna Christine \"Anni\" Friesinger-Postma","Medals:  5<br />Events: 11<br />Gender: Female<br />Name: Anni Helena Kivioja-Takalo","Medals:  5<br />Events: 11<br />Gender: Female<br />Name: Arianna Fontana","Medals:  5<br />Events:  9<br />Gender: Female<br />Name: Bente Skari-Martinsen","Medals:  5<br />Events: 18<br />Gender: Female<br />Name: Evi Sachenbacher-Stehle","Medals:  5<br />Events: 25<br />Gender: Female<br />Name: Gabriella Paruzzi","Medals:  5<br />Events:  5<br />Gender: Female<br />Name: Hayley Marie Wickenheiser","Medals:  5<br />Events:  5<br />Gender: Female<br />Name: Jayna Hefford","Medals:  5<br />Events:  7<br />Gender: Female<br />Name: Jeon I-Gyeong","Medals:  5<br />Events: 14<br />Gender: Female<br />Name: Justyna Kowalczyk","Medals:  5<br />Events: 12<br />Gender: Female<br />Name: Katja Seizinger (-Weber)","Medals:  5<br />Events:  9<br />Gender: Female<br />Name: Marina Charlotte Kalla","Medals:  5<br />Events:  7<br />Gender: Female<br />Name: Martina Sblkov","Medals:  5<br />Events:  7<br />Gender: Female<br />Name: Park Seung-Hui","Medals:  5<br />Events:  9<br />Gender: Female<br />Name: Verena \"Vreni\" Schneider","Medals:  4<br />Events: 16<br />Gender: Female<br />Name: Andrea Henkel (-Burke)","Medals:  4<br />Events:  4<br />Gender: Female<br />Name: Angela Marie Ruggiero","Medals:  4<br />Events:  6<br />Gender: Female<br />Name: Antje Misersky-Harvey","Medals:  4<br />Events:  4<br />Gender: Female<br />Name: Caroline Ouellette","Medals:  4<br />Events:  6<br />Gender: Female<br />Name: Cathy Ann Turner","Medals:  4<br />Events:  7<br />Gender: Female<br />Name: Choi Eun-Gyeong","Medals:  4<br />Events:  8<br />Gender: Female<br />Name: Christa Rothenburger-Luding","Medals:  4<br />Events:  6<br />Gender: Female<br />Name: Christina Wilhelmina \"Stien\" Baas-Kaiser","Medals:  4<br />Events:  7<br />Gender: Female<br />Name: Clara Hughes","Medals:  4<br />Events: 10<br />Gender: Female<br />Name: Darya Vladimirovna Domracheva","Medals:  4<br />Events:  7<br />Gender: Female<br />Name: Deborah Compagnoni","Medals:  4<br />Events:  6<br />Gender: Female<br />Name: Dianne Mary Holum","Medals:  4<br />Events:  6<br />Gender: Female<br />Name: Hannelore \"Hanni\" Wenzel (-Weirather)","Medals:  4<br />Events:  9<br />Gender: Female<br />Name: Hilkka Maria Riihivuori-Kuntola","Medals:  4<br />Events:  4<br />Gender: Female<br />Name: Jennifer Lori Botterill","Medals:  4<br />Events:  4<br />Gender: Female<br />Name: Jennifer Lynn \"Jenny\" Schmidgall-Potter","Medals:  4<br />Events: 13<br />Gender: Female<br />Name: Julia Marie Mancuso (-Fish)","Medals:  4<br />Events:  4<br />Gender: Female<br />Name: Julie Wu Chu","Medals:  4<br />Events:  8<br />Gender: Female<br />Name: Kaija Marja Mustonen","Medals:  4<br />Events:  9<br />Gender: Female<br />Name: Katrin Apel","Medals:  4<br />Events: 13<br />Gender: Female<br />Name: Klementina \"Tina\" Maze","Medals:  4<br />Events: 13<br />Gender: Female<br />Name: Kristina Nicole Groves","Medals:  4<br />Events:  4<br />Gender: Female<br />Name: Lyubov Vladimirovna Kozyreva-Baranova (-Vatina)","Medals:  4<br />Events:  9<br />Gender: Female<br />Name: Maria Hfl-Riesch","Medals:  4<br />Events:  6<br />Gender: Female<br />Name: Marjo Tuulevi Matikainen (-Kallstrm)","Medals:  4<br />Events:  6<br />Gender: Female<br />Name: Marlies Schild (-Raich)","Medals:  4<br />Events:  8<br />Gender: Female<br />Name: Martina Glagow-Beck","Medals:  4<br />Events:  8<br />Gender: Female<br />Name: Nataliya Anatolyevna Petrusyova (-Komarova)","Medals:  4<br />Events: 11<br />Gender: Female<br />Name: Nina Vasilyevna Gavrylyuk","Medals:  4<br />Events: 16<br />Gender: Female<br />Name: Olga Alekseyevna Zaytseva (-Augustin)","Medals:  4<br />Events:  4<br />Gender: Female<br />Name: Radiya Nikolayevna Yeroshina (-Nurgayeva)","Medals:  4<br />Events:  4<br />Gender: Female<br />Name: Rebecca D. \"Becky\" Kellar (-Duke)","Medals:  4<br />Events:  9<br />Gender: Female<br />Name: Ritva Marjatta Kajosmaa (Sakki-)","Medals:  4<br />Events:  7<br />Gender: Female<br />Name: Sabine Vlker","Medals:  4<br />Events:  9<br />Gender: Female<br />Name: Tania Vicent","Medals:  4<br />Events:  5<br />Gender: Female<br />Name: Tatyana Borisovna Averina-Barabash","Medals:  4<br />Events:  6<br />Gender: Female<br />Name: Toini Gustafsson (Karvonen-, -Rnnlund)","Medals:  4<br />Events: 16<br />Gender: Female<br />Name: Tora Berger (-Tveldal)","Medals:  3<br />Events:  8<br />Gender: Female<br />Name: Alexandra Meissnitzer","Medals:  3<br />Events: 11<br />Gender: Female<br />Name: Amy Eileen Peterson (-Peck)","Medals:  3<br />Events: 10<br />Gender: Female<br />Name: Anastasia Vladimirovna Kuzmina (Shipulina-)","Medals:  3<br />Events:  5<br />Gender: Female<br />Name: Andre Marguerite Blanche Brunet-Joly","Medals:  3<br />Events: 11<br />Gender: Female<br />Name: Anita Wachter (-Salzgeber)","Medals:  3<br />Events:  6<br />Gender: Female<br />Name: Anna Margret Haag (Hansson-)","Medals:  3<br />Events:  8<br />Gender: Female<br />Name: Anne Briand-Bouthiaux","Medals:  3<br />Events:  7<br />Gender: Female<br />Name: Anne Jahren","Medals:  3<br />Events:  6<br />Gender: Female<br />Name: Annemarie \"Mirl\" Buchner (Fischer-)","Medals:  3<br />Events:  6<br />Gender: Female<br />Name: Annemarie Moser-Prll","Medals:  3<br />Events:  5<br />Gender: Female<br />Name: Annie Perreault","Medals:  3<br />Events:  4<br />Gender: Female<br />Name: Atje Keulen-Deelstra","Medals:  3<br />Events:  6<br />Gender: Female<br />Name: Barbara Petzold (-Beyer)","Medals:  3<br />Events:  4<br />Gender: Female<br />Name: Beatrix Suzetta Loughran (-Harvey)","Medals:  3<br />Events:  9<br />Gender: Female<br />Name: Berit Kristine Aunli-Kvello","Medals:  3<br />Events:  6<br />Gender: Female<br />Name: Berit Mrdre-Lammedal","Medals:  3<br />Events:  8<br />Gender: Female<br />Name: Brit Pettersen (-Tofte)","Medals:  3<br />Events:  8<br />Gender: Female<br />Name: Britt Marianne Strandberg (-Lundn)","Medals:  3<br />Events:  3<br />Gender: Female<br />Name: Cassie Dawin Campbell (-Pascall)","Medals:  3<br />Events: 10<br />Gender: Female<br />Name: Catriona Ann Le May Doan","Medals:  3<br />Events:  3<br />Gender: Female<br />Name: Charline \"Charlie\" Labont","Medals:  3<br />Events:  3<br />Gender: Female<br />Name: Cherie Piper","Medals:  3<br />Events:  6<br />Gender: Female<br />Name: Christa Kinshofer-Gthlein (-Rembeck)","Medals:  3<br />Events:  9<br />Gender: Female<br />Name: Christine Diane \"Chris\" Witty","Medals:  3<br />Events:  3<br />Gender: Female<br />Name: Colleen Sostorics","Medals:  3<br />Events:  3<br />Gender: Female<br />Name: Danielle Goyette","Medals:  3<br />Events: 10<br />Gender: Female<br />Name: Elin Nilsen","Medals:  3<br />Events: 20<br />Gender: Female<br />Name: Emese Nemeth-Hunyady (-Jrvinen)","Medals:  3<br />Events: 14<br />Gender: Female<br />Name: Evgeniya Nikolova Radanova","Medals:  3<br />Events:  4<br />Gender: Female<br />Name: Gabriele \"Gabi\" Zange-Schnbrunn","Medals:  3<br />Events:  6<br />Gender: Female<br />Name: Galina Alekseyevna Kukleva","Medals:  3<br />Events:  3<br />Gender: Female<br />Name: Gillian Mary Apps","Medals:  3<br />Events: 13<br />Gender: Female<br />Name: Inger Helene Nybrten","Medals:  3<br />Events:  6<br />Gender: Female<br />Name: Inger Reidun Aufles (Dving-)","Medals:  3<br />Events:  3<br />Gender: Female<br />Name: Irina Konstantinovna Rodnina (-Zaytseva, -Minkovskaya)","Medals:  3<br />Events:  8<br />Gender: Female<br />Name: Isabelle Charest (-Charbonneau)","Medals:  3<br />Events:  7<br />Gender: Female<br />Name: Isolde Kostner (-Perathoner)","Medals:  3<br />Events:  4<br />Gender: Female<br />Name: Jin Seon-Yu","Medals:  3<br />Events:  4<br />Gender: Female<br />Name: Kari Traa","Medals:  3<br />Events:  3<br />Gender: Female<br />Name: Kathryn \"Katie\" King","Medals:  3<br />Events:  4<br />Gender: Female<br />Name: Kelly Clark","Medals:  3<br />Events:  3<br />Gender: Female<br />Name: Kim St-Pierre","Medals:  3<br />Events:  3<br />Gender: Female<br />Name: Klavdiya Sergeyevna Boyarskikh","Medals:  3<br />Events: 16<br />Gender: Female<br />Name: Kristina migun-Vhi","Medals:  3<br />Events:  7<br />Gender: Female<br />Name: Kvtoslava \"Kvta\" Jeriov (-Peckov)","Medals:  3<br />Events:  7<br />Gender: Female<br />Name: Leah Jean Poulos-Mueller","Medals:  3<br />Events: 12<br />Gender: Female<br />Name: Liv Grete Skjelbreid-Poire","Medals:  3<br />Events:  6<br />Gender: Female<br />Name: Lyudmila Yevgenyevna Titova","Medals:  3<br />Events:  4<br />Gender: Female<br />Name: Magdalena Neuner (-Holzer)","Medals:  3<br />Events:  9<br />Gender: Female<br />Name: Maria Aaltje \"Marianne\" Timmer","Medals:  3<br />Events:  6<br />Gender: Female<br />Name: Maria Walliser (-Anesini)","Medals:  3<br />Events:  6<br />Gender: Female<br />Name: Marianne St-Gelais","Medals:  3<br />Events:  8<br />Gender: Female<br />Name: Marie-Theres Nadig","Medals:  3<br />Events:  3<br />Gender: Female<br />Name: Marina Vladimirovna Klimova (-Ponomarenko)","Medals:  3<br />Events: 10<br />Gender: Female<br />Name: Marit Elisabeth Wold-Mikkelsplass","Medals:  3<br />Events:  3<br />Gender: Female<br />Name: Mariya Ivanovna Gusakova (Kudimova-)","Medals:  3<br />Events: 17<br />Gender: Female<br />Name: Martina Maria Ertl-Renz","Medals:  3<br />Events:  3<br />Gender: Female<br />Name: Meghan Christina Agosta (-Marciano)","Medals:  3<br />Events:  3<br />Gender: Female<br />Name: Meryl Elizabeth Davis","Medals:  3<br />Events:  9<br />Gender: Female<br />Name: Michaela Dorfmeister","Medals:  3<br />Events:  6<br />Gender: Female<br />Name: Micheline Franoise Marielle Goitschel","Medals:  3<br />Events:  9<br />Gender: Female<br />Name: Myriam Bdard","Medals:  3<br />Events:  3<br />Gender: Female<br />Name: Natalie Geisenberger","Medals:  3<br />Events:  3<br />Gender: Female<br />Name: Natalie Rose Darwitz","Medals:  3<br />Events:  5<br />Gender: Female<br />Name: Nathalie Brigitte Lambert","Medals:  3<br />Events:  6<br />Gender: Female<br />Name: Nicole Hosp","Medals:  3<br />Events:  7<br />Gender: Female<br />Name: Nina Viktorovna Fyodorova-Baldycheva","Medals:  3<br />Events: 11<br />Gender: Female<br />Name: Olga Valeryevna Danilova","Medals:  3<br />Events: 10<br />Gender: Female<br />Name: Olga Valeryevna Pylyova-Medvedtseva (Zamorozova-)","Medals:  3<br />Events:  3<br />Gender: Female<br />Name: Patricia \"Tricia\" Dunn-Luoma","Medals:  3<br />Events: 15<br />Gender: Female<br />Name: Pernilla Christina Wiberg (-Bjerke)","Medals:  3<br />Events:  4<br />Gender: Female<br />Name: Perrine Marie Pelen (-Mazzega)","Medals:  3<br />Events:  9<br />Gender: Female<br />Name: Petra Behle-Schaaf","Medals:  3<br />Events:  9<br />Gender: Female<br />Name: Rosemarie \"Rosi\" Mittermaier (-Neureuther)","Medals:  3<br />Events:  5<br />Gender: Female<br />Name: Sheila Grace Young (-Ochowicz)","Medals:  3<br />Events:  4<br />Gender: Female<br />Name: Shen Xue","Medals:  3<br />Events:  4<br />Gender: Female<br />Name: Shim Seok-Hui","Medals:  3<br />Events:  5<br />Gender: Female<br />Name: Siiri Johanna Rantanen (Lintunen-)","Medals:  3<br />Events:  3<br />Gender: Female<br />Name: Silke Kraushaar (-Pielach)","Medals:  3<br />Events:  4<br />Gender: Female<br />Name: Sonja Henie (-Topping, -Gardiner, -Onstad)","Medals:  3<br />Events:  5<br />Gender: Female<br />Name: Sonja Viola Edstrm-Ruthstrm","Medals:  3<br />Events:  5<br />Gender: Female<br />Name: Stephanie Beckert","Medals:  3<br />Events:  5<br />Gender: Female<br />Name: Susi-Lisa Erdmann (-Plankensteiner)","Medals:  3<br />Events:  9<br />Gender: Female<br />Name: Svetlana Irekovna Ishmuratova","Medals:  3<br />Events:  4<br />Gender: Female<br />Name: Tamara Ivanovna Tikhonova","Medals:  3<br />Events:  3<br />Gender: Female<br />Name: Tatjana Hfner","Medals:  3<br />Events:  3<br />Gender: Female<br />Name: Tessa Jane Virtue (-McCormick)","Medals:  3<br />Events:  7<br />Gender: Female<br />Name: Therese Johaug","Medals:  3<br />Events:  6<br />Gender: Female<br />Name: Tiril Kampenhaug Eckhoff","Medals:  3<br />Events: 13<br />Gender: Female<br />Name: Trude Dybendahl-Hartz","Medals:  3<br />Events:  5<br />Gender: Female<br />Name: Trude Jochum-Beiser","Medals:  3<br />Events:  3<br />Gender: Female<br />Name: Vicky Sunohara","Medals:  3<br />Events:  7<br />Gender: Female<br />Name: Viola Bauer","Medals:  3<br />Events:  5<br />Gender: Female<br />Name: Wang Chunlu","Medals:  3<br />Events:  5<br />Gender: Female<br />Name: Won Hye-Gyeong","Medals:  3<br />Events:  4<br />Gender: Female<br />Name: Ye Qiaobo","Medals:  3<br />Events:  8<br />Gender: Female<br />Name: Yvonne Maria Therse van Gennip","Medals:  3<br />Events:  6<br />Gender: Female<br />Name: Zhou Yang","Medals:  2<br />Events:  6<br />Gender: Female<br />Name: Alanna Kraus (-Handley)","Medals:  2<br />Events:  5<br />Gender: Female<br />Name: Alevtina Sergeyevna Olyunina-Smirnova (-Panarina)","Medals:  2<br />Events:  2<br />Gender: Female<br />Name: Alisa Peta Camplin (-Warner)","Medals:  2<br />Events:  4<br />Gender: Female<br />Name: Aljona Sawtchenko","Medals:  2<br />Events:  2<br />Gender: Female<br />Name: Allison Jaime \"A. J.\" Mleczko (-Griswold)","Medals:  2<br />Events:  4<br />Gender: Female<br />Name: Amelie Kober","Medals:  2<br />Events:  9<br />Gender: Female<br />Name: Andrea Bario Mead-Lawrence"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(51,51,51,1)","opacity":1,"size":7.55905511811024,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(51,51,51,1)"}},"hoveron":"points","name":"Female","legendgroup":"Female","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[13,9,8,8,8,8,8,7,7,7,7,7,7,7,7,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2],"y":[27,12,12,20,15,15,10,8,8,12,13,9,11,10,16,7,6,8,18,21,11,7,12,8,11,19,19,11,12,6,9,8,5,9,12,7,10,6,6,7,10,6,14,7,9,18,9,14,10,12,5,8,9,9,5,15,7,12,7,9,5,7,13,4,4,8,4,11,9,9,10,7,10,10,5,4,7,4,6,8,12,6,18,6,5,9,7,4,6,14,4,10,5,4,4,6,5,5,6,4,8,16,6,6,11,8,8,4,6,11,11,7,4,6,11,13,4,4,6,9,6,11,9,14,6,4,4,6,6,4,6,12,10,6,3,3,3,3,4,3,3,6,5,9,3,4,16,3,3,12,4,8,4,4,3,3,5,3,13,17,7,10,4,10,3,7,10,9,9,6,3,10,8,3,8,11,3,3,15,7,13,4,15,4,5,9,5,6,6,6,3,9,5,6,8,7,11,3,4,6,3,9,7,3,4,5,4,4,6,4,11,7,6,3,3,6,19,4,8,6,6,4,11,13,4,17,12,6,5,18,7,3,3,4,10,3,7,5,5,6,6,12,4,8,4,3,3,3,3,6,8,6,4,4,4,4,4,3,5,5,3,3,3,3,13,3,3,3,3,3,6,5,7,10,4,3,2,9,2,2,2,2,2,2,11,3,2,18,5,2,3,2,3,4,3,3,2,2,2,9,3,3,9,5,3,4,10,2,6,12,4],"text":["Medals: 13<br />Events: 27<br />Gender: Male<br />Name: Ole Einar Bjrndalen","Medals:  9<br />Events: 12<br />Gender: Male<br />Name: Edy Sixten Jernberg","Medals:  8<br />Events: 12<br />Gender: Male<br />Name: Apolo Anton Ohno","Medals:  8<br />Events: 20<br />Gender: Male<br />Name: Kjetil Andr Aamodt","Medals:  8<br />Events: 15<br />Gender: Male<br />Name: Ricco Gro","Medals:  8<br />Events: 15<br />Gender: Male<br />Name: Sven Fischer","Medals:  8<br />Events: 10<br />Gender: Male<br />Name: Viktor An","Medals:  7<br />Events:  8<br />Gender: Male<br />Name: Arnold Clas Robert Thunberg","Medals:  7<br />Events:  8<br />Gender: Male<br />Name: Bogdan Musiol","Medals:  7<br />Events: 12<br />Gender: Male<br />Name: Eero Antero Mntyranta","Medals:  7<br />Events: 13<br />Gender: Male<br />Name: Felix Gottwald","Medals:  7<br />Events:  9<br />Gender: Male<br />Name: Ivar Eugen Ballangrud (Eriksen-)","Medals:  7<br />Events: 11<br />Gender: Male<br />Name: Sven Kramer","Medals:  7<br />Events: 10<br />Gender: Male<br />Name: Veikko Johannes Hakulinen","Medals:  7<br />Events: 16<br />Gender: Male<br />Name: Vladimir Mikhaylovich Smirnov","Medals:  6<br />Events:  7<br />Gender: Male<br />Name: Armin Zggeler","Medals:  6<br />Events:  6<br />Gender: Male<br />Name: Eugenio Monti","Medals:  6<br />Events:  8<br />Gender: Male<br />Name: Gunde Anders Svan","Medals:  6<br />Events: 18<br />Gender: Male<br />Name: Halvard Hanevold","Medals:  6<br />Events: 21<br />Gender: Male<br />Name: Harri Tapani Kirvesniemi","Medals:  6<br />Events: 11<br />Gender: Male<br />Name: Johan Arne Olsson","Medals:  6<br />Events:  7<br />Gender: Male<br />Name: Johan Hagbart Pedersen Grttumsbraaten","Medals:  6<br />Events: 12<br />Gender: Male<br />Name: Mika Kristian Myllyl","Medals:  6<br />Events:  8<br />Gender: Male<br />Name: Roald Morel Larsen","Medals:  6<br />Events: 11<br />Gender: Male<br />Name: Robert Rintje Ritsma","Medals:  6<br />Events: 19<br />Gender: Male<br />Name: Samuel Bode Miller","Medals:  6<br />Events: 19<br />Gender: Male<br />Name: Sergey Vladimirovich Chepikov","Medals:  6<br />Events: 11<br />Gender: Male<br />Name: Thomas Alsgaard (Alsgrd-)","Medals:  6<br />Events: 12<br />Gender: Male<br />Name: Vegard Ulvang","Medals:  6<br />Events:  6<br />Gender: Male<br />Name: Wolfgang Hoppe","Medals:  5<br />Events:  9<br />Gender: Male<br />Name: Alberto Tomba","Medals:  5<br />Events:  8<br />Gender: Male<br />Name: Aleksandr Ivanovich Tikhonov","Medals:  5<br />Events:  5<br />Gender: Male<br />Name: Andr Lange","Medals:  5<br />Events:  9<br />Gender: Male<br />Name: Chad Paul Hedrick","Medals:  5<br />Events: 12<br />Gender: Male<br />Name: Emil Hegle Svendsen","Medals:  5<br />Events:  7<br />Gender: Male<br />Name: Eric Arthur Heiden","Medals:  5<br />Events: 10<br />Gender: Male<br />Name: Frank Luck","Medals:  5<br />Events:  6<br />Gender: Male<br />Name: Franois-Louis Tremblay","Medals:  5<br />Events:  6<br />Gender: Male<br />Name: Fritz Feierabend","Medals:  5<br />Events:  7<br />Gender: Male<br />Name: Georg Hackl","Medals:  5<br />Events: 10<br />Gender: Male<br />Name: Harald Grnningen","Medals:  5<br />Events:  6<br />Gender: Male<br />Name: Johann Olav Koss","Medals:  5<br />Events: 14<br />Gender: Male<br />Name: Juha Iisakki Mieto","Medals:  5<br />Events:  7<br />Gender: Male<br />Name: Kevin Kuske","Medals:  5<br />Events:  9<br />Gender: Male<br />Name: Knut Johannesen","Medals:  5<br />Events: 18<br />Gender: Male<br />Name: Lasse Kjus (Kristoffersen-)","Medals:  5<br />Events:  9<br />Gender: Male<br />Name: Lee Ho-Seok","Medals:  5<br />Events: 14<br />Gender: Male<br />Name: Li Jiajun","Medals:  5<br />Events: 10<br />Gender: Male<br />Name: Marc Gagnon","Medals:  5<br />Events: 12<br />Gender: Male<br />Name: Marco Albarello","Medals:  5<br />Events:  5<br />Gender: Male<br />Name: Matti Ensio Nyknen (-Paanala)","Medals:  5<br />Events:  8<br />Gender: Male<br />Name: Nikolay Semyonovich Zimyatov","Medals:  5<br />Events:  9<br />Gender: Male<br />Name: Peter Angerer","Medals:  5<br />Events:  9<br />Gender: Male<br />Name: Pl Bjarne Tyldum","Medals:  5<br />Events:  5<br />Gender: Male<br />Name: Samppa Kalevi Lajunen","Medals:  5<br />Events: 15<br />Gender: Male<br />Name: Silvio Fauner","Medals:  5<br />Events:  7<br />Gender: Male<br />Name: Yevgeny Romanovich Grishin","Medals:  4<br />Events: 12<br />Gender: Male<br />Name: Adam Henryk Maysz","Medals:  4<br />Events:  7<br />Gender: Male<br />Name: Adrie \"Ard\" Schenk","Medals:  4<br />Events:  9<br />Gender: Male<br />Name: Aleksandr Yuryevich Zubkov","Medals:  4<br />Events:  5<br />Gender: Male<br />Name: Aleksey Ivanovich Voyevoda","Medals:  4<br />Events:  7<br />Gender: Male<br />Name: Beat Hefti","Medals:  4<br />Events: 13<br />Gender: Male<br />Name: Benjamin Raich","Medals:  4<br />Events:  4<br />Gender: Male<br />Name: Bernhard Germeshausen","Medals:  4<br />Events:  4<br />Gender: Male<br />Name: Bernhard Lehmann","Medals:  4<br />Events:  8<br />Gender: Male<br />Name: Bernt Sverre Evensen","Medals:  4<br />Events:  4<br />Gender: Male<br />Name: Bjarte Engen Vik","Medals:  4<br />Events: 11<br />Gender: Male<br />Name: Bjrn Kircheisen","Medals:  4<br />Events:  9<br />Gender: Male<br />Name: Bob Johannes Carolus de Jong","Medals:  4<br />Events:  9<br />Gender: Male<br />Name: Carl Marcus Joakim Hellner","Medals:  4<br />Events: 10<br />Gender: Male<br />Name: Charles Hamelin","Medals:  4<br />Events:  7<br />Gender: Male<br />Name: Christoph Langen","Medals:  4<br />Events: 10<br />Gender: Male<br />Name: Cornelis Arie \"Kees\" Verkerk","Medals:  4<br />Events: 10<br />Gender: Male<br />Name: Denny Morrison","Medals:  4<br />Events:  5<br />Gender: Male<br />Name: Donat Acklin","Medals:  4<br />Events:  4<br />Gender: Male<br />Name: Erich Schrer","Medals:  4<br />Events:  7<br />Gender: Male<br />Name: Frank Ullrich","Medals:  4<br />Events:  4<br />Gender: Male<br />Name: Fred Anton Maier","Medals:  4<br />Events:  6<br />Gender: Male<br />Name: Fred Brre Lundberg","Medals:  4<br />Events:  8<br />Gender: Male<br />Name: Frode Estil","Medals:  4<br />Events: 12<br />Gender: Male<br />Name: Gatan T. Boucher","Medals:  4<br />Events:  6<br />Gender: Male<br />Name: Georg Hettich","Medals:  4<br />Events: 18<br />Gender: Male<br />Name: Giorgio Vanzetta","Medals:  4<br />Events:  6<br />Gender: Male<br />Name: Gregor Schlierenzauer","Medals:  4<br />Events:  5<br />Gender: Male<br />Name: Gustav Weder","Medals:  4<br />Events:  9<br />Gender: Male<br />Name: Hallgeir Brenden","Medals:  4<br />Events:  7<br />Gender: Male<br />Name: Hermann Maier","Medals:  4<br />Events:  4<br />Gender: Male<br />Name: Igor Aleksandrovich Kravchuk","Medals:  4<br />Events:  6<br />Gender: Male<br />Name: Ivar Formo","Medals:  4<br />Events: 14<br />Gender: Male<br />Name: Ivica Kosteli","Medals:  4<br />Events:  4<br />Gender: Male<br />Name: Jan Behrendt","Medals:  4<br />Events: 10<br />Gender: Male<br />Name: Jens Weiflog","Medals:  4<br />Events:  5<br />Gender: Male<br />Name: Jere Kalervo Lehtinen","Medals:  4<br />Events:  4<br />Gender: Male<br />Name: Ji Holk","Medals:  4<br />Events:  4<br />Gender: Male<br />Name: Josef \"Sepp\" Benz","Medals:  4<br />Events:  6<br />Gender: Male<br />Name: Julius Ferninand Skutnabb","Medals:  4<br />Events:  5<br />Gender: Male<br />Name: Kimmo Samuel Timonen","Medals:  4<br />Events:  5<br />Gender: Male<br />Name: Klaus Sulzenbacher","Medals:  4<br />Events:  6<br />Gender: Male<br />Name: Klaus-Michael Bonsack","Medals:  4<br />Events:  4<br />Gender: Male<br />Name: Leendert \"Leo\" Visser","Medals:  4<br />Events:  8<br />Gender: Male<br />Name: Magnus Hovdal Moan","Medals:  4<br />Events: 16<br />Gender: Male<br />Name: Mario Stecher","Medals:  4<br />Events:  6<br />Gender: Male<br />Name: Mark Kirchner","Medals:  4<br />Events:  6<br />Gender: Male<br />Name: Markus Zimmermann","Medals:  4<br />Events: 11<br />Gender: Male<br />Name: Martin Fourcade","Medals:  4<br />Events:  8<br />Gender: Male<br />Name: Martin Hllwarth","Medals:  4<br />Events:  8<br />Gender: Male<br />Name: Matti Antero Hautamki","Medals:  4<br />Events:  4<br />Gender: Male<br />Name: Meinhard Nehmer","Medals:  4<br />Events:  6<br />Gender: Male<br />Name: Pavel Konstantinovich Kolchin","Medals:  4<br />Events: 11<br />Gender: Male<br />Name: Petter Northug, Jr.","Medals:  4<br />Events: 11<br />Gender: Male<br />Name: Pietro Piller Cottrer","Medals:  4<br />Events:  7<br />Gender: Male<br />Name: ric Bdard","Medals:  4<br />Events:  4<br />Gender: Male<br />Name: Saku Antero Koivu","Medals:  4<br />Events:  6<br />Gender: Male<br />Name: Sergey Petrovich Tarasov","Medals:  4<br />Events: 11<br />Gender: Male<br />Name: Shani Earl Davis","Medals:  4<br />Events: 13<br />Gender: Male<br />Name: Simon Ammann","Medals:  4<br />Events:  4<br />Gender: Male<br />Name: Stefan Kraue","Medals:  4<br />Events:  4<br />Gender: Male<br />Name: Sten Einar Stensen","Medals:  4<br />Events:  6<br />Gender: Male<br />Name: Stephan Eberharter","Medals:  4<br />Events:  9<br />Gender: Male<br />Name: Sven Tomas Gustafson","Medals:  4<br />Events:  6<br />Gender: Male<br />Name: Teemu Ilmari Selnne","Medals:  4<br />Events: 11<br />Gender: Male<br />Name: Thomas Lars Wassberg","Medals:  4<br />Events:  9<br />Gender: Male<br />Name: Thomas Morgenstern","Medals:  4<br />Events: 14<br />Gender: Male<br />Name: Tobias Angerer","Medals:  4<br />Events:  6<br />Gender: Male<br />Name: Valery Alekseyevich Medvedtsev","Medals:  4<br />Events:  4<br />Gender: Male<br />Name: Ville Sakari Peltonen","Medals:  4<br />Events:  4<br />Gender: Male<br />Name: Vladislav Aleksandrovich Tretyak","Medals:  4<br />Events:  6<br />Gender: Male<br />Name: Vyacheslav Petrovich Vedenin","Medals:  4<br />Events:  6<br />Gender: Male<br />Name: Wolfgang Zimmerer","Medals:  4<br />Events:  4<br />Gender: Male<br />Name: Yevgeny Viktorovich Plyushchenko","Medals:  3<br />Events:  6<br />Gender: Male<br />Name: Aki Tapani Karvonen","Medals:  3<br />Events: 12<br />Gender: Male<br />Name: Aksel Lund Svindal","Medals:  3<br />Events: 10<br />Gender: Male<br />Name: Albert Mikhaylovich Demchenko","Medals:  3<br />Events:  6<br />Gender: Male<br />Name: Aleksandr Aleksandrovich Zavyalov","Medals:  3<br />Events:  3<br />Gender: Male<br />Name: Aleksandr Nikolayevich Maltsev","Medals:  3<br />Events:  3<br />Gender: Male<br />Name: Aleksandr Pavlovich Ragulin","Medals:  3<br />Events:  3<br />Gender: Male<br />Name: Aleksey Viktorovich Kasatonov","Medals:  3<br />Events:  3<br />Gender: Male<br />Name: Aleksey Yuryevich Zhamnov","Medals:  3<br />Events:  4<br />Gender: Male<br />Name: An Yulong","Medals:  3<br />Events:  3<br />Gender: Male<br />Name: Anatoly Nikolayevich Alyabyev","Medals:  3<br />Events:  3<br />Gender: Male<br />Name: Anatoly Vasilyevich Firsov","Medals:  3<br />Events:  6<br />Gender: Male<br />Name: Andreas Kofler","Medals:  3<br />Events:  5<br />Gender: Male<br />Name: Andreas Linger","Medals:  3<br />Events:  9<br />Gender: Male<br />Name: Andreas Widhlzl","Medals:  3<br />Events:  3<br />Gender: Male<br />Name: Andrey Valentinovich Khomutov","Medals:  3<br />Events:  4<br />Gender: Male<br />Name: Andris ics","Medals:  3<br />Events: 16<br />Gender: Male<br />Name: Andrus Veerpalu","Medals:  3<br />Events:  3<br />Gender: Male<br />Name: Anton Engelbert \"Toni\" Sailer","Medals:  3<br />Events:  3<br />Gender: Male<br />Name: Artur Valeryevich Dmitriyev","Medals:  3<br />Events: 12<br />Gender: Male<br />Name: Bart Veldkamp","Medals:  3<br />Events:  4<br />Gender: Male<br />Name: Bernhard Gruber","Medals:  3<br />Events:  8<br />Gender: Male<br />Name: Bernt Assar Rnnlund","Medals:  3<br />Events:  4<br />Gender: Male<br />Name: Birger Adolf Wasenius","Medals:  3<br />Events:  4<br />Gender: Male<br />Name: Birger Johannes Ruud","Medals:  3<br />Events:  3<br />Gender: Male<br />Name: Bo Hilding Martin Lundstrm","Medals:  3<br />Events:  3<br />Gender: Male<br />Name: Boris Petrovich Mikhaylov","Medals:  3<br />Events:  5<br />Gender: Male<br />Name: Chae Ji-Hun","Medals:  3<br />Events:  3<br />Gender: Male<br />Name: Charles Allen \"Charlie\" White, Jr.","Medals:  3<br />Events: 13<br />Gender: Male<br />Name: Christoph Bieler","Medals:  3<br />Events: 17<br />Gender: Male<br />Name: Christoph Sumann","Medals:  3<br />Events:  7<br />Gender: Male<br />Name: Cristian Zorzi","Medals:  3<br />Events: 10<br />Gender: Male<br />Name: Dario Alonzo Cologna","Medals:  3<br />Events:  4<br />Gender: Male<br />Name: Darius Vladovich Kasparaitis","Medals:  3<br />Events: 10<br />Gender: Male<br />Name: Dieter Thoma","Medals:  3<br />Events:  3<br />Gender: Male<br />Name: Dietmar Schauerhammer","Medals:  3<br />Events:  7<br />Gender: Male<br />Name: dne Sndrl","Medals:  3<br />Events: 10<br />Gender: Male<br />Name: Dominik Landertinger","Medals:  3<br />Events:  9<br />Gender: Male<br />Name: Eirik Kvalfoss","Medals:  3<br />Events:  9<br />Gender: Male<br />Name: Enrico Fabris","Medals:  3<br />Events:  6<br />Gender: Male<br />Name: Eric Frenzel","Medals:  3<br />Events:  3<br />Gender: Male<br />Name: Felix Loch","Medals:  3<br />Events: 10<br />Gender: Male<br />Name: Franck Piccard","Medals:  3<br />Events:  8<br />Gender: Male<br />Name: Frank-Peter Roetsch","Medals:  3<br />Events:  3<br />Gender: Male<br />Name: Frantiek Pospil","Medals:  3<br />Events:  8<br />Gender: Male<br />Name: Friedrich \"Fritz\" Fischer","Medals:  3<br />Events: 11<br />Gender: Male<br />Name: Frode Andresen","Medals:  3<br />Events:  3<br />Gender: Male<br />Name: Gianni Petrus Cornelis Romme","Medals:  3<br />Events:  3<br />Gender: Male<br />Name: Gillis Emanuel Grafstrm","Medals:  3<br />Events: 15<br />Gender: Male<br />Name: Giorgio Di Centa","Medals:  3<br />Events:  7<br />Gender: Male<br />Name: Gustavo Thoeni","Medals:  3<br />Events: 13<br />Gender: Male<br />Name: Hannu Kalevi Manninen","Medals:  3<br />Events:  4<br />Gender: Male<br />Name: Hans Rinn","Medals:  3<br />Events: 15<br />Gender: Male<br />Name: Hans van Helden","Medals:  3<br />Events:  4<br />Gender: Male<br />Name: Heikki Johannes Ikola","Medals:  3<br />Events:  5<br />Gender: Male<br />Name: Heikki Vihtori Hasu","Medals:  3<br />Events:  9<br />Gender: Male<br />Name: Heinz Ernst Kuttin","Medals:  3<br />Events:  5<br />Gender: Male<br />Name: Henri Jean Oreiller","Medals:  3<br />Events:  6<br />Gender: Male<br />Name: Hippolyt Marcel Kempf","Medals:  3<br />Events:  6<br />Gender: Male<br />Name: Hiroyasu Shimizu","Medals:  3<br />Events:  6<br />Gender: Male<br />Name: Hjalmar Johan Andersen","Medals:  3<br />Events:  3<br />Gender: Male<br />Name: Igor Nikolayevich Larionov","Medals:  3<br />Events:  9<br />Gender: Male<br />Name: Jaakko Tapio Tallus","Medals:  3<br />Events:  5<br />Gender: Male<br />Name: Jan Blokhuijsen","Medals:  3<br />Events:  6<br />Gender: Male<br />Name: Jan Ingemar Stenmark","Medals:  3<br />Events:  8<br />Gender: Male<br />Name: Jan Olof Daniel Richardsson","Medals:  3<br />Events:  7<br />Gender: Male<br />Name: Jari Markus Puikkonen","Medals:  3<br />Events: 11<br />Gender: Male<br />Name: Jari Olavi Isomets","Medals:  3<br />Events:  3<br />Gender: Male<br />Name: Jarmo Pentti Kalevi Myllys","Medals:  3<br />Events:  4<br />Gender: Male<br />Name: Jean Wicki","Medals:  3<br />Events:  6<br />Gender: Male<br />Name: Jean-Claude Killy","Medals:  3<br />Events:  3<br />Gender: Male<br />Name: Jochem Simon Uytdehaage","Medals:  3<br />Events:  9<br />Gender: Male<br />Name: John \"Johnny\" Spillane","Medals:  3<br />Events:  7<br />Gender: Male<br />Name: John Robert \"J. R.\" Celski","Medals:  3<br />Events:  3<br />Gender: Male<br />Name: John Rutherford \"Jack\" Heaton","Medals:  3<br />Events:  4<br />Gender: Male<br />Name: Jonathan Guilmette","Medals:  3<br />Events:  5<br />Gender: Male<br />Name: Josef \"Pepi\" Stiegler","Medals:  3<br />Events:  4<br />Gender: Male<br />Name: Josef ern","Medals:  3<br />Events:  4<br />Gender: Male<br />Name: Juris ics","Medals:  3<br />Events:  6<br />Gender: Male<br />Name: Kazuyoshi Funaki","Medals:  3<br />Events:  4<br />Gender: Male<br />Name: Kim Gi-Hun","Medals:  3<br />Events: 11<br />Gender: Male<br />Name: Kjetil Jansrud","Medals:  3<br />Events:  7<br />Gender: Male<br />Name: Kristen Skjeldal","Medals:  3<br />Events:  6<br />Gender: Male<br />Name: Lars Bystl","Medals:  3<br />Events:  3<br />Gender: Male<br />Name: Lasse Juhani Kukkonen","Medals:  3<br />Events:  3<br />Gender: Male<br />Name: Lee Jeong-Su","Medals:  3<br />Events:  6<br />Gender: Male<br />Name: Lee Seung-Hun","Medals:  3<br />Events: 19<br />Gender: Male<br />Name: Luk Bauer","Medals:  3<br />Events:  4<br />Gender: Male<br />Name: Magnar Solberg","Medals:  3<br />Events:  8<br />Gender: Male<br />Name: Maksim Mikhaylovich Vylegzhanin","Medals:  3<br />Events:  6<br />Gender: Male<br />Name: Mark Jan Hendrik Tuitert","Medals:  3<br />Events:  6<br />Gender: Male<br />Name: Markus Prock","Medals:  3<br />Events:  4<br />Gender: Male<br />Name: Martin Annen","Medals:  3<br />Events: 11<br />Gender: Male<br />Name: Martin Schmitt","Medals:  3<br />Events: 13<br />Gender: Male<br />Name: Masahiko Harada","Medals:  3<br />Events:  4<br />Gender: Male<br />Name: Mathieu Turcotte","Medals:  3<br />Events: 17<br />Gender: Male<br />Name: Maurilio De Zolt Lisabetta","Medals:  3<br />Events: 12<br />Gender: Male<br />Name: Michael Greis","Medals:  3<br />Events:  6<br />Gender: Male<br />Name: Nikolay Petrovich Anikin","Medals:  3<br />Events:  5<br />Gender: Male<br />Name: Nikolay Serafimovich Bazhukov","Medals:  3<br />Events: 18<br />Gender: Male<br />Name: Noriaki Kasai","Medals:  3<br />Events:  7<br />Gender: Male<br />Name: Odd-Willy Martinsen","Medals:  3<br />Events:  3<br />Gender: Male<br />Name: Oddbjrn Hagen","Medals:  3<br />Events:  3<br />Gender: Male<br />Name: Oldich Macha","Medals:  3<br />Events:  4<br />Gender: Male<br />Name: Olli Veli Pekka Jokinen","Medals:  3<br />Events: 10<br />Gender: Male<br />Name: Ondej Moravec","Medals:  3<br />Events:  3<br />Gender: Male<br />Name: Patrick Henry \"Pat\" Martin","Medals:  3<br />Events:  7<br />Gender: Male<br />Name: Paul-Peter Hildgartner","Medals:  3<br />Events:  5<br />Gender: Male<br />Name: Peter Utzschneider","Medals:  3<br />Events:  5<br />Gender: Male<br />Name: Pierre mile Ernest Brunet","Medals:  3<br />Events:  6<br />Gender: Male<br />Name: Piet Kleine","Medals:  3<br />Events:  6<br />Gender: Male<br />Name: Raimo Ilmari Helminen","Medals:  3<br />Events: 12<br />Gender: Male<br />Name: Raphal Stphane Louis Poire","Medals:  3<br />Events:  4<br />Gender: Male<br />Name: Robert Lang","Medals:  3<br />Events:  8<br />Gender: Male<br />Name: Ronny Ackermann","Medals:  3<br />Events:  4<br />Gender: Male<br />Name: Sami Sakari Salo","Medals:  3<br />Events:  3<br />Gender: Male<br />Name: Scott Patrick Moir","Medals:  3<br />Events:  3<br />Gender: Male<br />Name: Sergey Mikhaylovich Makarov","Medals:  3<br />Events:  3<br />Gender: Male<br />Name: Sergey Viktorovich Starikov","Medals:  3<br />Events:  3<br />Gender: Male<br />Name: Sergey Vladilenovich Ponomarenko","Medals:  3<br />Events:  6<br />Gender: Male<br />Name: Steven \"Steve\" Holcomb","Medals:  3<br />Events:  8<br />Gender: Male<br />Name: Sture Sivertsen","Medals:  3<br />Events:  6<br />Gender: Male<br />Name: Sven Hannawald (Phler-)","Medals:  3<br />Events:  4<br />Gender: Male<br />Name: Takanori Kono","Medals:  3<br />Events:  4<br />Gender: Male<br />Name: Teppo Kalevi Numminen","Medals:  3<br />Events:  4<br />Gender: Male<br />Name: Thomas Khler","Medals:  3<br />Events:  4<br />Gender: Male<br />Name: Thorleif Haug (Johnsen-)","Medals:  3<br />Events:  4<br />Gender: Male<br />Name: Toni Markus Nieminen","Medals:  3<br />Events:  3<br />Gender: Male<br />Name: Ulrich Wehling","Medals:  3<br />Events:  5<br />Gender: Male<br />Name: Uwe-Jens Mey","Medals:  3<br />Events:  5<br />Gender: Male<br />Name: Valery Alekseyevich Muratov","Medals:  3<br />Events:  3<br />Gender: Male<br />Name: Valery Borisovich Kharlamov","Medals:  3<br />Events:  3<br />Gender: Male<br />Name: Valery Ivanovich Vasilyev","Medals:  3<br />Events:  3<br />Gender: Male<br />Name: Veniamin Veniaminovich Aleksandrov","Medals:  3<br />Events:  3<br />Gender: Male<br />Name: Viktor Grigoryevich Kuzkin","Medals:  3<br />Events: 13<br />Gender: Male<br />Name: Vincent Franois Marie Defrasne","Medals:  3<br />Events:  3<br />Gender: Male<br />Name: Vitaly Semyonovich Davydov","Medals:  3<br />Events:  3<br />Gender: Male<br />Name: Vladimir Vladimirovich Petrov","Medals:  3<br />Events:  3<br />Gender: Male<br />Name: Vladimir Yevgenyevich Krutov","Medals:  3<br />Events:  3<br />Gender: Male<br />Name: Vladimr Dzurilla","Medals:  3<br />Events:  3<br />Gender: Male<br />Name: Vyacheslav Aleksandrovich \"Slava\" Fetisov","Medals:  3<br />Events:  6<br />Gender: Male<br />Name: William Joseph N. \"Joey\" Cheek","Medals:  3<br />Events:  5<br />Gender: Male<br />Name: Wolfgang Linger","Medals:  3<br />Events:  7<br />Gender: Male<br />Name: Yevgeny Prokopyevich Belyayev","Medals:  3<br />Events: 10<br />Gender: Male<br />Name: Yevgeny Romanovich Ustyugov","Medals:  3<br />Events:  4<br />Gender: Male<br />Name: Zhao Hongbo","Medals:  2<br />Events:  3<br />Gender: Male<br />Name: Aki-Petteri Arvid \"Aki\" Berg","Medals:  2<br />Events:  2<br />Gender: Male<br />Name: Aleksandr Davletovich Almetov","Medals:  2<br />Events:  9<br />Gender: Male<br />Name: Aleksandr Gennadyevich Legkov","Medals:  2<br />Events:  2<br />Gender: Male<br />Name: Aleksandr Gennadyevich Zaytsev","Medals:  2<br />Events:  2<br />Gender: Male<br />Name: Aleksandr Matveyevich Yelizarov","Medals:  2<br />Events:  2<br />Gender: Male<br />Name: Aleksandr Sergeyevich Yakushev","Medals:  2<br />Events:  2<br />Gender: Male<br />Name: Aleksandr Vasilyevich Privalov","Medals:  2<br />Events:  2<br />Gender: Male<br />Name: Aleksandr Vikentyevich Skvortsov","Medals:  2<br />Events:  2<br />Gender: Male<br />Name: Aleksandr Viktorovich Kozhevnikov","Medals:  2<br />Events: 11<br />Gender: Male<br />Name: Aleksandr Vladimirovich Popov","Medals:  2<br />Events:  3<br />Gender: Male<br />Name: Aleksandr Vladimirovich Tretyakov","Medals:  2<br />Events:  2<br />Gender: Male<br />Name: Aleksandr Vyacheslavovich \"Sasha\" Zhulin","Medals:  2<br />Events: 18<br />Gender: Male<br />Name: Aleksey Alekseyevich Prokurorov","Medals:  2<br />Events:  5<br />Gender: Male<br />Name: Aleksey Gennadyevich Grishin","Medals:  2<br />Events:  2<br />Gender: Male<br />Name: Aleksey Nikolayevich Zhitnik","Medals:  2<br />Events:  3<br />Gender: Male<br />Name: Aleksey Valeryevich Yashin","Medals:  2<br />Events:  2<br />Gender: Male<br />Name: Aleksey Vasilyevich Gusarov","Medals:  2<br />Events:  3<br />Gender: Male<br />Name: Aleksey Vyacheslavovich Kovalyov","Medals:  2<br />Events:  4<br />Gender: Male<br />Name: Alexander Brengle \"Alex\" Hurd","Medals:  2<br />Events:  3<br />Gender: Male<br />Name: Alexander Resch","Medals:  2<br />Events:  3<br />Gender: Male<br />Name: Alexandre \"Alex\" Bilodeau","Medals:  2<br />Events:  2<br />Gender: Male<br />Name: Alf Tomas Jonsson","Medals:  2<br />Events:  2<br />Gender: Male<br />Name: Alf Tommy Samuelsson","Medals:  2<br />Events:  2<br />Gender: Male<br />Name: Alfred Iosifovich Kuchevsky","Medals:  2<br />Events:  9<br />Gender: Male<br />Name: Alois Klin","Medals:  2<br />Events:  3<br />Gender: Male<br />Name: Alv Gjestvang","Medals:  2<br />Events:  3<br />Gender: Male<br />Name: an Koir","Medals:  2<br />Events:  9<br />Gender: Male<br />Name: Anders Bardal","Medals:  2<br />Events:  5<br />Gender: Male<br />Name: Andreas \"Anderl\" Molterer","Medals:  2<br />Events:  3<br />Gender: Male<br />Name: Andreas \"Anderl\" Ostler","Medals:  2<br />Events:  4<br />Gender: Male<br />Name: Andreas \"Andi\" Goldberger","Medals:  2<br />Events: 10<br />Gender: Male<br />Name: Andreas \"Andi\" Wenzel","Medals:  2<br />Events:  2<br />Gender: Male<br />Name: Andreas Kirchner","Medals:  2<br />Events:  6<br />Gender: Male<br />Name: Andreas Schaad","Medals:  2<br />Events: 12<br />Gender: Male<br />Name: Andreas Schltter","Medals:  2<br />Events:  4<br />Gender: Male<br />Name: Andreas Wank"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(204,204,204,1)","opacity":1,"size":7.55905511811024,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(204,204,204,1)"}},"hoveron":"points","name":"Male","legendgroup":"Male","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":51.0684931506849,"r":7.97011207970112,"b":48.3520132835201,"l":45.1639684516397},"plot_bgcolor":"transparent","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(117,117,117,1)","family":"sans","size":15.9402241594022},"title":{"text":"Most Successful Athletes of All Time","font":{"color":"rgba(117,117,117,1)","family":"sans","size":23.9103362391034},"x":0.5,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[1.45,13.55],"tickmode":"array","ticktext":["5","10"],"tickvals":[5,10],"categoryorder":"array","categoryarray":["5","10"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.98505603985056,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(117,117,117,1)","family":"sans","size":15.9402241594022},"tickangle":-0,"showline":true,"linecolor":"rgba(0,0,0,1)","linewidth":0.724555643609193,"showgrid":true,"gridcolor":"rgba(204,204,204,1)","gridwidth":0.724555643609193,"zeroline":false,"anchor":"y","title":{"text":"Medals","font":{"color":"rgba(102,102,102,1)","family":"sans","size":17.2685761726858}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.75,28.25],"tickmode":"array","ticktext":["10","20"],"tickvals":[10,20],"categoryorder":"array","categoryarray":["10","20"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.98505603985056,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(117,117,117,1)","family":"sans","size":15.9402241594022},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(204,204,204,1)","gridwidth":0.724555643609193,"zeroline":false,"anchor":"x","title":{"text":"Events","font":{"color":"rgba(102,102,102,1)","family":"sans","size":17.2685761726858}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":"transparent","line":{"color":"transparent","width":0.724555643609193,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":2.06156048675734,"font":{"color":"rgba(117,117,117,1)","family":"sans","size":15.9402241594022},"title":{"text":"Gender","font":{"color":"rgba(102,102,102,1)","family":"sans","size":15.9402241594022}}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"855e35a5c065":{"x":{},"y":{},"colour":{},"label":{},"type":"scatter"}},"cur_data":"855e35a5c065","visdat":{"855e35a5c065":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```
  
  
  _Note that in this ggplotly "upgrade" I incorporated more athletes (500 over 300) because the specific information like athlete names won't show up until someone selects a specific point. Therefore, the general view is a lot more clear than the static version when I don't have to manually label those points._

#### 6. Data Table

Prepare a selected data set and add a `datatable` to the output. Make sure the columns are clearly labelled. Select the appropriate options for the data table (e.g. search bar, sorting, column filters etc.). Suggest to the editor which kind of information you would like to provide in a data table in the online version of the article and why.


```r
library(DT)

ath_counts <- aae3 %>% 
  count(Games, Name) %>% 
  mutate("n"=1) %>% 
  count(Games) %>% 
  rename(ath_counts = n)

avg_height <- aae3 %>%
  filter(Height != '0') %>% 
  group_by(Games) %>% 
  summarise(Avg_Height=mean(Height),
            .groups='drop')

avg_weight <- aae3 %>%
  filter(Weight != '0') %>% 
  group_by(Games) %>% 
  summarise(Avg_Weight=mean(Weight),
            .groups='drop')

medal_counts <- aae3 %>% 
  group_by(Games) %>% 
  summarise(Medals=sum(as.numeric(Medal)),
            .groups='drop')

gold_counts <- aae3 %>% 
  group_by(Games) %>% 
  summarise(Medals=sum(MedalGold),
            .groups='drop') %>% 
  rename(Gold=Medals)

silver_counts <- aae3 %>% 
  group_by(Games) %>% 
  summarise(Medals=sum(MedalSilver),
            .groups='drop') %>% 
  rename(Silver=Medals)

bronze_counts <- aae3 %>% 
  group_by(Games) %>% 
  summarise(Medals=sum(MedalBronze),
            .groups='drop') %>% 
  rename(Bronze=Medals)

noc_counts <- aae3 %>% 
  count(Games, NOC) %>% 
  count(Games) %>% 
  rename(noc_counts = n)

avg_weight <- avg_weight$Avg_Weight
avg_height <- avg_height$Avg_Height
ath_counts <- ath_counts$ath_counts
medal_counts <- medal_counts$Medals
gold_counts <- gold_counts$Gold
silver_counts <- silver_counts$Silver
bronze_counts <- bronze_counts$Bronze

good_data <- data.frame(noc_counts,medal_counts, gold_counts, silver_counts, bronze_counts, ath_counts, avg_height, avg_weight)

fancy_table <- datatable(good_data,
          rownames = TRUE,
          colnames=c("Games", "Countries","Medals","Gold", "Silver", "Bronze", "Athletes", "Average Height", "Average Weight")) %>% 
  formatStyle('Games',  color = 'white', 
              backgroundColor = 'blue', fontWeight = 'bold') %>% 
  formatStyle('medal_counts',
              background = styleColorBar(good_data$medal_counts, 'lightblue'),
              backgroundSize = '98% 88%',
              backgroudRepeat = 'no-repeat',
              backgroundPosition = 'center') %>%
  formatStyle('bronze_counts',
              background = styleColorBar(good_data$bronze_counts, 'brown'),
              backgroundSize = '98% 88%',
              backgroudRepeat = 'no-repeat',
              backgroundPosition = 'center') %>% 
  formatStyle('gold_counts',
              background = styleColorBar(good_data$gold_counts, 'gold'),
              backgroundSize = '98% 88%',
              backgroudRepeat = 'no-repeat',
              backgroundPosition = 'center') %>% 
  formatStyle('silver_counts',
              background = styleColorBar(good_data$silver_counts, 'grey'),
              backgroundSize = '98% 88%',
              backgroudRepeat = 'no-repeat',
              backgroundPosition = 'center')

fancy_table
```

```{=html}
<div id="htmlwidget-c1fee61d7165b40c51ca" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-c1fee61d7165b40c51ca">{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22"],["1924 Winter","1928 Winter","1932 Winter","1936 Winter","1948 Winter","1952 Winter","1956 Winter","1960 Winter","1964 Winter","1968 Winter","1972 Winter","1976 Winter","1980 Winter","1984 Winter","1988 Winter","1992 Winter","1994 Winter","1998 Winter","2002 Winter","2006 Winter","2010 Winter","2014 Winter"],[9,12,9,16,11,15,24,24,34,36,33,29,33,45,47,56,64,68,76,77,81,87],[16,6,10,23,13,24,84,91,158,195,199,195,195,220,251,311,324,437,477,526,515,570],[7,5,4,13,3,11,24,35,50,63,70,62,67,74,86,102,105,144,161,176,173,190],[7,0,4,4,9,7,36,31,57,70,63,67,67,73,85,107,108,144,157,175,172,190],[2,1,2,6,1,6,24,25,51,62,66,66,61,73,80,102,111,149,159,175,170,190],[24,37,34,50,62,91,212,301,828,1110,998,789,845,1173,1114,1493,1629,2127,2359,2480,2517,2602],[172.121951219512,172.375,170.395833333333,172.724637681159,172.411764705882,173.451388888889,172.311377245509,170.658203125,172.15676077266,173.304898183819,173.444444444444,174.225038402458,175.065502183406,175.41568426448,176.088701684836,175.396694214876,175.176708179064,174.588686753837,174.728571428571,174.630469644903,174.918227501142,174.93901134175],[69.5609756097561,71.625,66.8333333333333,69.3478260869565,68.4705882352941,68.5798611111111,70.0763473053892,68.13671875,69.8525260029718,70.2611447440837,69.529304029304,70.531490015361,70.9687045123726,71.1068682726807,72.0027254707631,71.1167804527488,71.0621003029283,70.9440022740193,71.2016009852217,70.5148911798396,70.7408634079488,70.7601112775519]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>Games<\/th>\n      <th>Countries<\/th>\n      <th>Medals<\/th>\n      <th>Gold<\/th>\n      <th>Silver<\/th>\n      <th>Bronze<\/th>\n      <th>Athletes<\/th>\n      <th>Average Height<\/th>\n      <th>Average Weight<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"rowCallback":"function(row, data, displayNum, displayIndex, dataIndex) {\nvar value=data[1]; $(this.api().cell(row, 1).node()).css({'font-weight':'bold','color':'white','background-color':'blue'});\nvar value=data[3]; $(this.api().cell(row, 3).node()).css({'background':isNaN(parseFloat(value)) || value <= 6.000000 ? '' : 'linear-gradient(90.000000deg, transparent ' + Math.max(570.000000 - value, 0)/564.000000 * 100 + '%, lightblue ' + Math.max(570.000000 - value, 0)/564.000000 * 100 + '%)','background-size':'98% 88%','backgroud-repeat':'no-repeat','background-position':'center'});\nvar value=data[6]; $(this.api().cell(row, 6).node()).css({'background':isNaN(parseFloat(value)) || value <= 1.000000 ? '' : 'linear-gradient(90.000000deg, transparent ' + Math.max(190.000000 - value, 0)/189.000000 * 100 + '%, brown ' + Math.max(190.000000 - value, 0)/189.000000 * 100 + '%)','background-size':'98% 88%','backgroud-repeat':'no-repeat','background-position':'center'});\nvar value=data[4]; $(this.api().cell(row, 4).node()).css({'background':isNaN(parseFloat(value)) || value <= 3.000000 ? '' : 'linear-gradient(90.000000deg, transparent ' + Math.max(190.000000 - value, 0)/187.000000 * 100 + '%, gold ' + Math.max(190.000000 - value, 0)/187.000000 * 100 + '%)','background-size':'98% 88%','backgroud-repeat':'no-repeat','background-position':'center'});\nvar value=data[5]; $(this.api().cell(row, 5).node()).css({'background':isNaN(parseFloat(value)) || value <= 0.000000 ? '' : 'linear-gradient(90.000000deg, transparent ' + Math.max(190.000000 - value, 0)/190.000000 * 100 + '%, grey ' + Math.max(190.000000 - value, 0)/190.000000 * 100 + '%)','background-size':'98% 88%','backgroud-repeat':'no-repeat','background-position':'center'});\n}"}},"evals":["options.rowCallback"],"jsHooks":[]}</script>
```
  
  
  _This data table is a snapshot of some descriptive information of each Winter Olympics, including countries, medals, athletes, average height and weight. Those fundamental statistics would give any reader a clear sense of how each Winter Olympics differ. For example, there is a general increasing trend of everything, but 1980 Winter Olympics shows an oddly decrease in lots of indices in the table, so one might wonder if anything like boycotting happened. To make the data table more informative, one can also include host cities, number of sport events, gender ratio, etc._

## Technical Details

The data comes in a reasonably clean Excel data set. If needed for your visualization, you can add visual drapery like flag icons, icons for sports, icons for medals etc. but your are certainly not obligated to do that. 

Part of the your task will be transforming the dataset into a shape that allows you to plot what you want in `ggplot2`. For some plots, you will necessarily need to be selective in what to include and what to leave out. 

Make sure to use at least three different types of graphs, e.g. line graphs, scatter, histograms, bar chats, dot plots, heat maps etc.

## Submission

Please follow the [instructions](/Exercises/homework_submission_instructions.md) to submit your homework. The homework is due on Wednesday, February 16 at 5pm

## Please stay honest!

Yes, the medal counts of the olympics have surely been analyzed before.  If you do come across something, please no wholesale copying of other ideas. We are trying to practice and evaluate your abilities in using ggplot2 and data visualization not the ability to do internet searches. Also, this is an individually assigned exercise -- please keep your solution to yourself.
