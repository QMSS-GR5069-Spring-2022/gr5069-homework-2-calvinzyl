Assignment 1: Using ggplot2 for visualization
Thomas Brambor
Winter Olympics Medals over Time
Scenario
Imagine you are the data scientist at a respected media outlet – say the “New York Times”. For the Winter Olympics coverage, your editor-in-chief asks you to analyze some data on the history of Winter Olympics Medals by Year, Country, Event and Gender and prepare some data visualizations in which you outline the main patterns around which to base the story.

Since there is no way that all features of the data can be represented in such a memo, feel free to pick and choose some patterns that would make for a good story – outlining important patterns and presenting them in a visually pleasing way.

The full background and text of the story will be researched by a writer of the magazine – your input should be based on the data and some common sense (i.e. no need to read up on this).

Provide polished plots that are refined enough to include in the magazine with very little further manipulation (already include variable descriptions [if necessary for understanding], titles, source [e.g. “International Olympic Committee”], right color etc.) and are understandable to the average reader of the “New York Times”. The design does not need to be NYTimes-like. Just be consistent.

Data
The main data is provided as an excel sheet, containing the following variables on all participating athletes in all olympics from 1896 to 2016 (sadly, the original source of the data no longer updates beyond that year):

ID: a unique indentifier of the entry
Name: name of the athlete
Sex: sex of the athlete
Age: age of the athlete
Height: height of the athlete
Weight: weight of the athlete
Team: usually the country team of the athlete, with the exception of political accomodations, e.g. the “Refugee Olympic Athletes” team.
NOC: national olympic comittee abbreviation.
Games: year and season of games.
Year: year of games
Season: season of games.
City: host city
Sport: a grouping of disciplines
Event: the particular event / competition
Medal: the particular event / competition
For example, an event is a competition in a sport or discipline that gives rise to a ranking. Thus Alpine Skiing is the discipline, and Alpine Skiing Women's Downhills is a particular event.

In addition, you are provided with some additional information about the countries in a separate spreadsheet, including the IOC Country    Code, Population, and GDP per capita.
