# STA 404 Individual Project
# March 24, 2022

# image from https://thestarwarswikia.fandom.com/wiki/Star_Wars:_Episode_VII_The_Force_Awakens

library(tidyverse)
library(ggplot2)
library(plotly)
library(lubridate)
library(zoo)

# source: Kaggle --> https://www.kaggle.com/sanjeetsinghnaik/top-1000-highest-grossing-movies
movies <- read.csv("Highest_Holywood_Grossing_Movies.csv")
head(movies)
nrow(movies) # 918

# CLEANING ##########################################################################
# remove lines with missing data
movies <- na.omit(movies)
nrow(movies) # check --> 744
# grab only year from release date
#movies$Release.Date <- sapply(strsplit(movies$Release.Date," +"), `[`, 3)
######## instead, change to mon-year format
movies$Release.Date <- as.Date(movies$Release.Date,format='%B %d, %Y')

head(movies) # check
# combine same/similar distributors (there are too many!)
movies <- movies %>% mutate(Distributor=ifelse((Distributor=="United Artists Releasing"),"United Artists",Distributor)) %>% 
  mutate(Distributor=ifelse((Distributor=="DreamWorks Distribution"|Distributor=="DreamWorks Releasing"),"DreamWorks",Distributor)) %>%
  mutate(Distributor=ifelse((Distributor=="Sony Pictures Entertainment (SPE)"|Distributor=="Sony Pictures Classics"),"Sony Pictures",Distributor)) %>%
  mutate(Distributor=ifelse((Distributor=="Walt Disney Studios Motion Pictures"),"Walt Disney",Distributor))

# need the $$ in millions
movies <- movies %>%
  mutate(Domestic.Sales.mil = Domestic.Sales..in... / 1000000) %>%
  mutate(International.Sales.mil = International.Sales..in... / 1000000) %>%
  mutate(World.Sales.mil = World.Sales..in... / 1000000)

#############################################################################################################################################
# 1) BAR - y=counts, x=license, fill=distributor ############################################################################################
# get an idea of what it looks like
ggplot(data=movies) +
  geom_bar(aes(x=License, fill=Distributor)) +
  labs(x="Movie Rating", y="Frequency") +
  ggtitle("Movie Rating Counts by Distributor") +
  theme(plot.title = element_text(hjust = 0.5))

# organize a little bit more
movies_bar <- movies %>%
  count(License, Distributor, sort = TRUE) %>%
  filter(n >= 10) # just keep the ones with n > 10 since there are so many

# arr <- movies_bar %>% count(Distributor, sort = TRUE) %>% # for ordering
#   arrange(n)

movies_bar %>% group_by(License) %>%
  mutate(yCoord = )

# bar plot
cols <- c('#e69f00','deepskyblue4','#009e73','#f0e442','darkred','darkslateblue','#d55e00','#cc79a7','darkseagreen')

# df <- data.frame(License=c("PG-13","PG-13","PG-13","PG-13","R","PG-13","PG","PG-13","R","PG","R","PG","PG",
#                            "R","R","PG"),
#                  Distributor=c("Warner Bros.","Universal Pictures","Sony Pictures","Twentieth Century Fox",
#                                "Warner Bros.","Paramount Pictures","Walt Disney","Walt Disney","Universal Pictures",
#                                "Twentieth Century Fox","Sony Pictures","Sony Pictures","Warner Bros.",
#                                "Paramount Pictures","Twentieth Century Fox","DreamWorks"),
#                  n=c(59,57,55,48,48,47,46,39,34,33,22,20,19,19,19,17))

plot_ly(df, x=~License, y=~n, color=~Distributor, colors=cols, type='bar', text=~n, textposition="auto",
        hoverinfo='text', hovertext=~paste('</br> License: ', License, '</br> Distributor: ', Distributor,
                                           '</br> Count: ', n)) %>%
  layout(yaxis = list(title = "Count"),
         barmode = "stack", title = "Distributors with 10+ of the ~1000 Highest Grossing Movies") %>%
  add_annotations(showarrow=F, y=(~n/2))


# 2) SCATTER - y=wrld_sales x=release_yr (jitter and opaqueness) ############################################################################
# floor dates and summarize counts grouped by month year
movies_month <- movies %>%
  mutate(MonYr = floor_date(Release.Date, "month")) %>%
  group_by(MonYr) %>%
  summarize(n = n())
movies_yr <- movies %>%
  mutate(Yr = floor_date(Release.Date, "year")) %>%
  group_by(Yr) %>%
  summarize(n = n())
movies_yr$label <- sapply(strsplit(as.character(movies_yr$Yr),"-|s"), `[`, 1)

# line time series - keep this one
fig1 <- plot_ly(movies_month, type='scatter', mode='lines', x=~MonYr, y=~n, name='Month')
fig2 <- plot_ly(movies_yr, type='scatter', mode='lines', x=~Yr, y=~n, name='Year', hoverinfo='text',
                text=paste(movies_yr$label, ",", movies_yr$n))
subplot(fig1, fig2, nrows=2) %>%
  layout(plot_bgcolor='#e5ecf6', title='Time Series of Top ~1000 Highest Grossing Movie Releases',
         legend=list(orientation='h', title=list(text='Dates floored by '))) %>%
  add_annotations(text="Release Date", x=0.5, y=-0.01, yref = "paper", xref="paper", xanchor="center",
                  yanchor="bottom", yshift=-35, showarrow=F, font=list(size=15)) %>%
  add_annotations(text="Number of Movies Released", x=-0.09, y=0.5, xref='paper', yref='paper', font=list(size=15), 
                  textangle=270, showarrow=F)

# scatter - did not end up using...
fig1 <- plot_ly(movies, x=~jitter(as.numeric(Release.Date),1), y=~World.Sales.mil, size=4, hoverinfo='text', name="World Sales",
        type='scatter', mode='markers', marker=list(color = 'darkseagreen', opacity=.6, size = 8), # change opacity for overlaps
        text=paste('</br><b>Movie Title:</b>', movies$Title,
                   '</br><b>World Sales:</b>', paste0('$',ceiling(movies$World.Sales.mil),'m')))
fig2 <- plot_ly(movies, x=~jitter(as.numeric(Release.Date),1), y=~Domestic.Sales.mil, size=4, hoverinfo='text', name="US Sales",
            type='scatter', mode='markers', marker=list(color = 'coral1', opacity=.6, size = 8), # change opacity for overlaps
            text=paste('</br><b>Movie Title:</b>', movies$Title,
                       "</br><b>Domestic Sales:</b>", paste0('$',ceiling(movies$Domestic.Sales.mil),'m')))
subplot(fig1, fig2, nrows=2) %>% layout(title="Lifetime Gross (mil) of Top ~1000 Highest Grossing\nMovies", 
                                        yaxis=list(hoverformat='$,.0f'), legend=list(orientation='h'),
                                        margin=list(l=70, r=0, t=40, b=60)) %>%
  add_annotations(text="Release Year", x=0.5, y=-0.01, yref = "paper", xref="paper", xanchor="center",
    yanchor="bottom", yshift=-35, showarrow=F, font=list(size=15)) %>%
  add_annotations(text="Lifetime Gross in Millions (USD)", x=-0.09, y=0.5, xref='paper', yref='paper', font=list(size=15), 
                  textangle=270, showarrow=F)

# 3) BOXPLOT+VIOLIN - split by genre, y=wrld_sales ##########################################################################################
head(movies)
# reformat genre column
movies$Genre <- gsub("\\[|\\]", "", movies$Genre)
movies$Genre <- gsub("'", "", movies$Genre)
movies_unlist <- movies %>% separate_rows(Genre) %>%
  mutate(Genre=ifelse((Genre=="Sci"|Genre=="Fi"),"SciFi",Genre))

movies_unlist <- movies_unlist[!duplicated(movies_unlist), ] # fixes a problem with the separate_rows
# after viewing boxplots, take out some of the less frequent genres
movies_unlist <- movies_unlist %>%
  filter(!Genre %in% c('Sport', 'Documentary', 'Biography', 'Western', 'History', 'Horror', 'Music','War'))

# domestic
plot_ly(movies_unlist, y=~Domestic.Sales.mil, color=~Genre) %>%
  add_trace(type='violin', box = list(visible = T), meanline = list(visible = T)) %>%
  layout(yaxis=list(title="Domestic Lifetime Gross (Milion USD)"), 
         xaxis=list(title="Genre", tickangle=60), title="Domestic Sales (Mil) of Highest ~1000 
         Grossing Movies in Top Genres", margin=list(t=60))
# world
plot_ly(movies_unlist, y=~World.Sales.mil, color=~Genre) %>%
  add_trace(type='violin', box = list(visible = T), meanline = list(visible = T)) %>%
  layout(yaxis=list(title="World Lifetime Gross (Milion USD)"), 
         xaxis=list(title="Genre", tickangle=60), title="World Sales (Mil) of Highest ~1000 
         Grossing Movies in Top Genres", margin=list(t=60))


# 3) BUBBLE - x=domestic_sales y=world_sales size=diff ########################################################################################
movies_diff <- movies %>%
  mutate(Differential.mil = World.Sales.mil-Domestic.Sales.mil)
# differential/size will tell us whether it was mostly popular in just the US or around entire world
# very high difference will indicate more profits came from outside US

plot_ly(movies_diff, x=~Domestic.Sales.mil, y=~World.Sales.mil, size=~Differential.mil, type = 'scatter', 
        mode = 'markers', hoverinfo='text', marker=list(sizemode = 'diameter'), sizes = c(1, 50),
        text=paste('</br><b>$ Diff:</b>', paste0('$',ceiling(movies_diff$Differential.mil),'m'), 
                   '</br><b>Movie Title:</b>', movies_diff$Title)) %>%
  layout(yaxis=list(title="<b>World</b> Lifetime Gross (Milion USD)"), 
         xaxis=list(title="<b>Domestic</b> Lifetime Gross (Milion USD)"), 
title="Differences in Domestic and World Lifetime Gross (Million USD) Highest ~1000 Grossing Movies")

# Think Like a Man, Step Brothers, This is the End: these movies cater to Western/American humor and culture so
# naturally the difference between world and domestic sales are among the lowest in the group, meaning
# most of the movie's profits came from viewers inside the US.

# Avengers, Avatar, Star Wars, Jurrasic World: aside from some of these being leading franchises around the world, 
# these movies chose plots and characters that most of the world can enjoy without a huge a cultural barrier



# NEW PLOT w example (start over w cleaning)
library(dygraphs)
library(xts)          # To make the convertion data-frame / xts format
library(lubridate)
library(zoo)
install.packages("remotes")
remotes::install_github("reconhub/linelist")
movies <- read.csv("Highest_Holywood_Grossing_Movies.csv")
movies <- na.omit(movies)
# Since my time is currently a character, I have to convert it to a date-time format!
movies$Release.Date <- gsub(",", "", movies$Release.Date)
movies$Release.Date <- linelist::guess_dates(movies$Release.Date)
class(movies$Release.Date) # Date
# remove the day, keep month / year
movies <- transform(movies, Release.Date = as.yearmon(Release.Date))
movies_agg <- movies %>%
  count(Release.Date)
# Then you can create the xts necessary to use dygraph
don <- xts::xts(x = movies$count, order.by = movies$Release.Date)
#https://www.r-graph-gallery.com/318-custom-dygraphs-time-series-example.html
# Finally the plot
p <- dygraphs::dygraph(don) %>%
  dygraphs::dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
  dygraphs::dyRangeSelector() %>%
  dygraphs::dyCrosshair(direction = "vertical") %>%
  dygraphs::dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dygraphs::dyRoller(rollPeriod = 1)


# jackass
movies %>%
  filter(Title == "Jackass 3D (2010)"|Title=="Clash of the Titans (2010)")
