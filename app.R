# STA 404 indiv proj -- Logan Kocka
library(shiny)
library(tidyverse)
library(plotly)
library(lubridate)
library(zoo)

movies <- read.csv("Highest_Holywood_Grossing_Movies.csv")
# CLEANING
movies <- na.omit(movies)
movies$Release.Date <- as.Date(movies$Release.Date,format='%B %d, %Y')
# combine/clean distributor column
movies <- movies %>% mutate(Distributor=ifelse((Distributor=="United Artists Releasing"),"United Artists",Distributor)) %>% 
  mutate(Distributor=ifelse((Distributor=="DreamWorks Distribution"|Distributor=="DreamWorks Releasing"),"DreamWorks",Distributor)) %>%
  mutate(Distributor=ifelse((Distributor=="Sony Pictures Entertainment (SPE)"|Distributor=="Sony Pictures Classics"),"Sony Pictures",Distributor)) %>%
  mutate(Distributor=ifelse((Distributor=="Walt Disney Studios Motion Pictures"),"Walt Disney",Distributor))
# change gross $ to millions
movies <- movies %>%
  mutate(Domestic.Sales.mil = Domestic.Sales..in... / 1000000) %>%
  mutate(International.Sales.mil = International.Sales..in... / 1000000) %>%
  mutate(World.Sales.mil = World.Sales..in... / 1000000)


# ui start
ui <- fluidPage(
  navbarPage("STA 404 Solo Project",
             tabPanel("Home",
                      fluidRow(column(12, h3(div("by Logan Kocka", style="color: #2439B0")))),
                      fluidRow(column(12, h4("This dashboard creates static visualizations from a public dataset containing information on
                                      the 1000 highest lifetime grossing hollywood movies."))), br(),
                      fluidRow(column(12, h4("Citation"))),
                      fluidRow(column(1, ""), column(9, "Author: Sanjeet Singh Naik. Updated January 2022. Title: Top 1000 Highest Grossing Movies
                                                  (Version 1).", br(), br(), "Retrieved from Kaggle: ", uiOutput("url"), br(), br(), 
                                                     "Packages used: shiny, tidyverse, lubridate, zoo, plotly")),
                      
                      br(), br(), br(),
                      wellPanel(
                        fluidRow(column(12, align="center", h4("TRIVIA QUESTION: "), 
                                        h3("Avatar is the highest grossing movie globally."),
                                        h3("What is the highest grossing movie based on ", strong("domestic"), "revenue only?"))),
                        fluidRow(column(12, align="center", actionButton(inputId = "answer", icon("fas fa-magic"), label = "Answer"))),
                        fluidRow(column(12, align="center", br(), uiOutput(outputId = "starwars")))
                        
                      )),
             
             tabPanel("Distributors",
                      fluidRow(column(12, strong("Description:"), "If we're interested in seeing the leading companies producing some of the highest
grossing movies, we can look at the distributor breakdown by license. A movie's association with a particular distributor has impact on things like branding
and funding and is, I would guess, a predictor for the success of the movie. Companies which distributed fewer than 10 movies in this data set were omitted
for clarity.")), br(), br(),
                      plotlyOutput("bar"), br(), 
                      wellPanel(
                        fluidRow(column(12, h5("Comments:"))), 
                        fluidRow(column(1, ""), column(11, uiOutput("barComms"))))
             ),
             
             tabPanel("Time Series",
                      fluidRow(column(12, strong("Description:"), "Perhaps we want insights on the release dates of the highest grossing movies
for looking at the effect of technology increases on movie success or in order to choose a release date for an upcoming new movie.
We can plot the count of highest grossing movie releases over time by both month and year. The top subplot shows monthly counts which can be useful for identifying
seasonal cyclic patterns within the year, and the bottom can give information on longer-term trends spanning several years.")), br(), br(),
                      plotlyOutput("line"), br(), 
                      wellPanel(
                        fluidRow(column(12, h5("Comments:"))), 
                        fluidRow(column(1, ""), column(11, uiOutput("lineComms"))))
             ),
             
             tabPanel("Genre Distributions",
                      fluidRow(column(12, strong("Description:"), "Now we're interested in looking at the distribution of lifetime gross of each
movie genre. With this information we can identify movie genres which were global favorites or those of which that were only liked by groups 
of people with more niche interests. Movies falling into more than one category were counted in each genre group. Due to space limitations, genres with smaller
distributions (Sport, Documentary, Biography, Western, History, Horror, Music, and War) were removed.")), br(), 
                      plotlyOutput("violin"), br(), 
                      wellPanel(
                        fluidRow(column(12, h5("Comments:"))), 
                        fluidRow(column(1, ""), column(11, uiOutput("vioComms"))))
             ),
             
             tabPanel("Domestic vs World Gross",
                      fluidRow(column(12, strong("Description:"), "Let's compare movie consumption (lifetime gross in millions $) 
in the U.S. with consumption internationally and globally. Each data point represents a movie in the top ~1000 highest grossing movies list.
The x-axis shows domestic sales and y-axis shows world sales, so we can see where each movies falls by looking at placement. 
The size of each bubble represents international sales i.e. only sales from outside of the U.S.")), br(), 
                      plotlyOutput("bubble"), br(),
                      wellPanel(
                        fluidRow(column(12, h5("Comments:"))), 
                        fluidRow(column(1, ""), column(11, uiOutput("bubComms"))))
             )),
  
  
  tags$head(tags$style(HTML('#answer{background-color:#FCF22E}')))
  
)

# server start
server <- function(input, output) {
  # create hyperlink for the citation
  url <- a("https://www.kaggle.com/sanjeetsinghnaik/top-1000-highest-grossing-movies", href="https://www.kaggle.com/sanjeetsinghnaik/top-1000-highest-grossing-movies")
  output$url <- renderUI({
    tagList(url)
  })
  
  # show image on click, needs reactive event + observe
  printImage <- eventReactive(input$answer, {
    output$starwars <- renderUI({
      img(src='The_Force_Awakens.jpeg', height='180px', width='350px')
    })
  })
  observe(printImage())
  
  # create bar
  output$bar <- renderPlotly({
    movies_bar <- movies %>%
      count(License, Distributor, sort = TRUE) %>%
      filter(n >= 10) # just keep the ones with n > 10 since there are so many
    
    arr <- movies_bar %>% count(Distributor, sort = TRUE) %>% # for ordering
      arrange(n)
    
    cols <- c('#e69f00','deepskyblue4','#009e73','#f0e442','darkred','darkslateblue','#d55e00','#cc79a7','darkseagreen')
    
    plot_ly(movies_bar, x=~License, y=~n, color=~Distributor, colors=cols, type='bar', hoverinfo='text', 
            text=~paste('</br> License: ', License,
                        '</br> Distributor: ', Distributor,
                        '</br> Count: ', n)) %>%
      layout(yaxis = list(title = "Count"),
             barmode = "stack", title=list(text="Distributors with 10+ of the ~1000 Highest Grossing Movies",
                                           font=list(color='darkred')))
  })
  
  # create bulleted list for text output (comments)
  output$barComms <- renderUI(HTML("<ul><li> The PG-13 rating group produces the highest number of highest grossing movies,
which makes sense because this category is most appropriate\n and appealing to a large number of people and
contains many different genres. </li><li> Warner Brothers leads both the PG-13 and R rating categories with 59 and 48 movies 
respectively. </li><li> Walt Disney Company leads the PG rating category with 46 movies as expected due to the high volume of classic kids 
movies that Disney is known for producing. </li></ul>"))
  
  # create line
  output$line <- renderPlotly({
    # floor dates and summarize counts grouped by month year and just year
    movies_month <- movies %>%
      mutate(MonYr = lubridate::floor_date(Release.Date, "month")) %>%
      group_by(MonYr) %>%
      summarize(n = n())
    movies_yr <- movies %>%
      mutate(Yr = lubridate::floor_date(Release.Date, "year")) %>%
      group_by(Yr) %>%
      summarize(n = n())
    movies_yr$label <- sapply(strsplit(as.character(movies_yr$Yr),"-|s"), `[`, 1)
    
    # line time series
    fig1 <- plot_ly(movies_month, type='scatter', mode='lines', x=~MonYr, y=~n, name='Month')
    fig2 <- plot_ly(movies_yr, type='scatter', mode='lines', x=~Yr, y=~n, name='Year', hoverinfo='text',
                    text=paste0("(",movies_yr$label, ", ", movies_yr$n,")"))
    subplot(fig1, fig2, nrows=2) %>%
      layout(plot_bgcolor='#e5ecf6', title=list(text='Time Series of Top ~1000 Highest Grossing Movie Releases',
                                                font=list(color='#FD6200')),
             legend=list(orientation='h', title=list(text='Dates floored by '))) %>%
      add_annotations(text="Release Date", x=0.5, y=-0.01, yref = "paper", xref="paper", xanchor="center",
                      yanchor="bottom", yshift=-35, showarrow=F, font=list(size=15)) %>%
      add_annotations(text="Number of Movies Released", x=-0.03, y=0.5, xref='paper', yref='paper', font=list(size=15), 
                      textangle=270, showarrow=F)
  })
  
  # comments for line
  output$lineComms <- renderUI(HTML("<ul><li>There was a huge spike in releases of hit movies around 20 years ago. This could be attributed to 
the profound impact that computers and improved digital technology have had on production value.</li><li>There was a significant drop in hit movie
releases in 2020 compared with the previous year; 34 in 2019 to just 3 in 2020. COVID-19 caused a sharp decline in the entertainment industry.
</li><li>There is a clear cyclic pattern based on seasonality in the top subplot. Year after year, popular release times for the highest grossing
movies are summer and at the year's end.</li></ul>"))
  
  # create violin/boxplot
  output$violin <- renderPlotly({
    movies$Genre <- gsub("\\[|\\]", "", movies$Genre)
    movies$Genre <- gsub("'", "", movies$Genre)
    movies_unlist <- movies %>% separate_rows(Genre) %>%
      mutate(Genre=ifelse((Genre=="Sci"|Genre=="Fi"),"SciFi",Genre))
    movies_unlist <- movies_unlist[!duplicated(movies_unlist), ] # fixes a problem with the separate_rows
    # remove some of the less frequent genres
    movies_unlist <- movies_unlist %>%
      filter(!Genre %in% c('Sport', 'Documentary', 'Biography', 'Western', 'History', 'Horror', 'Music','War'))
    
    # world
    plot_ly(movies_unlist, y=~World.Sales.mil, color=~Genre) %>%
      add_trace(type='violin', box = list(visible = T), meanline = list(visible = T)) %>%
      layout(yaxis=list(title="World Lifetime Gross (Million USD)"), 
             xaxis=list(title="Genre", tickangle=60), 
             title=list(text="World Sales (Million USD) of Highest ~1000 Grossing Movies in Top Genres", 
                        font=list(color='#AD7B1C')),
             margin=list(t=60))
  })
  
  # comments for violin
  output$vioComms <- renderUI(HTML("<ul><li> Medians between genres are not as different as was expected. </li>
<li> SciFi, Fantasy, Adventure, and Action have similar distributions. Many movies with a SciFi theme 
inherently have themes of Fantasy, Adventure and Action as well, causing overlap between these specific genres. 
This explains the similarity.</li>
<li> Most high grossing outliers are SciFi/Fantasy/Adventure/Action but the medians and upper quantiles tell us
that this is not the case for all movies in those genres. </li>
<li> The genres with many high outliers, which reach upward of $3 billion, have universally appealing themes such as Action, 
which are not specific to just one country or culture. Some of the lower grossing genres only appeal to niche interests or 
groups of people, such as Western (omitted). </li></ul>"))
  
  #create bubble
  output$bubble <- renderPlotly({
    movies_diff <- movies %>%
      mutate(Differential.mil = World.Sales.mil-Domestic.Sales.mil)
    # differential/size will tell us whether it was mostly popular in just the US or around entire world
    # very high difference will indicate more profits came from outside US
    
    plot_ly(movies_diff, x=~Domestic.Sales.mil, y=~World.Sales.mil, size=~Differential.mil, type = 'scatter', 
            mode = 'markers', hoverinfo='text', marker=list(sizemode = 'diameter', color='#40CD31'), sizes = c(1, 50),
            text=paste('</br><b>$ Diff:</b>', paste0('$',ceiling(movies_diff$Differential.mil),'m'), 
                       '</br><b>Movie Title:</b>', movies_diff$Title)) %>%
      layout(yaxis=list(title="<b>World</b> Lifetime Gross (Million USD)"), 
             xaxis=list(title="<b>Domestic</b> Lifetime Gross (Million USD)"), 
             title=list(text="Differences in Domestic and World Lifetime Gross (Million USD) Highest ~1000 Grossing Movies",
                        font=list(color='darkblue')), plot_bgcolor='#F0ECE2')
  }) 
  
  # comments for bubble
  output$bubComms <- renderUI(HTML("<ul>
<li> The Avengers, Avatar, Star Wars, Jurrasic World: aside from some of these being popular 
franchises around the world, these movies chose plots and characters that most of the world can enjoy without much of a cultural barrier. 
These movies have the larger differentials between world and domestic lifetime gross, meaning those movies brought in a lot of money 
from outside of the US. This reaffirms the findings from the ditribution plots. </li>
<li> Many of the movies with smaller data points were mainly only popular in the United States because they cater to Western/American
humor or pop culture or aren't internationally known e.g. Think Like a Man, Step Brothers, This is the End. </li>
<li> A larger majority of the bubbles are on the small side, meaning people in the U.S. spend a lot more movies than those outside the U.S.
This could be why movies with targeted American humor or cultural elements can still be financially feasible. </li></ul>"))
  


}

# Run the application 
shinyApp(ui = ui, server = server)

