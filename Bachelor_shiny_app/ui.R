library(shiny, shinyjs)
library(pacman)
p_load(
    reshape2,
    jsonlite,
    tidyverse,
    tm,
    topicmodels,
    tidytext,
    plotly,
    widyr,
    data.table,
    Morpho,
    pracma,
    plotly,
    lubridate,
    tidyr,
    LDAvis,
    text2vec,
    UsingR
)
load("workspace.RData")

getwd()
# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    shinyjs::useShinyjs(),
    navbarPage(
        "HOPE - Bachelor",
        tabPanel("TFIDF",
                 sidebarLayout(
                     titlePanel("TFIDF"),
                     sidebarPanel(substr(lorem,1,500)),
                     mainPanel(tabsetPanel(
                         type = "tabs",
                         tabPanel("Personer", plotOutput("names")),
                         tabPanel("Uden Personer", plotOutput("noNames"))
                     ))
                 )),
        tabPanel("GloVE",
                 sidebarLayout(
                     titlePanel("GloVe"),
                     sidebarPanel(substr(lorem, 1, 500)),
                     mainPanel(
                         selectizeInput(
                             "GloVeLemma",
                             "Search word",
                             totalVocab$term,
                             selected = "danmark_propn",
                             options = list(allowEmptyOption = FALSE)
                         ),
                         actionButton(
                             inputId = "search_button",
                             label = "Start Søgning"
                         ), plotOutput("GloVE")
                         )
                     )
                 ), 
        tabPanel("LDAvis",
                 sidebarLayout(
                     titlePanel("LDAvis"),
                     sidebarPanel(substr(lorem,1,500)),
                     mainPanel(
                         tabsetPanel(
                             type = "tabs",
                             id = "navid",
                             tabPanel("Før Udbrud",
                                      includeHTML("LDAvisModels/baselineLDAvis.html")),
                             tabPanel(
                                 "Udbrud",
                                 ## Include new DIV here & Set initial height
                                 div(id = "appendhere"),
                                 includeHTML("LDAvisModels/outbreakLDAvis.html")
                             )
                         ,
                         tabPanel(
                             "Post Udbrud",
                             ## Include new DIV here & Set initial height
                             div(id = "appendhere"),
                             includeHTML("LDAvisModels/postOutbreakLDAvis.html")
                         ))
                     )
                 )),
        tabPanel("Tabeller",
                 sidebarLayout(
                     titlePanel("Tables"),
                     sidebarPanel(substr(lorem,1,500)),
                     mainPanel(
                         tabsetPanel(
                         type = "tabs",
                         tabPanel("Baseline",
                                  DT::dataTableOutput("tableBaseline")),
                         tabPanel("Outbreak",
                                  DT::dataTableOutput("tableOutbreak")),
                         tabPanel("Post Outbreak",
                                  DT::dataTableOutput("tablePostOutbreak"))
                     ))
                 )),
        tabPanel("Om",
                 sidebarLayout(
                     titlePanel("About"),
                     sidebarPanel(substr(lorem,1,500)),
                     mainPanel(lorem)
                 ))
    )
)


