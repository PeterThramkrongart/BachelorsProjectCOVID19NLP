if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, shiny, shinyjs,UsingR)


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
                     sidebarPanel(substr(lorem,1,500)),
                     mainPanel(h2("tfidf"),
                         tabsetPanel(
                         type = "tabs",
                         tabPanel("Personer", plotOutput("names")),
                         tabPanel("Uden Personer", plotOutput("noNames"))
                     ))
                 )),
        tabPanel("GloVe",
                 sidebarLayout(
                     sidebarPanel(substr(lorem, 1, 500)),
                     mainPanel(h2("GloVe"),
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
                     sidebarPanel(substr(lorem,1,500)),
                     mainPanel(h2("LDAvis"),
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
                     sidebarPanel(substr(lorem,1,500)),
                     mainPanel(h2("Tables"),
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
                     sidebarPanel(substr(lorem,1,500)),
                     mainPanel(h2("About"),lorem)
                 ))
    )
)


