
if (!require("pacman"))
    install.packages("pacman")

pacman::p_load(tidyverse,
               text2vec,
               tidytext,
               shiny,
               shinyjs,
               UsingR
)

load("workspace.RData")

getwd()
# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    shinyjs::useShinyjs(),
    navbarPage(
        "HOPE - SemTrackR",
        tabPanel("Frontpage",
                 sidebarLayout(
                     sidebarPanel(substr(lorem,1,500)),
                     mainPanel(h2("Hope SemTrackR"),substr(lorem,1,1000))
                 )),
        tabPanel("Distinct Words",
                 sidebarLayout(
                     sidebarPanel(substr(lorem,1,500)),
                     mainPanel(h2("Distinct Words"),
                               tabsetPanel(
                                   type = "tabs",
                                   tabPanel("Names Only", plotOutput("names")),
                                   tabPanel("Names filtered", plotOutput("noNames"))
                               ))
                 )),
        tabPanel("Association Search Engine",
                 sidebarLayout(
                     sidebarPanel(substr(lorem, 1, 500)),
                     mainPanel(h2("Word Association Search Engine"),
                               selectizeInput(
                                   "GloVeLemma",
                                   "Search word",
                                   totalVocab$term,
                                   selected = "danmark_propn",
                                   options = list(allowEmptyOption = FALSE)
                               ),
                               actionButton(
                                   inputId = "search_button",
                                   label = "Start search engine!"
                               ), plotOutput("GloVE")
                     )
                 )
        ), 
        tabPanel("Topic Models",
                 sidebarLayout(
                     sidebarPanel(substr(lorem,1,500)),
                     mainPanel(h2("Topic Models"),
                               tabsetPanel(
                                   type = "tabs",
                                   id = "navid",
                                   tabPanel("Before COVID-19 in Denmark",
                                            includeHTML("LDAvisModels/baselineLDAvis.html")),
                                   tabPanel(
                                       "During lockdown",
                                       ## Include new DIV here & Set initial height
                                       div(id = "appendhere"),
                                       includeHTML("LDAvisModels/outbreakLDAvis.html")
                                   )
                                   ,
                                   tabPanel(
                                       "After lockdown",
                                       ## Include new DIV here & Set initial height
                                       div(id = "appendhere"),
                                       includeHTML("LDAvisModels/postOutbreakLDAvis.html")
                                   ))
                     )
                 )),
        tabPanel("Tables",
                 sidebarLayout(
                     sidebarPanel(substr(lorem,1,500)),
                     mainPanel(h2("Tables"),
                               tabsetPanel(
                                   type = "tabs",
                                   tabPanel("Before COVID-19 in Denmark",
                                            DT::dataTableOutput("tableBaseline")),
                                   tabPanel("During lockdown",
                                            DT::dataTableOutput("tableOutbreak")),
                                   tabPanel("After lockdown",
                                            DT::dataTableOutput("tablePostOutbreak"))
                               ))
                 )),
        tabPanel("About",
                 sidebarLayout(
                     sidebarPanel(substr(lorem,1,500)),
                     mainPanel(h2("About"),lorem)
                 ))
    )
)


factor_levels <- levels(as.factor(c("Baseline","Outbreak","PostOutbreak")))

search_and_plot <-
    function(word,
             top_n = 15 ,
             baseline = baselineGloVe,
             outbreak = outbreakGloVe,
             postOutbreak = postOutbreakGloVe) {
        baseline = try(baseline %>% coSimFunc(word, top_n, "Baseline"), silent = T)
        outbreak = try(outbreak %>% coSimFunc(word, top_n, "Outbreak"), silent = T)
        postOutbreak = try(postOutbreak %>% coSimFunc(word, top_n, "PostOutbreak"),
                           silent = T)
        
        df = rbind(baseline, outbreak, postOutbreak) %>%
            filter(
                CoronaStatus == "Baseline" |
                    CoronaStatus == "Outbreak" | CoronaStatus == "PostOutbreak"
            )
        df %>%
            mutate(
                CoronaStatus = as.factor(CoronaStatus),
                lemma1 = reorder_within(as.factor(lemma), as.numeric(similarity), CoronaStatus)
            ) %>%
            ggplot(aes(lemma1, as.numeric(as.character(similarity)), fill = CoronaStatus)) +
            geom_col(show.legend = FALSE) +
            labs(x = NULL, y = "similarity") +
            coord_flip() +
            facet_wrap( ~ CoronaStatus, scales = "free_y") +
            scale_x_reordered() +
            scale_y_continuous(n.breaks = 5) +
            ggtitle(paste("top", top_n, "words related to:", word)) + 
            scale_fill_discrete(drop=TRUE,
                                limits = factor_levels)
        
        
    }


tfidf_df<- tfidf_df %>%
    group_by(CoronaStatus) %>%
    slice_max(20, n = tf_idf) %>%
    ungroup() %>%
    mutate(
        CoronaStatus = as.factor(CoronaStatus),
        lemma = reorder_within(lemma, tf_idf, CoronaStatus)
    )


tfidf_dfNames<- tfidf_dfNames %>%
    group_by(CoronaStatus) %>%
    slice_max(20, n = tf_idf) %>%
    ungroup() %>%
    mutate(
        CoronaStatus = as.factor(CoronaStatus),
        lemma = reorder_within(lemma, tf_idf, CoronaStatus)
    )

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    output$names <- renderPlot({
        tfidf_dfNames %>%
            ggplot(aes(lemma, tf_idf, fill = CoronaStatus)) +
            geom_col(show.legend = FALSE) +
            labs(x = NULL, y = "tf-idf") +
            facet_wrap( ~ CoronaStatus, scales = "free_y") +
            coord_flip() +
            ggtitle("TF-IDF: Names") +
            scale_y_continuous(n.breaks = 3) +
            scale_x_reordered()
    })
    output$noNames <- renderPlot({
        tfidf_df %>%
            ggplot(aes(lemma, tf_idf, fill = CoronaStatus)) +
            geom_col(show.legend = FALSE) +
            labs(x = NULL, y = "tf-idf") +
            facet_wrap( ~ CoronaStatus, scales = "free_y") +
            coord_flip() +
            ggtitle("TF-IDF: NER-filtered") +
            scale_x_reordered() +
            scale_y_continuous(n.breaks = 3)
    })
    
    observeEvent(input$search_button, {
        output$GloVE <-
            renderPlot(if (input$GloVeLemma != "") {
                search_and_plot(input$GloVeLemma, 20)
            })
    })
    
    observe({
        req(input$navid == "Outbreak")
        shinyjs::runjs('$("#appendhere").resize()')
    })
    
    observe({
        req(input$navid == "Post Outbreak")
        shinyjs::runjs('$("#appendhere").resize()')
    })
    
    # Replace the renderTable() with DT's version
    output$tableBaseline <- DT::renderDataTable({
        table_df %>%
            filter(CoronaStatus == "Baseline") %>%
            arrange(desc(n)) %>% 
            slice_max(n, n = 1000 )
    })
    output$tableOutbreak <- DT::renderDataTable({
        table_df %>%
            filter( CoronaStatus == "Outbreak") %>%
            arrange(desc(n))%>% 
            slice_max(n, n = 1000 )
    })
    output$tablePostOutbreak <- DT::renderDataTable({
        table_df %>% 
            filter( CoronaStatus == "PostOutbreak") %>%
            arrange(desc(n))%>% 
            slice_max(n, n= 1000 )
    })
}


#options(shiny.host = '0.0.0.0')
#options(shiny.host = "127.0.0.2")

#options(shiny.port = 8890) #set port to the right number

# Run the application 
shinyApp(ui = ui, server = server)
