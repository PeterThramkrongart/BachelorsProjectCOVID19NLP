


if (!require("pacman"))
    install.packages("pacman")

pacman::p_load(tidyverse,
               text2vec,
               tidytext,
               shiny,
               shinyjs,
               UsingR,
               shinythemes)

load("workspace.RData")

getwd()
# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    theme = shinytheme("cosmo"),
    shinyjs::useShinyjs(),
    navbarPage(
        "HOPE - SemTrackR",
        tabPanel(
            "Front Page",
            h2("Hope SemTrackR"),
            "Welcome to the HOPE SemTrackR.
 This is a tool for exploring how the COVID-19 pandemic has the language used in Danish newspapers.
 The models are based on the text from 1898 front pages from Danish mainstream newspapers.
 This includes front pages from Berlingske Tidende, B.T., Ekstrabladet, Information, Jyllands Posten, Politiken, Kristeligt Dagblad, and Weekendavisen.
 The data was gathered from 2019-12-02 to 2020-09-10.
 The data is divided into 3 distinct periods.
 (1) A baseline period from before COVID-19 reached Denmark (before 2020-02-26),
 (2) an outbreak period (2020-02-28 to 2020-04-12) spanning from the first infection cases in Denmark to the reopening of the first lockdown in Denmark,
 and (3) a post outbreak period (after 2020-04-12) that covers a period after the lockdown.
 Click on the tabs in the top panel to proceed to various models and start exploring!
"
        )
        ,
        tabPanel(
            "Distinct Words",
            
            h2("Distinct Words"),
            "Term frequency- inverse document frequency (TF-IDF) is a weighting metric to score words by how unique they are in a document compared to the rest of the documents.
            By treating the whole instead of comparing documents to documents,
            these bar plots show the 20 most distinct words (sorted by TF-IDF weights) in each period compared to the two other periods.
            There are two models:
            One based on person names only and one based on nouns, verbs, adjectives, and adverbs only.
            Use the tabs to navigate between the two.",
            br(),
            br(),
            tabsetPanel(
                type = "tabs",
                tabPanel("Names Only", plotOutput("names")),
                tabPanel(
                    "Nouns, verbs, adjectives, and adverbs only",
                    plotOutput("noNames")
                )
            )
        )
        ,
        tabPanel(
            "Word Similarity Search Engine",
            h2("Word Similarity Search Engine"),
            "This is the SemTrackR Similarity Search Engine.
 It searches for the top 20 words most similar to the search word in each period.
 You can use the search word selector to select a word to search for.
 The words are tagged with their word class.
 The classes are nouns, proper nouns, verbs, adjectives, and adverbs with the tags noun, propn,  verb, adj, and adv respectively.
 The words have been transformed into their dictionary form (lemmatized) and transform to all lowercase.
 When searching, the search engine will attempt to find the words that are most similar to the search word.
 If the search word does not appear more than five times in a period, the search cannot be computed for that specific period.
 \n
 "
            ,
            br(),
            br(),
            "
How do we measure word similarity?
 The meanings of the words are computationally inferred by how often words appear together.
 The argument is that words that tend to appear in the same context must have a similar meaning.
 These computational inferences can be represented as arrays of numbers called embeddings.
 The mathematical model used for the embeddings used for this search engine is called GloVe-embeddings.
 Each period has its own matrix of vocabulary represented in an embedded matrix.
 The actual search consists of linear algebraic computation called cosine similarity, which calculates the similarity between the search word and each word in a matrix. \n
",
            br(),
            br(),
            "
    DISCLAIMER:
 GloVe-embeddings need a large amount of data to work properly.
 The embeddings used for this search engine are therefore not trained on enough data to give robust results.
 Use this search engine at your own discretion.\n"
            ,
            br(),
            br(),
            "
Some ideas for search words are: sygdom_noun, økonomi_noun, smitte_verb, isolation_noun, and isolere_verb, test_noun
",
            br(),
            br(),
            selectizeInput(
                "GloVeLemma",
                "Select Search Word",
                totalVocab$term,
                selected = "danmark_propn",
                options = list(allowEmptyOption = FALSE)
            ),
            actionButton(inputId = "search_button",
                         label = "Start search engine!"),
            plotOutput("GloVE")
        )
        
        ,
        tabPanel(
            "Topic Models",
            h2("Topic Models"),
            substr(lorem, 1, 500),
            br(),
            br(),
            tabsetPanel(
                type = "tabs",
                id = "navid",
                tabPanel(
                    "Before COVID-19 in Denmark",
                    includeHTML("LDAvisModels/baselineLDAvis.html")
                ),
                tabPanel(
                    "During first outbreak and lockdown",
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
                )
            )
        )
        ,
        tabPanel(
            "Tables",
            h2("Tables"),
            substr(lorem, 1, 500),
            br(),
            br(),
            tabsetPanel(
                type = "tabs",
                tabPanel(
                    "Before COVID-19 in Denmark",
                    DT::dataTableOutput("tableBaseline")
                ),
                tabPanel("During lockdown",
                         DT::dataTableOutput("tableOutbreak")),
                tabPanel("After lockdown",
                         DT::dataTableOutput("tablePostOutbreak"))
            )
        )
        ,
        tabPanel("About",
                 h2("About"), lorem)
    )
)



factor_levels <-
    levels(as.factor(c("Baseline", "Outbreak", "PostOutbreak")))

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
                    CoronaStatus == "Outbreak" |
                    CoronaStatus == "PostOutbreak"
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
            facet_wrap(~ CoronaStatus, scales = "free_y") +
            scale_x_reordered() +
            scale_y_continuous(n.breaks = 5, limits = c(0,1)) +
            ggtitle(paste("Top", top_n, "words related to:", word)) +
            scale_fill_discrete(drop = TRUE,
                                limits = factor_levels)
        
        
    }


tfidf_df <- tfidf_df %>%
    group_by(CoronaStatus) %>%
    top_n(20, tf_idf) %>%
    ungroup() %>%
    mutate(
        CoronaStatus = as.factor(CoronaStatus),
        lemma = reorder_within(lemma, tf_idf, CoronaStatus)
    )


tfidf_dfNames <- tfidf_dfNames %>%
    group_by(CoronaStatus) %>%
    top_n(20, tf_idf) %>%
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
            labs(x = NULL, y = "Term Frequency–Inverse Document Frequency") +
            facet_wrap(~ CoronaStatus, scales = "free_y") +
            coord_flip() +
            ggtitle("TF-IDF: Names Only") +
            scale_y_continuous(n.breaks = 3) +
            scale_x_reordered()
    })
    output$noNames <- renderPlot({
        tfidf_df %>%
            ggplot(aes(lemma, tf_idf, fill = CoronaStatus)) +
            geom_col(show.legend = FALSE) +
            labs(x = NULL, y = "Term Frequency–Inverse Document Frequency") +
            facet_wrap(~ CoronaStatus, scales = "free_y") +
            coord_flip() +
            ggtitle("TF-IDF: Nouns, verbs, adjectives, and adverbs only") +
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
            filter(CoronaStatus == "Baseline")
    })
    output$tableOutbreak <- DT::renderDataTable({
        table_df %>%
            filter(CoronaStatus == "Outbreak") %>%
            arrange(desc(n))
    })
    output$tablePostOutbreak <- DT::renderDataTable({
        table_df %>%
            filter(CoronaStatus == "PostOutbreak") %>%
            arrange(desc(n))
    })
}



#options(shiny.host = "http://127.0.0.1:8119")

options(shiny.port = 8119) #set port to the right number

# Run the application
shinyApp(ui = ui, server = server)
