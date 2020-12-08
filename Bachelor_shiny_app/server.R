
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
factor_levels <- levels(as.factor(c("Baseline","Outbreak","PostOutbreak")))

search_and_plot <-
    function(word,
             top_n = 15 ,
             baseline = baselineGloVe,
             outbreak = outbreakGloVe,
             postOutbreak = postOutbreakGloVe) {
        print(class(word))
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
    top_n(20, tf_idf) %>%
    ungroup() %>%
    mutate(
        CoronaStatus = as.factor(CoronaStatus),
        lemma = reorder_within(lemma, tf_idf, CoronaStatus)
    )


tfidf_dfNames<- tfidf_dfNames %>%
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
