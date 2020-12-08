#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
    titlePanel("Old Faithful Geyser Data"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(sidebarPanel("hello"),
                  
                  # Show a plot of the generated distribution
                  mainPanel(
                      tabsetPanel(
                          type = "tabs",
                          tabPanel("Names", plotOutput("names")),
                          tabPanel("No Names", plotOutput("noNames"))
                      )
                  )))
# Define server logic required to draw a histogram
server <- function(input, output, session) {
    output$names <- renderPlot({
        tfidf_dfNames %>%
            arrange(desc(tf_idf)) %>%
            mutate(lemma = factor(lemma, levels = rev(unique(lemma)))) %>%
            group_by(CoronaStatus) %>%
            top_n(20) %>%
            ungroup() %>%
            ggplot(aes(lemma, tf_idf, fill = CoronaStatus)) +
            geom_col(show.legend = FALSE) +
            labs(x = NULL, y = "tf-idf") +
            facet_wrap( ~ CoronaStatus, scales = "free") +
            coord_flip() +
            ggtitle("TF-IDF: Names")
    })
    output$noNames <- renderPlot({
        tfidf_df %>%
            arrange(desc(tf_idf)) %>%
            mutate(lemma = factor(lemma, levels = rev(unique(lemma)))) %>%
            group_by(CoronaStatus) %>%
            top_n(20) %>%
            ungroup() %>%
            ggplot(aes(lemma, tf_idf, fill = CoronaStatus)) +
            geom_col(show.legend = FALSE) +
            labs(x = NULL, y = "tf-idf") +
            facet_wrap( ~ CoronaStatus, scales = "free") +
            coord_flip() +
            ggtitle("TF-IDF: NER-filtered")
        
        
    })
    
    data(TwentyNewsgroups, package = "LDAvis")
    output$ldavis <- renderVis({
        with(TwentyNewsgroups, 
             createJSON(phi, theta, doc.length, vocab, term.frequency, 
                        R = input$nTerms))
    }
    )
        
    }


