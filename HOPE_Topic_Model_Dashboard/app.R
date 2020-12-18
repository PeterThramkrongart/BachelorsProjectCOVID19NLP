
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
    "HOPE - Topic Models Dashboad",
    tabPanel(
      "Front Page",
      titlePanel("HOPE - Topic Models Dashboad"),
      sidebarLayout(
        sidebarPanel(
          p(strong("Welcome to the HOPE Topic Models Dashboad.")),
          br(),
          "This is a tool for exploring how the COVID-19 pandemic has the language used in Danish newspapers.
 The models are based on the text from 1898 front pages from Danish mainstream newspapers.
 This includes front pages from Berlingske Tidende, B.T., Ekstrabladet, Information, Jyllands Posten, Politiken, Kristeligt Dagblad, and Weekendavisen.\n
 ",
          br(),
          br(),
          "The data was gathered from 2019-12-02 to 2020-09-10.
 The data is divided into 3 distinct periods.
 (1) A baseline period from before COVID-19 reached Denmark (before 2020-02-26),
 (2) an outbreak period (2020-02-28 to 2020-04-12) spanning from the first infection cases in Denmark to the reopening of the first lockdown in Denmark,
 and (3) a post outbreak period (after 2020-04-12) that covers a period after the lockdown.
 Click on the tabs in the top panel to proceed to various models and start exploring!
",
          width = 6
        ),
        mainPanel(
          img(
            src = 'HOPE Logo.png',
            height = "75%",
            width = "75%",
            align = "right"
          ),
          width = 6
        )
      )
    )
    ,
    tabPanel(
      "Distinct Words",
      
      titlePanel("Distinct Words"),
      sidebarLayout(
        sidebarPanel(
          p(strong(
            "How do we find distinct words for each period?"
          )),
          "Term frequency- inverse document frequency (TF-IDF) is a weighting metric to score words by how unique they are in a document compared to the rest of the documents.
            By treating the whole instead of comparing documents to documents,
            these bar plots show the 20 most distinct words (sorted by TF-IDF weights) in each period compared to the two other periods.
            There are two models:
            One based on person names only and one based on nouns, verbs, adjectives, and adverbs only.
            Use the tabs to navigate between the two."
        ),
        
        mainPanel(tabsetPanel(
          type = "tabs",
          tabPanel(
            "Nouns, verbs, adjectives, and adverbs only",
            plotOutput("noNames")),
            tabPanel("Names Only",
                     plotOutput("names"))
          )
        )
      )
    )
    ,
    tabPanel(
      "Topic Models",
      titlePanel("Topic Models"),
      p(strong("What are topic models?")),
      "Topic models are a type of statistical models for used for discovering and exploring abstract topics that occur in collections of text documents.
 This type of modeling is useful for mapping the contents of collections of text that are too large to read.
",
      br(),
      br(),
      p(strong("How do I read the model?")),
      "This model has five elements:
 (1) Three tabs to select a period to explore,
 (2) a section to select a specific topic to focus on,
 (3) a two-dimensional representation of how similar topics are to each other called the Intertopic Distance Map,
 (4) a bar chart of the top 30 most relevant words how frequent they appear within the topic and overall,
 and (5) a slider to adjust to define how the relevancy of words is determined.
 Use that tabs to select a period to explore.
 Use can either use the topic selection section to pick a topic or you can click on a topic bubble in the Intertopic Distance Map.
 This map shows how similar topics are to each other.
 If they share many words they will overlap.
 This map also shows how large the topics are.
 The larger the bubble, the more words that belong to this topic.
 When you select a topic,
 you can explore it in the interactive bar chart on the right.
 The words are ordered by how relevant they are to the selected topic.
 The definition of relevancy can be adjusted on the slider above the chart.
 If the slider is set to 0.0, the chart will emphasize words distinct to the chosen topic,
 If the slider is at 1.0, the chart will emphasize frequent words.
 We recommend starting with the slider at 0.6.
 You can hover the mouse over a specific word in the bar chart,
 and it will show how what topics it belongs to and how much it does so.
 If not specific topic or word is selected, the words shown in the bar chart will be ranked by a measure called saliency.
 It describes how much information a word conveys about a topic.
 You can think of the top-30 most salient words,
 as the 30 overall most important words to describe the topics the period.

",
      br(),
      br(),
      p(strong("Technical information:")),
      "These topic models were computed with Latent Dirichlet Allocation (LDA).
 It is a probabilistic topic modeling technique that attempts to assign probabilities to each word in a collection of texts of how likely it is to belong to a given topic.
 The K number of topics is predetermined and the model has to discover what words the topics consist of.
 That is why topics do not have titles.
",
      br(),
      br(),
      tabsetPanel(
        type = "tabs",
        id = "navid",
        tabPanel(
          "Before COVID-19 in Denmark",
          ## Include new DIV here & Set initial height
          div(id = "appendhere"),
          includeHTML("LDAvisModels/baselineLDAvis.html")
        ),
        tabPanel(
          "During the first outbreak and lockdown",
          ## Include new DIV here & Set initial height
          div(id = "appendhere"),
          includeHTML("LDAvisModels/outbreakLDAvis.html")
        )
        ,
        tabPanel(
          "After the first lockdown",
          ## Include new DIV here & Set initial height
          div(id = "appendhere"),
          includeHTML("LDAvisModels/postOutbreakLDAvis.html")
        )
      )
    )
    ,
    tabPanel(
      "Tables",
      titlePanel("Tables"),
      sidebarLayout(
        sidebarPanel(
          "Here are interactive tables describing the frequency of the top 1000 most frequent words in each period.
 The columns are as follows;
 CoronaStatus signifies the period,
 n describes the number of times a word appears in the period,
 the word column represents the word in dictionary form,
 and TermFrequency describes Term Frequency which is the number of appearances divided by the total number of words in the period.
 You can use this metric to compare words between periods.
 You can sort the columns,
 search for specific words,
 and modify the number of shown entries.
"
        ),
        mainPanel(tabsetPanel(
          type = "tabs",
          tabPanel(
            "Before COVID-19 in Denmark",
            DT::dataTableOutput("tableBaseline")
          ),
          tabPanel("During lockdown",
                   DT::dataTableOutput("tableOutbreak")),
          tabPanel("After lockdown",
                   DT::dataTableOutput("tablePostOutbreak"))
        ))
      )
    )
  )
)


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
      mutate(word = lemma,
             lemma = NULL,
             TermFrequency = n / sum(n))
  })
  output$tableOutbreak <- DT::renderDataTable({
    table_df %>%
      filter(CoronaStatus == "Outbreak") %>%
      mutate(word = lemma,
             lemma = NULL,
             TermFrequency = n / sum(n)) %>%
      arrange(desc(n))
  })
  output$tablePostOutbreak <- DT::renderDataTable({
    table_df %>%
      filter(CoronaStatus == "PostOutbreak") %>%
      arrange(desc(n)) %>%
      mutate(word = lemma,
             lemma = NULL,
             TermFrequency = n / sum(n))
  })
}


#options(shiny.host = "http://127.0.0.1:8119")

options(shiny.port = 8119) #set port to the right number

# Run the application
shinyApp(ui = ui, server = server)
