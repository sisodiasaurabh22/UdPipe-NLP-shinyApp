library(shiny)
library(udpipe)
library(textrank)
library(lattice)
library(igraph)
library(ggraph)
library(ggplot2)
library(wordcloud)
library(stringr)
library(dplyr)
library(wordcloud)




####global functions###########################
##############################################

annotate_doc <- function(corpus1){
  
  corpus1  =  str_replace_all(corpus1, "<.*?>", "") # get rid of html junk 
  str(corpus1)
  
  destfile="./english-ewt-ud-2.4-190531.udpipe" 
  if (!file.exists(destfile)) {
    ud_model_english <- udpipe_download_model(language = "english")
  }
  
  english_model = udpipe_load_model("./english-ewt-ud-2.4-190531.udpipe")
  x <- udpipe_annotate(english_model, x = corpus1)
  x <- as.data.frame(x)
  
  return(x)
}

filtered_doc <- function(x,filter1){
  return(x %>% subset(., upos %in% filter1) %>% select(.,-c(sentence)))
}


wordcloud_noun <- function(x){
  all_nouns = x %>% subset(., upos %in% "NOUN") 
  top_nouns = txt_freq(all_nouns$lemma)
  wordcloud(words = top_nouns$key, 
            freq = top_nouns$freq, 
            min.freq = 2, 
            max.words = 100,
            random.order = FALSE, 
            scale = c(4,0.5),
            colors = brewer.pal(6, "Dark2"))
}


wordcloud_verb <- function(x){
  all_verbs = x %>% subset(., upos %in% "VERB") 
  top_verbs = txt_freq(all_verbs$lemma)
  wordcloud(words = top_verbs$key, 
            freq = top_verbs$freq, 
            min.freq = 2, 
            max.words = 100,
            random.order = FALSE,
            scale = c(4,0.5),
            colors = brewer.pal(6, "Dark2"))
}

Cooccurence_graph <- function(x){  # x annotated corpus
  nokia_colloc <- keywords_collocation(x = x,   # try ?keywords_collocation
                                       term = "token", 
                                       group = c("doc_id", "paragraph_id", "sentence_id"),
                                       ngram_max = 4)  # 0.42 secs
  nokia_cooc <- cooccurrence(     # try `?cooccurrence` for parm options
    x = subset(x, upos %in% c("NOUN", "ADJ")), 
    term = "lemma", 
    group = c("doc_id", "paragraph_id", "sentence_id"))
  
  nokia_cooc_gen <- cooccurrence(x = x$lemma, 
                                 relevant = x$upos %in% c("NOUN", "ADJ")) 
  nokia_cooc_skipgm <- cooccurrence(x = x$lemma, 
                                    relevant = x$upos %in% c("NOUN", "ADJ"), 
                                    skipgram = 4)
  wordnetwork <- head(nokia_cooc, 30)
  wordnetwork <- igraph::graph_from_data_frame(wordnetwork) # needs edgelist in first 2 colms.
  
  ggraph(wordnetwork, layout = "fr") +  
    
    geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "orange") +  
    geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
    
    theme_graph(base_family = "Arial Narrow") +  
    theme(legend.position = "none") +
    
    labs(title = "Cooccurrences within 3 words distance", subtitle = "Nouns & Adjective")
  
}



Cooccurence_graph2 <- function(x,filter2){  # x annotated corpus
  nokia_colloc <- keywords_collocation(x = x,   # try ?keywords_collocation
                                       term = "token", 
                                       group = c("doc_id", "paragraph_id", "sentence_id"),
                                       ngram_max = 4)  # 0.42 secs
  nokia_cooc <- cooccurrence(     # try `?cooccurrence` for parm options
    x = subset(x, upos %in% c("NOUN", "ADJ")), 
    term = "lemma", 
    group = c("doc_id", "paragraph_id", "sentence_id"))
  
  nokia_cooc_gen <- cooccurrence(x = x$lemma, 
                                 relevant = x$upos %in% filter2) 
  nokia_cooc_skipgm <- cooccurrence(x = x$lemma, 
                                    relevant = x$upos %in% filter2, 
                                    skipgram = 4)
  wordnetwork <- head(nokia_cooc, 30)
  wordnetwork <- igraph::graph_from_data_frame(wordnetwork) # needs edgelist in first 2 colms.
  
  ggraph(wordnetwork, layout = "fr") +  
    
    geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "orange") +  
    geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
    
    theme_graph(base_family = "Arial Narrow") +  
    theme(legend.position = "none") +
    
    labs(title = "Cooccurrences within 3 words distance", subtitle = "upos")
  
}


##############################################
#################################################
##################################################

ui <- fluidPage(
  titlePanel("NLP using Udpipe"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload text file"),
      br(),
      checkboxGroupInput("Checkboxgroup",label="Universal part-of-speech tags (upos)",
                         choices = list(
                           "adjective (ADJ)" = "ADJ",
                           "noun (NOUN)" = "NOUN",
                           "proper noun (PROPN)" = "PROPN",
                           "adverb (ADV)" = "ADV",
                           "verb (VERB)" = "VERB"
                         ),selected=c("ADJ","NOUN","PROPN"))
    ),
    mainPanel(
      
      
      
      tabsetPanel(type = "tabs",
                  tabPanel("Overview",h4(p("About this App")),
                           tabPanel("Overview",
                                    h4(p("Input Data ")),
                                    p("This app supports only text files (.txt) as data files and will compute and depict wordcloud,annotations,co occurence graphs"),
                                    p("Please refer to the link below for sample csv file."),
                                    a(href="https://github.com/sudhir-voleti/sample-data-sets/blob/master/Segmentation%20Discriminant%20and%20targeting%20data/ConneCtorPDASegmentation.csv"
                                      ,"Sample file to test"),  
                                    br(),
                                    h4('How can i check out this APP'),
                                    br(),
                                    p('To use this app, click on:'),
                                    
                                    p( '1.',span(strong("'browse button' of Upload text file in on the top left panel")),
                                       'and load the .txt  file to process'),
                                    p('2. select the UPOS to process for wordcloud, annotations, co-occurence graphs'),
                                    p('3. Verify the output in the respective output tabs '),
                                    p('Screenshot:'),
                                    img(src="Sample.png", height = 200, width = 500)
                           )),
                  tabPanel("Annotated documents",
                           downloadButton('downloadData2', 'Download annotated doc (Works only in browser)'),
                           fluidRow(dataTableOutput("annotdoc"))),
                  tabPanel("Wordcloud",
                           h4("word cloud for nouns in the corpus"),
                           plotOutput("wordcloud_noun" ,height = 300, width = 800),
                           h4("word cloud for verbs in the corpus"),
                           plotOutput("wordcloud_verb",height = 300, width = 800)),
                  tabPanel("Co-occurence graph",
                           plotOutput("Cooc_graph")))
#                  tabPanel("Annotation table",
#                           dataTableOutput("annotatedtext")),
#                  tabPanel("Word Cloud"),
#                  tabPanel("Co-occurence graph")
      )
      
    )
  )


server <- function(input, output, session) {
  set.seed=2092014
 
  
 dataset <- reactive({
    if (is.null(input$file)) {return(NULL)}
    else {
      Document = readLines(input$file$datapath)
      return(Document)}
  })
 output$inputframe <- renderPrint({dataset()})
 annotated_corpus <- reactive({
   annotate_doc(dataset())
 })
 output$annotdoc <- renderDataTable({
   #annotated_corpus <- annotate_doc(dataset()) %>% head(.,100)
   annotated_filtered <- filtered_doc(annotated_corpus(),input$Checkboxgroup) %>% head(.,100)
 })
  
 output$wordcloud_noun <- renderPlot({
   wordcloud_noun(annotated_corpus())
 })
 output$wordcloud_verb <- renderPlot({
   wordcloud_verb(annotated_corpus())
 })
 
output$Cooc_graph <- renderPlot({
  #Cooccurence_graph(annotated_corpus())
  Cooccurence_graph2(annotated_corpus(),input$Checkboxgroup)
})

output$downloadData2 <- downloadHandler(
  filename = function() { "annotated_corpus.csv" },
  content = function(file) {
    write.csv(annotated_corpus(), file, row.names=F)
  })
#  output$annotatedtext <- renderDataTable({
#    annotate_doc(dataset()$Document,input$Checkboxgroup)
#  })
 
  
}

# Create Shiny app ----
shinyApp(ui, server)