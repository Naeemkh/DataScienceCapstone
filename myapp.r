library(shiny)
library(shinydashboard)
library(ggplot2)
library(wordcloud)



source('myapp_user.r')
source('subfunctions.r')


appDocuments <- rmarkdown::render('documentation.Rmd')

server <- function(input, output) {

  output$pred.words <- renderText({
    predicted.words <- getNextWordProb(input$inputIdPhone,10, unigrams, bigrams, trigrams, tetagrams, pentagrams)
    paste(predicted.words[1,1]," ",predicted.words[2,1]," ",predicted.words[3,1])
  })  
  output$wordProbggplot <-   renderPlot({
    
    predicted.words <- getNextWordProb(input$inputIdGG,10, unigrams, bigrams, trigrams, tetagrams, pentagrams)
    ggplot(predicted.words,aes(x=reorder(ngram,prob),y=prob*100))+
                             geom_bar(stat="identity") + coord_flip()+
                             labs(x="Predicted Word", y="Probability (%)")+
                             theme(text = element_text(size=20),
                             axis.text.x = element_text(size=20)) 


    })
  
  output$wordCloud <-   renderPlot({
    
    predicted.words <- getNextWordProb(input$inputIdCloud,200, unigrams, bigrams, trigrams, tetagrams, pentagrams)
    predicted.words <- predicted.words[!is.na(predicted.words[,1]),]
    wordcloud(words = predicted.words$ngram, freq = ceiling(predicted.words$prob*200), min.freq = 1,
              max.words=Inf, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2")) 
     })
  
  output$accT <-   renderPlot({
    
    predicted.words <- getNextWordProb(input$inputIdAccT,200, unigrams, bigrams, trigrams, tetagrams, pentagrams)
    predicted.words <- predicted.words[!is.na(predicted.words[,1]),]
    wordcloud(words = predicted.words$ngram, freq = ceiling(predicted.words$prob*200), min.freq = 1,
              max.words=Inf, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2")) 
    })
  
  
  getPage<-function() {
    return(includeHTML(appDocuments))
  }
  
  output$inc<-renderUI({getPage()})
  
  
}

shinyApp(ui, server)