



ui <- dashboardPage(
  dashboardHeader(title = "NLP"),
  dashboardSidebar(disable = TRUE # sidebar start
   
    # sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
    #                   label = "Search..."),
    # radioButtons("C_grams","Choose n-grams",c("2-grams","3-grams","4-grams","5-grams"),selected = "5-grams", inline = FALSE,
    #              width = NULL)

  ), # sidebar end
  dashboardBody(
    fluidRow(
      tabBox(
        title = NULL,
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1", height = "auto",width="250px",
        tabPanel("Phone Style",
                 textInput("inputIdPhone",NULL,value = "Type your text here",width ="100%", placeholder = NULL),
                 verbatimTextOutput("pred.words")
        ),
        tabPanel("Next Word Probability",
                 textInput("inputIdGG",NULL,value = "Type your text here",width ="100%", placeholder = NULL),
                 plotOutput("wordProbggplot")
                 ),
        tabPanel("Word Cloud",
                 textInput("inputIdCloud",NULL,value = "Type your text here",width ="100%", placeholder = NULL),
                 plotOutput("wordCloud")
                 ),
        tabPanel("Documentation",style = "overflow-y:scroll; max-height: 90vh",
                 htmlOutput("inc")
                )
        
        
      )

    )
  )


)