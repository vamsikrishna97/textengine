library(shiny)

# Define UI for dataset viewer application
shinyUI (fluidPage(
  
  # Application title
  titlePanel("TEXT ENGINE"),
  
  # Sidebar with controls to select a dataset and specify the
  # number of observations to view
  sidebarLayout(
    sidebarPanel(
#       radioButtons("dist", "Wordcloud type:",
#                    c("Normal" = "norm",
#                      "Uniform" = "unif"
#                      ) )
#       selectInput("option", "Choose a wordcloud type:", 
#                   choices = c("general",'associative')),
      
      textInput("query", "Enter the query"),
      radioButtons("radio", label = h3("WordCloud Type"),
                   choices = list("Normal" = "normal", "Associative" = "associative"),selected = "Normal"),
      submitButton("Submit"),h6(textOutput("text1")), width = 3
    ),
    
    
    mainPanel(
       plotOutput("wordcloud"),
       h4(textOutput("documents")),
       h4(verbatimTextOutput("text2")),
       h4(textOutput("keywords")),
       h4(verbatimTextOutput("text3"))
       
      
    )  
    )
  )
)
