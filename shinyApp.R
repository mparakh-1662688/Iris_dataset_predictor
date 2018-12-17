library("shiny")
library("dplyr")

#setwd("~/Desktop/info201/project")
source("iris_dataset.R")
my_ui <- fluidPage(
  sidebarPanel(
    selectInput("Type", "What algorithm do you want to use?",
                choices = c("Sepal Length", "Sepal Width",
                            "Petal Length", "Petal Width"))
  ),
  mainPanel(
    plotOutput("my_plot"),
    textOutput("Description")
  )
)

my_server <- function(input, output) { 
  
  output$Description <- renderPrint({      
    gen_type <- input$Type
    return(paste0("This ",
                  "wisker plot shows the range of values ", 
                  "the minimum, maximum, lower quaterile, upper quaterile",
                  " and the median of the data presented in ", gen_type ,
                  ". This will help us vizualize the data we are working with ",
                  "and help us make our machine learning model more effecient"
                  ))})
  
  output$my_plot <- renderPlot({
    gen_type <- input$Type 
    if(gen_type == "Sepal Length") {
      boxplot(x[,1], main=names(iris)[1], col = "Yellow")
    }
    if(gen_type == "Petal Length") {
      boxplot(x[,3], main=names(iris)[3], col = "Red")
    }
    if(gen_type == "Petal Width") {
      boxplot(x[,4], main=names(iris)[4], col = "Blue")
    }
    if(gen_type == "Sepal Width") {
      boxplot(x[,2], main=names(iris)[2], col = "Green")
    }
  })
  
}

shinyApp(my_ui, my_server)

