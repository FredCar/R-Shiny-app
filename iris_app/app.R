library(shiny)
library(shinythemes)

# User Interface ####
ui <- navbarPage(theme = shinytheme("united"),
                 "Mini APP",
                 
                 # Page "Plot" ####
                 tabPanel("Plot",
                          sidebarLayout(
                                        sidebarPanel(h3("Selection des variables"),
                                                     selectInput("selectX",
                                                                 label = "Selection de X",
                                                                 choices = list("Sepal Length" = "Sepal.Length",
                                                                                "Sepal Width" = "Sepal.Width",
                                                                                "Petal Length" = "Petal.Length",
                                                                                "Petal Width" = "Petal.Width")
                                                     ),
                                                     
                                                     selectInput("selectY",
                                                                 label = "Selection de Y",
                                                                 choices = list("Sepal Length" = "Sepal.Length",
                                                                                "Sepal Width" = "Sepal.Width",
                                                                                "Petal Length" = "Petal.Length",
                                                                                "Petal Width" = "Petal.Width")
                                                     ),
                                                     
                                                     h3("---------------------------------------"),
                                                     
                                                     h3("Filtrer les données"),
                                                     
                                                     selectInput("select_filtre",
                                                                 label = "filtrer selon :",
                                                                 choices = list("Sepal Length" = "Sepal.Length",
                                                                                "Sepal Width" = "Sepal.Width",
                                                                                "Petal Length" = "Petal.Length",
                                                                                "Petal Width" = "Petal.Width")
                                                      ),
                                                     
                                                     uiOutput("slideFiltre"),
                                                     
                                                     actionButton("action",
                                                                  label = "Action"
                                                     )
                                        ),
                            
                            mainPanel(h3("Plot"),
                                      plotOutput("plot"),
                            )
                          )
                 ),
                 
                 # Page "DataSet" ####
                 tabPanel("DataSet",
                          selectInput("select_data",
                                      label = "Selection de l'espèce",
                                      choices = list("Tout" = "tout",
                                                "Versicolor" = "versicolor",
                                                "Virginica" = "virginica",
                                                "Setosa" = "setosa")),
                          DT::dataTableOutput("data")
                 ),
                 
                 # Page "Statistiques" ####
                 tabPanel("Statistiques",
                          sidebarLayout(
                                        sidebarPanel(h3("Selection des variables"),
                                                     selectInput("selectVar",
                                                                 label = "",
                                                                 choices = list("Sepal Length" = "Sepal.Length",
                                                                                "Sepal Width" = "Sepal.Width",
                                                                                "Petal Length" = "Petal.Length",
                                                                                "Petal Width" = "Petal.Width"))),
                                        
                                        mainPanel(h3("Statistiquess"),
                                                  textOutput("stat"))
                          )
                          
                          
                 )
) # Fin de la partie "UI"

# Server ####

server <- function(input, output) {
  data("iris")
  
  # Page "Plot" ####
  
  
  output$slideFiltre <- renderUI({
    sliderInput(
      "filtre",
      label = paste("Filtrer selon ", input$select_filtre),
      min = min(iris[[input$select_filtre]]),
      max = max(iris[[input$select_filtre]]),
      value = c(min,max)
    )
  })
  
  output$textX <- renderText({
    input$selectX
  }) 
  
  output$textY <- renderText({
    input$selectY
  })
  
  output$slide <- renderText({
    input$filtre
  })
  
  output$action <- renderText({
    input$action
  })
  
  # Gestion du boutton action
  action <- eventReactive(input$action, {
    iris_1 <- iris[(iris[[input$select_filtre]] >= input$filtre[1]) & (iris[[input$select_filtre]] <= input$filtre[2]),]
    plot(x = iris_1[[input$selectX]],
         y = iris_1[[input$selectY]],
         main = paste(input$selectX, " / ", input$selectY),
         xlab = input$selectX,
         ylab = input$selectY,
         col = iris_1$Species)
  })
  
  output$plot <- renderPlot({
    action()
  })
  
  
  # Page "Dataset" ####
  output$data <- DT::renderDataTable({
    if (input$select_data == "tout"){
      iris
    } else{
      iris[iris["Species"] == input$select_data,]
    }
      
  })
  
  
  # Page "Statistiques ####
  output$stat <- renderText({
    moyenne <- mean(iris[[input$selectVar]])
    paste("La moyenne de ", input$selectVar, " est de : ", moyenne)
  })
  
  
} # Fin de la fonction "server"

# Run the application 
shinyApp(ui = ui, server = server)
