library(shiny)
library(tidyverse)

ui <- fluidPage(
    titlePanel("561000 Restoranu ir pagaminto valgio teikimo veikla"),
    sidebarLayout(
        sidebarPanel(
            selectizeInput(inputId = "imones_pavadinimas", label = "Imones pavadinimas", choices = NULL, selected = NULL)
        ),
        mainPanel(tabsetPanel(
            tabPanel("Atlyginimo grafikas", plotOutput("plot1")),
            tabPanel("Darbuotoju grafikas", plotOutput("plot2")),
            tabPanel("lentele", tableOutput("table")),
            tabPanel("Mokesciu grafikas", plotOutput("plot3"))
        )
        )
    )
)
server <- function(input, output, session) {
    data <- read_csv("../data/lab_sodra.csv") %>% filter(ecoActCode == "561000")
    
    updateSelectizeInput(session, "imones_pavadinimas", choices = data$name, server = TRUE)
    
    output$table <- renderTable(
        data %>%
            filter(name == input$imones_pavadinimas) , digits = 0
    )
    
    output$plot3 <- renderPlot(
        data %>%
            filter(name == input$imones_pavadinimas) %>%
            ggplot(aes(x = month, y = tax)) +
            geom_line() + geom_point()  
    )
    
    output$plot2 <- renderPlot(
        data %>%
            filter(name == input$imones_pavadinimas) %>%
            ggplot(aes(x = month, y = numInsured)) +
            geom_line() + geom_point()   
    )
    
    output$plot1 <- renderPlot(
        data %>%
            filter(name == input$imones_pavadinimas) %>%
            ggplot(aes(x = month, y = avgWage)) +
            geom_line() + geom_point()    
    )
}
shinyApp(ui, server)