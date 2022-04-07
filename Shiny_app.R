# Load shiny
library(shiny)

# Get data
phen<-read.csv("/Users/karachristinad/Library/CloudStorage/OneDrive-MichiganStateUniversity/CSS 844/Module 3/PhenologyData/CleanedPhenologyData2017to2021.csv")

# Define UI
ui <- pageWithSidebar(
        headerPanel('Phenology'),
        sidebarPanel(width = 4,
             selectInput('SPECIES', 'Choose a species:',paste(phen$SPECIES)),
             selectInput("variable", "Variable:", 
                         c("Leaf Color" = "color",
                           "Leaf Fall" = "fall"))),
             mainPanel())

# Define server
server <- function(input, output) {
        formulaText <- reactive({
                paste("temp ~", input$variable)
        })
        
        output$caption <- renderText({
                formulaText()
        })
        
        output$plot <- renderPlot({
                plot(as.formula(formulaText()),
                     data=phen)
        })
        
}
shinyApp(ui, server)
