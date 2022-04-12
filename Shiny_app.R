# Load shiny
library(shiny)
library(magrittr)
library(tidyverse)

# Get data
# Paths to our individual computers
WendyPath <- 'C:/Users/Wendy/OneDrive\ -\ Michigan\ State\ University/GitHub/PhenologyShiny/'
KaraPath <- "/Users/karachristinad/Library/CloudStorage/OneDrive-MichiganStateUniversity/CSS 844/Module 3/PhenologyData/"

# Change Path to your path for the code 
phen<-read.csv(paste0(KaraPath, "CleanedPhenologyData2017to2021.csv"))

# Make simpler data to play with
SimplePlot <- phen %>% 
  group_by(SPECIES, species, Year, Week) %>% 
  summarize(ColorR5 = mean(ColorR5),
            FallR5 = mean(FallR5)) %>% 
  pivot_longer(cols = c(ColorR5, FallR5), 
               names_to = 'Measurement',
               values_to = 'Values')

# Define UI
ui <- pageWithSidebar(
        headerPanel('Phenology'),
        sidebarPanel(width = 4,
             selectInput('SPECIES', 'Choose a species:',paste(unique(SimplePlot$SPECIES))),
             selectInput("Measurement", "Variable:", 
                         c("Leaf Color" = "ColorR5",
                           "Leaf Fall" = "FallR5"))),
             mainPanel(plotOutput("plot")))


# Define server
server <- function(input, output) {
        selectedData <- reactive({
                SimplePlot %>% 
                filter(SPECIES == input$SPECIES,
                       Measurement == input$Measurement)
        })
    
        # formulaText <- reactive({
        #         paste("Values ~ Week")
        # })
        formulaText <- 'Values ~ Week'
        
        output$caption <- renderText({
                formulaText
        })
        
        output$plot <- renderPlot({
                plot(as.formula(formulaText),
                     data=selectedData())
        })
        
}

shinyApp(ui, server)
