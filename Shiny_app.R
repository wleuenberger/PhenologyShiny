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
SimplePlot$Year <- as.factor(SimplePlot$Year)

SimplePlot2 <- phen %>% # for within species variation
        group_by(SPECIES, individual, Year) %>% 
        summarize(ColorR5 = mean(ColorR5),
                  FallR5 = mean(FallR5)) %>% 
        pivot_longer(cols = c(ColorR5, FallR5), 
                     names_to = 'Measurement',
                     values_to = 'Values')
SimplePlot2$Year <- as.factor(SimplePlot2$Year)

# Define UI
ui <- pageWithSidebar(
        headerPanel('Phenology'),
        sidebarPanel(width = 4,
             selectInput('SPECIES', 'Choose a species:',paste(unique(SimplePlot$SPECIES))),
             selectInput("Measurement", "Variable:", 
                         c("Leaf Color" = "ColorR5",
                           "Leaf Fall" = "FallR5"))),
             mainPanel(type="tabs",
                       tabsetPanel(
                               tabPanel("Yearly variation - one species",
                                        helpText(""),
                                        plotOutput("plot")),
                               tabPanel("Within species variation",
                                        helpText(""),
                                        plotOutput("plot2"))
                               )
                       )
        )


# Define server
server <- function(input, output) {
        selectedData <- reactive({
                SimplePlot %>% 
                filter(SPECIES == input$SPECIES,
                       Measurement == input$Measurement)
        })
        selectedData2 <- reactive({
                SimplePlot2 %>% 
                        filter(SPECIES == input$SPECIES,
                               Measurement == input$Measurement)
        })
       
        formulaText <- 'Values ~ Week' #KD: do we need this anymore?
        
        output$caption <- renderText({ #KD: also not sure we need this, not sure if it even works anyways
                formulaText
        })
        
        #output$plot <- renderPlot({
        #        plot(as.formula(formulaText),
        #             data=selectedData())
        #})
        
        output$plot <- renderPlot({
                ggplot(selectedData(), aes(x=Week, y=Values, group=Year)) +
                        geom_point(aes(color=Year)) +
                        geom_line(aes(color=Year)) +
                        labs(x="Week of Year", y="Percent of Leaf Color/Fall") +
                        theme_bw()
        })
        # this second plot needs some work - currently shows yearly average percent of color
        # should make it so that x = year, y = date when the individual reached 50% color/fall
        output$plot2 <- renderPlot({
                ggplot(selectedData2(), aes(x=Year, y=Values, group=individual)) +
                        geom_point(aes(color=individual)) +
                        geom_line(aes(color=individual)) +
                        labs(x="Year", y="Percent of Leaf Color/Fall") +
                        theme_bw()
        })
}

shinyApp(ui, server)
