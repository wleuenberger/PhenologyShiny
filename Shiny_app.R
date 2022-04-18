# Load shiny
library(shiny)
library(magrittr)
library(tidyverse)

# Standard error function
se <- function(x, na.rm = FALSE){ 
  sqrt(var(x, na.rm = na.rm) / length(x))
}

# Get data
# Paths to our individual computers
WendyPath <- 'C:/Users/Wendy/OneDrive\ -\ Michigan\ State\ University/GitHub/PhenologyShiny/'
KaraPath <- "/Users/karachristinad/Library/CloudStorage/OneDrive-MichiganStateUniversity/CSS 844/Module 3/PhenologyData/"

# Change Path to your path for the code 
phen<-read.csv(paste0(WendyPath, "CleanedPhenologyData2017to2021.csv"))

# Summarize data for manipulation
ColorFallLong <- phen %>% 
  pivot_longer(cols = starts_with(c('Color', 'Fall'), 
                                  ignore.case = FALSE), 
               names_to = 'Metric',
               values_to = 'Values') %>% 
  mutate(ColorFall = str_extract(Metric, 
                                 '[:upper:]{1}[:lower:]{3,4}'),
         Rounding = str_extract(Metric, '[:digit:]{1,2}'),
         Year = factor(Year)) %>% 
  select(SPECIES, species, individual, Year, Month, Day, Week, 
         ColorFall, Rounding, Values) %>% 
  filter(Rounding == 5,
         Week %in% 36:49) 

# At what point did the values reach 50%? If they reached 50%?
ColorFall50 <- ColorFallLong %>% 
  group_by(individual, Year) %>%
  filter(Values >= 50) %>% 
  filter((Values - 50) == min(Values - 50))

# ggplot settings ####
tbw <- theme_bw(base_size = 16)
fw <- facet_wrap(~ ColorFall)

# Test plot outside of shiny
# ggplot(ColorFallLong %>% filter(species == 'ACRU'),
#        aes(x = Week, y = Values, color = factor(Year),
#            fill = factor(Year))) +
#   geom_smooth() +
#   # geom_jitter(alpha = 0.2) +
#   tbw +
#   facet_grid(~ ColorFall)



# Define UI
ui <- pageWithSidebar(
        headerPanel('Phenology'),
        sidebarPanel(width = 4,
             selectInput('SPECIES', 'Choose a species:',paste(unique(SimplePlot$SPECIES)))),
             # selectInput("Measurement", "Variable:", 
             #             c("Leaf Color" = "Color",
             #               "Leaf Fall" = "Fall"))),
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
                ColorFallLong %>% 
                filter(SPECIES == input$SPECIES)#,
                       # ColorFall == input$Measurement)
        })
        selectedData2 <- reactive({
                ColorFall50 %>% 
                        filter(SPECIES == input$SPECIES)#,
                               # ColorFall == input$Measurement)
        })
       
        output$plot <- renderPlot({
                ggplot(selectedData(), aes(x=Week, y=Values, group=Year)) +
                        # geom_point(aes(color=Year)) +
                        geom_smooth(aes(color=Year, fill = Year)) +
                        labs(x="Week of Year", y="Percent of Leaf Color/Fall") +
                        tbw + ylim(0, 100) + xlim(36, 49) +
                        fw
        })
        # this second plot needs some work - currently shows yearly average percent of color
        # should make it so that x = year, y = date when the individual reached 50% color/fall
        output$plot2 <- renderPlot({
                ggplot(selectedData2(), 
                       aes(x=Year, y=Week)) +
                        geom_boxplot() +
                        geom_jitter(aes(color=individual)) +
                        # labs(x="Week", y="Percent of Leaf Color/Fall") +
                        # facet_wrap(~ Year) +
                        tbw + ylim(36, 49) + 
                        fw
        })
}

shinyApp(ui, server)
