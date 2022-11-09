# Load shiny
library(shiny)
library(magrittr)
library(tidyverse)
# geosphere is for daylength. Can probably remove once we merge Group 3's data
# library(geosphere)
# ggarrange
library(ggpubr)
# Dates
library(chron)
library(lubridate)
library(rsconnect)

# Standard error function
se <- function(x, na.rm = FALSE){ 
        sqrt(var(x, na.rm = na.rm) / length(x))
}

# Get data
# Paths to our individual computers
WendyPath <- 'C:/Users/Wendy/OneDrive\ -\ Michigan\ State\ University/GitHub/PhenologyShiny/'
WendyPathMSU <- 'C:/Users/leuenbe9/OneDrive\ -\ Michigan\ State\ University/GitHub/PhenologyShiny/'
KaraPath <- "/Users/karachristinad/Library/CloudStorage/OneDrive-MichiganStateUniversity/CSS 844/Module 3/PhenologyData/"

# Change Path to your path for the code 
#phen<-read.csv(paste0(KaraPath, "CleanedPhenologyData2017to2021.csv"))
phen<-read.csv("CleanedPhenologyData2017to2021.csv")
# Phenology data
# phen<-read.csv(paste0(WendyPath, "CleanedPhenologyData2017to2021.csv"))
# phen<-read.csv(paste0(WendyPathMSU, "CleanedPhenologyData2017to2021.csv"))
# Weather data from group 3
#weather <- read.csv(
#  paste0(KaraPath, 
#         #WendyPathMSU,
#         'weather_data_daymet_newvariablesApr20.csv'),
#  skip = 7)
weather<-read.csv('weather_data_daymet_newvariablesApr20.csv',skip = 7)

# # Add daylength data (Removed because it's in the weather data)
# Add month/day to the weather data so it can join the phenology data
# Number of days per year
CumulativeDays <- tibble(year = 2015:2021,
                         DaysinYear = c(365, 366, 365, 365, 365, 
                                        366, 365),
                         AddDays = c(0, 365, 365+366, 365*2+366, 
                                     365*3+366, 365*4+366, 
                                     365*4+366*2))
weather %<>% left_join(CumulativeDays)
# Make Julian days relative to 1/0/2015 (day 1 = 1/1/2015) 
weather %<>% 
        mutate(JDays = yday + AddDays)
# Create a data frame with day, month, and year based on julian days
MDYs <- month.day.year(jul = weather$JDays, 
                       origin. = c(month = 1, day = 0, year = 2015))
MDYs %<>% as.data.frame
MDYs %<>% add_column(yday = weather$yday)
# join dates back to data frame
weather %<>% left_join(MDYs)
weather %<>% 
        rename(Month = month,
               Day = day, 
               Year = year)
phen %<>% left_join(weather)

# Summarize data for manipulation
ColorFallLong <- phen %>% 
        pivot_longer(cols = starts_with(c('Color', 'Fall'), 
                                        ignore.case = FALSE), 
                     names_to = 'Metric',
                     values_to = 'Values') %>% 
        mutate(ColorFall = str_extract(Metric, 
                                       '[:upper:]{1}[:lower:]{3,5}'),
               Rounding = str_extract(Metric, '[:digit:]{1,2}'),
               Year = factor(Year)) %>% 
        select(SPECIES, species, individual, Year, Month, Day, Week, 
               ColorFall, Rounding, Values) %>% 
        filter(Rounding == 5 | is.na(Rounding),
               Week %in% 36:49)

PlotWeatherWeek <- phen %>% 
        mutate(Year = factor(Year)) %>% 
        filter(Week %in% 36:49)
PlotWeatherWeek %<>% 
        mutate(DayHr = dayl..s. / 60 / 60) 
WeekSum <- PlotWeatherWeek %>% 
        group_by(Year, Week, spring_precip) %>% 
        summarize(DayHr = mean(DayHr),
                  Precip_mm = mean(prcp..mm.day.),
                  MeanTemp = mean(mean_temp),
                  GDD = mean(gdd),
                  TempDiff = mean(temp_diff))

# At what point did the values reach 50%? If they reached 50%?
ColorFall50 <- ColorFallLong %>% 
        group_by(SPECIES, species, individual, Year, Week, ColorFall) %>%
        summarize(MeanValue = mean(Values)) %>% 
        ungroup() %>%
        filter(MeanValue >= 50) %>% 
        group_by(SPECIES, species, individual, Year, ColorFall) %>% 
        filter((MeanValue - 50) == min(MeanValue - 50))

# There are a number of pines with >50 color/fall reported
# Take a look
ColorFall50 %>%
        filter(species == 'PIST') %>% 
        select(individual, Week, MeanValue) %>% 
        ungroup %>% 
        select(individual, MeanValue, Year) %>%
        table
# Not very many at all now that we're using the 
# mean for each individual/week/year



# ggplot settings ####
tbw <- theme_bw(base_size = 16)
fw <- facet_grid(ColorFall ~ ., scales = 'free_y')
fwys <- facet_grid(ColorFall ~ .)  # Set scales as the same
gs <- geom_smooth()
gs0.9 <- geom_smooth(span = 0.9)
ytd <- ylab('Temperature differential (max - min)')
ydl <- ylab('Day length (hr)')
ypr <- ylab('Precipitation (mm)')
yt <- ylab('Temperature (Celsius)')
ygdd <- ylab('Growing degree days')
ysp <- ylab('Spring precipitation (mm)')

# removing daylength variable from this dataframe so the first plot is only color + fall faceted
ColorFallLong_nolength <- ColorFallLong[ColorFallLong$ColorFall != "Length", ]

# # Test plot outside of shiny
# # Daily data
# ggplot(PlotWeatherWeek, 
#        aes(x = Week, y = temp_diff, color = Year, fill = Year)) +
#   gs + tbw + ytd
# ggplot(PlotWeatherWeek, 
#        aes(x = Week, y = DayHr, color = Year, fill = Year)) +
#   gs + tbw + ydl
# ggplot(PlotWeatherWeek,
#        aes(x = Week, y = prcp..mm.day., color = Year, fill = Year)) + 
#   gs + tbw + ypr
# ggplot(PlotWeatherWeek,
#        aes(x = Week, y = mean_temp, color = Year, fill = Year)) + 
#   gs + tbw + yt
# ggplot(PlotWeatherWeek,
#        aes(x = Week, y = gdd, color = Year, fill = Year)) + 
#   gs + tbw + ygdd
# 
# # Weekly means
# ggplot(WeekSum,
#        aes(x = Week, y = TempDiff, color = Year, fill = Year)) +
#   gs0.9 + tbw + ytd
# ggplot(WeekSum,
#        aes(x = Week, y = DayHr, color = Year, fill = Year)) +
#   gs + tbw + ydl
# ggplot(WeekSum,
#        aes(x = Week, y = Precip_mm, color = Year, fill = Year)) +
#   gs0.9 + tbw + ypr
# ggplot(WeekSum,
#        aes(x = Week, y = MeanTemp, color = Year, fill = Year)) +
#   gs + tbw + yt
# ggplot(WeekSum,
#        aes(x = Week, y = GDD, color = Year, fill = Year)) +
#   gs + tbw + ygdd
# ggplot(WeekSum,
#        aes(x = Year, y = spring_precip, color = Year, fill = Year)) +
#   geom_bar(stat = 'identity') + tbw + ysp

# wide to long for selecting an input & fixing weather var names & removing spring precip from this dataframe
WeekSum_long <- WeekSum %>%
        gather(key=weather_var, value=value, -Year, -Week)
WeekSum_long$weather_var[WeekSum_long$weather_var == "DayHr"] <- "Daylight length (hours)"
WeekSum_long$weather_var[WeekSum_long$weather_var == "GDD"] <- "Growing degree days"
WeekSum_long$weather_var[WeekSum_long$weather_var == "MeanTemp"] <- "Mean temperature (°C)"
WeekSum_long$weather_var[WeekSum_long$weather_var == "TempDiff"] <- "Temperature differential (max temp - min temp)"
WeekSum_long$weather_var[WeekSum_long$weather_var == "Precip_mm"] <- "Precipitation (mm)"
WeekSum_long2 <- WeekSum_long[!grepl("spring_precip",WeekSum_long$weather_var),]

# Within year among species
# ggplot(ColorFallLong %>% filter(Year == 2018),
#        aes(x = Week, y = Values, group = SPECIES)) +
#   geom_smooth(aes(color = SPECIES, fill = SPECIES)) +
#   labs(x="Week of Year", y="Percent of Leaf Color/Fall") +
#   tbw + ylim(-5, 105) + #xlim(36, 49) +
#   fwys
# # Just oaks and maples
# ggplot(ColorFallLong %>% 
#          filter(Year == 2018, 
#                 species %in% c('ACRU', 'ACSA', 'QUAL', 'QURU')),
#        aes(x = Week, y = Values, group = SPECIES)) +
#   geom_smooth(aes(color = SPECIES, fill = SPECIES)) +
#   labs(x="Week of Year", y="Percent of Leaf Color/Fall") +
#   tbw + ylim(-5, 105) + #xlim(36, 49) +
#   fwys

# Box plots for within year among species
ggplot(ColorFall50 %>% filter(Year == 2021),
       aes(x = species, y = Week, group = SPECIES)) +
        geom_boxplot() +
        geom_jitter() +
        labs(y="Week when tree reaches 50% leaf change", x = 'Species') +
        # facet_wrap(~ Year) +
        tbw + #ylim(36, 49) + 
        fw 

# Table with scientific names and abbreviations
ColorFall50 %>% 
        ungroup %>% 
        select(species, SPECIES) %>% 
        distinct %>% 
        rename('Abbreviation' = 'species',
               'ScientificName' = 'SPECIES')

# To have unique dropdowns per tab, the inputs need to be unique columns for each tab
# Duplicating the needed columns so that the dropdowns work for each tab
ColorFallLong$SPECIES2 <- ColorFallLong$SPECIES
ColorFallLong$SPECIES3 <- ColorFallLong$SPECIES
ColorFallLong$SPECIES4 <- ColorFallLong$SPECIES
ColorFallLong$Year2 <- ColorFallLong$Year
ColorFallLong$Year3 <- ColorFallLong$Year

# Define UI
ui <- pageWithSidebar(
        headerPanel('Phenology'),
        sidebarPanel(width = 4,
                     conditionalPanel(condition = 'input.tabselected==1',
                                      selectInput('SPECIES', 'Choose a species:',paste(unique(ColorFallLong$SPECIES)) %>% sort)),
                     conditionalPanel(condition = 'input.tabselected==2',
                                      selectInput('SPECIES2', 'Choose a species:',paste(unique(ColorFallLong$SPECIES2)) %>% sort)),
                     conditionalPanel(condition = 'input.tabselected==5',
                                      selectInput('Year3', 'Choose a year:',paste(unique(ColorFallLong$Year3)) %>% sort)),
                     conditionalPanel(condition = 'input.tabselected==4',
                                      selectInput('Year', 'Choose a year:',paste(unique(ColorFallLong$Year)) %>% sort)),
                     conditionalPanel(condition = 'input.tabselected==3',
                                      selectInput('Year2', 'Choose a year:',paste(unique(ColorFallLong$Year2)) %>% sort)),
                     conditionalPanel(condition = 'input.tabselected==6',
                                      selectInput('SPECIES3', 'Choose a species:',paste(unique(ColorFallLong$SPECIES3)) %>% sort),
                                      selectInput('weather_var', 'Choose a weather variable:',paste(unique(WeekSum_long2$weather_var)) %>% sort)),
                     conditionalPanel(condition = 'input.tabselected==7',
                                      selectInput('SPECIES4', 'Choose a species:',paste(unique(ColorFallLong$SPECIES4)) %>% sort))),
        mainPanel(type="tabs",
                  tabsetPanel(
                          tabPanel("Yearly variation for one species",
                                   value = 1,
                                   helpText(""),
                                   plotOutput("plot")),
                          tabPanel("Within-species individual yearly variation",
                                   value = 2,
                                   helpText(""),
                                   plotOutput("plot2")),
                          tabPanel("Variation between maples and oaks by year",
                                   value = 3,
                                   helpText(""),
                                   plotOutput("plot6")),
                          tabPanel("Variation among species by year - regression",
                                   value = 4,
                                   helpText(""),
                                   plotOutput("plot5")),
                          tabPanel("Variation among species by year - boxplot",
                                   value = 5,
                                   helpText(""),
                                   plotOutput("plot7"),
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                   tableOutput("table1")),
                          tabPanel("Phenology and fall weather",
                                   value = 6,
                                   helpText(""),
                                   plotOutput("plot3")),
                          tabPanel("Phenology and spring precipitation",
                                   value = 7,
                                   helpText(""),
                                   plotOutput("plot4")),
                          id = "tabselected"
                  )
        )
)


# Define server
server <- function(input, output) {
        selectedData <- reactive({
                ColorFallLong_nolength %>% 
                        filter(SPECIES == input$SPECIES)
        })
        selectedData2 <- reactive({
                ColorFall50 %>% 
                        filter(SPECIES == input$SPECIES2)
        })
        selectedData3 <- reactive({
                ColorFallLong %>% 
                        filter(SPECIES == input$SPECIES3)
        })
        selectedData4 <- reactive({
                ColorFallLong %>% 
                        filter(SPECIES == input$SPECIES3) %>%
                        filter(ColorFall == "Color")
        })
        selectedData5 <- reactive({
                ColorFallLong %>% 
                        filter(SPECIES == input$SPECIES3) %>%
                        filter(ColorFall == "Fall")
        })
        
        selectedData6 <- reactive({
                WeekSum_long2 %>% 
                        filter(weather_var == input$weather_var)
        })
        
        selectedData7 <- reactive({
                ColorFallLong %>% 
                        filter(SPECIES == input$SPECIES4) %>%
                        filter(ColorFall == "Color")
        })
        selectedData8 <- reactive({
                ColorFallLong %>% 
                        filter(SPECIES == input$SPECIES4) %>%
                        filter(ColorFall == "Fall")
        })
        
        selectedData9 <- reactive({
                WeekSum_long %>% 
                        filter(weather_var == "spring_precip")
        })
        
        selectedData10 <- reactive({
                ColorFallLong %>% 
                        filter(Year == input$Year)
        })
        
        selectedData11 <- reactive({
                ColorFallLong %>% 
                        filter(Year == input$Year2)
        })
        
        selectedData12 <- reactive({
                ColorFall50 %>% 
                        filter(Year == input$Year3)
        })
        
        output$plot <- renderPlot({
                ggplot(selectedData(), aes(x=Week, y=Values, group=Year)) +
                        # geom_point(aes(color=Year)) +
                        geom_smooth(aes(color=Year, fill = Year)) +
                        labs(x="Week of Year", y="Percent of Leaf Color/Fall") +
                        tbw + ylim(-5, 105) + #xlim(36, 49) +
                        fwys
                #geom_point(data = ll, aes(x = Week, y = Values), alpha = 0)
        }, height=500)
        
        output$plot2 <- renderPlot({
                ggplot(selectedData2(), 
                       aes(x=Year, y=Week)) +
                        geom_boxplot() +
                        geom_jitter(aes(color=individual)) +
                        labs(color="Individual tree ID") +
                        labs(y="Week when tree reaches 50% leaf change") +
                        # facet_wrap(~ Year) +
                        tbw + #ylim(36, 49) + 
                        fw
        })
        
        output$plot3 <- renderPlot({
                color <- ggplot(selectedData4(), aes(x=Week, y=Values, group=Year)) +
                        # geom_point(aes(color=Year)) +
                        geom_smooth(aes(color=Year, fill = Year)) +
                        labs(x=NULL,y=NULL,title="Color") +
                        tbw + ylim(-5, 105) #+ xlim(36, 49)
                #geom_point(data = ll, aes(x = Week, y = Values), alpha = 0)
                
                fall <- ggplot(selectedData5(), aes(x=Week, y=Values, group=Year)) +
                        # geom_point(aes(color=Year)) +
                        geom_smooth(aes(color=Year, fill = Year)) +
                        labs(x=NULL,y=NULL,title="Fall") +
                        tbw + ylim(-5, 105)# + xlim(36, 49)
                #geom_point(data = ll, aes(x = Week, y = Values), alpha = 0)
                
                weather <- ggplot(selectedData6(), aes(x=Week, y=value, group=Year)) +
                        # geom_point(aes(color=Year)) +
                        geom_smooth(aes(color=Year, fill = Year)) +
                        labs(x=NULL,y=NULL,title="Selected Weather Variable") +
                        tbw #ylim(0, 100) + xlim(36, 49)
                
                annotate_figure(ggarrange(color, fall, weather,
                                          ncol = 1, common.legend = T, legend="right"),
                                left = text_grob("Percent of Leaf Color/Fall", color = "black", rot = 90, size=20),
                                bottom = text_grob("Week of Year", color = "black", size=20))
                
        }, height=700)
        
        
        output$plot4 <- renderPlot({
                color <- ggplot(selectedData7(), aes(x=Week, y=Values, group=Year)) +
                        # geom_point(aes(color=Year)) +
                        geom_smooth(aes(color=Year, fill = Year)) +
                        labs(x=NULL,y=NULL,title="Color") +
                        tbw + ylim(-5, 105) #+ xlim(36, 49)
                #geom_point(data = ll, aes(x = Week, y = Values), alpha = 0)
                
                fall <- ggplot(selectedData8(), aes(x=Week, y=Values, group=Year)) +
                        # geom_point(aes(color=Year)) +
                        geom_smooth(aes(color=Year, fill = Year)) +
                        labs(x=NULL,y=NULL,title="Fall") +
                        tbw + ylim(-5, 105)# + xlim(36, 49)
                #geom_point(data = ll, aes(x = Week, y = Values), alpha = 0)
                
                weather <- ggplot(selectedData9(), aes(x=Year, y=value, color=Year, fill=Year)) +
                        # geom_point(aes(color=Year)) +
                        geom_bar(stat="identity") +
                        labs(x=NULL,y=NULL,title="Spring precipitation (mm)") +
                        tbw #ylim(0, 100) + xlim(36, 49)
                
                annotate_figure(ggarrange(color, fall, weather,
                                          ncol = 1, common.legend = T, legend="right"),
                                left = text_grob("Percent of Leaf Color/Fall", color = "black", rot = 90, size=20),
                                bottom = text_grob("Week of Year", color = "black", size=20))
                
        }, height=700)
        
        output$plot5 <- renderPlot({
                ggplot(selectedData10(),
                       aes(x=Week, y=Values, group=SPECIES)) +
                        geom_smooth(aes(color=SPECIES, fill = SPECIES)) +
                        labs(x="Week of Year", y="Percent of Leaf Color/Fall") +
                        tbw + ylim(-5, 105) +
                        fwys
        }, height=500)
        
        output$plot6 <- renderPlot({
                ggplot(selectedData11() %>% 
                               filter(species %in% c('ACRU', 'ACSA', 'QUAL', 'QURU')),
                       aes(x=Week, y=Values, group=SPECIES)) +
                        geom_smooth(aes(color=SPECIES, fill = SPECIES)) +
                        labs(x="Week of Year", y="Percent of Leaf Color/Fall") +
                        tbw + ylim(-5, 105) +
                        fwys
        }, height=500)
        
        output$plot7 <- renderPlot({
                ggplot(selectedData12(),
                       aes(x = species, y = Week, group = SPECIES)) +
                        geom_boxplot() +
                        geom_jitter() +
                        labs(y="Week when tree reaches 50% leaf change", x = 'Species') +
                        # facet_wrap(~ Year) +
                        tbw + #ylim(36, 49) + 
                        fw
        }, height=500)
        
        output$table1 <- renderTable({
                ColorFall50 %>% 
                        ungroup %>% 
                        select(species, SPECIES) %>% 
                        distinct %>% 
                        rename('Abbreviation' = 'species',
                               'ScientificName' = 'SPECIES')
        })
}

shinyApp(ui, server)



# Old version
# Define UI
#ui <- pageWithSidebar(
#        headerPanel('Phenology'),
#        sidebarPanel(width = 4,
#                selectInput('SPECIES', 'Choose a species:',paste(unique(ColorFallLong$SPECIES)) %>% sort),
#                selectInput('weather_var', 'Choose a weather variable (for tab 3):',paste(unique(WeekSum_long2$weather_var)) %>% sort),
#                selectInput('Year', 'Choose a year (for tab 5):',paste(unique(ColorFallLong$Year)) %>% sort)),
#             # selectInput("Measurement", "Variable:",
#             #             c("Leaf Color" = "Color",
#             #               "Leaf Fall" = "Fall"))),
#             mainPanel(type="tabs",
#                       tabsetPanel(
#                               tabPanel("Yearly variation for one species",
#                                        helpText(""),
#                                        plotOutput("plot")),
#                               tabPanel("Within-species individual yearly variation",
#                                        helpText(""),
#                                        plotOutput("plot2")),
#                               tabPanel("Phenology and fall weather",
#                                        helpText(""),
#                                        plotOutput("plot3")),
#                               tabPanel("Phenology and spring precipitation",
#                                        helpText(""),
#                                        plotOutput("plot4")),
#                               tabPanel("Variation between species by year",
#                                        helpText(""),
#                                        plotOutput("plot5"))
#                               )
#                       )
#        )
#
#
## Define server
#server <- function(input, output) {
#        selectedData <- reactive({
#                ColorFallLong_nolength %>% 
#                filter(SPECIES == input$SPECIES)#,
#                       # ColorFall == input$Measurement)
#        })
#        selectedData2 <- reactive({
#                ColorFall50 %>% 
#                        filter(SPECIES == input$SPECIES)#,
#                               # ColorFall == input$Measurement)
#        })
#        selectedData3 <- reactive({
#                ColorFallLong %>% 
#                        filter(SPECIES == input$SPECIES)#,
#                # ColorFall == input$Measurement)
#        })
#        selectedData4 <- reactive({
#                ColorFallLong %>% 
#                        filter(SPECIES == input$SPECIES) %>%
#                        filter(ColorFall == "Color") #,
#                # ColorFall == input$Measurement)
#        })
#        selectedData5 <- reactive({
#                ColorFallLong %>% 
#                        filter(SPECIES == input$SPECIES) %>%
#                        filter(ColorFall == "Fall") #,
#                # ColorFall == input$Measurement)
#        })
#        
#        selectedData6 <- reactive({
#                WeekSum_long2 %>% 
#                        filter(weather_var == input$weather_var)#,
#                # ColorFall == input$Measurement)
#        })
#        
#        selectedData7 <- reactive({
#                WeekSum_long %>% 
#                        filter(weather_var == "spring_precip")#,
#                # ColorFall == input$Measurement)
#        })
#        
#        selectedData8 <- reactive({
#                ColorFallLong %>% 
#                        filter(Year == input$Year)
#        })
#        
#        output$plot <- renderPlot({
#                ggplot(selectedData(), aes(x=Week, y=Values, group=Year)) +
#                        # geom_point(aes(color=Year)) +
#                        geom_smooth(aes(color=Year, fill = Year)) +
#                        labs(x="Week of Year", y="Percent of Leaf Color/Fall") +
#                        tbw + ylim(-5, 105) + #xlim(36, 49) +
#                        fwys
#            #geom_point(data = ll, aes(x = Week, y = Values), alpha = 0)
#        }, height=500)
#        
#        output$plot2 <- renderPlot({
#                ggplot(selectedData2(), 
#                       aes(x=Year, y=Week)) +
#                        geom_boxplot() +
#                        geom_jitter(aes(color=individual)) +
#                        labs(color="Individual tree ID") +
#                        labs(y="Week when tree reaches 50% leaf change") +
#                        # facet_wrap(~ Year) +
#                        tbw + #ylim(36, 49) + 
#                        fw
#        })
#        
#        # attempted to make a function w/ variable input, but can't subset reactives
#        #third_tab <- function(var) {
#        #        third_plot <- subset(selectedData3(), ColorFall == var)
#        #        return(ggplot(third_plot, aes(x=Week, y=Values, group=Year)) +
#        #                       # geom_point(aes(color=Year)) +
#        #                       geom_smooth(aes(color=Year, fill = Year)) +
#        #                       labs(x="Week of Year", y="Percent of Leaf Color/Fall") +
#        #                       tbw
#        #               ) #ylim(0, 100) + xlim(36, 49))
#        #}
#        #
#        #color <- renderPlot({third_tab("Color")})
#        #fall <- renderPlot({third_tab("Fall")})
#        #length <- renderPlot({third_tab("Length")}
#        
#        output$plot3 <- renderPlot({
#                color <- ggplot(selectedData4(), aes(x=Week, y=Values, group=Year)) +
#                        # geom_point(aes(color=Year)) +
#                        geom_smooth(aes(color=Year, fill = Year)) +
#                        labs(x=NULL,y=NULL,title="Color") +
#                        tbw + ylim(-5, 105) #+ xlim(36, 49)
#                        #geom_point(data = ll, aes(x = Week, y = Values), alpha = 0)
#
#                fall <- ggplot(selectedData5(), aes(x=Week, y=Values, group=Year)) +
#                        # geom_point(aes(color=Year)) +
#                        geom_smooth(aes(color=Year, fill = Year)) +
#                        labs(x=NULL,y=NULL,title="Fall") +
#                        tbw + ylim(-5, 105)# + xlim(36, 49)
#                        #geom_point(data = ll, aes(x = Week, y = Values), alpha = 0)
#
#                weather <- ggplot(selectedData6(), aes(x=Week, y=value, group=Year)) +
#                        # geom_point(aes(color=Year)) +
#                        geom_smooth(aes(color=Year, fill = Year)) +
#                        labs(x=NULL,y=NULL,title="Selected Weather Variable") +
#                        tbw #ylim(0, 100) + xlim(36, 49)
#
#                annotate_figure(ggarrange(color, fall, weather,
#                                        ncol = 1, common.legend = T, legend="right"),
#                                left = text_grob("Percent of Leaf Color/Fall", color = "black", rot = 90, size=20),
#                                bottom = text_grob("Week of Year", color = "black", size=20))
#                
#                }, height=700)
#        
#        
#        output$plot4 <- renderPlot({
#                color <- ggplot(selectedData4(), aes(x=Week, y=Values, group=Year)) +
#                        # geom_point(aes(color=Year)) +
#                        geom_smooth(aes(color=Year, fill = Year)) +
#                        labs(x=NULL,y=NULL,title="Color") +
#                        tbw + ylim(-5, 105) #+ xlim(36, 49)
#                #geom_point(data = ll, aes(x = Week, y = Values), alpha = 0)
#                
#                fall <- ggplot(selectedData5(), aes(x=Week, y=Values, group=Year)) +
#                        # geom_point(aes(color=Year)) +
#                        geom_smooth(aes(color=Year, fill = Year)) +
#                        labs(x=NULL,y=NULL,title="Fall") +
#                        tbw + ylim(-5, 105)# + xlim(36, 49)
#                #geom_point(data = ll, aes(x = Week, y = Values), alpha = 0)
#                
#                weather <- ggplot(selectedData7(), aes(x=Year, y=value, color=Year, fill=Year)) +
#                        # geom_point(aes(color=Year)) +
#                        geom_bar(stat="identity") +
#                        labs(x=NULL,y=NULL,title="Spring precipitation (mm)") +
#                        tbw #ylim(0, 100) + xlim(36, 49)
#
#                annotate_figure(ggarrange(color, fall, weather,
#                                          ncol = 1, common.legend = T, legend="right"),
#                                left = text_grob("Percent of Leaf Color/Fall", color = "black", rot = 90, size=20),
#                                bottom = text_grob("Week of Year", color = "black", size=20))
#                
#        }, height=700)
#        
#        output$plot5 <- renderPlot({
#                ggplot(selectedData8(),
#                       #selectedData8() %>% 
#                       #        filter(species %in% c('ACRU', 'ACSA', 'QUAL', 'QURU')),
#                       aes(x=Week, y=Values, group=SPECIES)) +
#                        geom_smooth(aes(color=SPECIES, fill = SPECIES)) +
#                        labs(x="Week of Year", y="Percent of Leaf Color/Fall") +
#                        tbw + ylim(-5, 105) +
#                        fwys
#        }, height=500)
#        
#
#        # old fig 3
#        #output$plot3 <- renderPlot({
#        #        ggplot(selectedData3(), aes(x=Week, y=Values, group=Year)) +
#        #                # geom_point(aes(color=Year)) +
#        #                geom_smooth(aes(color=Year, fill = Year)) +
#        #                labs(x="Week of Year", y="Percent of Leaf Color/Fall") +
#        #                tbw + #ylim(0, 100) + xlim(36, 49) +
#        #                fw
#        #        #geom_point(data = ll, aes(x = Week, y = Values), alpha = 0)
#        #}, height=500)
#}
#
#shinyApp(ui, server)
#
