# PhenologyShiny
Phenology data and shiny creation for lesson plan

### Wendy Leuenberger, Kara Dobson, Brianna Brown, Harry Shomer, Abby Bryson

------

## Data

[CleanedPhenologyData2017to2023.csv](/ShinyApp/CleanedPhenologyData2017to2023.csv): This file contains all of the data and is cleaned. This file is created from the [DataManagement.Rmd](DataManagement.Rmd) code for 2017-2022; 2023 data was copied into this file from the raw excel sheet for 2023.  

[weather_data_daymet_newvariablesApr2024.csv](weather_data_daymet_newvariablesApr2024.csv): Complete weather data  

## Metadata

[WeatherDataCollectionDocument.docx](WeatherDataCollectionDocument.docx): Metadata from Group 3 on how they collected weather data and processed to create some of the columns that we needed. For each new year, data is downloaded from DayMet, copied into the previously complete weather data csv, and the functions are applied to create any new columns.  

## Code

This file was used to merge data for 2017-2022. Moving forward, starting with 2023 data, it can be copied at the end of the previously complete data frame.  
[DataManagement.Rmd](DataManagement.Rmd): R code to process all four data files and produce [CleanedPhenologyData2017to2022](CleanedPhenologyData2017to2022.csv). Also produces [DataManagement.md](DataManagement.md) (readable on GitHub) and [DataManagement.html](DataManagement.html) (readable in browser) that detail the data cleaning process. [DataManagement_files](DataManagement_files) is also part of this process.

## Shiny

[Shiny_app.R](Shiny_app.R): Code for data management and creates the Shiny. Uses [CleanedPhenologyData2017to2023.csv](CleanedPhenologyData2017to2023.csv) and [weather_data_daymet_newvariablesApr2024.csv](weather_data_daymet_newvariablesApr2024.csv) as inputs

## Lesson plan

[Phenology_Lesson_Plan.docx](Phenology_Lesson_Plan.docx): Lesson plan that works with the Shiny app. Also accessible on the PLB 843 Forum's Google Drive: [https://docs.google.com/document/d/1xHASuwbRc__C6bLMj2aRPokvVurwpWXC9nYD0_PquIA/edit](https://docs.google.com/document/d/1xHASuwbRc__C6bLMj2aRPokvVurwpWXC9nYD0_PquIA/edit)
