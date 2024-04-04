# PhenologyShiny
Phenology data and shiny creation for lesson plan

### Wendy Leuenberger, Kara Dobson, Brianna Brown, Harry Shomer, Abby Bryson

------

## Data

[CleanedPhenologyData2017to2023.csv](CleanedPhenologyData2017to2023.csv): This file contains all of the data and is cleaned. This file is created from the [DataManagement.Rmd](DataManagement.Rmd) code for 2017-2022; 2023 data was copied into this file from the raw excel sheet for 2023.

[test_form5_2019_12_19_14_56_01_542496.csv](test_form5_2019_12_19_14_56_01_542496.csv): 2017 data

[Form_BS162_2018_v2_2019_12_19_14_56_54_642030.csv](Form_BS162_2018_v2_2019_12_19_14_56_54_642030.csv): 2018 data

[Form_BS162_2019_2019_12_19_14_57_47_271712.csv](Form_BS162_2019_2019_12_19_14_57_47_271712.csv): 2019 data

2021Fall_TreePhenologyData_All_wPhotoLinks.xlsx: 2021 data. This data is in a different format than the previous years

[weather_data_daymet_newvariablesApr2024.csv](weather_data_daymet_newvariablesApr2024.csv): Weather data from Group 3.

[11928_lat_42.7257_lon_-84.4777_2022-04-18_152926.csv](11928_lat_42.7257_lon_-84.4777_2022-04-18_152926.csv): Initial weather data from Group 3. [weather_data_daymet_newvariablesApr2024.csv](weather_data_daymet_newvariablesApr2024.csv) is the updated version

## Metadata

[WeatherDataCollectionDocument.docx](WeatherDataCollectionDocument.docx): Metadata from Group 3 on how they collected weather data and processed to create some of the columns that we needed

## Code

[DataManagement.Rmd](DataManagement.Rmd): R code to process all four data files and produce [CleanedPhenologyData2017to2023](CleanedPhenologyData2017to2023.csv). Also produces [DataManagement.md](DataManagement.md) (readable on GitHub) and [DataManagement.html](DataManagement.html) (readable in browser) that detail the data cleaning process. [DataManagement_files](DataManagement_files) is also part of this process.

## Shiny

[Shiny_app.R](Shiny_app.R): Code for data management and creates the Shiny. Uses [CleanedPhenologyData2017to2023.csv](CleanedPhenologyData2017to2023.csv) and [weather_data_daymet_newvariablesApr2024.csv](weather_data_daymet_newvariablesApr2024.csv) as inputs

## Lesson plan

[Phenology_Lesson_Plan.docx](Phenology_Lesson_Plan.docx): Lesson plan that works with the Shiny app. Also accessible on the PLB 843 Forum's Google Drive: [https://docs.google.com/document/d/1xHASuwbRc__C6bLMj2aRPokvVurwpWXC9nYD0_PquIA/edit](https://docs.google.com/document/d/1xHASuwbRc__C6bLMj2aRPokvVurwpWXC9nYD0_PquIA/edit)
