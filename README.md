# Air Quality and Demographics in California

## Description

This project is intended to showcase the skills learned in Stats 506. It was created during Fall Semester 2022. 

Here, we use R, R Shiny, SQL, and Census and EPA APIs to investigate the relationship between air quality and demographics in California. 

The data comes from the Census bureau through the Census API using the [censusapi package](https://cran.r-project.org/web/packages/censusapi/censusapi.pdf) and the Environmental Protection Agency (EPA) through the EPA API using the [RAQSAPI package](https://cran.r-project.org/web/packages/RAQSAPI/vignettes/RAQSAPIvignette.html). 

dplyr is used to extract the data from each API and to clean the data. 

sql is used to build a database that combines all of the cleaned data frames to make it easier to access and extract data.

Finally, R Shiny is used to create a app that will plot the demographic and air quality data in a choropleth. 


## Installation

Possibly include how to install the EPA and Census APIs HERE, and instructions for getting the key for each API. 

## Usage

Possibly include a usage example of the Shiny app HERE. 

## Authors and acknowledgment

The authors of this project are Abigail Seeger, Jordan Majoros, Marley Lund, and Victoria Chang. 

## Project status

As of November 11, 2022, this is a work in progress and is expected to be completed by December 15. 