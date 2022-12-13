# Air Quality and Demographics in California

## Description

This project is intended to showcase the skills learned in Stats 506. It was created during Fall Semester 2022.

Here, we use R, R Shiny, SQL, and Census and EPA APIs to investigate the relationship between air quality and demographics in California.

The data comes from the Census Bureau through the Census API using the censusapi package and the Environmental Protection Agency (EPA) through the EPA API by web scraping. We are also using data from the U.S. House Elections (1976-2020) and the U.S Presidential Elections (2000-2020) from the MIT Election Lab accessed here: https://electionlab.mit.edu/data.

All functions are contained in the src folder. 

We start by creating a function to extract data from the Census API. We will refer to this function as the **population function**. The population function will take in a state and year, and will output a dataframe with the population for each demographic group for each county for the specified state and year.

A second function was created to extract data from the EPA API. We will refer to this function as the **EPA** function. The EPA function will take in a state, pollutant, and year. The following pollutants can be used in the function: ozone, SO2, CO, NO2, PM2.5 (FRM), PM2.5, and PM10. The function will output a dataframe with the average concentration and highest concentration for each county for the specified state, pollutant, and year.

The data frames from the population function and EPA function will be merged using the county variable. We will only focus on West Coast states (CA, OR, WA) for this project. We will focus on the years 2012, 2016, and 2020.

A third function was created to extract data from the U.S. House (1976-2020) csv file. We will refer to this function as the **election function**. The election function will take in a state and year, and output a dataframe with county, party (Democrat, Republican, or Other), candidate votes, total votes, and the vote percentage. This data is found in the data folder. 

DBeaver will be used to build a database that combines all of the cleaned data frames to make it easier to access and extract data.

Finally, R Shiny is used to create an app that will plot the demographic, air quality, and election data in a choropleth. A fourth function was made to create an interactive plot for the R Shiny app. We will refer to this function as the **plot function**. The plot function will take in year, state, email address, and EPA API key. The plot function includes the population function, EPA function, and the election function. The plot function extracts geometry data from the population function then joins the geometry data with EPA pollution data and election data by county. Finally, the plot function outputs the population data, EPA pollution data, and election data as a list.

The Shiny app can be accessed at this link: https://dmxjfw-marley-lund.shinyapps.io/air-quality-demographics-project/


The dplyr package in R is used to extract the data from each API and to clean the data.

The OutdatedFunctions folder contains older version of the scripts contained in the src folder. They are not necessary for the Shiny app or our final deliverable, though are included to show our code progress. 

## Installation

A Census API key must be requested to be able to extract data. A key can be requested at this link: https://api.census.gov/data/key_signup.html. After obtaining a key and loading the tidycensus package in R, the following code can be run:
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY")

The get_acs() function can be used to extract data.

A EPA API key must be requested to be able to extract data. A key can be issued by typing in the following link, replacing myemail@example.com with the user's email: https://aqs.epa.gov/data/api/signup?email=myemail@example.com.


## Authors and acknowledgment

The authors of this project are Abigail Seeger, Jordan Majoros, Marley Lund, and Victoria Chang. 

## Project status

As of December 14, this project is complete. 
