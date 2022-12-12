##########################################################################
###
### epa_function.R contains annual_epa, which take state (two-digit FIPs code as a character) 
### and year (integer) and param (a pollutant in c("ozone","so2","co","no2","pm25.frm","pm25","pm10")) and returns both the 
### mean pollutant concentration by county and the highest recorded pollutant by county for the specified year and state. 
###
### Also, the function takes as input the email and key required to use the EPA API
###
##########################################################################

annual_EPA <- function(state, year, email = "vgc@umich.edu", key = "taupekit14", param = c("ozone","so2","co","no2","pm25.frm","pm25","pm10")){
  ### Convert between the specified pollutant and the 
  if(param=="ozone"){param="44201"}
  if(param=="so2"){param="42401"}
  if(param=="co"){param="42101"}
  if(param=="no2"){param="42602"}
  if(param=="pm25.frm"){param="88101"}
  if(param=="pm25"){param="88502"}
  if(param=="pm10"){param="81102"}
  ### Create a URL to scrape the data using the specified email, key, pollutant, year, and state
  URL <- paste0("https://aqs.epa.gov/data/api/annualData/byState?",
                "email=", email,"&",
                "key=", key,"&", 
                "param=", param,"&", 
                ### Begin on January 1 and end on December 31
                "bdate=", year,"0101&",
                "edate=", year,"1231&",
                "state=", state)
  ### Extract the data using the URL 
  df <- data.frame(fromJSON(URL))
  ### Remove the first four columns of the data
  df <- subset(df, select = c(5:ncol(df)))
  ### Remove Data. from all of the column names in the data
  colnames(df) <- gsub("Data.", "", colnames(df), fixed=TRUE)
  ### Group by county, year, and parameter
  new_df <- df %>% 
    group_by(county, year, parameter) %>% 
    ### Find the mean of the pollutant values
    summarize(mean_conc_year = mean(arithmetic_mean),
              ### first_max_value is the same for each pollutant for each year/county/state/pollutant, so no need to summarize
              highest_conc = first_max_value) 
  new_df <- new_df%>%
    select(county, mean_conc_year, highest_conc, year, parameter)
  return(new_df)}
