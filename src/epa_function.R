annual_EPA <- function(state, year, param = c("ozone","so2","co","no2","pm25.frm","pm25","pm10")){
  if(param=="ozone"){param="44201"}
  if(param=="so2"){param="42401"}
  if(param=="co"){param="42101"}
  if(param=="no2"){param="42602"}
  if(param=="pm25.frm"){param="88101"}
  if(param=="pm25"){param="88502"}
  if(param=="pm10"){param="81102"}
  
  # For annual data, only the year portion of the bdate and edate are used and only whole years of data are returned. For example, bdate = 20171231 and edate = 20180101 will return full data for 2017 and 2018
  
  URL <- paste0("https://aqs.epa.gov/data/api/annualData/byState?",
                "email=", "vgc@umich.edu","&",
                "key=", "taupekit14","&", 
                "param=", param,"&", 
                "bdate=", year,"0101&",
                "edate=", year,"1231&",
                "state=", state)
  print(URL)
  df <- data.frame(fromJSON(URL))
  df <- subset(df, select = c(5:ncol(df)))
  colnames(df) <- gsub("Data.", "", colnames(df), fixed=TRUE)
  
  # Arithmetic mean variable - The measure of central tendency obtained from the sum of the observed pollutant data values in the yearly data set divided by the number of values that comprise the sum for the yearly data set.
  
  # First Max Value - The highest value for the year.
  
  new_df <- df %>% 
    group_by(county, year, parameter) %>% 
    summarize(mean_conc_year = mean(arithmetic_mean),
              highest_conc = first_max_value) 
  head(new_df)
  
  new_df <- new_df %>% select(county, mean_conc_year, highest_conc, year, parameter)
  
  return(new_df)}
