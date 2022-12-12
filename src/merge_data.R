##########################################################################
###
### merge_data.R uses population_func to extract data from WA, CA, and OR
### for 2012, 2016, and 2020
###
##########################################################################

### Load in the necessary packages
source("packages.R")
### Read in population_func from population_function.R and annual_EPA from epa_function.R
source("population_function.R")
source("epa_function.R")

### Call population function to get data for California in 2012, 2016, and 2020
ca1 <- population_func(2012, "CA")
ca2 <- population_func(2016, "CA")
ca3 <- population_func(2020, "CA")
### Combine CA data for 2012, 2016, and 2020
ca <- rbind(ca1,ca2,ca3)
### Create a county variable that does not include state name. Will be used to merge population data with EPA data
ca$county <- substr(ca$NAME,1,nchar(ca$NAME)-18)

### Call population function to get data for Oregon in 2012, 2016, and 2020
or1 <- population_func(2012, "OR")
or2 <- population_func(2016, "CA")
or3 <- population_func(2020, "CA")
### Combine OR data for 2012, 2016, and 2020
or <- rbind(or1, or2,or3)
### Create a county variable that does not include state name. Will be used to merge population data with EPA data
or$county <- substr(or$NAME,1,nchar(or$NAME)-14)

### Call population function to get data for Washington in 2012, 2016, and 2020
wa1 <- population_func(2012, "WA")
wa2 <- population_func(2016, "WA")
wa3 <- population_func(2020, "WA")
wa <- rbind(wa1,wa2,wa3)
### Create a county variable that does not include state name. Will be used to merge population data with EPA data
wa$county <- substr(wa$NAME,1,nchar(wa$NAME)-18)

### Call EPA function to extract data for CA
### Years 2012, 2016, 2020 
### Each pollutant
ca_2012_1 <- annual_EPA(state = "06", year = "2012", param = "ozone")
ca_2012_2 <- annual_EPA(state = "06", year = "2012", param = "so2")
ca_2012_3 <- annual_EPA(state = "06", year = "2012", param = "co")
ca_2012_4 <- annual_EPA(state = "06", year = "2012", param = "no2")
ca_2012_5 <- annual_EPA(state = "06", year = "2012", param = "pm25.frm")
ca_2012_6 <- annual_EPA(state = "06", year = "2012", param = "pm25")
ca_2012_7 <- annual_EPA(state = "06", year = "2012", param = "pm10")
### Combine all pollutants for CA for 2012
ca_2012 <- rbind(ca_2012_1, ca_2012_2, ca_2012_3, ca_2012_4, ca_2012_5, ca_2012_6, ca_2012_7)

ca_2016_1 <- download.annualEPA(state = "06", year = "2016", param = "ozone")
ca_2016_2 <- download.annualEPA(state = "06", year = "2016", param = "so2")
ca_2016_3 <- download.annualEPA(state = "06", year = "2016", param = "co")
ca_2016_4 <- download.annualEPA(state = "06", year = "2016", param = "no2")
ca_2016_5 <- download.annualEPA(state = "06", year = "2016", param = "pm25.frm")
ca_2016_6 <- download.annualEPA(state = "06", year = "2016", param = "pm25")
ca_2016_7 <- download.annualEPA(state = "06", year = "2016", param = "pm10")
### Combine all pollutants for CA for 2016
ca_2016 <- rbind(ca_2016_1, ca_2016_2, ca_2016_3, ca_2016_4, ca_2016_5, ca_2016_6, ca_2016_7)

ca_2020_1 <- download.annualEPA(state = "06", year = "2020", param = "ozone")
ca_2020_2 <- download.annualEPA(state = "06", year = "2020", param = "so2")
ca_2020_3 <- download.annualEPA(state = "06", year = "2020", param = "co")
ca_2020_4 <- download.annualEPA(state = "06", year = "2020", param = "no2")
ca_2020_5 <- download.annualEPA(state = "06", year = "2020", param = "pm25.frm")
ca_2020_6 <- download.annualEPA(state = "06", year = "2020", param = "pm25")
ca_2020_7 <- download.annualEPA(state = "06", year = "2020", param = "pm10")
### Combine all pollutants for CA for 2020
ca_2020 <- rbind(ca_2020_1, ca_2020_2, ca_2020_3, ca_2020_4, ca_2020_5, ca_2020_6, ca_2020_7)
### Combine all of the CA data
ca_epa <- rbind(ca_2012, ca_2016,ca_2020)

### Call EPA function to extract data for OR
### Years 2012, 2016, 2020 
### Each pollutant
or_2012_1 <- download.annualEPA(state = "41", year = "2012", param = "ozone")
or_2012_2 <- download.annualEPA(state = "41", year = "2012", param = "so2")
or_2012_3 <- download.annualEPA(state = "41", year = "2012", param = "co")
or_2012_4 <- download.annualEPA(state = "41", year = "2012", param = "no2")
or_2012_5 <- download.annualEPA(state = "41", year = "2012", param = "pm25.frm")
or_2012_6 <- download.annualEPA(state = "41", year = "2012", param = "pm25")
or_2012_7 <- download.annualEPA(state = "41", year = "2012", param = "pm10")
### Combine all pollutants for OR for 2012
or_2012 <- rbind(or_2012_1, or_2012_2, or_2012_3, or_2012_4, or_2012_5, or_2012_6, or_2012_7)

or_2016_1 <- download.annualEPA(state = "41", year = "2016", param = "ozone")
or_2016_2 <- download.annualEPA(state = "41", year = "2016", param = "so2")
or_2016_3 <- download.annualEPA(state = "41", year = "2016", param = "co")
or_2016_4 <- download.annualEPA(state = "41", year = "2016", param = "no2")
or_2016_5 <- download.annualEPA(state = "41", year = "2016", param = "pm25.frm")
or_2016_6 <- download.annualEPA(state = "41", year = "2016", param = "pm25")
or_2016_7 <- download.annualEPA(state = "41", year = "2016", param = "pm10")
### Combine all pollutants for OR for 2016
or_2016 <- rbind(or_2016_1, or_2016_2, or_2016_3, or_2016_4, or_2016_5, or_2016_6, or_2016_7)

or_2020_1 <- download.annualEPA(state = "41", year = "2020", param = "ozone")
or_2020_2 <- download.annualEPA(state = "41", year = "2020", param = "so2")
or_2020_3 <- download.annualEPA(state = "41", year = "2020", param = "co")
or_2020_4 <- download.annualEPA(state = "41", year = "2020", param = "no2")
or_2020_5 <- download.annualEPA(state = "41", year = "2020", param = "pm25.frm")
or_2020_6 <- download.annualEPA(state = "41", year = "2020", param = "pm25")
or_2020_7 <- download.annualEPA(state = "41", year = "2020", param = "pm10")
### Combine all pollutants for OR for 2020
or_2020 <- rbind(or_2020_1, or_2020_2, or_2020_3, or_2020_4, or_2020_5, or_2020_6, or_2020_7)
### Combine all OR data
or_epa <- rbind(or_2012, or_2016,or_2020)
### Similar process for WA 
wa_2012_1 <- download.annualEPA(state = "53", year = "2012", param = "ozone")
wa_2012_2 <- download.annualEPA(state = "53", year = "2012", param = "so2")
wa_2012_3 <- download.annualEPA(state = "53", year = "2012", param = "co")
wa_2012_4 <- download.annualEPA(state = "53", year = "2012", param = "no2")
wa_2012_5 <- download.annualEPA(state = "53", year = "2012", param = "pm25.frm")
wa_2012_6 <- download.annualEPA(state = "53", year = "2012", param = "pm25")
wa_2012_7 <- download.annualEPA(state = "53", year = "2012", param = "pm10")

wa_2012 <- rbind(wa_2012_1, wa_2012_2, wa_2012_3, wa_2012_4, wa_2012_5, wa_2012_6, wa_2012_7)

wa_2016_1 <- download.annualEPA(state = "53", year = "2016", param = "ozone")
wa_2016_2 <- download.annualEPA(state = "53", year = "2016", param = "so2")
wa_2016_3 <- download.annualEPA(state = "53", year = "2016", param = "co")
wa_2016_4 <- download.annualEPA(state = "53", year = "2016", param = "no2")
wa_2016_5 <- download.annualEPA(state = "53", year = "2016", param = "pm25.frm")
wa_2016_6 <- download.annualEPA(state = "53", year = "2016", param = "pm25")
wa_2016_7 <- download.annualEPA(state = "53", year = "2016", param = "pm10")

wa_2016 <- rbind(wa_2016_1, wa_2016_2, wa_2016_3, wa_2016_4, wa_2016_5, wa_2016_6, wa_2016_7)

wa_2020_1 <- download.annualEPA(state = "53", year = "2020", param = "ozone")
wa_2020_2 <- download.annualEPA(state = "53", year = "2020", param = "so2")
wa_2020_3 <- download.annualEPA(state = "53", year = "2020", param = "co")
wa_2020_4 <- download.annualEPA(state = "53", year = "2020", param = "no2")
wa_2020_5 <- download.annualEPA(state = "53", year = "2020", param = "pm25.frm")
wa_2020_6 <- download.annualEPA(state = "53", year = "2020", param = "pm25")
wa_2020_7 <- download.annualEPA(state = "53", year = "2020", param = "pm10")

wa_2020 <- rbind(wa_2020_1, wa_2020_2, wa_2020_3, wa_2020_4, wa_2020_5, wa_2020_6, wa_2020_7)

wa_epa <- rbind(wa_2012, wa_2016, wa_2020)


### Merge Census and EPA data for each CA
ca <- ca[,-1]
ca$county <- stringr::str_trim(ca$county)
ca_full <- left_join(ca, ca_epa, by = "county")
ca_full2 <- ca_full[,c(1:7,9,10,11)]
colnames(ca_full2)[which(colnames(ca_full2) == "year.x")] <- "year"
### Merge Census and EPA data for each OR
or$county <- stringr::str_trim(or$county)
or_full <- left_join(or[,-1], or_epa, by = "county")
or_full2 <- or_full[,c(1:7,9,10,11)]
colnames(or_full2)[which(colnames(or_full2) == "year.x")] <- "year"
### Merge Census and EPA data for each WA
wa$county <- stringr::str_trim(wa$county)
wa_full <- left_join(wa[,-1], wa_epa, by = "county")
wa_full2 <- wa_full[,c(1:7,9,10,11)]
colnames(wa_full2)[which(colnames(wa_full2) == "year.x")] <- "year"
### Output only distinct rows
ca_full2 <- ca_full2 %>% distinct(.keep_all = TRUE)
or_full2 <- or_full2 %>% distinct(.keep_all = TRUE)
wa_full2 <- wa_full2 %>% distinct(.keep_all = TRUE)


