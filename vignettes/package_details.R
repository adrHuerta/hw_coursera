## ------------------------------------------------------------------------
#cleaning the environment
rm(list = ls()) 

#set working directory with all the data
setwd("G:/accident_data")

#set library
library(hwcoursera)

#reading the 2013 database
name_db <- fars_read("accident_2013.csv")
class(name_db)

# if you want to create another file name and reading a zipped file
name_db_2014 <- make_filename("2014")
name_db <- fars_read(name_db_2014)
class(name_db)

#reading by year
fars_read_years("2015")

#reading and giving summarize information
fars_summarize_years("2015")

#map of the count of accidents in 2015 and the state number 1 
fars_map_state(state.num = "1", year = "2015")

