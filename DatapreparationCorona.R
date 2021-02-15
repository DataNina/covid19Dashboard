
########################################## Datapreparation for Covid-19 Dashboard###################################

#####Settings----

#Install packages if necessary

#Load necessary packages
requiredPackges <- c("purrr", "plyr", "dplyr", "reshape2",
                     "lubridate", "wbstats", "stringdist",
                     "tidyverse", "zoo")
suppressMessages(suppressWarnings(invisible(lapply(requiredPackges, library, character.only = TRUE))))


start_time <- Sys.time()


##### Loading and preparation of covid data ----

#List URL
#confirmed
confirmed <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"

#deaths
death <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"

#recovered
recovered <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"


# Extract the data from the webpage and read it as dataframe

#List Filenames
lstFiles <- list("confirmed_global", "deaths_global", "recovered_global")



scrape_csv <- function(lstFiles){
  #create urls
  covid.urls <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_",lstFiles,".csv")
  #read in file
  df <- read_csv( as.character(covid.urls) )
  #add category name as separate column
  df$category <- lstFiles
  return(df)
}


dataframe_total <- lstFiles %>% map_df(scrape_csv)
#Change column names
dataframe_total <- dataframe_total %>% setNames(gsub("/", "_", colnames(dataframe_total)))

##### Loading of population data ----
popData  <- wb_data("SP.POP.TOTL") %>% select(country,date,TotalPop=SP.POP.TOTL) %>% filter(date==2019)


#Aggregate the dataframe by "Country_Region" and "category"
dataframe_aggregated <- dataframe_total %>% 
  group_by(Country_Region, category) %>% 
  summarise_if(is.numeric, list(sum))



#Add new category "activeCases" (acTiveCases=confirmed_global-recovered_global-deaths-global)

##Generate subset of the dataframe for each country in order to save the results of ActiveCases
dump <- dataframe_aggregated[dataframe_aggregated$category=="confirmed_global",]
dump$category="activeCases"
dataframe_aggregated=rbind(dataframe_aggregated, dump)

uniqueCountries <- (unique(dataframe_aggregated$Country_Region))

for (i in 1:length(uniqueCountries)) {
  dataframe_aggregated[dataframe_aggregated$Country_Region==uniqueCountries[i]&dataframe_aggregated$category=="activeCases",
  3:ncol(dataframe_aggregated)]=dataframe_aggregated[dataframe_aggregated$Country_Region==uniqueCountries[i]&dataframe_aggregated$category=="confirmed_global",
  3:ncol(dataframe_aggregated)]-dataframe_aggregated[dataframe_aggregated$Country_Region==uniqueCountries[i]&dataframe_aggregated$category=="recovered_global",
  3:ncol(dataframe_aggregated)]-dataframe_aggregated[dataframe_aggregated$Country_Region==uniqueCountries[i]&dataframe_aggregated$category=="deaths_global",3:ncol(dataframe_aggregated)]
}


#Reshape the dataframe from wide to long, keeping the date as additional variable
dataframe_long=melt(data=dataframe_aggregated,id.vars=c("Country_Region","category"),measure.vars=5:ncol(dataframe_aggregated))

#Change column names
dataframe_long <- setNames(dataframe_long, c("country", "category", "date", "number")) 


#Convert date to datetime-format
dataframe_long$date <-strptime(dataframe_long$date,format="%m_%d_%y")
dataframe_long$date <-as.Date(dataframe_long$date,format="%d-%m-%Y")



##### Prepare merging of  covid and population data ----
#Identify countries in covid data which have no corresponding country in population data
countries <- uniqueCountries
popCountries <- unique(popData$country)
onlyCovidCoun <- countries[(!countries %in% popCountries)==TRUE] 


#Check for different writings of countries only present in covid data (onlyCovidCoun) 

#and search for the most likely corresponding country in population data using the jaro winkler similarity
#The Jaro algorithm is a measure of characters in common, being no more than half the length of the longer string in distance, 
#with consideration for transpositions. Differences near the start of the string are more significant than differences near the end of the string.
lstDist <- map(onlyCovidCoun, function(covidCountries) {1-(stringdist(covidCountries, popCountries, method="jw"))}) %>% setNames(onlyCovidCoun)
greatestOverlap <- map(lstDist, function(distance){which(distance==max(distance))})
equivalent <- map(greatestOverlap, function(overlap) {popCountries[overlap]})

#Manually searching for a match for those who don't give a reasonable matching partner by the record linkage method 
##Burma=Myranmar
##Congo (Brazzaville)=Cong, Rep.
##Congo (Kinshasa)=Congo, Dem. Rep.
##Gambia=Gambia, The
##Iran=Iran, Islamic Rep.
##Korea, South=Korea, Dem. People's Rep.
##Kyrgyzstan=Kyrgyz Republic
##Slovakia=Slovak, Republic
#Syria=Syrian Arab Republic
#US=United States


##Only Keep record linkage match
equivalentRL <- within(equivalent, rm("Burma", "Congo (Brazzaville)", "Congo (Kinshasa)", "Gambia", "Iran", "Korea, South", "Kyrgyzstan", "Slovakia",
                                      "Syria", "US", "Diamond Princess", "MS Zaandam", "Holy See", "Taiwan*"))
#Prepare dataframe with countries
countryDiff <- equivalentRL %>% flatten_dfc() %>% t() %>% as.data.frame() %>% rownames_to_column() %>% setNames(c("covidCountries", "popCountries"))


#Convert manually identified matches into dataframe
countryDiffManually <- data.frame(covidCountries=c("Burma", "Congo(Brazzaville)", "Congo(Kinshasa)","Gambia","Iran","Korea, South", "Kyrgyzstan","Slovakia",
                                                   "Syria", "US"), popCountries=c("Myranmar", "Congo, Rep.", "Congo, Dem. Rep.", "Gambia, The", "Iran, Islamic Rep.",
                                                                                  "Korea, Dem. People's Rep.", "Kyrgyz Republic", "Slovak, Republic", 
                                                                                  "Syrian Arab Republic", "Unites States"))

#Combine manuallyKeys and mergeKeys
countryDiff <- countryDiff %>% rbind(countryDiffManually)





#Rename countries in popData for merging
for (i in 1:length(countryDiff$popCountries)) {
  popData$country[popData$country==countryDiff$popCountries[i]]=countryDiff$covidCountries[i]
}

##### Merge covid data and population data 
#Subset dataframe
#dataframe_confirmed <- dataframe_total %>% filter(category=="confirmed_global")

dataframe_long <- dataframe_long %>% left_join(popData  %>% select(-date), by=c("country"="country"))




##### Calculation of the incidence over 7 days per 100 000 inhabitants and the reproduction number ----
dataframe_long <- dataframe_long %>% dplyr::group_by(country, category) %>% 
  dplyr::mutate(NewInfections=c(NA,diff(number, lag=1))) %>% 
  dplyr::group_by(country, category) %>% 
  dplyr::mutate(SevenDaySum = zoo::rollsum(NewInfections, k = 7, fill=NA, align="right")) %>%
  dplyr::group_by(country, category) %>% 
  dplyr::mutate(Incidence = (SevenDaySum/TotalPop)*100000) %>% 
  dplyr::mutate(FourDaySum=zoo::rollsum(NewInfections, k=4, fill=NA, align="right")) %>% 
  dplyr::group_by(country, category) %>% 
  dplyr::mutate(ReproductionNumber=FourDaySum/lag(FourDaySum, n=4)) %>%
  dplyr::group_by(country, category) %>%
  dplyr::mutate(Number100 = date - min(date[number>=100]))


#Add newInfections as additional category
dataframe_long <- dataframe_long %>% select(country,category, date, NewInfections) %>% filter(category=="confirmed_global") %>%
  mutate(category="newCases",
         number=NewInfections) %>% 
  select(country, category, date, number) %>% 
  rbind(dataframe_long) %>% arrange(country, date)


#Remove "_global" out of category name
dataframe_long$category[dataframe_long$category=="confirmed_global"]="confirmedCases"
dataframe_long$category[dataframe_long$category=="recovered_global"]="recoveredCases"
dataframe_long$category[dataframe_long$category=="deaths_global"]="deaths"

#Save selected variables as integer
lstInt <- c("number","FourDaySum", "Number100", "TotalPop", "Incidence")
dataframe_long[,lstInt] <- lapply(dataframe_long[,lstInt], as.integer)


#Rename and save the dataframe
covid19data <- dataframe_long
save(covid19data, file="C:/Users/Nina/Documents/R/Corona/Covid19Data")

end_time <- Sys.time()

end_time - start_time


#clear all
#rm(list = ls(all.names = TRUE))




