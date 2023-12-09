#Library call statements
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(sf)



#Read in data
nyc <- read.csv("NYPD_Arrests_Data__Historic__20231126.csv") #Note: df is too large to upload to GitHub so you have to download from: https://data.cityofnewyork.us/Public-Safety/NYPD-Arrests-Data-Historic-/8h9b-rp9u
air <- read.csv("Air_Quality.csv")

#Remove unnecessary columns
nyc <- select(nyc, c("ARREST_DATE", "OFNS_DESC", "LAW_CAT_CD", "ARREST_BORO", "ARREST_PRECINCT", "AGE_GROUP", "PERP_SEX", "PERP_RACE"))
air <- select(air, c("Name", "Measure", "Measure.Info", "Geo.Type.Name", "Geo.Place.Name", "Time.Period", "Data.Value"))

#Filter out Ozone and PM2.5 pollutant for only Boroughs & create new categorical variable season
pattern <- "Ozone|Fine Particulate Matter|Nitrogen Dioxide|Sulfur Dioxide"
UHF_graph <- filter(air, Geo.Type.Name=="UHF42", str_detect(Time.Period, "Summer|Winter"), str_detect(Name, pattern))
UHF_graph <- mutate(UHF_graph, season_abb=substr(Time.Period, 1, 1), Year=as.numeric(paste0("20", str_sub(Time.Period, -2))))
air <- filter(air, Geo.Type.Name=="Borough", str_detect(Time.Period, "Summer|Winter"), str_detect(Name, pattern))
air <- mutate(air, season_abb=substr(Time.Period, 1, 1), Year=as.numeric(paste0("20", str_sub(Time.Period, -2))))

#Takes in Borough abbreviation and convert it into full String
convert <- function(ch) {
  if (ch == "B") {
    return("Bronx")
  } else if (ch =="K") {
    return("Brooklyn")
  } else if (ch == "M") {
    return("Manhattan")
  } else if (ch == "Q") {
    return("Queens")
  } else {
    return("Staten Island")
  }
}

#Returns whether pollutant is above NAAQS standard: https://www.epa.gov/criteria-air-pollutants/naaqs-table
NAAQS <- function(type, value) {
  if (type == "Nitrogen Dioxide (NO2)") {
    return(value > 53)
  } else if (type == "Fine Particulate Matter (PM2.5)") {
    return(value > 10)
  } else if (type == "Ozone (O3)"){
    return(value > 70)
  } else {
    return(value > 75)
  }
}

#Categorize month by nearest season/temperature 
to_season <- function(month) {
  if (month=="03"|month=="04"|month=="05"|month=="06"|month=="07"|month=="08") {
    return("S")
  } else {
    return("W")
  }
}

#New column holding separate month and year values as numeric
nyc <- mutate(nyc, Month=substr(ARREST_DATE, 1, 2), Year=as.numeric(str_sub(ARREST_DATE, -4)))

#Uses function to create new categorical column with converted Borough name and categorize month into Seasons
nyc$Borough_Fix <- mapply(convert, nyc$ARREST_BORO)
nyc$Season <- mapply(to_season, nyc$Month)

#New columns for counts of each elements within each Borough throughout each year
nyc_grp <- group_by(nyc, Borough_Fix, Season, Year)
nyc_yr <- summarize(nyc_grp, male=length(which(PERP_SEX=="M")), female=length(which(PERP_SEX=="F")),
                    felony=length(which(LAW_CAT_CD=="F")), misdemeanor=length(which(LAW_CAT_CD=="M")),
                    violation=length(which(LAW_CAT_CD=="V")), `<18`=length(which(AGE_GROUP=="<18")),
                    `18-24`=length(which(AGE_GROUP=="18-24")), `25-44`=length(which(AGE_GROUP=="25-44")),
                    `45-64`=length(which(AGE_GROUP=="45-64")), `65+`=length(which(AGE_GROUP=="65+")),
                    drug_use=length(which(OFNS_DESC=="DANGEROUS DRUGS")), larceny=length(which(str_detect(OFNS_DESC, "LARCENY"))),
                    DUI=length(which(OFNS_DESC=="INTOXICATED & IMPAIRED DRIVING")), assault=length(which(str_detect(OFNS_DESC, "ASSAULT"))))

precinct_grp <- group_by(nyc, ARREST_PRECINCT, Year)
precinct_graph <- summarize(precinct_grp, male=length(which(PERP_SEX=="M")), female=length(which(PERP_SEX=="F")),
                            felony=length(which(LAW_CAT_CD=="F")), misdemeanor=length(which(LAW_CAT_CD=="M")),
                            violation=length(which(LAW_CAT_CD=="V")), `<18`=length(which(AGE_GROUP=="<18")),
                            `18-24`=length(which(AGE_GROUP=="18-24")), `25-44`=length(which(AGE_GROUP=="25-44")),
                            `45-64`=length(which(AGE_GROUP=="45-64")), `65+`=length(which(AGE_GROUP=="65+")),
                            drug_use=length(which(OFNS_DESC=="DANGEROUS DRUGS")), larceny=length(which(str_detect(OFNS_DESC, "LARCENY"))),
                            DUI=length(which(OFNS_DESC=="INTOXICATED & IMPAIRED DRIVING")), assault=length(which(str_detect(OFNS_DESC, "ASSAULT"))))

#Merge data
df <- merge(x=air, y=nyc_yr, by.x=c("Geo.Place.Name","Year","season_abb"), by.y=c("Borough_Fix","Year","Season"), all.x=TRUE)

#New numerical total_crime column that keeps track of number of total crime within each Borough per year
df <- mutate(df, total_crime=male+female, crime_per_value=total_crime/Data.Value)
precinct_graph <- mutate(precinct_graph, total_crime=male+female)

#New categorical column that states whether air quality data value is above NAAQS standard & new season var
df$above_NAAQS_standard <- mapply(NAAQS, df$Name, df$Data.Value)
UHF_graph$above_NAAQS_standard <- mapply(NAAQS, UHF_graph$Name, UHF_graph$Data.Value)
df <- mutate(df, start_season=ifelse(season_abb == "S", "Summer", "Winter"))

#Clean up joined df
df <- arrange(df, start_season)
df <- rename(df, avg_value=Data.Value, Borough=Geo.Place.Name)
df <- select(df, -c(Measure, Geo.Type.Name, Time.Period, season_abb))

#Data summary
df_grp <- group_by(df, Year, Name, start_season)
df_summary <- summarize(df_grp, avg_pollution_value=mean(avg_value), total_borough_crime=sum(total_crime))


