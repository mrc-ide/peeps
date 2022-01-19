#' Get life table data
#' 
#' This function allows you to download the life table data from the 
#' UN Population division, available here (https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/). 
#' The data is downloaded to a temporary file and returned as a 
#' dataframe. Any new download will override the old one. You can specify country,
#' gender, time span, and span of age groups. Currently all available life table 
#' data is returned. For death rates specifically, see get_deathrates().
#' 
#' Here are the terms of use for all UN produced data: https://www.un.org/en/about-us/terms-of-use.
#' Please look through before downloading the data. The data can be copied, 
#' altered, and distributed, even for commercial use.
#' 
#' One issue with this function is that the complete csv is 140 MB. 
#' Currently the usage of the vroom package means that memory usage is minimized, 
#' as the vroom function downloads pointers and only loads what is actually called.
#' However, an initial check of available memory could be added in order to be prudent.
#' 
#' Requirements: vroom, base, countrycode, stringr, data.table, tidyr
#' 
#' Future additions:
#' Memory check. Check of years and ages inputs.
#' Spell checks. Url check - make sure its still there.
#' 
#' @param country all, iso3c code, or country name in English. If not available or 
#' misspelled, will throw error. To do: add a list of all countries/regions that we 
#' have data for.
#' @param gender both (only total), female, male, all (female, male, and total)
#' @param years.start Life table data is available for a range of years. This is 
#' the first year from which life table data will be returned. Minimum is 1950.
#' @param years.end The end of the period of life table data that is returned. Maximum is 2100. 
#' @param ages.start Life table data starts at age 0 (default), and is bounded in 5 year age
#' groups.If this age is not available, will return 
#' a table from the lowest bound of the age group older than the age specified.
#' @param ages.end This is the last age of the specified life table. Since the life table is in groups, will 
#' return a life table where the oldest age group includes the ages.end value. 
#'
#' @keywords UN, demography, download, population, life table, deathrates, mx, ax, dx, gender, age groups
#' @return Data frame of life table data.
#' @export
#' 
#' @import vroom
#' @import countrycode
#' @import tidyr
#' @import data.table
#' @import stringr
#' 
#' @examples
#' get_lifetable(country="NZA")
get_lifetable <- function(country="all", 
                           gender="both", 
                           years.start=2000, 
                           years.end=NA, 
                           ages.start=0,
                           ages.end=NA){

  stopifnot("Country must be a character." = is.character(country))
  stopifnot("Gender must be a character."=is.character(gender))
  stopifnot("Years.start must be numeric"=is.numeric(years.start))
  stopifnot("Ages.start must be numeric"=is.numeric(ages.start))

  url<-"https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_Life_Table_Medium.csv"

  DT = vroom::vroom(url)

  # check format of country input and reformat to country name
  
  if(country != "all") {
    # convert iso3c codes
    if(country %in% countrycode::codelist$iso3c){
      country <- countrycode::countrycode(country, origin="iso3c", destination="country.name")
    } 
    # filter by country - has tolower in case someone misspelled something
    if(tolower(country) %in% tolower(DT$Location)){
      DT = DT[tolower(DT$Location) == tolower(country),]
    }
  }
  # another thing: there are some regions that are included that aren't countries per se - should we filter these out or keep them?
  
  # add gender check
  gender <- tolower(gender)
  if(gender!="all"){ # if user says all, will return female, male and total
    if(gender!="both"){
      gender <- ifelse(startsWith(gender, "f"), "Female", "Male")
    } else {
      gender<-"Total"
    }
    DT=DT[DT$Sex==gender,]
  }
  # check years start & end
  # chop up time into time into time.start and time.end
  DT <- as.data.frame(DT)
  DT <- tidyr::separate(DT, Time, c("TimeStart", "TimeEnd"))
  DT$TimeStart<-as.numeric(DT$TimeStart)
  DT$TimeEnd<-as.numeric(DT$TimeEnd)
  
  if(is.numeric(years.start)){ 
    DT=DT[DT$TimeStart>=years.start,]
    DT=DT[DT$TimeEnd>years.start,]
  } 
  if(!is.na(years.end)){
    DT=DT[DT$TimeStart<years.end,]
    DT=DT[DT$TimeEnd<=years.end,]
  } 
  
  # check ages start & end
  if(is.numeric(ages.start)){
    if(ages.start %in% DT$AgeGrpStart){
      DT<-DT[DT$AgeGrpStart>=ages.start,]
    } else { # age start is not at the natural bound of the age group, so return the age group including this age
      while(!(ages.start %in% DT$AgeGrpStart)){
        ages.start<-ages.start-1
      }
      DT<-DT[DT$AgeGrpStart>=ages.start,]      
    }
  }
  
  if(!is.na(ages.end)){
    DT<-DT[DT$AgeGrpStart<ages.end,]
  }

  # return life table without certain columns as dataframe
  DT<-DT[, c(2,5:7,9:22)]
  return(DT) # return as dataframe
}

#' Get deathrates if input a life table returned from the [get_lifetable()] function
#' @param data data frame of life table data returned from [get_lifetable()]
#'
#' @keywords UN, demography, download, population, life table, deathrates, mx, ax, dx, gender, age groups
#' @return Data frame of death rates
#' @export
#' 
#' @examples
#' extract_deathrates(DT)
extract_deathrates <- function(data){
  data<-data[, c("Location", "TimeStart", "TimeEnd", "Sex", "AgeGrpStart", "AgeGrp", "mx")]
  names(data)[names(data) == "mx"] <- "deathrates"
  return(data)
}