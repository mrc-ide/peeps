#' Get population data
#' 
#' @description
#' This function allows you to download the population data from the 
#' UN Population division, available here (https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/). 
#' The data is downloaded to a temporary file and returned as a 
#' dataframe. Any new download will override the old one. You can specify country,
#' gender, time span, and whether you want data by individual year of age or 5 year age groups.
#' 
#' @details
#' Here are the terms of use for all UN produced data: https://www.un.org/en/about-us/terms-of-use.
#' Please look through before downloading the data. The data can be copied, 
#' altered, and distributed, even for commercial use.
#' 
#' One issue with this function is that the complete csv is large, as before, 
#' and takes time and memory to load. 
#' Currently the usage of the vroom package means that memory usage is minimized, 
#' as the vroom function downloads pointers and only loads what is actually called.
#' However, an initial check of available memory could be added in order to be prudent.
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
#' @param age.groups TRUE/FALSE. Defaults to TRUE, if TRUE returns population by 
#' 5 year age bands, which is available from 1950-2100.
#' If FALSE, returns population estimates by single age, available range is from 1950-2019.
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
#' get_population(country="Norway")
get_population <- function(country="all", 
                          gender="all", 
                          years.start=2000, 
                          years.end=NA, 
                          ages.start=0,
                          ages.end=NA,
                          age.groups=TRUE){
  
  stopifnot("Country must be a character." = is.character(country))
  stopifnot("Gender must be a character."=is.character(gender))
  stopifnot("Years.start must be numeric"=is.numeric(years.start))
  stopifnot("Ages.start must be numeric"=is.numeric(ages.start))
  stopifnot("Age.groups must be logical"=is.logical(age.groups))
  
  if(age.groups==TRUE){
    url<-"https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_PopulationByAgeSex_Medium.csv"
  } else {
    url<-"https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_PopulationBySingleAgeSex_1950-2019.csv"
  }

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

  # recode gender so that it's the same as the life table data
  names(DT)[names(DT) == "PopMale"] <- "Male"
  names(DT)[names(DT) == "PopFemale"] <- "Female"
  names(DT)[names(DT) == "PopTotal"] <- "Total"
  DT<-tidyr::pivot_longer(DT, c("Male", "Female", "Total"), names_to="Sex", values_to="Population")
  
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
  if(is.numeric(years.start)){ 
    DT=DT[DT$Time>=years.start,]
  } 
  if(!is.na(years.end)){
    DT=DT[DT$Time<=years.end,]
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
    if(age.groups==TRUE){
      DT<-DT[DT$AgeGrpStart<ages.end,]
    } else {
      DT<-DT[DT$AgeGrpStart<=ages.end,]
    }

  }
  
  # return life table without certain columns as dataframe
  DT<-DT[, c(2,5,7:8,10:length(DT))]
  
  # return life table without certain columns as dataframe
  return(DT) # return as dataframe
}
