#' Get population data
#' 
#' Issue here is that downloading the big csv takes up 113 MBs. 
#' 
#' This function allows you to download the population data from the 
#' UN Population division, available here (https://population.un.org/wpp/Download/Standard/Population/). 
#' The data is downloaded to a temporary file and returned as a 
#' dataframe. Any new download will override the old one.
#' 
#' NEEDS VROOM AND BASE and COUNTRYCODE
#' 
#' Here is the terms of use for all UN produced data: https://www.un.org/en/about-us/terms-of-use.
#' Please look through before downloading the data.
#' 
#' @param gender both, female, male, all
#' @param age_groups TRUE/FALSE
#' @param annual TRUE/FALSE
#' 
#' @keywords UN, demography, download, population, gender, age groups
#' @return Data frame of population data.
#' @export
#' 
#' @examples
# download pop data
get_demography <- function(country="all", 
                           gender="both", 
                           years.start=2000, 
                           years.end=2050, 
                           ages.start=0,
                           ages.end=NA){
  # check memory
  # check inputs, throw error
  # options to add spell checks? for now we handle capitalization errors, not spelling
  
  # check that URL is up to date
  url<-"https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_Life_Table_Medium.csv"

  DT = vroom::vroom(url)

  # check format of country input and reformat to country name
  
  if(country != "all") {
    # convert iso3c codes
    if(country %in% countrycode::codelist$iso3c){
      country <- countrycode::countrycode(country, origin="iso3c", destination="country.name")
    } 
    # no matter what, convert to first capitalized letter rest lowercase
    country<-stringr::str_to_title(country)
    if(country %in% DT$Location){
      DT = DT[DT$Location == country,]
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
  if(is.numeric(years.start) && is.numeric(years.end)){
    DT=DT[DT$TimeStart>=years.start & DT$TimeStart<years.end,]
    DT=DT[DT$TimeEnd>years.start & DT$TimeEnd<=years.end,]
  } # i think this is right, but need to sleep on it and look at it again
  # check ages start & end
  if(is.numeric(ages.start)){
    DT<-DT[DT$AgeGrpStart>=ages.start,]
    if(!is.na(ages.end)){
      DT<-DT[DT$AgeGrpStart<ages.end,]
    }
  }
    

  # where to find the tables
  DT<-DT[, c("Location", "TimeStart", "TimeEnd", "Sex", "AgeGrp", "mx", "qx")]
  return(DT) # return as dataframe
}

# download pop data
download_pop_data <- function() {
  
  # add checks of inputs
  # check memory

  # where to find the tables
  url <- "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_PopulationByAgeSex_Medium.csv"
  
  tmp<-tempfile()
  curl::curl_download(url, tmp)
  #data<-readxl::read_excel(destfile) # load it into R 
  data<-read.csv(tmp)
  return(data) # return as dataframe
}

t1=Sys.time()
mydata<-download_pop_data()
t2=Sys.time()
print(paste0("Downloading all data took ", t2-t1))


download_pop_data2 <- function() {
  
  # add checks of inputs
  # check memory
  
  # where to find the tables
  url <- "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_PopulationByAgeSex_Medium.csv"
  
  tmp<-tempfile()
  curl::curl_download(url, tmp)
  #data<-readxl::read_excel(destfile) # load it into R 
  # issue: some of the Locations have commas in them, which makes reading in the data frames difficult
  data.vroom<-vroom(tmp)
  mydata = sqldf::read.csv.sql(tmp, sql = "select * from file where 'Location' = '\"Afghanistan\"' ")
  return(mydata) # return as dataframe
}
t1=Sys.time()
mydata.sql<-download_pop_data2()
t2=Sys.time()
print(paste0("Downloading  Afghanistan data took ", t2-t1))
# get death rates
# get birth rates

# get total population counts

file.copy(tmp, tmp <- tempfile())

# fread
rm(DT)
system.time({
  DT = data.table::fread("https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_PopulationByAgeSex_Medium.csv")
  DT = DT[Location == 'Afghanistan']
})
#user  system elapsed 
#3.606   2.420 123.744 

# vroom
rm(DT)
system.time({
  DT = vroom("https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_PopulationByAgeSex_Medium.csv")
  DT = DT[DT$Location == 'Afghanistan',]
})
# user  system elapsed 
# 3.458   2.597  87.116 

# download file
rm(DT)
system.time({
  tmp<-tempfile()
  download.file("https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_PopulationByAgeSex_Medium.csv", tmp)
  DT= read.csv(tmp)
  DT = DT[DT$Location == 'Afghanistan',]
})
# user  system elapsed 
# 14.265   2.343 102.338 

# curl
rm(DT)
system.time({
  tmp<-tempfile()
  curl::curl_download(url, tmp)
  DT = read.csv(tmp)
  DT = DT[DT$Location == 'Afghanistan',]
})
# user  system elapsed 
# 12.260   2.302  94.863 

