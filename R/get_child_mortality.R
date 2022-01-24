#' Get child mortality rates
#' 
#' @description
#' This function computes and returns the child mortality rates developed by the 
#' UN Inter-Agency Group for Child Mortality Estimation, which includes UNICEF, 
#' WHO, the World Bank, and the UN DESA Population Division. This data is retrieved
#' from the World Bank website, and terms of use are found here: https://www.worldbank.org/en/about/legal/terms-of-use-for-datasets.
#' 
#' @details 
#' Neonatal mortality: the probability of dying between the first 28 days of life, expressed per 1000 live births.
#' Infant mortality: the probability of dying between birth and exact age 5, expressed per 1000 live births.
#' Under 5 mortality rate: the probability of dying between birth and exact age 5, expressed per 1000 live births.
#' 
#' Future additions:
#' Memory use improvements. Check of years and ages inputs. Uses at least an extra 2.6 MB. 
#' Spell checks. Url check - make sure its still there.
#' 
#' @param country all, iso3c code, or country name in English. If not available or 
#' misspelled, will throw error. To do: add a list of all countries/regions that we 
#' have data for.
#' @param gender both (only total), female, male, all (female, male, and total).
#' Data by gender is only available for infant mortality and under 5 mortality.
#' @param years.start Mortality data is available between 1960-2019. This specifies
#' the first year for which values are returned, default is 1960.
#' @param years.end Specified the last year for which values are returned. By default
#' returns up to 2019.
#' @param indicator Indicators for the function to return. Either returns all, 
#' infant mortality ("infant"), neonatal mortality("neonatal"), or under 5 
#' mortality ("under 5"). 
#' 
#' @keywords UN, demography, download, population, child, deathrates, infant, mortality, neonatal, under 5
#' @return List of mortality rates.
#' @export
#' 
#' @import countrycode
#' @import tidyr
#' @import data.table
#' @import stringr
#' @import tidyverse
#' @import dplyr
#' 
#' @examples
#' get_child_mortality(country="Norway")
get_child_mortality <- function(country="all", 
                           gender="all", 
                           years.start=1960, 
                           years.end=NA, 
                           indicator="all"){
  
  stopifnot("Country must be a character string. Options include ISO3C codes 
            or country names." = is.character(country))
  stopifnot("Gender must be a character string. Options include female, male, 
            both, or all (which returns female, male, and total). 
            \n Only infant mortality and under 5 mortality are available 
            separated by genders."=is.character(gender))
  stopifnot("Years.start must be numeric. Range starts at 1960 and continues 
            until 2019."=is.numeric(years.start))
  stopifnot("Indicator must be a character string. Options include infant (for 
            infant mortality), neonatal, under 5, and all."=is.character(indicator))
 
  urls<-data.frame(n=c("neo_mort", "inf_mort", "inf_f_mort", 
                       "inf_m_mort", "u5_mort", "u5_f_mort", "u5_m_mort"))
  urls$url<-c("https://api.worldbank.org/v2/en/indicator/SH.DYN.NMRT?downloadformat=csv", 
              "https://api.worldbank.org/v2/en/indicator/SP.DYN.IMRT.IN?downloadformat=csv",
              "https://api.worldbank.org/v2/en/indicator/SP.DYN.IMRT.FE.IN?downloadformat=csv",
              "https://api.worldbank.org/v2/en/indicator/SP.DYN.IMRT.MA.IN?downloadformat=csv",
              "https://api.worldbank.org/v2/en/indicator/SH.DYN.MORT?downloadformat=csv",
              "https://api.worldbank.org/v2/en/indicator/SH.DYN.MORT.FE?downloadformat=csv",
              "https://api.worldbank.org/v2/en/indicator/SH.DYN.MORT.MA?downloadformat=csv")
  
  indicator<-tolower(indicator)
  if(indicator != "all"){
    if(indicator=="neonatal"){
      urls<-urls[1,]
    } else if (indicator=="infant"){
      urls<-urls[2:4,]
    } else if (indicator=="under 5"){
      urls<-urls[5:7,]
    } else {
      print(paste0("Please input indicator again. System failed to recognize 
                   indicator specified (not neonatal, infant, under 5, or all), 
                   so has returned all."))
    }
  }
  df<-data.frame()
  # neonatal data, only up to 2005
  for(i in 1:length(urls$url)){
    tmp<-tempfile()
    download.file(urls$url[i],tmp, quiet=TRUE)
    files<-unzip(zipfile = tmp, list=TRUE)
    filename<-files[startsWith(files$Name, "API_S"),1]
    f<-unzip(zipfile=tmp, files=filename)
    mydata <- read.csv(f, skip=4)
    mydata<-as.data.frame(mydata)
    mydata<-mydata[, sapply(mydata, class) != "logic"]
    varName <- urls$n[i]
    if(grepl("_[a-z]_", varName)) {
      mydata$Sex<-rep(stringr::str_match(varName, "_[a-z]_"), nrow(mydata))
    } else {
      mydata$Sex<-rep("Total", nrow(mydata))
    }
    #assign(x=varName,value=mydata)
    df<-dplyr::bind_rows(df,mydata)
    unlink(tmp)
    rm(list=c('mydata','files','filename','tmp','varName','f'))

  }
  
  # do some data wrangling/recoding
  # recode sex
  df$Sex<-ifelse(df$Sex=="_f_", "Female", ifelse(df$Sex=="_m_", "Male", df$Sex))
  # gets rid of empty columns

  # deletes X prefix in year columns
  names(df) <- sub('^X', '', names(df))

  # subset by country
  if(country != "all") {
    # convert iso3c codes
    if(tolower(country) %in% tolower(countrycode::codelist$iso3c)){
      df = df[tolower(df$Country.Code) == tolower(country),]
    } else if (tolower(country) %in% tolower(df$Country.Name)){
      df = df[tolower(df$Country.Name) == tolower(country),]
    } else {
      print("Country specified not found. Check the spelling - returned all.
            Check 'Country.Name' for countries available.")
    }
  }

  # subset by gender
  gender <- tolower(gender)
  if(gender!="all"){ # if user says all, will return female, male and total
    if(gender!="both"){
      gender <- ifelse(startsWith(gender, "f"), "Female", "Male")
    } else {
      gender<-"Total"
    }
    df=df[df$Sex==gender,]
  }
  # subset by year start & end
  if(is.numeric(years.start) | is.numeric(years.end)){
    n<-unlist(lapply(df, is.numeric))
    nm<-colnames(df)[n]
    if(is.numeric(years.start)){
      vec<-nm[as.numeric(nm)>=years.start]
    } else {
      vec<-nm
    }
    if(is.numeric(years.end)){
      vec<-vec[as.numeric[vec]<=years.end]
    } # dont need second if statement, because have vec no matter what
    vec<-append(vec, c("Country.Name", "Country.Code", "Indicator.Name", "Sex"))
    df<-df[colnames(df) %in% vec]
  }

  return(df)

}


  