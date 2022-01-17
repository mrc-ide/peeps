#' Get model life tables from UNDP
#'
#' This function allows you to download the model life tables from the 
#' UNDP, available here (https://www.un.org/development/desa/pd/data/model-life-tables). 
#' The life tables are downloaded to a temporary file and returned as a 
#' dataframe. If you have previously downloaded a life table the new life table 
#' will override the old one. 
#'
#'big changes 
#'
#' Here is the terms of use for all UN produced data: https://www.un.org/en/about-us/terms-of-use.
#' Please look through before downloading the data.
#' Requires 
#' @param year Year increment for calculations, either 2.5 or 1. Input as string or integer.
#' @param type "Abridged" or "complete" life tables. 
#' Input can be a single letter a or c or abridged/complete. 
#' Abridged tables are broken into 4 year age groups, complete life tables are computed per year.
#' @param path Location to save undp life table.
#' 
#' @keywords UNDP, life table, download, population
#' @return List of life table.
#' @export
#' 
#' @examples
#' download_undp_life_table(year=2.5, type="A", path="./data/")
#' download_undp_life_table(2.5, "Complete", "C:/Users/ines/population/data/")
#' lifetable_2.5_complete <-download_undp_life_table(2.5, "Complete", "C:/Users/ines/population/data/")

download_undp_life_table <- function(year = 1, type = "complete") {
  
  # making sure that the type formatting is converted to the correct format
  type <- tolower(type)
  type <- ifelse(startsWith(type, "a"), "abridged", "complete")
  
  # where to find the tables
  url <- "https://www.un.org/development/desa/pd/data/model-life-tables"
  text<- paste0("_",year, "y_", type, ".xlsx") # current syntax of how life tables are saved
  
  # look at all the urls on the website
  htmlcode = xml2::read_html(url)
  
  # find the relevant url to download the lifetable you're looking for 
  nodes<-rvest::html_nodes(htmlcode,xpath=paste0('//*[contains(@href, "',text,'")]'))
  nodes<-rvest::html_attr(nodes, "href")
  df=as.data.frame(as.character(nodes))
  names(df)="link"
  
  #destfile<-paste0(path, year, "y_", type, "_lifetable.xlsx") # path of file + name, i.e. 1y_abridged.xlsx
  #curl::curl_download(df$link[1], destfile) # download the url that matches the string found, in the destination specified
  tmp<-tempfile()
  curl::curl_download(df$link[1], tmp)
  #data<-readxl::read_excel(destfile) # load it into R 
  data<-readxl::read_excel(tmp)
  return(data) # return as dataframe
}

