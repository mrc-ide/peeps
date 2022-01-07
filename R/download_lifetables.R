#' Get demographic data from UNDP
#'
#' This function allows you to download the most recent life tables from the 
#' UNDP most recent life table estimates. The life tables are downloaded where 
#' you specify and also returned as a dataframe. If you have previously downloaded
#' a life table the new life table will override the old one.
#' 
#' Note: It is unclear when these will next be updated, and when they do the urls 
#' in the code need to change as well.
#' 
#' @param year Year increment for calculations, either 2.5 or 1. Input as string or integer.
#' @param type "Abridged" or "complete" life tables. 
#' Input can be a single letter a or c or abridged/complete. 
#' Abridged tables are broken into 4 year age groups, complete life tables are computed per year.
#' @param path Location to save undp life table.
#' @keywords UNDP, life table, download, population
#' @return Data frame of life table.
#' @export
#' @examples
#' download_undp_life_table(year=2.5, type="A", path="./data/")
#' download_undp_life_table(2.5, "Complete", "C:/Users/ines/population/data/")
#' lifetable_2.5_complete <-download_undp_life_table(2.5, "Complete", "C:/Users/ines/population/data/")
download_undp_life_table <- function(year = 1, type = "complete", path) {
  
  # making sure that the type formatting is converted to the correct format
  type <- tolower(type)
  type <- ifelse(startsWith(type, "a"), "abridged", "complete")
  
  # Year=1 and type="Complete" stored at different URL pattern than the rest, so handle this with 
  # if statement for now.
  if (year==1 & type=="complete"){
    url <- "https://www.un.org/en/development/desa/population/publications/pdf/mortality/EMLT/MLT_UN2011_130_1y_complete.xlsx"
  } else {
    url <- paste0("https://www.un.org/development/desa/pd/sites/www.un.org.development.desa.pd/files/unpd_2011_mlt_130_",
                 year, "y_", type, ".xlsx")
  }
  
  destfile<-paste0(path, year, "y_", type, "_lifetable.xlsx") # save as excel file
  download.file(url, destfile)
  
  data<-readxl::read_excel(destfile)
  return(data)
}
