#' Download data
#' Updated by Ines on 14 feb.
#'
#' @param indicator Deathrates or population.
#'
#' @importFrom vroom vroom
#' @importFrom httr GET
#' @importFrom httr write_disk
#' @importFrom readxl read_excel
#' @importFrom utils download.file read.csv unzip
#'
#' @return Either list of dataframes containing deathrates, or dataframe of
#' population by year.
#' @export
#'
download_data <- function(indicator = "deathrates") {
  indicator <- tolower(indicator)

  if (indicator == "deathrates") {

    # download total population death rates
    url <- "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_Life_Table_Medium.csv"
    lifetable <- vroom::vroom(url, show_col_types = FALSE)
    lifetable <- lifetable[, c(2, 5, 7:8, 10:length(lifetable))]

    # get the neonatal deathrates
    url <- "https://api.worldbank.org/v2/en/indicator/SH.DYN.NMRT?downloadformat=csv"

    # the httr package avoids corrupting the zipfiles
    zipfile <- "neo.zip"
    httr::GET(
      url = url,
      httr::write_disk(zipfile, overwrite = T)
    ) -> res

    # lists the files without extracting
    list_of_files <- utils::unzip(zipfile = zipfile, list = TRUE)

    # find the correct file
    filename <- list_of_files[startsWith(list_of_files$Name, "API_S"), 1]

    # Unzip the data file from the downloaded zip file
    f <- utils::unzip(zipfile = zipfile, files = filename)
    df <- utils::read.csv(f, skip = 4)
    df <- df[!duplicated(df), ]

    # download and add the world bank income group
    tmp <- tempfile()
    if (Sys.info()[1] == "Windows") {
      utils::download.file("http://databank.worldbank.org/data/download/site-content/CLASS.xlsx",
        tmp,
        quiet = TRUE, mode = "wb"
      )
    } else {
      utils::download.file("http://databank.worldbank.org/data/download/site-content/CLASS.xlsx",
        tmp,
        quiet = TRUE
      )
    }
    wb <- readxl::read_excel(tmp)
    unlink(tmp)
    unlink(filename)
    unlink(zipfile)

    # make them all dataframes
    lifetable <- as.data.frame(lifetable)
    df <- as.data.frame(df)
    wb <- as.data.frame(wb)

    dt <- list(neorates = df, deathrates = lifetable, worldbank = wb)
  } else if (indicator == "population") {
    url <- "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_PopulationBySingleAgeSex_1950-2019.csv"
    dt <- vroom::vroom(url, show_col_types = FALSE)
    dt <- as.data.frame(dt)
  }

  return(dt)
}
