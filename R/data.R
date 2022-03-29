#' Age specific mortality rates
#'
#' A list of all age smoothed mortality rates currently available from the
#' UNWPP, who released the data with a free license to use and distribute as
#' desired, with modifications. Year indicated is from the start of a 5 year
#' period of estimation, (i.e. values associated with 1950 indicates values
#' averaged over 1950.5 until 1955.5). Ages ranges are inclusive of the age
#' indicated in age_to. The death rates are from the mx values of the UNWPP 
#' lifetables, and further details on how they were computed can be found at
#' \url{https://www.un.org/development/desa/pd/data/model-life-tables}.
#'
#'@format A data frame with 225720 rows and 7 variables:
#' \describe{
#'   \item{country_code}{Iso3c codes}
#'   \item{country}{Country name}
#'   \item{age_from}{Starting age of the age band, inclusive.}
#'   \item{age_to}{Ending age of the age band, inclusive. Is 0 for <1 year. Max
#'   years is 120.}
#'   \item{year}{Starting year of 5 year group, i.e. 1960 for 1960-64, etc. From
#'   1950-2100.}
#'   \item{gender}{"both", "female", or "male"}
#'   \item{value}{Age specific mortality rate, or the central rate of 
#'   mortality, mx.}
#' }
#' @source UNWPP, wrangled into a csv file by the VIMC team at
#' Imperial, distributed through Montagu.
"asmr"

#' Neonatal mortality rates
#' 
#' The Imperial team calculated the NMR by taking the NMR/IMR ratio from the
#' child mortality dataset (see \url{childmortality.org} for more details) and 
#' applying that transformation to the IMR values calculated in the life tables
#' of the UNWPP. This in order to keep data consistent and at the UNWPP scale,
#' while incorporating the higher accuracy and granularity of the child 
#' mortality project estimates.
#'
#'@format A data frame with 17100 rows and 7 variables:
#' \describe{
#'   \item{country_code}{Iso3c codes}
#'   \item{country}{Country name}
#'   \item{age_from}{Starting age of the age band, inclusive}
#'   \item{age_to}{Ending age of the age band, inclusive. Is 0.083333 for all
#'   neonatal infants.}
#'   \item{year}{Year applicable, ranges from 1950-2100}
#'   \item{gender}{"both" genders only.}
#'   \item{value}{Probability of dying between birth and exact age 1 month. It 
#'   is expressed as average annual deaths per 1,000 births.}
#' }
#' 
#' @source Child mortality project and UNWPP.
"nmr"

#' Infant mortality rates
#' 
#' Infant mortality values provided by the UNWPP.
#'
#'@format A data frame with 17100 rows and 7 variables:
#' \describe{
#'   \item{country_code}{Iso3c codes}
#'   \item{country}{Country name}
#'   \item{age_from}{Starting age of the age band, inclusive.}
#'   \item{age_to}{Ending age of the age band, inclusive. Is 1 for all infants.}
#'   \item{year}{Year applicable, ranges from 1950-2100}
#'   \item{gender}{"both" genders only.}
#'   \item{value}{Probability of dying between birth and exact age 1 year. It is 
#'   expressed as average annual deaths per 1,000 births.}
#' 
#' @source UNWPP.
"imr"

#' Under 5 mortality rates
#' 
#' Under 5 mortality rates provided by the UNWPP.
#'@format A data frame with 17100 rows and 7 variables:
#' \describe{
#'   \item{country_code}{Iso3c codes}
#'   \item{country}{Country name}
#'   \item{age_from}{Starting age of the age band, inclusive.}
#'   \item{age_to}{Ending age of the age band, inclusive. Is 5 for all entries.}
#'   \item{year}{Year applicable, ranges from 1950-2100}
#'   \item{gender}{"both" genders only.}
#'   \item{value}{Probability of dying between birth and exact age 5. It is 
#'   expressed as average annual deaths per 1,000 births.}
#' }
#' 
#' @source UNWPP
"u5mr"

#' Crude death rates
#'
#' Crude death rates calculated by the UNWPP. More information on calculations
#' can be found in the mortality_data vignette, or [here]
#' (https://population.un.org/wpp/Publications/Files/WPP2019_Methodology.pdf).
#'
#'@format A data frame with 17100 rows and 7 variables:
#' \describe{
#'   \item{country_code}{Iso3c codes}
#'   \item{country}{Country name}
#'   \item{age_from}{Starting age of the age band, inclusive.}
#'   \item{age_to}{Ending age of the age band, inclusive. Is 120 for all entries.}
#'   \item{year}{Year applicable, ranges from 1950-2099}
#'   \item{gender}{"both" genders only.}
#'   \item{value}{Number of deaths over a given period divided by the person-
#'   years lived by the population over that period. It is expressed as average
#'   annual number of deaths per 1,000 population.}
#' }
#' 
#' @source UNWPP.
"cdr"

#' Crude birth rates
#'
#'@format A data frame with 17100 rows and 7 variables:
#' \describe{
#'   \item{country_code}{Iso3c codes}
#'   \item{country}{Country name}
#'   \item{age_from}{Starting age of the age band, inclusive.}
#'   \item{age_to}{Ending age of the age band, inclusive. Is 120 for all entries.}
#'   \item{year}{Year applicable, ranges from 1950-2099}
#'   \item{gender}{"both" genders only.}
#'   \item{value}{Number of births over a given period divided by the person-
#'   years lived by the population over that period. It is expressed as average
#'   annual number of births per 1,000 population.}
#' }
#' 
#' 
#' @source UNWPP
"cbr"
