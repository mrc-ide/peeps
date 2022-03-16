#' Data Objects Documentation
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
#' @format A list
#' @source UNWPP, wrangled into a csv file by the VIMC team at
#' Imperial, distributed through Montagu.
"asmr"

#' A list of neonatal mortality rates.
#' 
#' The Imperial team calculated the NMR by taking the NMR/IMR ratio from the
#' child mortality dataset (see \url{childmortality.org} for more details) and 
#' applying that transformation to the IMR values calculated in the life tables
#' of the UNWPP. This in order to keep data consistent and at the UNWPP scale,
#' while incorporating the higher accuracy and granularity of the child 
#' mortality project estimates.
#'
#' @format A list
#' @source Child mortality project and UNWPP.
"nmr"