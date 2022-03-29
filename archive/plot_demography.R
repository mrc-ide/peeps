#' Plot demography
#' 
#' JUST DO A VIGNETTE WITH PLOTS
#' 
#' 1. Get yearly averages of deathrates + birthrates, & population counts.
#' 2. Format consistently.
#' 3. Produce plots: 3 individual, one of death & birthrates only, and one of all three.
#' 4. Option to slice by specific age range. 
#' 5. Option to slice by year range.
#' 6. Option to specify by country.
#' 7. Option to specify by income group/region.
#' 8. Option to specify gender.
#' 
#' Also code - population pyramids.
#' 
#' @param asmr
#' @import ggplot2
#' @import patchwork
#' 
#' @export
plot_dem_transition<-function(cdr = peeps::cdr, cbr = peeps::cbr, 
                              pop = peeps::pop){
  names(cdr)[8]<-"deathrate"
  names(cbr)[8]<-"birthrate"
  names(pop)[8]<-"population"

  rates<-merge(cdr, cbr)
  rates <- subset(rates, select=-c(age_from, age_to))
  pop<- subset(pop, select=-c(age_from, age_to))
  all_data<-merge(rates, pop)
  # base r
  par(mar = c(5, 4, 4, 8) + 0.3)              # Additional space for second y-axis
  plot(df$year, df$birthrate, col = 2,
       xlab="Year", ylab="Crude Death and Birth Rates (per 1,000 in population')")
  par(new = TRUE) # Create first plot
  plot(df$year, df$deathrate, col = 4, 
       axes = FALSE, xlab = "", ylab = "")
  par(new = TRUE)                             # Add new plot
  plot(df$year, df$population, col = 3,              # Create second plot without axes
       axes = FALSE, xlab = "", ylab = "")
  legend("topright", title="Values",
         c("Crude Birth Rate","Crude Death Rate","Total Population"), 
         col=c(2,4,3), fill=c(2,4,3))# Add second axis
  axis(side = 4, at = pretty(range(df$population))) 

  mtext("Population", side = 4, line = 3)

  
  # coefficient used to scale the data so its on the same graph
  coeff = 10000000000
  p1 <- ggplot2::ggplot(data=df, aes(x=year)) +
    geom_line(aes(y=birthrate)) +
    geom_line(aes(y=deathrate)) +
    geom_line(aes(y=population/coeff)) +
    scale_y_continuous(
      
      # Features of the first axis
      name = "Birth and Death Rates",
      
      # Add a second axis and specify its features
      sec.axis = sec_axis(~.*coeff, name="Population")
    ) 
  p1
}