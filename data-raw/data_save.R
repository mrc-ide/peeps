# data wrangle
asmr<-read.csv("data-raw/202110gavi-3_dds-201910_2_mort_rate_both.csv")
asmr_f<-read.csv("data-raw/202110gavi-3_dds-201910_2_mort_rate_female.csv")
asmr_m<-read.csv("data-raw/202110gavi-3_dds-201910_2_mort_rate_male.csv")

asmr_total<-dplyr::bind_rows(asmr, asmr_f, asmr_m)
asmr<-asmr_total
saveRDS(asmr, file = "data/asmr.rds")

nmr<-read.csv("data-raw/202110gavi-3_dds-201910_2_unwpp_cm_nmr_both.csv")
saveRDS(nmr, file="data/nmr.rds")
