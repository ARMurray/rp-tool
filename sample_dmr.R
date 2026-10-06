library(dplyr)
library(lubridate)
library(stringr)
library(echor)


# Get permits
global_permit_data <- tryCatch({
  echoWaterGetFacilityInfo(output = "df", p_st = "PR", p_ptype = "NPD")%>%
    mutate(
      FacLat = as.numeric(FacLat),
      FacLong = as.numeric(FacLong),
      display_label = paste0(SourceID, " - ", CWPName)
    )
}, error = function(e) {
  message("Global Fetch Error: ", e)
  NULL
})

# Dates
start_fmt <- format(mdy("01-01-2018"), "%m/%d/%Y")
end_fmt   <- format(mdy("12-31-2025"), "%m/%d/%Y")

permits <- unique(global_permit_data$SourceID)

# Sample

# Units
units <- data.frame()
pb <- txtProgressBar(min = 0, max = length(permits), style = 3)

for(n in 1:length(permits)){
  # Sample parameters
  dmr_raw <- echoGetEffluent(p_id = permits[n],
                             output = "df",
                             start_date = start_fmt,
                             end_date = end_fmt)%>%
    mutate(monitoring_period_end_date = mdy(monitoring_period_end_date))%>%
    select(parameter_desc ,parameter_code,dmr_unit_desc)%>%
    drop_na()%>%
    filter(!dmr_unit_desc == "")%>%
    group_by(parameter_desc ,parameter_code,dmr_unit_desc)%>%
    summarise(nRecords = n(), .groups = "drop")
  
  units <- rbind(units,dmr_raw)
  
  setTxtProgressBar(pb,n)
}


summary <- units%>%
  group_by(parameter_desc ,parameter_code,dmr_unit_desc)%>%
  summarise(total = sum(nRecords,na.rm = TRUE), .groups = "drop")

paste(sort(unique(summary$dmr_unit_desc)),collapse = "', '")
