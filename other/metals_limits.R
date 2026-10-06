## Metals

constants <- data.frame(
  CRITERION_ID = c("79228","79238","79236","79253","79248","79251","79264"),
  LC = c(0.7977, 0.8190, 0.8545, 1.273, 0.8460, 1.72, 0.8473),
  RC = c(3.909, 0.6848, 1.702, 4.705, 0.0584, 6.59, 0.884)
)

metals_limit <- function(wqs_id = "79228", hardness = "range") {
  
  # Use a different name for the argument (wqs_id) to avoid 
  # filter(CRITERION_ID == CRITERION_ID) which filters everything to TRUE.
  constant_select <- constants %>%
    filter(CRITERION_ID == wqs_id)
  
  # Check if we actually found a match
  if(nrow(constant_select) == 0) stop("ID not found.")
  
  # Vectorized approach: No 'for' loop or 'rbind' needed!
  hardness_presets <- seq(25, 400, 1)
  
  if(hardness == "range"){
    limits <- data.frame(
      CRITERION_ID = wqs_id,
      hardness = hardness_presets,
      limit = exp(constant_select$LC * log(hardness_presets) - constant_select$RC)
    )
  } else {
    limits <- data.frame(
      CRITERION_ID = wqs_id,
      hardness = hardness,
      limit = exp(constant_select$LC * log(hardness) - constant_select$RC)
    )
  }
  return(limits)
}


all_limits <- data.frame()

for(wqs in constants$CRITERION_ID){
  limit_range <- metals_limit(wqs,'range')
  all_limits <- rbind(all_limits,limit_range)
}

write_csv(all_limits,here("www/metal_limits.csv"))


# Plot
ggplot(all_limits)+
  geom_line(aes(x = hardness, y = limit, group = CRITERION_ID, color = CRITERION_ID))
