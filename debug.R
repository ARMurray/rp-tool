library(tidyverse)
library(echor)
library(lubridate)
library(here)
# Load crosswalk
crosswalk <- read_csv("data/crosswalk.csv",
                      col_types = cols(parameter_code = col_character()))%>%
  mutate(parameter_code = str_pad(parameter_code,5,"left",pad = "0"))

# Load NPDES forms
npdes_forms <- read_csv("data/NPDES_Forms_Pollutants_1.csv")

crosswalk_filt <- crosswalk%>%
  left_join(npdes_forms, by = c("NPDES_Pollutant" = "Pollutant"))
  filter(Form %in% c("2C","2F"))


start_fmt <- format(Sys.Date() - years(5),"%m/%d/%Y")
end_fmt <- format(Sys.Date(),"%m/%d/%Y")

permit_id <- "PR0026671"


# Check mercury / class SD / outfall 001


dmr_raw <- echoGetEffluent(p_id = permit_id,
                  output = "df",
                  start_date = start_fmt,
                  end_date = end_fmt)%>%
  mutate(monitoring_period_end_date = mdy(monitoring_period_end_date))
    filter(parameter_code %in% crosswalk_filt$parameter_code)%>%
    filter(
      perm_feature_type_code == "EXO",
      statistical_base_type_code == "MAX" |
        (parameter_code == "00400" & statistical_base_type_code %in% c("MAX","MIN"))
    )%>%
  mutate(dmr_value_nmbr = as.numeric(dmr_value_nmbr),
         dmr_value_nmbr = ifelse(nodi_code %in% c("B","Q"),0,dmr_value_nmbr))%>%
  drop_na(dmr_value_nmbr)





limits <- dmr_raw%>%
  select(parameter_code,parameter_desc,limit_value_nmbr,limit_unit_desc)%>%
  mutate(limit_value_nmbr = as.numeric(limit_value_nmbr))%>%
  distinct()%>%
  drop_na(limit_value_nmbr)

# Checking unit conversion and flagging
dmr_clean <- dmr_raw %>%
  # Standardize codes first
  mutate(parameter_code = str_pad(parameter_code, 5, "left", "0")) %>%
  # Clean up missing units
  group_by(parameter_code)%>%
  # Join with crosswalk to see what the WQS unit should be
  left_join(crosswalk %>% select(parameter_code, UNIT_NAME) %>% distinct(), 
            by = "parameter_code") %>%
  rowwise() %>%
  mutate(
    conv_data = list(get_unit_conversion(dmr_unit_desc, UNIT_NAME)),
    Conv_Flag = conv_data$flag,
    # Perform the math
    dmr_value_nmbr = dmr_value_nmbr * conv_data$mult
  ) %>%
  ungroup()%>%
  # Temporary selection to inspect
  #select(parameter_desc,parameter_code,dmr_unit_desc,UNIT_NAME,conv_data,Conv_Flag,dmr_value_nmbr)
  select(-conv_data, -UNIT_NAME) # Clean up helper columns





# Forms 2C/2F
# Class SB and SD

crosswalk_filt <- crosswalk%>%
  left_join(npdes_forms, by = c("NPDES_Pollutant" = "Pollutant"), relationship = "many-to-many")%>%
  filter(Form %in% c("2C","2F") | parameter_code %in% c("00010","00400"))%>%
  filter(USE_CLASS_NAME_LOCATION_ETC %in% c("class SD waters", "class SD waters- drinking water","class SB waters") | parameter_code %in% c("00010","00400"))


# Get Criterion IDS for metals (match to current crosswalk)
wqs_metals <- crosswalk %>%
  dplyr::select(CRITERION_ID, parameter_code) %>%
  dplyr::mutate(CRITERION_ID = as.character(CRITERION_ID)) %>%
  dplyr::filter(
    CRITERION_ID %in% c("79228","79238","79236","79253","79248","79251","79264") &
      CRITERION_ID %in% crosswalk_filt$CRITERION_ID
  )

cat("\nRetrieved metals crosswalk")

# Split into non-concentration-based (ncb) and concentration-based (cb)
ncb <- dplyr::filter(dmr_clean, parameter_code %in% c("00070","00010","00080","00400"))
cat(paste0("\nfiltered ncb -- ", length(unique(ncb$parameter_code)), " parameters & ", nrow(ncb), " samples"))

cb <- dmr_clean %>%
  dplyr::filter(!parameter_code %in% c("00070","00010","00080","00400")) %>%
  dplyr::filter(is.na(Conv_Flag) | Conv_Flag == "PASS")

cat("\nfiltered cb")

# Calculate RWC for concentration-based parameters
rwc <- compute_rwc(
  cb,
  dilution_ratio = 1,
  confidence_level = 0.95,
  target_percentile = 0.95
)

cat("\nrwc calculated")

# Metals limits (always using numeric hardness for RP)
metals_filt <- wqs_metals %>%
  dplyr::filter(parameter_code %in% cb$parameter_code)
hardness_setting <- 100

if (nrow(metals_filt) > 0) {
  all_metals_limits <- metal_limits_cache  # cache loaded at startup
  
  # hardness_setting <- suppressWarnings(as.numeric(input$hardness_value))
  # if (!is.finite(hardness_setting)) {
  #   showNotification("Invalid hardness value; using default 100 mg/L as CaCO3.", type = "warning")
  #   hardness_setting <- 100
  # }
  
  limits_filt <- all_metals_limits %>%
    dplyr::filter(CRITERION_ID %in% metals_filt$CRITERION_ID) %>%
    dplyr::mutate(hardness_set = hardness_setting,
                  diff = abs(hardness_set - hardness)) %>%
    dplyr::group_by(CRITERION_ID) %>%
    dplyr::slice_min(diff, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(CRITERION_ID, limit)
  
  hardness <- hardness_setting
}

cat("\n Made it past metals limits")



cat("\n Joining rwc to criterion IDS")

# Build base rwc_criteria (crosswalk x rwc by parameter_code)
rwc_criteria <- dplyr::select(
  crosswalk_filt,
  NPDES_Pollutant, CRITERION_ID, CRITERION_VALUE, UNIT_NAME, parameter_code, USE_CLASS_NAME_LOCATION_ETC
) %>%
  dplyr::mutate(CRITERION_ID = as.character(CRITERION_ID)) %>%
  dplyr::left_join(rwc, by = "parameter_code", relationship = "many-to-many") %>%
  dplyr::mutate(CRITERION_ID = as.character(CRITERION_ID))

if (nrow(metals_filt) > 0) {
  rwc_criteria <- rwc_criteria %>%
    dplyr::mutate(CRITERION_ID = as.character(CRITERION_ID)) %>%
    dplyr::left_join(
      limits_filt %>% dplyr::mutate(CRITERION_ID = as.character(CRITERION_ID)),
      by = "CRITERION_ID"
    ) %>%
    dplyr::mutate(
      CRITERION_VALUE = dplyr::if_else(!is.na(limit), limit, CRITERION_VALUE)
    ) %>%
    dplyr::select(-limit)
}


rp_concentration_findings <- rwc_criteria %>%
  #dplyr::left_join(max_dmr_vals, by = "parameter_code")
  dplyr::mutate(
    RP = ifelse(max_value > CRITERION_VALUE, "YES", "NO")
  )






copper <- dmr_clean%>%
  filter(parameter_code == "01042")


mercury <- dmr_clean%>%
  filter(perm_feature_nmbr == '001' & parameter_code == "71900")

rwc <- compute_rwc(mercury)

ph <- dmr_clean%>%
  filter(parameter_code == "00400")


# Debugging crashing issue on SB waters
permit_id <- "PR0001031"
start_fmt <- format(ymd("2016-04-30"),"%m/%d/%Y")
end_fmt <- format(ymd("2024-02-29"),"%m/%d/%Y")

crosswalk_filt <- crosswalk%>%
  left_join(npdes_forms, by = c("NPDES_Pollutant" = "Pollutant"))%>%
  filter(Form %in% c("2C","2F") & USE_CLASS_NAME_LOCATION_ETC == "class SB waters")

raw <- echoGetEffluent(p_id = permit_id,
                       output = "df",
                       start_date = start_fmt,
                       end_date   = end_fmt) %>%
  filter(parameter_code %in% crosswalk_filt$parameter_code | parameter_code %in% c("00010","00400")) %>%
  filter(
    perm_feature_type_code == "EXO",
    statistical_base_type_code == "MAX" |
      (parameter_code == "00400" & statistical_base_type_code %in% c("MAX","MIN"))
  ) %>%
  mutate(
    dmr_value_nmbr = as.numeric(dmr_value_nmbr),
    dmr_unit_desc  = as.character(dmr_unit_desc)
  )


dom_units <- raw %>%
  filter(!is.na(dmr_unit_desc), dmr_unit_desc != "") %>%
  group_by(parameter_code, dmr_unit_desc) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  slice_max(n, with_ties = FALSE) %>%
  ungroup() %>%
  select(parameter_code, dominant_unit = dmr_unit_desc)

# (2) Flag parameters where ALL rows have missing/blank units
all_missing_tbl <- raw %>%
  group_by(parameter_code) %>%
  summarise(all_units_missing = all(is.na(dmr_unit_desc) | dmr_unit_desc == ""), .groups = "drop")

# (3) Bring in UNIT_NAME from crosswalk for fallback
unit_hint <- crosswalk_filt %>%
  distinct(parameter_code, UNIT_NAME)

dmr_raw <- raw %>%
  left_join(dom_units, by = "parameter_code") %>%
  left_join(all_missing_tbl, by = "parameter_code") %>%
  left_join(unit_hint, by = "parameter_code") %>%
  mutate(
    # Set value for NODI B/Q to 0 (your policy)
    dmr_value_nmbr = ifelse(nodi_code %in% c("B","Q"), 0, dmr_value_nmbr),
    # Fill units:
    # - If all rows for this parameter have missing units, use UNIT_NAME for all missing/blank units
    # - Else, for NODI rows with missing unit, use dominant_unit when available
    dmr_unit_desc = case_when(
      all_units_missing & (is.na(dmr_unit_desc) | dmr_unit_desc == "") ~ UNIT_NAME,
      nodi_code %in% c("B","Q") & (is.na(dmr_unit_desc) | dmr_unit_desc == "") & !is.na(dominant_unit) ~ dominant_unit,
      TRUE ~ dmr_unit_desc
    )
  ) %>%
  select(-dominant_unit, -all_units_missing, -UNIT_NAME) %>%
  drop_na(dmr_value_nmbr)

dmr <- dmr_raw %>%
  # Standardize codes first
  mutate(parameter_code = str_pad(parameter_code, 5, "left", "0")) %>%
  # Join with crosswalk to see what the WQS unit should be
  left_join(crosswalk %>% select(parameter_code, UNIT_NAME) %>% distinct(), 
            by = "parameter_code") %>%
  rowwise() %>%
  mutate(
    conv_data = list(get_unit_conversion(dmr_unit_desc, UNIT_NAME)),
    Conv_Flag = conv_data$flag,
    # Perform the math
    dmr_value_nmbr = dmr_value_nmbr * conv_data$mult
  ) %>%
  ungroup() %>%
  select(-conv_data, -UNIT_NAME) # Clean up helper columns

# Get Criterion IDS for metals
wqs_metals <- crosswalk%>%
  select(CRITERION_ID,parameter_code)%>%
  filter(CRITERION_ID %in% c("79228","79238","79236","79253","79248","79251","79264") & CRITERION_ID %in% crosswalk_filt$CRITERION_ID)

cat("Retrieved metals crosswalk")
# Split dmr into concentration based and non-concentration based data frames

# Non-concentration based
# Turbidity: '00070', Temp: '00010', color: '00080', pH: '00400'
ncb <- filter(dmr,
              parameter_code %in% c('00070','00010','00080','00400'))

cat(paste0("filtered ncb -- ",length(unique(ncb$parameter_code))," parameters & ",nrow(ncb)," samples"))


# Concentration Based
cb <- dmr_clean %>%
  dplyr::filter(!parameter_code %in% c("00070","00010","00080","00400")) %>%
  dplyr::filter(is.na(Conv_Flag) | Conv_Flag == "PASS")



rwc <- compute_rwc(
  cb,
  dilution_ratio = 1,
  confidence_level = 0.95,
  target_percentile = 0.95
)
  dplyr::mutate(
    MF     = ifelse(MF < 1, 1, MF),
    RWC_rs = ifelse(RWC_rs < max_value, max_value, RWC_rs)
  )






# Check if we have metals data and calculate limits
metals_filt <- wqs_metals %>%
  filter(parameter_code %in% cb$parameter_code)

cat(paste0("\nchecked for metals data - Found: ",
           paste(unique(metals_filt$parameter_code), collapse = ", ")))



metals_dmr <- dmr%>%
  filter(parameter_code %in% wqs_metals$parameter_code)










coliform <- dmr_raw%>%
  filter(parameter_code == "74055")


dmr_files <- list.files("data/dmr", full.names = TRUE)
dmr_all <- read_csv(dmr_files)
unique(dmr_all$dmr_unit_desc)

# Checking metals
wqs_metals <- crosswalk%>%
  select(CRITERION_ID,parameter_code,POLLUTANT_NAME)%>%
  filter(CRITERION_ID %in% c("79228","79238","79236","79253","79248","79251","79264"))

metals_filt <- wqs_metals%>%
  filter(parameter_code %in% dmr_clean$parameter_code)

cb <- filter(dmr_clean,
             !parameter_code %in% c('00070','00010','00080','00400'))%>%
  filter(is.na(Conv_Flag) | Conv_Flag == "PASS")

metals_dmr <- dmr_clean%>%
  filter(parameter_code %in% metals_filt$parameter_code)

if(nrow(metals_filt)>0){
  cat("\n Calculating metals limits")
  metals_limits <- data.frame()
  
  for(metal in unique(metals_filt$CRITERION_ID)){
    limit <- metals_limit(wqs_id = metal, hardness = 50)
    metals_limits <- rbind(metals_limits,limit)
  }
}

copper <- dmr_clean%>%
  filter(parameter_code == "01042")%>%
  select(parameter_code,parameter_desc,dmr_value_nmbr,dmr_unit_desc,monitoring_period_end_date)%>%
  arrange(monitoring_period_end_date)

hardness_mode <- "range"
# Determine hardness input: "range" (default) or numeric value
hardness_setting <- if (!is.null(hardness_mode) &&
                        hardness_mode == "set" &&
                        is.finite(suppressWarnings(as.numeric(hardness_value)))) {
  as.numeric(hardness_value)
} else {
  "range"
}
  

# Simplify classes
# pH
pH_stats <- dmr_clean %>%
  dplyr::filter(parameter_code == "00400") %>%
  dplyr::group_by(parameter_code, perm_feature_nmbr) %>%
  dplyr::summarise(
    max_pH = max(dmr_value_nmbr, na.rm = TRUE),
    min_pH = min(dmr_value_nmbr, na.rm = TRUE),
    .groups = "drop"
  )

pH_findings <- crosswalk %>%
  mutate(CRITERION_ID = as.character(CRITERION_ID))%>%
  dplyr::filter(CRITERION_ID %in% c('79595','79606')) %>%
  dplyr::select(NPDES_Pollutant, CRITERION_ID, Method, CRITERION_VALUE, parameter_code, USE_CLASS_NAME_LOCATION_ETC) %>%
  dplyr::left_join(pH_stats, by = "parameter_code") %>%
  dplyr::mutate(
    RWC_rs = dplyr::if_else(Method == "Max", max_pH, min_pH),
    RP = dplyr::case_when(
      Method == "Max" & RWC_rs > CRITERION_VALUE ~ "YES",
      Method == "Max" & RWC_rs <= CRITERION_VALUE ~ "NO",
      Method == "Min" & RWC_rs < CRITERION_VALUE ~ "YES",
      TRUE ~ "NO"
    ),
    RP = as.character(RP)
  )


# Checking inspector plot
ph_temp <- dmr_raw %>%
  filter(parameter_code %in% c("00400","00010"))%>%
  group_by(parameter_code,monitoring_period_end_date, perm_feature_nmbr) %>%
    summarise(meanVal = mean(dmr_value_nmbr,na.rm = TRUE))%>%
    ungroup()%>%
  mutate(label = ifelse(parameter_code == "00010","tempC","pH"))%>%
  select(!parameter_code)%>%
  pivot_wider(names_from = label,values_from = meanVal)


check <- dmr_clean%>%
  filter(parameter_code == "00400" & perm_feature_nmbr == "001" & statistical_base_type_code == "MAX")




obs <- dmr_clean%>%
  filter(parameter_code == "00400" & statistical_base_type_code == "MIN")




ph_dmr <- dmr_clean %>%
  dplyr::filter(parameter_code == "00400") %>%
  dplyr::select(parameter_code, perm_feature_nmbr,dmr_value_nmbr,statistical_base_type_code, monitoring_period_end_date)%>%
  mutate(NPDES_Pollutant = ifelse(statistical_base_type_code == "MAX", "pH (maximum)",
                                  ifelse(statistical_base_type_code== "MIN","pH (minimum)",NA)))%>%
  drop_na()


pH_findings <- crosswalk %>%
  dplyr::mutate(CRITERION_ID = as.character(CRITERION_ID)) %>%
  dplyr::filter(CRITERION_ID %in% c("79595","79606") & parameter_code == "00400")%>%
  dplyr::select(NPDES_Pollutant, CRITERION_ID, Method, CRITERION_VALUE, UNIT_NAME, parameter_code, USE_CLASS_NAME_LOCATION_ETC) %>%
  dplyr::left_join(ph_dmr, by = c("parameter_code","NPDES_Pollutant"))%>%
  drop_na()%>%
  group_by(NPDES_Pollutant,CRITERION_ID,Method,CRITERION_VALUE,UNIT_NAME,parameter_code,USE_CLASS_NAME_LOCATION_ETC,perm_feature_nmbr)%>%
  summarise(max_value = max(dmr_value_nmbr, na.rm = TRUE),
            min_value = min(dmr_value_nmbr, na.rm = TRUE))%>%
  ungroup()%>%
  mutate(RP = ifelse(Method == "Max" & max_value > CRITERION_VALUE,"YES",
                     ifelse(Method == "Min" & min_value < CRITERION_VALUE,"YES","NO")))

unit_colors <- c("deg C"="#1b9e77","mg/L"="#d95f02","SU"="#7570b3","mL/L"="#e7298a","ug/L"="#66a61e")
class_colors <- c("class SB waters"="#1f78b4","class SD waters- drinking water"="#33a02c",
                  "class SG waters- drinking water"="#e31a1c","class SD waters"="#ff7f00",
                  "class SG waters"="#6a3d9a","surface waters"="#b2df8a")


sub_rp <- pH_findings%>%
  dplyr::filter(NPDES_Pollutant == "pH (minimum)",
                perm_feature_nmbr == "001")


rwc_val <- sub_rp$RWC_rs[1]
wqs_lines <- sub_rp %>% dplyr::select(USE_CLASS_NAME_LOCATION_ETC, CRITERION_VALUE) %>% dplyr::distinct()

ggplot(obs, aes(x = monitoring_period_end_date, y = dmr_value_nmbr)) +
  geom_line(color = "lightgrey", linetype = "dotted", alpha = 0.5) +
  # Existing WQS lines (you may choose to exclude CRITERION_ID 79613 from wqs_lines if it’s now variable)
  geom_hline(data = wqs_lines, aes(yintercept = CRITERION_VALUE, color = USE_CLASS_NAME_LOCATION_ETC),
             linetype = "dashed", linewidth = 0.7) +
  # RWC line
  geom_hline(aes(yintercept = rwc_val, color = "Calculated RWC"), linetype = "solid", linewidth = 1) +
  # TAN variable limit line (if present)
  geom_point(aes(color = dmr_unit_desc,
                 text = paste0("Date: ", monitoring_period_end_date,
                               "<br>Value: ", round(dmr_value_nmbr, 4), " ", dmr_unit_desc,
                               "<br>Limit: ", limit_value_nmbr)), size = 2) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_color_manual(
    name = "Legend",
    values = c(class_colors,
               "Calculated RWC" = "#2c7fb8",
               "TAN Limit (variable)" = "#bf5b17",  # choose a distinct color
               unit_colors)
  ) +
  labs(title = paste("Analysis for", "pH (minimum)", "– Outfall", "001"),
       x = "Date", y = sub_rp$UNIT_NAME[1]) +
  theme_minimal()
    

# Temperature findings
tempFindings <- NULL

  
tempStats <- dmr_clean %>%
  dplyr::filter(parameter_code == "00010") %>%
  dplyr::group_by(parameter_code, perm_feature_nmbr) %>%
  dplyr::summarise(maxTemp = max(dmr_value_nmbr, na.rm = TRUE), .groups = "drop")

tempFindings <- crosswalk %>%
  dplyr::mutate(CRITERION_ID = as.character(CRITERION_ID)) %>%
  dplyr::filter(parameter_code == "00010") %>%
  dplyr::select(NPDES_Pollutant, CRITERION_ID, Method, CRITERION_VALUE, parameter_code, USE_CLASS_NAME_LOCATION_ETC) %>%
  dplyr::left_join(tempStats, by = "parameter_code") %>%
  dplyr::mutate(
    RWC_rs = maxTemp,
    RP = ifelse(maxTemp > CRITERION_VALUE, "YES", "NO"),
    RP = as.character(RP)
  )%>%
  distinct()

