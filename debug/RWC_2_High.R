library(tidyverse)
library(echor)
library(tidyr)
library(plotly)
library(here)

# Load helper functions
source("R/functions.R")

permit_id <- "PR0025984"

# Load metals limits
metal_limits_cache <- read_csv("www/metal_limits.csv", show_col_types = FALSE)

# Load crosswalk
crosswalk <- read_csv("data/full_join.csv",
                      col_types = cols(parameter_code = col_character()))%>%
  mutate(parameter_code = str_pad(parameter_code,5,"left",pad = "0"))

# Load NPDES forms
npdes_forms <- read_csv("data/NPDES_Forms_Pollutants_1.csv")

crosswalk_filt <- crosswalk%>%
  left_join(npdes_forms, by = c("NPDES_Pollutant" = "Pollutant"))%>%
  filter(Form %in% c("2C","2F"))


start_fmt <- format(Sys.Date() - years(5),"%m/%d/%Y")
end_fmt <- format(Sys.Date(),"%m/%d/%Y")


# Check mercury / class SB / outfall 001


dmr_raw <- echoGetEffluent(p_id = permit_id,
                           output = "df",
                           start_date = start_fmt,
                           end_date = end_fmt)%>%
  mutate(monitoring_period_end_date = mdy(monitoring_period_end_date))%>%
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


crosswalk_filt <- crosswalk%>%
  left_join(npdes_forms, by = c("NPDES_Pollutant" = "Pollutant"), relationship = "many-to-many")%>%
  filter(Form %in% c("2C","2F") | parameter_code %in% c("00010","00400"))%>%
  filter(USE_CLASS_NAME_LOCATION_ETC %in% c("class SB waters") | parameter_code %in% c("00010","00400"))


# Get Criterion IDS for metals (match to current crosswalk)
wqs_metals <- crosswalk %>%
  dplyr::select(CRITERION_ID, parameter_code) %>%
  dplyr::mutate(CRITERION_ID = as.character(CRITERION_ID)) %>%
  dplyr::filter(
    CRITERION_ID %in% c("79228","79238","79236","79253","79248","79251","79264") &
      CRITERION_ID %in% crosswalk$CRITERION_ID
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

if (nrow(metals_filt) > 0) {
  all_metals_limits <- metal_limits_cache  # cache loaded at startup
  
  hardness_setting <- 100
  if (!is.finite(hardness_setting)) {
    showNotification("Invalid hardness value; using default 100 mg/L as CaCO3.", type = "warning")
    hardness_setting <- 100
  }
  
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

# Calculate TAN Limits
if (any(as.character(crosswalk$CRITERION_ID) == "79613", na.rm = TRUE)) {
  pH    <- 6.1
  tempC <- 30
  
  if (!is.finite(pH) || !is.finite(tempC)) {
    showNotification("TAN limit not updated: provide numeric pH and Temperature.", type = "warning", duration = 8)
  } else {
    term1       <- 0.0278 / (1 + 10^(7.688 - pH))
    term2       <- 1.1994 / (1 + 10^(pH - 7.688))
    temp_factor <- 2.126 * 10^(0.028 * (20 - tempC))
    tan_mgN_L   <- (term1 + term2) * temp_factor
    
    # Overwrite in rv$crosswalk safely
    crosswalk <- crosswalk %>%
      dplyr::mutate(
        CRITERION_ID = as.character(CRITERION_ID),
        CRITERION_VALUE = dplyr::if_else(CRITERION_ID == "79613", tan_mgN_L, CRITERION_VALUE)
      )
  }
}

cat("\n Joining rwc to criterion IDS")

# Build base rwc_criteria (crosswalk x rwc by parameter_code)
rwc_criteria <- dplyr::select(
  crosswalk,
  NPDES_Pollutant, CRITERION_ID, CRITERION_VALUE, UNIT_NAME, parameter_code, USE_CLASS_NAME_LOCATION_ETC
) %>%
  dplyr::mutate(CRITERION_ID = as.character(CRITERION_ID)) %>%
  dplyr::left_join(rwc, by = "parameter_code") %>%
  dplyr::mutate(CRITERION_ID = as.character(CRITERION_ID))

# pH findings
pH_findings <- NULL
if ("00400" %in% dmr_clean$parameter_code) {
  ph_dmr <- dmr_clean %>%
    dplyr::filter(parameter_code == "00400") %>%
    dplyr::select(parameter_code, perm_feature_nmbr,dmr_value_nmbr,dmr_unit_desc,limit_value_nmbr,limit_begin_date,limit_end_date,statistical_base_type_code, monitoring_period_end_date)%>%
    mutate(NPDES_Pollutant = ifelse(statistical_base_type_code == "MAX", "pH (maximum)",
                                    ifelse(statistical_base_type_code== "MIN","pH (minimum)",NA)))%>%
    drop_na()
  
  ph_dmr <- ph_dmr
  
  pH_findings <- crosswalk %>%
    dplyr::mutate(CRITERION_ID = as.character(CRITERION_ID)) %>%
    dplyr::filter(CRITERION_ID %in% c("79595","79606") & parameter_code == "00400")%>%
    dplyr::select(NPDES_Pollutant, CRITERION_ID, Method, CRITERION_VALUE, UNIT_NAME, parameter_code, USE_CLASS_NAME_LOCATION_ETC) %>%
    dplyr::left_join(ph_dmr, by = c("parameter_code","NPDES_Pollutant"))%>%
    drop_na()%>%
    group_by(NPDES_Pollutant,CRITERION_ID,Method,CRITERION_VALUE,UNIT_NAME,parameter_code,USE_CLASS_NAME_LOCATION_ETC,perm_feature_nmbr)%>%
    summarise(max_value = max(dmr_value_nmbr, na.rm = TRUE),
              min_value = min(dmr_value_nmbr, na.rm = TRUE),
              mean_value = mean(dmr_value_nmbr, na.rm = TRUE))%>%
    ungroup()%>%
    mutate(RP = ifelse(Method == "Max" & max_value > CRITERION_VALUE,"YES",
                       ifelse(Method == "Min" & min_value < CRITERION_VALUE,"YES","NO")),
           RWC_rs = ifelse(Method == "Max",max_value,min_value))
}

# Temperature findings
tempFindings <- NULL
if ("00010" %in% dmr_clean$parameter_code) {
  temp_dmr <- dmr_clean %>%
    dplyr::filter(parameter_code == "00010") %>%
    mutate(NPDES_Pollutant = "Temperature")%>%
    dplyr::select(NPDES_Pollutant, parameter_code, perm_feature_nmbr,dmr_value_nmbr,dmr_unit_desc,limit_value_nmbr,limit_begin_date,limit_end_date,statistical_base_type_code, monitoring_period_end_date)%>%
    drop_na()
  
  tempFindings <- crosswalk %>%
    dplyr::mutate(CRITERION_ID = as.character(CRITERION_ID)) %>%
    dplyr::filter(CRITERION_ID == "79218") %>%
    dplyr::select(CRITERION_ID, Method, CRITERION_VALUE, UNIT_NAME, parameter_code, USE_CLASS_NAME_LOCATION_ETC) %>%
    dplyr::left_join(temp_dmr, by = "parameter_code") %>%
    group_by(NPDES_Pollutant, CRITERION_ID,Method,CRITERION_VALUE,UNIT_NAME,parameter_code,USE_CLASS_NAME_LOCATION_ETC,perm_feature_nmbr)%>%
    summarise(min_value = min(dmr_value_nmbr, na.rm = TRUE),
              mean_value = mean(dmr_value_nmbr, na.rm = TRUE),
              max_value = max(dmr_value_nmbr, na.rm = TRUE))%>%
    ungroup()%>%
    mutate(RP = ifelse(max_value > CRITERION_VALUE,"YES", "NO"),
           RWC_rs = max_value)
  
  temp_dmr <- temp_dmr
  
}

cat("\n joining metals")

# Join numeric hardness-based limits into rwc_criteria (no range placeholders)
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

cat("\n Finding maximums\n")

# Max value per parameter × outfall
# max_dmr_vals <- dmr_clean %>%
#   dplyr::group_by(parameter_code, perm_feature_nmbr) %>%
#   dplyr::summarise(max_value = max(dmr_value_nmbr, na.rm = TRUE), .groups = "drop")

#cat(paste0("columns in max_dmr_vals: ",paste(colnames(max_dmr_vals), collapse = ", ")))

cat("\n Determining RP")


cat(paste0("\n rwc_criteria columns: ",paste(colnames(rwc_criteria),collapse = ", "),"\n"))
# Base RP (YES/NO)
rp_concentration_findings <- rwc_criteria %>%
  #dplyr::left_join(max_dmr_vals, by = "parameter_code")%>%
  dplyr::mutate(RP = ifelse(max_value > CRITERION_VALUE, "YES", "NO"))

cat(paste0("\ncolumns in joined data: ",paste(colnames(rp_concentration_findings), collapse = ", ")))



# Combine all findings (bind_rows drops NULL automatically)
all_findings <- dplyr::bind_rows(
  rp_concentration_findings,
  pH_findings,
  tempFindings
)

rp_concentration <- all_findings

# Update outfall choices for the Findings UI
outfalls <- rp_concentration %>%
  dplyr::distinct(perm_feature_nmbr) %>%
  dplyr::arrange(perm_feature_nmbr) %>%
  dplyr::pull()

updateSelectInput(session, "selected_outfall",
                  choices = outfalls,
                  selected = outfalls[[1]])

current_page("rp")
cat("\n All RP functions succesful")


mercury <- dmr_clean%>%
  filter(parameter_code == "71900")
