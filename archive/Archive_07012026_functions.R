# Functions for calculating RP

##########
# LIMITS #
##########

## Metals

metals_limit <- function(wqs_id = "79228", hardness = "range") {
  constants <- data.frame(
    CRITERION_ID = c("79228","79238","79236","79253","79248","79251","79264"),
    LC = c(0.7977, 0.8190, 0.8545, 1.273, 0.8460, 1.72, 0.8473),
    RC = c(3.909, 0.6848, 1.702, 4.705, 0.0584, 6.59, 0.884)
  )
  
  # Use a different name for the argument (wqs_id) to avoid 
  # filter(CRITERION_ID == CRITERION_ID) which filters everything to TRUE.
  constant_select <- constants %>%
    filter(CRITERION_ID == wqs_id)
  
  # Check if we actually found a match
  if(nrow(constant_select) == 0) stop("ID not found.")
  
  # Vectorized approach: No 'for' loop or 'rbind' needed!
  hardness_presets <- seq(25, 400, 5)
  
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

# Testing
#cadmium <- metals_limit(wqs_id = "79228")

# Example Plot
# ggplot(cadmium,aes(x = hardness, y = limit))+
#   geom_line()+
#   scale_x_continuous(breaks = seq(0,400,50))+
#   labs(x = "Hardness (mg/L as CaCO3)", y = "Cadmium Limit (mg/L)")+
#   theme_bw()




##################
# CROSSWALK PREP #
##################

#' Defensive deduplication of the WQS crosswalk.
#'
#' The source crosswalk CSV is built by joining the NPDES form pollutant list
#' to the PR WQS criterion table using several independent strategies (manual
#' mapping, name match, CAS match). When more than one strategy succeeds for
#' the same criterion, the build process leaves both rows in the file with
#' the same (parameter_code, CRITERION_ID) but different NPDES_Pollutant
#' labels — e.g. for total phosphorus (00665, criterion 79599) the file
#' contains both "Phosphorus" (Manual join) and "Total phosphorus" (Name join).
#' Downstream this produces duplicate rows in the coverage table.
#'
#' This helper collapses those artifacts. The rule is intentionally narrow so
#' that genuine dual labels survive:
#'
#'   - If a (parameter_code, CRITERION_ID) pair appears in multiple rows with
#'     more than one distinct NPDES_WQS_Join method, keep only the row(s)
#'     joined via "Manual" (the authoritative form-name mapping).
#'   - If all rows for a (parameter_code, CRITERION_ID) pair share the same
#'     join method (e.g. pH min/max and temperature summer/winter, both
#'     "Manual"), keep them all — those are intentional dual labels.
#'
#' Safe to call on a crosswalk missing the NPDES_WQS_Join column; it returns
#' the input unchanged in that case.
dedupe_crosswalk <- function(cw) {
  if (is.null(cw) || nrow(cw) == 0) return(cw)
  needed <- c("parameter_code", "CRITERION_ID", "NPDES_WQS_Join")
  if (!all(needed %in% names(cw))) return(cw)

  cw %>%
    dplyr::group_by(parameter_code, CRITERION_ID) %>%
    dplyr::mutate(.n_joins = dplyr::n_distinct(NPDES_WQS_Join)) %>%
    dplyr::filter(.n_joins == 1 | NPDES_WQS_Join == "Manual") %>%
    dplyr::select(-.n_joins) %>%
    dplyr::ungroup()
}


#' Render a water-class label, combining the base class with sub-class when
#' present.
#'
#' Some water classes carry a sub-class distinction whose WQS values differ
#' within the same class — e.g. PR's class SD waters have separate criteria
#' for streams vs reservoirs/lakes for total nitrogen, total phosphorus, and
#' selenium. The sub_class column on the crosswalk captures that distinction;
#' this helper produces the display string used wherever water class is shown
#' to the user (Findings RP table, chart legend, report kables).
#'
#' Designed to be jurisdiction-agnostic. Today only PR class SD waters have
#' populated sub_class values, but the column is generic and will work for
#' any future state/region that introduces analogous sub-types.
#'
#' @param class      Vector of USE_CLASS_NAME_LOCATION_ETC values
#' @param sub_class  Vector of sub_class values (NA or "" for "no sub-type")
#' @param sep        Separator between class and sub-class (default " — ")
#' @return character vector of display labels
format_water_class <- function(class, sub_class = NA, sep = " — ") {
  has_sub <- !is.na(sub_class) & nzchar(as.character(sub_class))
  ifelse(has_sub,
         paste0(as.character(class), sep, as.character(sub_class)),
         as.character(class))
}


###############
# CONVERSIONS #
###############

#' Resolve a reported DMR unit against a target WQS unit.
#'
#' Returns a list(mult, flag) describing how to convert the DMR value into the
#' WQS unit, plus a status flag that drives both the Coverage Summary display
#' and which rows enter the RP calculation. The six possible flags are:
#'
#'   NA              — units match (identity or alias); use value as-is
#'   PASS            — multiplicative conversion succeeded; multiply by mult
#'   CONVERT_F_TO_C  — temperature; caller applies the (F-32)*5/9 offset
#'   EXCLUDED        — mass-load or flow unit; not comparable to a concentration
#'                     criterion. Dropped from RP by design, not an error.
#'   NO_WQS          — no WQS criterion exists for this parameter in the
#'                     selected water class (target is NA). Dropped from RP;
#'                     specific reason is surfaced in the Needs Attention tab.
#'   FAIL            — genuine unit problem (reported unit not recognised and
#'                     not on the mass-load list). Dropped from RP; the row is
#'                     retained in the exported DMR table with its flag.
#'
#' Rows flagged NA or PASS are the ones the downstream RP filter keeps; every
#' other status is excluded from the RWC math while still being preserved in
#' the dataset for transparency.
get_unit_conversion <- function(reported, target) {
  # NA/blank target = no WQS criterion for this parameter in the selected
  # water class. Distinguish from a true unit mismatch.
  target_missing <- is.null(target) || is.na(target) ||
    identical(trimws(as.character(target)), "")
  reported_missing <- is.null(reported) || is.na(reported) ||
    identical(trimws(as.character(reported)), "")

  if (target_missing) {
    if (reported_missing) return(list(mult = 1, flag = "FAIL"))
    return(list(mult = 1, flag = "NO_WQS"))
  }
  if (reported_missing) {
    return(list(mult = 1, flag = "FAIL"))
  }

  rep_raw  <- trimws(as.character(reported))
  targ_raw <- trimws(as.character(target))
  rep  <- tolower(rep_raw)
  targ <- tolower(targ_raw)

  # ── Special-method placeholder ─────────────────────────────────────────────
  # Method == "Special" criteria (e.g. TAN, criterion 79613) carry "[no units]"
  # in the crosswalk because the limit is recomputed at RP-runtime from pH and
  # temperature. The reported DMR unit (typically mg/L for TAN) doesn’t need
  # to be converted; the runtime calculation produces a value in the same unit
  # space. Treat as a clean match so the row enters RP. Hardness-metal Special
  # criteria carry their real target unit (ug/L) and fall through to the
  # identity / conversion branches normally.
  if (targ == "[no units]") {
    return(list(mult = 1, flag = NA_character_))
  }

  # ── Mass-load and flow units ─────────────────────────────────────────────────────
  # Effluent load and volumetric flow measurements are structurally
  # incompatible with concentration-based WQS criteria. They are excluded
  # from the WQS analysis by design — not a data error. Includes both
  # abbreviated and long-form (ICIS) spellings since both appear in the data.
  mass_load_units <- c(
    "kg/d", "gal/d", "mgd", "m3/d", "lbs/d",
    "cubic meters per day"
  )
  if (rep %in% mass_load_units) {
    return(list(mult = 1, flag = "EXCLUDED"))
  }

  # ── Identity, including aliases for the same physical unit ────────────────
  # Some physical units have multiple string representations in the data
  # (e.g. the bacterial enumeration unit "#/100mL" appears as both "#/100mL"
  # and the ICIS long form "Number per 100 Milliliters"). Treat any pair
  # within an alias group as a clean match — no numeric conversion needed.
  identity_groups <- list(
    c("color units", "col unit (pc)"),
    c("#/100ml", "number per 100 milliliters", "mpn/100ml")
  )
  for (grp in identity_groups) {
    if (rep %in% grp && targ %in% grp) {
      return(list(mult = 1, flag = NA_character_))
    }
  }
  if (rep == targ) {
    return(list(mult = 1, flag = NA_character_))
  }

  # ── Known concentration / scale conversions ───────────────────────────────
  # Multiplicative conversions (value_in_target_units = value_reported * mult).
  # ppm is treated as the aqueous mg/L equivalent (1 ppm = 1 mg/L = 1000 ug/L);
  # the long-form ICIS spelling "Parts per Million" is matched as well.
  mult <- dplyr::case_when(
    rep == "ug/l"  & targ == "mg/l"  ~  0.001,
    rep == "mg/l"  & targ == "ug/l"  ~  1000,
    rep == "ng/l"  & targ == "ug/l"  ~  0.001,
    rep == "ng/l"  & targ == "mg/l"  ~  0.000001,
    rep %in% c("ppm", "parts per million") & targ == "mg/l" ~ 1,
    rep %in% c("ppm", "parts per million") & targ == "ug/l" ~ 1000,
    TRUE ~ NA_real_
  )

  if (!is.na(mult)) {
    return(list(mult = mult, flag = "PASS"))
  }

  # ── Temperature offset conversion (non-multiplicative) ─────────────────────
  # Caller applies (value - 32) * 5/9 when it sees CONVERT_F_TO_C.
  is_fahrenheit <- rep %in% c("deg f", "\u00b0f", "f", "fahrenheit",
                               "degrees f", "degree f", "degrees fahrenheit")
  is_celsius    <- targ %in% c("deg c", "\u00b0c", "c", "celsius",
                                "degrees c", "degree c", "degrees celsius")
  if (is_fahrenheit && is_celsius) {
    return(list(mult = NA_real_, flag = "CONVERT_F_TO_C"))
  }

  # ── Unrecognised concentration unit ────────────────────────────────────────────
  # Reached only when the reported unit is NOT a known mass-load/flow unit
  # and no conversion rule matches. Genuine data-quality issue.
  return(list(mult = 1, flag = "FAIL"))
}

# Example usage on a data frame `dmr` with columns dmr_value_nmbr, dmr_unit_desc, and WQS UNIT_NAME column UNIT_NAME:
# dmr <- dmr %>%
#   mutate(
#     value_wqs = convert_to_wgs(dmr_value_nmbr, dmr_unit_desc, UNIT_NAME,
#                                flow_mgd = 2.5,          # if needed for kg/d -> ug/L
#                                DO_sat   = 8.26)         # if needed for % -> ug/L (DO only)
#   )




#################
# CALCULATE RWC #
#################

# Minimal RWC calculator for concentration-based parameters
# - Assumes units already standardized to WQS units
# - Assumes non-concentration parameters have been removed
# - Returns one row per parameter with all variables needed to reproduce the MF/RWC formulas

# TESTING
#library(tidyr)
#library(dplyr)
# dmr_files <- list.files("data/dmr", full.names = TRUE)
# 
# dmr_all <- read_csv(dmr_files)%>%
#   filter(
#     statistical_base_type_code == "MAX" |
#       (parameter_code == "00400" & statistical_base_type_code %in% c("MAX","MIN"))
#   )%>%
#   mutate(dmr_value_nmbr = ifelse(nodi_code %in% c("B","Q"),0,dmr_value_nmbr))%>%
#   drop_na(dmr_value_nmbr)
# 
# 
# 
# count_samples <- dmr_all%>%
#   drop_na(dmr_value_nmbr)%>%
#   group_by(npdes_id,parameter_code)%>%
#   summarise(nSamples = n())%>%
#   ungroup()%>%
#   filter(nSamples > 20)%>%
#   group_by(npdes_id)%>%
#   mutate(nParams = length(unique(parameter_code)))%>%
#   ungroup()%>%
#   filter(nParams > 10)
# 
# dmr <- dmr_all%>%
#   filter(npdes_id == "PR0026042")
# 
#dilution_ratio <- 1
#confidence_level <- 0.95
#target_percentile <- 0.95

compute_rwc <- function(dmr,
                        dilution_ratio = 1,
                        confidence_level = 0.95,
                        target_percentile = 0.95) {
  # Local MF helper (lognormal projection)
  rp_calc_mf <- function(x, CL = 0.95, P_target = 0.95) {
    x <- x[is.finite(x) & x > -1]
    n <- length(x)
    if (n == 0) {
      return(list(
        n = 0, cv = NA_real_, sigma_g = NA_real_,
        p = NA_real_, z_p = NA_real_, z_target = NA_real_,
        MF = NA_real_
      ))
    }
    # CV policy
    if (n < 10) {
      cv <- 0.6
    } else {
      mu  <- mean(x)
      sdv <- stats::sd(x)
      cv  <- if (!is.finite(mu) || mu == 0 || !is.finite(sdv)) 0.6 else sdv / mu
    }
    sigma_g  <- sqrt(log(cv^2 + 1))
    p        <- (1 - CL)^(1 / n)
    z_p      <- stats::qnorm(p)
    z_target <- stats::qnorm(P_target)
    MF       <- exp((z_target - z_p) * sigma_g)
    if (MF < 1) MF <- 1
    list(
      n = n, cv = cv, sigma_g = sigma_g,
      p = p, z_p = z_p, z_target = z_target,
      MF = MF
    )
  }
  
  stopifnot(
    "parameter_code"    %in% names(dmr),
    "dmr_value_nmbr"    %in% names(dmr),
    "perm_feature_nmbr" %in% names(dmr)
  )
  dmr[["dmr_value_nmbr"]]    <- suppressWarnings(as.numeric(dmr[["dmr_value_nmbr"]]))
  dmr[["perm_feature_nmbr"]] <- as.character(dmr[["perm_feature_nmbr"]])
  
  dplyr::group_by(dmr, .data$parameter_code, .data$perm_feature_nmbr) %>%
    dplyr::summarise(
      parameter_desc = if ("parameter_desc" %in% names(dmr)) dplyr::first(.data$parameter_desc) else NA_character_,
      .vals    = list(.data$dmr_value_nmbr[is.finite(.data$dmr_value_nmbr) & .data$dmr_value_nmbr > -1]),
      n_used   = length(.vals[[1]]),
      min_value = if (n_used > 0) min(.vals[[1]], na.rm = TRUE) else NA_real_,
      max_value  = if (n_used > 0) max(.vals[[1]], na.rm = TRUE) else NA_real_,
      mean_value = if (n_used > 0) mean(.vals[[1]], na.rm = TRUE) else NA_real_,
      sd_val   = if (n_used > 0) stats::sd(.vals[[1]], na.rm = TRUE) else NA_real_,
      mf       = list(rp_calc_mf(.vals[[1]], CL = confidence_level, P_target = target_percentile)),
      cv          = mf[[1]]$cv,
      sigma_g     = mf[[1]]$sigma_g,
      p_percentile= mf[[1]]$p,
      z_p         = mf[[1]]$z_p,
      z_target    = mf[[1]]$z_target,
      MF          = mf[[1]]$MF,
      MF = if(MF <1) 1 else MF,
      RWC_rs      = if (is.finite(MF) && is.finite(max_value) && dilution_ratio > 0) (max_value * MF) / dilution_ratio else NA_real_,
      dilution_ratio    = dilution_ratio,
      confidence_level  = confidence_level,
      target_percentile = target_percentile,
      notes = if (n_used == 0) "no positive values" else NA_character_,
      .groups = "drop"
    )
}

# dmr_std <- dmr_all%>%
#   filter(npdes_id == "PR0026042")%>%
#   mutate(monitoring_period_end_date = mdy(monitoring_period_end_date ))%>%
#   filter(monitoring_period_end_date > mdy("03/11/2021") & monitoring_period_end_date < mdy("03/12/2026"))
# 
# rwc_df <- compute_rwc(dmr = dmr_std,
#                       dilution_ratio = 1,
#                       confidence_level = 0.95,
#                       target_percentile = 0.95)

#########################################
# CALCULATE TOTAL AMMONIA NITRATE (TAN) #
#########################################

calc_tan_limits <- function(df) {
  # Requires columns: monitoring_period_end_date, pH, tempC, perm_feature_nmbr
  stopifnot(
    "monitoring_period_end_date" %in% names(df),
    "pH"                 %in% names(df),
    "tempC"              %in% names(df),
    "perm_feature_nmbr"  %in% names(df)
  )
  
  pH    <- suppressWarnings(as.numeric(df$pH))
  tempC <- suppressWarnings(as.numeric(df$tempC))
  
  # PR WQS Rule 1303.2(C)(2)(l):
  # TAN = 0.8876 * (0.0278/(1+10^(7.688-pH)) + 1.1994/(1+10^(pH-7.688))) * (2.126 * 10^(0.028*(20-T)))
  term1       <- 0.0278 / (1 + 10^(7.688 - pH))
  term2       <- 1.1994 / (1 + 10^(pH - 7.688))
  temp_factor <- 2.126 * 10^(0.028 * (20 - tempC))
  tan_mgN_L   <- 0.8876 * (term1 + term2) * temp_factor
  
  # Any rows with NA pH or T produce NA TAN
  bad <- !is.finite(pH) | !is.finite(tempC)
  tan_mgN_L[bad] <- NA_real_
  
  data.frame(
    monitoring_period_end_date = df$monitoring_period_end_date,
    perm_feature_nmbr          = as.character(df$perm_feature_nmbr),
    pH                         = pH,
    tempC                      = tempC,
    CRITERION_ID               = "79613",
    CRITERION_VALUE            = tan_mgN_L,
    stringsAsFactors           = FALSE
  )
}

################
# CALCULATE RP #
################

calc_rp <- function(rwc_table = rwc, c_limits = rv$crosswalk,
                    m_limits = NA, tan_limits = NA){
  concentration_limits <- c_limits%>%
    select(CRITERION_ID,CRITERION_VALUE)
}



#####################################
# COMBINE ALPHA AND BETA ENDOSULFAN #
#####################################
calculate_critical_hardness <- function(wqs_id, rwc_rs) {
  constants <- data.frame(
    CRITERION_ID = c("79228","79238","79236","79253","79248","79251","79264"),
    LC = c(0.7977, 0.8190, 0.8545, 1.273, 0.8460, 1.72, 0.8473),
    RC = c(3.909, 0.6848, 1.702, 4.705, 0.0584, 6.59, 0.884)
  )
  
  con <- constants[constants$CRITERION_ID == wqs_id, ]
  if(nrow(con) == 0) return(NA)
  
  # Solve: rwc_rs = exp(LC * log(H) - RC)
  # log(rwc_rs) = LC * log(H) - RC
  # log(H) = (log(rwc_rs) + RC) / LC
  critical_h <- exp((log(rwc_rs) + con$RC) / con$LC)
  return(critical_h)
}





###############################################
# RECOGNIZED CONCENTRATION UNITS FOR MANUAL  #
# WQS ENTRY (must be handleable by           #
# get_unit_conversion)                       #
###############################################

#' Returns the vector of concentration units that get_unit_conversion() can
#' handle.  Used to restrict the unit selector in the manual-WQS review panel.
recognized_concentration_units <- function() {
  c("mg/L", "ug/L", "ng/L", "deg C", "SU")
}


###############################################
# FLAG UNMATCHED PARAMETERS                  #
###############################################

#' Identify parameters in rv$dmr that have no usable WQS entry in the
#' water-class-filtered crosswalk (crosswalk_full).
#'
#' Returns a data frame with one row per unmatched parameter_code containing:
#'   parameter_code, parameter_desc, case_reason, n_samples, unique_units
#'
#' Case reasons:
#'   "No crosswalk entry"      – code not present in crosswalk at all (Case 2)
#'   "Wrong water class"       – code exists in crosswalk but not for selected
#'                               water class(es) (Case 1)
#'   "Missing criterion value" – code + class match exists but CRITERION_VALUE
#'                               is NA (Case 3)
#'
#' @param dmr            rv$dmr data frame (all fetched records, nothing dropped)
#' @param crosswalk_full crosswalk filtered by water class only (not by form)
#' @param crosswalk_all  the full unfiltered startup crosswalk object
#' @param dmr_parameters the dmr_parameters lookup (parameter_code, parameter_desc)
flag_unmatched_params <- function(dmr, crosswalk_full, crosswalk_all, dmr_parameters) {

  # Parameters present in the fetched DMR. pH (00400), temperature (00010),
  # turbidity (00070), and color (00080) used to be force-skipped here
  # because pH/temp were also force-fed through RP regardless of water class.
  # That force-feed has been removed (the class filter now applies uniformly),
  # so these parameters are evaluated like any other: matched if they have a
  # usable WQS criterion in the selected class, flagged otherwise.
  #
  # Additionally, drop any parameter whose every DMR record is unit-EXCLUDED
  # (mass-load / flow units like kg/d, MGD, "Cubic Meters per Day"). Those
  # records cannot be compared to a concentration criterion regardless of
  # whether a WQS exists for the parameter, so asking the user to "resolve"
  # them on the Needs Attention tab is meaningless — they will never enter
  # the RP calculation. The check is row-level on Conv_Flag so partially
  # mass-load parameters (some kg/d, some mg/L) are still flagged if the
  # mg/L portion needs a criterion.
  dmr_codes <- dmr %>%
    {
      if ("Conv_Flag" %in% names(.)) {
        dplyr::group_by(., parameter_code) %>%
          dplyr::filter(!all(Conv_Flag == "EXCLUDED", na.rm = TRUE)) %>%
          dplyr::ungroup()
      } else .
    } %>%
    dplyr::distinct(parameter_code)

  if (nrow(dmr_codes) == 0) return(dplyr::tibble(
    parameter_code  = character(),
    parameter_desc  = character(),
    case_reason     = character(),
    n_samples       = integer(),
    unique_units    = character()
  ))

  # A crosswalk row is "usable" if it has a non-NA CRITERION_VALUE, OR if it
  # is a Method == "Special" criterion (TAN, hardness metals, etc.) whose
  # value is computed at RP-runtime. Special-method rows must not be flagged
  # as "Missing criterion value" because the empty CRITERION_VALUE is by
  # design — it gets filled in by the runtime calculation.
  has_method_col <- "Method" %in% names(crosswalk_full)
  matched_codes <- crosswalk_full %>%
    {
      if (has_method_col) {
        dplyr::filter(., !is.na(CRITERION_VALUE) |
                         (!is.na(Method) & Method == "Special"))
      } else {
        dplyr::filter(., !is.na(CRITERION_VALUE))
      }
    } %>%
    dplyr::distinct(parameter_code) %>%
    dplyr::pull(parameter_code)

  # Codes that exist in crosswalk_full but every non-Special row has a NA
  # CRITERION_VALUE (Case 3 — "Missing criterion value"). Special-method
  # rows are excluded from this all_na test, since their NA is by design.
  all_na_codes <- crosswalk_full %>%
    {
      if (has_method_col) {
        dplyr::filter(., is.na(Method) | Method != "Special")
      } else {
        .
      }
    } %>%
    dplyr::group_by(parameter_code) %>%
    dplyr::summarise(all_na = all(is.na(CRITERION_VALUE)), .groups = "drop") %>%
    dplyr::filter(all_na) %>%
    dplyr::pull(parameter_code)

  # Codes that exist somewhere in the full crosswalk (any water class) – Case 1
  any_class_codes <- crosswalk_all %>%
    dplyr::distinct(parameter_code) %>%
    dplyr::pull(parameter_code)

  # Build the flagged set
  flagged <- dmr_codes %>%
    dplyr::filter(!parameter_code %in% matched_codes) %>%
    dplyr::mutate(
      case_reason = dplyr::case_when(
        parameter_code %in% all_na_codes                              ~ "Missing criterion value",
        parameter_code %in% any_class_codes                          ~ "Wrong water class",
        TRUE                                                          ~ "No crosswalk entry"
      )
    )

  if (nrow(flagged) == 0) return(dplyr::tibble(
    parameter_code  = character(),
    parameter_desc  = character(),
    case_reason     = character(),
    n_samples       = integer(),
    unique_units    = character()
  ))

  # Sample counts + unique units from the DMR
  dmr_meta <- dmr %>%
    dplyr::filter(parameter_code %in% flagged$parameter_code) %>%
    dplyr::group_by(parameter_code) %>%
    dplyr::summarise(
      n_samples    = dplyr::n(),
      unique_units = paste(
        sort(unique(dmr_unit_desc[!is.na(dmr_unit_desc) & dmr_unit_desc != ""])),
        collapse = ", "
      ),
      .groups = "drop"
    )

  # Human-readable name — try three sources in priority order:
  #   1. dmr_parameters lookup (REF_PARAMETER from ICIS)
  #   2. parameter_desc already in the DMR data (from SQLite query)
  #   3. raw parameter_code as last resort
  desc_lookup <- dmr_parameters %>%
    dplyr::distinct(parameter_code, parameter_desc)

  # Also pull desc directly from DMR for the fallback
  dmr_desc <- dmr %>%
    dplyr::filter(parameter_code %in% flagged$parameter_code) %>%
    dplyr::distinct(parameter_code, parameter_desc) %>%
    dplyr::filter(!is.na(parameter_desc), parameter_desc != "") %>%
    dplyr::rename(parameter_desc_dmr = parameter_desc)

  flagged %>%
    dplyr::left_join(dmr_meta,    by = "parameter_code") %>%
    dplyr::left_join(desc_lookup, by = "parameter_code") %>%
    dplyr::left_join(dmr_desc,    by = "parameter_code") %>%
    dplyr::mutate(
      parameter_desc = dplyr::coalesce(parameter_desc,
                                       parameter_desc_dmr,
                                       parameter_code),
      n_samples      = tidyr::replace_na(n_samples, 0L),
      unique_units   = tidyr::replace_na(unique_units, "")
    ) %>%
    dplyr::select(parameter_code, parameter_desc, case_reason, n_samples, unique_units)
}


###############################################
# BUILD FORM LOOKUP                          #
###############################################

#' Build a lookup table: parameter_code -> comma-separated form labels
#' for the forms that were selected by the user.
#'
#' @param crosswalk_form  crosswalk filtered by water class AND forms
#'                        (i.e. rv$crosswalk after form join)
#' @param selected_forms  character vector of selected form IDs
#'                        (e.g. c("2C","2D"))
build_form_lookup <- function(crosswalk_form, selected_forms) {
  crosswalk_form %>%
    dplyr::filter(!is.na(Form), Form %in% selected_forms) %>%
    dplyr::distinct(parameter_code, Form) %>%
    dplyr::group_by(parameter_code) %>%
    dplyr::summarise(
      associated_forms = paste(sort(unique(Form)), collapse = " & "),
      .groups = "drop"
    )
}


###############################################
# BUILD MANUAL CROSSWALK ROWS                #
###############################################

#' Convert the rv$wqs_overrides decisions (decision == "include") into
#' synthetic crosswalk rows that can be bound into rv$crosswalk before
#' Run RP executes.
#'
#' @param overrides   data frame produced by the review panel with columns:
#'   parameter_code, parameter_desc, decision, criterion_value, unit,
#'   water_classes (character, comma-separated), criterion_type, notes
#' @param npdes_pollutant_lookup  optional named vector parameter_code->NPDES_Pollutant;
#'   if NULL, parameter_desc is used as NPDES_Pollutant
build_manual_crosswalk_rows <- function(overrides, npdes_pollutant_lookup = NULL) {

  # Guard against NULL or empty overrides (normal when no review panel decisions made)
  if (is.null(overrides) || nrow(overrides) == 0) return(NULL)

  includes <- overrides %>%
    dplyr::filter(decision == "include") %>%
    dplyr::filter(!is.na(criterion_value), !is.na(unit))

  if (nrow(includes) == 0) return(NULL)

  # Expand one row per water class (water_classes is comma-separated string)
  includes %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      wc_list = list(trimws(strsplit(water_classes, ",")[[1]]))
    ) %>%
    dplyr::ungroup() %>%
    tidyr::unnest(wc_list) %>%
    dplyr::rename(USE_CLASS_NAME_LOCATION_ETC = wc_list) %>%
    dplyr::mutate(
      NPDES_Pollutant  = if (!is.null(npdes_pollutant_lookup))
                           dplyr::coalesce(npdes_pollutant_lookup[parameter_code], parameter_desc)
                         else parameter_desc,
      CRITERION_ID     = paste0("MANUAL_", parameter_code),
      CRITERION_VALUE  = criterion_value,
      UNIT_NAME        = unit,
      Method           = "Max",          # default; manual criteria are upper-bound
      manually_entered = TRUE,
      manual_notes     = notes,
      criterion_type   = criterion_type,
      Form             = NA_character_,  # not form-associated
      sub_class        = NA_character_   # manual entries don't carry sub-class
    ) %>%
    dplyr::select(
      NPDES_Pollutant, CRITERION_ID, CRITERION_VALUE, UNIT_NAME,
      parameter_code, USE_CLASS_NAME_LOCATION_ETC,
      Method, manually_entered, manual_notes, criterion_type, Form,
      sub_class
    )
}


#########################
# Render Math Functions #
#########################

render_math_formula <- function(f, dr, confidence_level) {
  # Extract values
  n_val  <- f$n_used %||% NA_real_
  cv_val <- f$cv %||% NA_real_
  sg_val <- f$sigma_g %||% NA_real_
  p_val  <- f$p_percentile %||% NA_real_
  zp_val <- f$z_p %||% NA_real_
  zt_val <- f$z_target %||% NA_real_
  mf_val <- f$MF %||% NA_real_
  max_num <- if ("max_value" %in% names(f) && is.finite(f$max_value)) f$max_value else f$max_val
  dr      <- dr
  rwc_num <- if ("RWC_rs" %in% names(f) && is.finite(f$RWC_rs)) f$RWC_rs
  else if (is.finite(max_num) && is.finite(mf_val) && is.finite(dr) && dr > 0) (max_num * mf_val) / dr
  else NA_real_
  
  core_ok <- all(is.finite(c(cv_val, sg_val, p_val, zp_val, zt_val, mf_val))) &&
    is.finite(dr) &&
    (is.finite(max_num) || is.finite(rwc_num))
  if (!core_ok) return(NULL)
  
  withMathJax(
    tags$div(
      tags$p(sprintf("$$\\sigma_g = \\sqrt{\\ln(%s^2+1)} = %s$$",
                     format(signif(cv_val,2), scientific = FALSE),
                     format(signif(sg_val,2), scientific = FALSE))),
      tags$p(sprintf("$$p = (1 - %s)^{1/%s} = %s$$",
                     format(signif(confidence_level,2), scientific = FALSE),
                     format(n_val),
                     format(signif(p_val,2), scientific = FALSE))),
      tags$p(sprintf("$$z_p = %s,\\; z_{\\text{target}} = %s$$",
                     format(signif(zp_val,2), scientific = FALSE),
                     format(signif(zt_val,2), scientific = FALSE))),
      tags$p(sprintf("$$\\mathrm{MF} = \\exp\\big((%s - %s)\\times %s\\big) = %s$$",
                     format(signif(zt_val,2), scientific = FALSE),
                     format(signif(zp_val,2), scientific = FALSE),
                     format(signif(sg_val,2), scientific = FALSE),
                     format(signif(mf_val,2), scientific = FALSE))),
      tags$p(sprintf(
        "$$\\mathrm{RWC} = \\frac{\\max(x)\\times MF}{DR} = \\frac{%s\\times%s}{%s} = %s$$",
        if (is.finite(max_num)) format(signif(max_num, 6), scientific = FALSE) else "\\max(x)",
        format(signif(mf_val,  6), scientific = FALSE),
        format(signif(dr,      6), scientific = FALSE),
        if (is.finite(rwc_num)) format(signif(rwc_num, 6), scientific = FALSE) else ""
      ))
    )
  )
}
