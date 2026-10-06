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




###############
# CONVERSIONS #
###############

get_unit_conversion <- function(reported, target) {
  # Handle NA/blank inputs up front
  if (is.null(reported) || is.null(target) ||
      is.na(reported)   || is.na(target)) {
    return(list(mult = 1, flag = "FAIL"))
  }
  rep_raw <- trimws(as.character(reported))
  targ_raw <- trimws(as.character(target))
  if (identical(rep_raw, "") || identical(targ_raw, "")) {
    return(list(mult = 1, flag = "FAIL"))
  }
  
  rep <- tolower(rep_raw)
  targ <- tolower(targ_raw)
  
  # Identity
  if (rep == targ) {
    return(list(mult = 1, flag = NA_character_))  # matched units; no conversion needed
  }
  
  # Known mappings
  mult <- dplyr::case_when(
    rep == "ug/l" & targ == "mg/l" ~ 0.001,
    rep == "mg/l" & targ == "ug/l" ~ 1000,
    rep == "ng/l" & targ == "ug/l" ~ 0.001,
    rep == "ng/l" & targ == "mg/l" ~ 0.000001,
    TRUE ~ NA_real_
  )
  
  if (is.na(mult)) {
    # Unknown mapping
    return(list(mult = 1, flag = "FAIL"))
  } else {
    return(list(mult = mult, flag = "PASS"))
  }
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
