library(dplyr)
library(tidyr)

## ═══════════════════════════════════════════════════════════════════════════
## STEP 1: CREATE CONSOLIDATED FLAGS FROM COLUMN GROUPS
## ═══════════════════════════════════════════════════════════════════════════

# Helper function to create binary flags from column groups
create_flag <- function(df, cols) {
  if (length(cols) == 0) return(rep.int(0L, nrow(df)))
  cols <- intersect(cols, names(df))  # only existing columns
  if (length(cols) == 0) return(rep.int(0L, nrow(df)))
  
  tmp <- as.data.frame(df)[, cols, drop = FALSE]
  tmp[is.na(tmp)] <- 0
  as.integer(rowSums(tmp) > 0)
}

# Helper function to safely count across columns
count_or_zero <- function(df, cols) {
  cols <- intersect(cols, names(df))  # only existing columns
  if (length(cols) == 0) return(rep(0L, nrow(df)))
  rowSums(as.data.frame(df)[, cols, drop = FALSE], na.rm = TRUE)
}

# Define column groups
transplant_cols <- grep("_transplant(?:_1)?$", names(aki_subset), value = TRUE, perl = TRUE)
transplant_rej_cols <- grep("_transplant_rejection$", names(aki_subset), value = TRUE)
autoimmune_cols <- c("rheumatoid_arthritis", "autoimmune_disease", "sle_unified", "sle", "scleroderma")
cardiac_hist_cols <- c("historyofmi", "pcihi", "cabghi")
invasive_vent_cols <- c("invasive_vent", "tracheostomy")
noninv_vent_cols <- c("hfnc", "noninv_vent")
stroke_cols <- c("tia", "sah", "ais", "ich")

# Drug categories for nephrotoxin exposure
high_risk_drugs <- c("abx_nepr_high", "antiviral_crystal", "calcineurin_inhib", "contrast",
                     "highdose_mtx_sys", "iv_arsenic_trioxide", "nsaids", "pentamidine_sys", "platinum_chemo")
moderate_risk_drugs <- c("abx_nepr_mod", "acei_arb", "antiviral_nuc_analogs", "antiviral_oral_nucs",
                         "cyclophosphamide_sys", "iv_bisphosphonate", "lithium_all", "loop_diuretics",
                         "mtor_inhib", "potassium_sparing", "ppi_sys", "thiazides_diuretics")
low_risk_drugs <- c("abx_nepr_low", "alteplase", "amiodarone", "antiplatelets", "atropine", "betablocker",
                    "ccb",  "gold_thiomalate_par", "h2_blocker_sys", "heparin", "hydralazine_sys",
                    "inotropes", "insulin", "oral_antidiabetics_flag",
                    "procainamide_any",  "statins", "urate_phosphate_sys", 
                    "vasopressors", "warfarin")

# Create all flags and counts in one step
aki_subset <- aki_subset %>%
  dplyr::mutate(
    # Replace NA with 0 for relevant columns
    dplyr::across(dplyr::all_of(intersect(c(transplant_cols, transplant_rej_cols, autoimmune_cols, 
                                            cardiac_hist_cols, high_risk_drugs, moderate_risk_drugs, 
                                            low_risk_drugs), names(.))), 
                  ~ tidyr::replace_na(.x, 0)),
    
    # Create consolidated flags
    any_transplant = create_flag(pick(everything()), transplant_cols),
    any_transplant_rejection = create_flag(pick(everything()), transplant_rej_cols),
    any_autoimmune = create_flag(pick(everything()), autoimmune_cols),
    any_cardiac_history = create_flag(pick(everything()), cardiac_hist_cols),
    invasive_vent = create_flag(pick(everything()), invasive_vent_cols),
    noninv_vent = create_flag(pick(everything()), noninv_vent_cols),
    suppoxygen = create_flag(pick(everything()), "suppoxygen"),
    stroke = create_flag(pick(everything()), stroke_cols),
    
    # Create drug exposure counts and flags
    high_risk_drug_count = count_or_zero(pick(everything()), high_risk_drugs),
    any_high_risk_drug = as.integer(high_risk_drug_count > 0),
    moderate_risk_drug_count = count_or_zero(pick(everything()), moderate_risk_drugs),
    any_moderate_risk_drug = as.integer(moderate_risk_drug_count > 0),
    low_risk_drug_count = count_or_zero(pick(everything()), low_risk_drugs),
    any_low_risk_drug = as.integer(low_risk_drug_count > 0),
    
    # Convert heart rate to numeric
    heart_rate_hours_over_limit_48h = as.numeric(heart_rate_hours_over_limit_48h)
  )

## ═══════════════════════════════════════════════════════════════════════════
## STEP 2: COMPREHENSIVE COLUMN REMOVAL
## ═══════════════════════════════════════════════════════════════════════════

nm <- names(aki_subset)

# Pattern-based deletions
pattern_drops <- c(
  grep("(_max_minus_avg|_min_minus_avg|_last_minus_first)$", nm, value = TRUE),
  grep("_slope$", nm, value = TRUE) %>% 
    setdiff(grep("^(heart_rate|sbp|dbp|mbp|respiratory_rate|temperature_celsius)_slope$", nm, value = TRUE)) %>%
    setdiff(grep("egfr", nm, value = TRUE, ignore.case = TRUE)),
  grep("_range$", nm, value = TRUE),
  grep("_hours_(over|under)_limit_48h$", nm, value = TRUE),
  grep("^(heart_rate|sbp|dbp|mbp|respiratory_rate|temperature_celsius)_(first|last)$", nm, value = TRUE),
  grep("_avg$", nm, value = TRUE) %>% setdiff("temperature_celsius_avg"),
  grep("^temperature_celsius_", nm, value = TRUE) %>% setdiff("temperature_celsius_avg"),
  grep("^anion_gap_", nm, value = TRUE),
  grep("^(troponin|pt|ptt|lactate|po2|pco2|ck_mb|ck|ntprobnp)", nm, value = TRUE),
  grep("_last$", nm, value = TRUE),
  # Count variables EXCEPT medication counts
  grep("_count$", nm, value = TRUE) %>% 
    setdiff(c("high_risk_drug_count", "moderate_risk_drug_count", "low_risk_drug_count"))
)

# Explicit column names to remove
explicit_drops <- intersect(c(
  # Original source columns that were consolidated into flags
  transplant_cols, transplant_rej_cols, autoimmune_cols, cardiac_hist_cols,
  invasive_vent_cols, noninv_vent_cols, stroke_cols,
  
  # Extra drops from original code
  "adequate_followup",  "rhabdomyolysis_unified",
  "mitralstenosis", "multiple_myeloma", "pyelonephritis_unified", "v_fib", "tia",
  "v_tach", "atrial_flutter", "historyofstroke", "unstableangina", "historyofcardiacarrest",
  "aids", "dkd", "has_valve", "pcihi", "cabghi", "historyofpevte", "aorticstenosis",
  "mitralinsuff", "cirrhosis", "ddp4i", "sulfonylureas", "metformin", "doac", "warfarin",
  "has_kdigo_data_7d", "has_kdigo_data_6d", "gold_thiomalate_par", "race",
  
  # Lab values and biomarkers
  "wbc_max_minus_avg", "wbc_min_minus_avg", "chloride_last_minus_first",
  "chloride_max_minus_avg", "aniongap_last_minus_first", "aniongap_max_minus_avg",
  "aniongap_min_minus_avg", "bicarbonate_last_minus_first", "bicarbonate_max_minus_avg",
  "bicarbonate_min_minus_avg", "wbc_last_minus_first", "magnesium_last_minus_first",
  "magnesium_min_minus_avg", "magnesium_max_minus_avg", "worst_creatinine", 
  "mcv_min" ,"mcv_max",  "mch_min", "mch_max", "hematocrit_max" , "hemoglobin_max",
  "platelet_max", "rbc_max", "rdw_min", "wbc_min", "angiongap_min", "bicarbonate_max",
  "calcium_max", "chloride_min", "magnesium_min", "phosphate_min", "ph_max", "pulmonary_htn",
   
  
  # Specific nephrotoxin columns
  "antiviral_crystal", "calcineurin_inhib", "highdose_mtx_sys", "iv_arsenic_trioxide",
  "pentamidine_sys", "platinum_chemo", "abx_nepr_mod", "acei", "antiviral_nuc_analogs",
  "antiviral_oral_nucs", "arb", "cyclophosphamide_sys", "iv_bisphosphonate", "lithium_all",
  "mtor_inhib", "ppi_sys", "abx_nepr_low", "beta_alpha_beta", "beta_classiii", "beta_isa",
  "ccb", "ddp4i", "gold_thiomalate_par", "h2_blocker_sys", "incretin_memitics",
  "insulin_human", "insulin_long", "insulin_rapid", "metformin", "procainamide_any",
  "sglt2i", "sulfonylureas", "urate_phosphate_sys", "aorticinsuff", "hematological_cancer",
  "dvt", "endocarditis", "suppoxygen",
  
  # AKI timing and staging
  "early_aki_exclude1",  "akd_aki",
   "aki_48h_to_7d",
  "first_aki_time_48h_to_7d", "late_aki_after_week1", "days_to_first_aki",
  
  # Fluid balance variables
  "intaketotal_sum_24h", "outputtotal_sum_24h",
  "dialysistotal_sum_24h",  "urine_output_measurements_24h",
  "total_iv_input_24h", "total_input_24h",  "net_fluid_balance_24h", 
  "urine_output_24h",  "nettotal_sum_24h", "total_input_24h",
  "phosphate_max",
  "ph_min",
  "hours_to_death",
  "dopamine",
  "epinephrine",
  "vasopressin",
  "norepinephrine",
  "dobutamine",
  "milrinone",
  "phenylephrine",
  
  # Clinical variables
  "troponin_i_max", "diuretics", "nsaids", "acs", "ahf", "chronichf", "antiplatelets",
  "mild_liver_disease", "severe_liver_disease", "tamponade", "mixed_inotropes_vasopressors",
  "mitralvalve", "aorticvalve", "ifosfamide_sys", "renal_tma_chemo", "gcs_unable",
  "gcs_total_min", "diseases_of_aorta", "pyelonephritis", "alteplase", "immunodeficiency",
  "statins", "beta_nonselective", "beta_cardioselective", "chronic_rf"
), nm)

# Remove all identified columns
cols_to_remove <- unique(c(pattern_drops, explicit_drops))
aki_subset <- aki_subset %>%
  dplyr::select(-dplyr::all_of(cols_to_remove))

## ═══════════════════════════════════════════════════════════════════════════
## SUMMARY
## ═══════════════════════════════════════════════════════════════════════════
cat("Final dataset created with", ncol(aki_subset), "columns\n")
cat("Preserved medication count variables:\n")
cat("- high_risk_drug_count\n- moderate_risk_drug_count\n- low_risk_drug_count\n")

glimpse(aki_subset)
colnames(aki_subset)
