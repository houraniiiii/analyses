library(dplyr)

# ------------------------------------------------------------------
# 1A.  Columns we were already keeping  (identical to previous reply)
# ------------------------------------------------------------------
cols_to_keep <- c(
  # Identifiers
  "subject_id_std","hadm_id_std","stay_id_std","dataset","gender",
  # LOS / outcome
  "los","hospital_death","hours_to_death",
  # Unified comorbidities / conditions
  "myocardial_infarct_unified","pyelonephritis_unified","mild_liver_disease_unified",
  "rhabdomyolysis_unified","sepsis_unified","severe_liver_disease_unified",
  "sle_unified","obstructive_uropathy_unified","stroke_unified",
  "history_mi_unified","race_unified",
  # Medications / infusions (original list)
  "acei_arb","oral_antidiabetics_count","oral_antidiabetics_flag",
  "dopamine","epinephrine","vasopressin","norepinephrine",
  "dobutamine","milrinone","phenylephrine",
  # Cardiac / renal / transplant history, etc.
  "acs","unstableangina","historyofmi","ahf","chronichf","congestivehf","ca","af",
  "historyofstroke","burn_or_trauma","atrial_flutter",
  "kidney_transplant","heart_transplant","has_valve","liver_transplant",
  "pancreas_transplant","marrow_transplant","other_transplant",
  "kidney_transplant_rejection","heart_transplant_rejection",
  "liver_transplant_rejection","other_transplant_rejection","lung_transplant",
  "v_fib","v_tach","diseases_of_aorta","svt","scleroderma",
  "pyelonephritis","sle","historyofcardiacarrest","rheumatoid_arthritis",
  "historyofpevte","pcihi","cabghi","valvulopathies","mitralvalve",
  "mitralstenosis","mitralinsuff","aorticvalve","aorticstenosis",
  "aorticinsuff","pulmonary_htn","diabetes_two","diabetes_one","dkd",
  "hypertension","pvd","hematological_cancer","hyperlipidemia","copd",
  "chronic_rf","ais","tia","ich","sah","chronic_kidney_failure",
  "mild_liver_disease","malignant_cancer","severe_liver_disease",
  "metastatic_solid_tumor","aids","cirrhosis","immunodeficiency",
  "multiple_myeloma","pancreatic_disease","pad_flag","endocarditis",
  "autoimmune_disease","tamponade","dvt","pe","shock",
  # Previously added lab / vitals blocks
  "troponin_t_max","ntprobnp_max","ck_mb_max","ck_max",
  "pco2_min","pco2_max","po2_min","po2_max",
  "hematocrit_min","hematocrit_max","hematocrit_avg","hematocrit_last",
  "hematocrit_range","hematocrit_slope",
  "hemoglobin_min","hemoglobin_max","hemoglobin_avg","hemoglobin_last",
  "hemoglobin_range","hemoglobin_slope",
  "mch_min","mch_max","mch_avg","mch_last","mch_range","mch_slope",
  "mcv_min","mcv_max","mcv_avg","mcv_last","mcv_range","mcv_slope",
  "platelet_min","platelet_max","platelet_avg","platelet_last",
  "platelet_range","platelet_slope",
  "rbc_min","rbc_max","rbc_avg","rbc_last","rbc_range","rbc_slope",
  "rdw_min","rdw_max","rdw_avg","rdw_last","rdw_range","rdw_slope",
  "creatinine_count","creatinine_min","creatinine_max","creatinine_avg",
  "creatinine_last","creatinine_last_minus_first","creatinine_range",
  "creatinine_max_minus_avg","creatinine_min_minus_avg","creatinine_slope",
  "glucose_count","glucose_min","glucose_max","glucose_avg",
  "glucose_last","glucose_last_minus_first","glucose_range",
  "glucose_max_minus_avg","glucose_min_minus_avg","glucose_slope"
)

# ------------------------------------------------------------------
# 1B.  NEW columns requested in the last message
# ------------------------------------------------------------------
extra_cols <- c(
  # --- Coagulation -------------------------------------------------
  "pt_min","pt_max","pt_avg","ptt_min","ptt_max","ptt_avg",
  # --- White‐blood‐cell panel -------------------------------------
  "wbc_count","wbc_min","wbc_max","wbc_avg","wbc_last",
  "wbc_last_minus_first","wbc_range","wbc_max_minus_avg",
  "wbc_min_minus_avg","wbc_slope",
  # --- Electrolytes / acid–base & metabolites ---------------------
  "aniongap_count","aniongap_min","aniongap_max","aniongap_avg","aniongap_last",
  "aniongap_last_minus_first","aniongap_range","aniongap_max_minus_avg",
  "aniongap_min_minus_avg","aniongap_slope",
  "bicarbonate_count","bicarbonate_min","bicarbonate_max","bicarbonate_avg",
  "bicarbonate_last","bicarbonate_last_minus_first","bicarbonate_range",
  "bicarbonate_max_minus_avg","bicarbonate_min_minus_avg","bicarbonate_slope",
  "bun_count","bun_min","bun_max","bun_avg","bun_last","bun_last_minus_first",
  "bun_range","bun_max_minus_avg","bun_min_minus_avg","bun_slope",
  "calcium_count","calcium_min","calcium_max","calcium_avg","calcium_last",
  "calcium_last_minus_first","calcium_range","calcium_max_minus_avg",
  "calcium_min_minus_avg","calcium_slope",
  "chloride_count","chloride_min","chloride_max","chloride_avg","chloride_last",
  "chloride_last_minus_first","chloride_range","chloride_max_minus_avg",
  "chloride_min_minus_avg","chloride_slope",
  "sodium_count","sodium_min","sodium_max","sodium_avg","sodium_last",
  "sodium_last_minus_first","sodium_range","sodium_max_minus_avg",
  "sodium_min_minus_avg","sodium_slope",
  "potassium_count","potassium_min","potassium_max","potassium_avg",
  "potassium_last","potassium_last_minus_first","potassium_range",
  "potassium_max_minus_avg","potassium_min_minus_avg","potassium_slope",
  "magnesium_count","magnesium_min","magnesium_max","magnesium_avg",
  "magnesium_last","magnesium_last_minus_first","magnesium_range",
  "magnesium_max_minus_avg","magnesium_min_minus_avg","magnesium_slope",
  "phosphate_count","phosphate_min","phosphate_max","phosphate_avg",
  "phosphate_last","phosphate_slope",
  "ph_count","ph_min","ph_max","ph_avg","ph_last","ph_slope",
  "lactate_max","lactate_slope",
  # --- Vitals ------------------------------------------------------
  "heart_rate_min","heart_rate_max","heart_rate_avg","heart_rate_first",
  "heart_rate_last","heart_rate_slope","heart_rate_hours_under_limit_48h",
  "heart_rate_hours_over_limit_48h",
  "sbp_min","sbp_max","sbp_avg","sbp_first","sbp_last","sbp_slope",
  "sbp_hours_under_limit_48h","sbp_hours_over_limit_48h",
  "dbp_min","dbp_max","dbp_avg","dbp_first","dbp_last","dbp_slope",
  "dbp_hours_under_limit_48h","dbp_hours_over_limit_48h",
  "mbp_min","mbp_max","mbp_avg","mbp_first","mbp_last","mbp_slope",
  "mbp_hours_under_limit_48h","mbp_hours_over_limit_48h",
  "respiratory_rate_min","respiratory_rate_max","respiratory_rate_avg",
  "respiratory_rate_first","respiratory_rate_last","respiratory_rate_slope",
  "respiratory_rate_hours_under_limit_48h",
  "respiratory_rate_hours_over_limit_48h",
  "temperature_celsius_min","temperature_celsius_max","temperature_celsius_avg",
  "temperature_celsius_first","temperature_celsius_last",
  "temperature_celsius_slope","temperature_celsius_hours_under_limit_48h",
  "temperature_celsius_hours_over_limit_48h",
  # --- Anthropometrics --------------------------------------------
  "weight","height",
  # --- Transplant flags (alt versions) ----------------------------
  "liver_transplant_1","kidney_transplant_1","heart_transplant_1",
  "lung_transplant_1","pancreas_transplant_1",
  # --- Surgical / procedural --------------------------------------
  "major_abdominal_surgery","any_major_cardiac_procedure",
  "open_cardiac_surgery","perc_cardiac_surgery",
  # --- Medication exposures ---------------------------------------
  "alteplase","antiplatelets","warfarin","doac","heparin","inotropes",
  "vasopressors","mixed_inotropes_vasopressors","amiodarone","atropine",
  "acei","statins","abx_nepr_high","abx_nepr_mod","abx_nepr_low","nsaids",
  "insulin","insulin_human","insulin_rapid","insulin_long","metformin","arb",
  "ccb","diuretics","loop_diuretics","potassium_sparing","thiazides_diuretics",
  "beta_nonselective","beta_cardioselective","beta_alpha_beta","beta_isa",
  "beta_classiii","betablocker","sglt2i","ddp4i","incretin_memitics",
  "sulfonylureas","platinum_chemo","highdose_mtx_sys","ifosfamide_sys",
  "antiviral_nuc_analogs","calcineurin_inhib","lithium_all","pentamidine_sys",
  "iv_bisphosphonate","iv_arsenic_trioxide","ppi_sys","h2_blocker_sys",
  "antiviral_crystal","antiviral_oral_nucs","renal_tma_chemo","mtor_inhib",
  "cyclophosphamide_sys","gold_thiomalate_par","hydralazine_sys",
  "procainamide_any","urate_phosphate_sys",
  # --- Neurologic / GCS -------------------------------------------
  "gcs_min_first_day","gcs_motor_avg","gcs_verbal_avg","gcs_eyes_avg",
  "gcs_unable","gcs_total_min",
  # --- Respiratory support ----------------------------------------
  "ventilated","invasive_vent","hfnc","noninv_vent","suppoxygen","tracheostomy",
  # --- Fluid balance ----------------------------------------------
  "net_fluid_balance_24h","urine_output_24h","intaketotal_sum_24h",
  "outputtotal_sum_24h","nettotal_sum_24h","dialysistotal_sum_24h",
  "total_input_24h","urine_output_measurements_24h","total_iv_input_24h",
  # --- Additional biomarkers / AKI timing -------------------------
  "troponin_i_max","first_aki_3_time_24h_to_7d",
  # --- KDIGO / AKI flags ------------------------------------------
  "early_aki_exclude1","has_kdigo_data_7d","has_kdigo_data_6d","aki_24h_to_7d",
  "first_aki_time_24h_to_7d","aki_3_24h_to_7d","persistent_aki_48h","akd_aki",
  "persistent_aki_3_48h","first_aki_time_3_24h_to_7d","max_aki_stage_24h_to_7d",
  "aki_48h_to_7d","first_aki_time_48h_to_7d","late_aki_after_week1",
  "days_to_first_aki","adequate_followup",
  # --- Misc demographics / context --------------------------------
  "contrast","age","surgical","worst_creatinine","egfr_slope_48h",
  "race","unitvisitnumber","hospitalid","region"
)

# ------------------------------------------------------------------
# 1C.  Merge and de-duplicate the two vectors
# ------------------------------------------------------------------
cols_to_keep <- unique(c(cols_to_keep, extra_cols))

# ------------------------------------------------------------------
# 2.  Verify columns exist in merged_aki
# ------------------------------------------------------------------
missing_cols <- setdiff(cols_to_keep, names(merged_aki))
if (length(missing_cols) > 0) {
  warning(sprintf(
    "The following %d requested columns are not in merged_aki and will be skipped:\n%s",
    length(missing_cols), paste(missing_cols, collapse = ", ")
  ))
  cols_to_keep <- setdiff(cols_to_keep, missing_cols)
}

# ------------------------------------------------------------------
# 3.  Create the pared-down dataset
# ------------------------------------------------------------------
aki_subset <- merged_aki %>% 
  dplyr::select(dplyr::all_of(cols_to_keep))

# Quick sanity check (optional)
glimpse(aki_subset)