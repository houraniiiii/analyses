# efficient row-wise merge of MIMIC-III, MIMIC-IV and eICU extracts
# → adds “dataset” identifier ‑> harmonises columns with same names

library(data.table)


## ------------------------------------------------------------------
## 1. Load   --------------------------------------------------------
## fread() already returns a data.table, but setDT() keeps us safe
mimic_iii <- fread("mimic_iii.csv"); setDT(mimic_iii)
mimic_iv  <- fread("mimic_iv.csv");  setDT(mimic_iv)
eicu      <- fread("eicu.csv");      setDT(eicu)

## ------------------------------------------------------------------
## 2. Harmonise names  +  add identifier  ---------------------------
clean_add_tag <- function(dt, tag){
  setnames(dt, tolower(gsub("[^[:alnum:]]+", "_", names(dt))))
  dt[, dataset := tag]
  dt[]
}

mimic_iii <- clean_add_tag(mimic_iii, "mimic_iii")
mimic_iv  <- clean_add_tag(mimic_iv,  "mimic_iv")
eicu      <- clean_add_tag(eicu,      "eicu")

## ------------------------------------------------------------------
## 3. Reconcile column classes  -------------------------------------
##   – for every column that appears in ≥2 datasets and whose classes
##     differ, coerce all occurrences to character
reconcile_classes <- function(dts){
  all_cols <- Reduce(union, lapply(dts, names))
  
  for (col in all_cols){
    # classes present for this column
    cls <- unique(na.omit(sapply(dts, \(dt)
                                 if (col %in% names(dt)) class(dt[[col]])[1] else NA)))
    
    # if mixed classes → convert every dataset’s column to character
    if (length(cls) > 1){
      for (i in seq_along(dts)){
        if (col %in% names(dts[[i]]))
          set(dts[[i]], j = col, value = as.character(dts[[i]][[col]]))
      }
    }
    
    # optional: drop factor attributes even when not mixed
    if (length(cls) == 1 && cls == "factor"){
      for (i in seq_along(dts)){
        if (col %in% names(dts[[i]]))
          set(dts[[i]], j = col, value = as.character(dts[[i]][[col]]))
      }
    }
  }
  dts
}

list_dts <- reconcile_classes(list(mimic_iii, mimic_iv, eicu))
mimic_iii <- list_dts[[1]]
mimic_iv  <- list_dts[[2]]
eicu      <- list_dts[[3]]

## ------------------------------------------------------------------
## 4. Row-bind keeping all columns  ---------------------------------
combined <- rbindlist(
  list(mimic_iii, mimic_iv, eicu),
  use.names   = TRUE,
  fill        = TRUE,
  ignore.attr = TRUE          # now safe; class conflicts already solved
)

# ensure identifier is the first column
setcolorder(combined, c("dataset", setdiff(names(combined), "dataset")))

# combined is ready
print(combined)

# (optional) keep 'dataset' column first
setcolorder(combined, c("dataset", setdiff(names(combined), "dataset")))

## Combined object is ready
print(dim(combined))     # rows / cols
print(table(combined$dataset))

nrow(combined)
merged_aki <- combined            # give the object its new name
















## ------------- 1. make sure the source columns exist -------------------------
id_src <- c("subject_id", "uniquepid",
            "hadm_id", "patienthealthsystemstayid",
            "icustay_id", "patientunitstayid")

absent <- setdiff(id_src, names(merged_aki))
if (length(absent))
  merged_aki[, (absent) := NA_character_]        # create as NA (character)

## ------------- 2. create the standardised identifiers ------------------------
merged_aki[, subject_id_std := fcoalesce(as.character(subject_id),
                                         as.character(uniquepid))]

merged_aki[, hadm_id_std    := fcoalesce(as.character(hadm_id),
                                         as.character(patienthealthsystemstayid))]

merged_aki[, stay_id_std    := fcoalesce(as.character(icustay_id),
                                         as.character(stay_id),
                                         as.character(patientunitstayid))]

## ------------- 3. (optional) move new IDs to the front -----------------------
setcolorder(merged_aki,
            c("subject_id_std", "hadm_id_std", "stay_id_std",
              setdiff(names(merged_aki),
                      c("subject_id_std", "hadm_id_std", "stay_id_std"))))










## -----------------------------------------------------------------
##  1. declare the five “exclusion flags”
## -----------------------------------------------------------------
flag_vars <- c("pregnancy",
               "do_not_resuscitate",
               "cmo",
               "preeclampsia_severe",
               "preeclampsia_mild")

## make sure every flag column exists; if a dataset never had it,
## create it now and fill with 0
missing_flags <- setdiff(flag_vars, names(merged_aki))
if (length(missing_flags))
  merged_aki[, (missing_flags) := 0L]              # integer 0

## -----------------------------------------------------------------
##  2. coerce to integer and replace NA with 0  ( “NA means 0” )
## -----------------------------------------------------------------
merged_aki[, (flag_vars) := lapply(.SD, \(x)
                                   fifelse(is.na(x), 0L, as.integer(x))),
           .SDcols = flag_vars]

## -----------------------------------------------------------------
##  3. keep only patients with *all* flags equal to 0
## -----------------------------------------------------------------
merged_aki <- merged_aki[
  !(pregnancy == 1L |
      do_not_resuscitate == 1L |
      cmo == 1L |
      preeclampsia_severe == 1L |
      preeclampsia_mild  == 1L)
]

## sanity-check
print(dim(merged_aki))        # rows / cols after filtering


















###############################################################################
##  STEP-3 ─────────────────────────────────────────────────────────────────────
##         create the requested “unified” variables
###############################################################################
library(data.table)

## ---------- 3·a  make sure all source columns exist -------------------------
unify_map <- list(
  myocardial_infarct_unified      = c("myocardial_infarct",            "acute_mi"),
  pyelonephritis_unified          = c("pyelonephritis",                "acute_pyelonephritis"),
  mild_liver_disease_unified      = c("mild_liver_disease",            "mildliverdisease"),
  rhabdomyolysis_unified          = c("rhabdomyolysis_flag",           "rhabdomyolysis"),
  sepsis_unified                  = c("sepsis3",                       "sepsis"),
  severe_liver_disease_unified    = c("severe_liver_disease",          "severeliverdisease"),
  sle_unified                     = c("sle",                           "lupus"),
  obstructive_uropathy_unified    = c("obstructive_uropathy_flag",     "obstructive_uropathy"),
  stroke_unified                  = c("tia", "ais", "sah", "ich"),
  history_mi_unified              = c("cabghi", "pcihi", "historyofmi"),
  race_unified                    = c("race",                          "ethnicity"),   # character
  diabetes_all                    = c("diabetes_one",                  "diabetes_two"),
  acei_arb                        = c("acei",                          "arb")
)

needed_cols <- unique(unlist(unify_map))
miss <- setdiff(needed_cols, names(merged_aki))
if (length(miss)) merged_aki[, (miss) := NA]

## ---------- 3·b  build every unified column ---------------------------------
## ---------- 3·b  build every unified column ---------------------------------
for (nm in names(unify_map)) {
  
  src <- unify_map[[nm]]
  
  if (nm == "race_unified") {                       # character columns
    merged_aki[, (nm) :=
                 do.call(fcoalesce,                 # <- USE do.call()  !!!
                         lapply(src, \(x) as.character(get(x))))]
    
  } else {                                          # binary / numeric flags
    merged_aki[, (nm) := as.integer(rowSums(.SD, na.rm = TRUE) > 0),
               .SDcols = src]
  }
}
###############################################################################
##  STEP-4 ─────────────────────────────────────────────────────────────────────
##         build flag + count for oral anti-diabetic agents
###############################################################################
po_drugs <- c("metformin", "sglt2i", "ddp4i", "incretin_memitics", "sulfonylureas")
miss <- setdiff(po_drugs, names(merged_aki))
if (length(miss)) merged_aki[, (miss) := 0]

merged_aki[, oral_antidiabetics_count := rowSums(.SD > 0, na.rm = TRUE),
           .SDcols = po_drugs]
merged_aki[, oral_antidiabetics_flag  := as.integer(oral_antidiabetics_count > 0)]

###############################################################################
##  STEP-5 ─────────────────────────────────────────────────────────────────────
##         create / harmonise “*_used” flags for vaso-active agents
###############################################################################
vaso <- c('dopamine','epinephrine','vasopressin',
          'norepinephrine','dobutamine','milrinone','phenylephrine')

# guarantee columns exist
for (v in vaso) {
  used <- paste0(v, "_used")
  if (!v    %in% names(merged_aki)) merged_aki[, (v)    := NA_real_]
  if (!used %in% names(merged_aki)) merged_aki[, (used) := NA_integer_]
  
  merged_aki[, (used) :=
               fifelse(get(used) == 1L | (!is.na(get(v)) & get(v) > 0),
                       1L, 0L)]
}



###############################################################################
##
###############################################################################
if (!"hospitalid" %in% names(merged_aki))
  merged_aki[, hospitalid := NA_integer_]

# fill in the two special codes
merged_aki[is.na(hospitalid) & !is.na(icustay_id), hospitalid :=  9999L]
merged_aki[is.na(hospitalid) & !is.na(stay_id),  hospitalid := 19999L]









###############################################################################
##  Standardise “hospital_death”  (0 = survived, 1 = died)
###############################################################################
library(data.table)

# 1 ───── ensure that every source column exists so the code never errors
src_cols <- c("hospital_expire_flag",
              "hospitaldischargestatus",
              "unitdischargestatus",
              "discharge_location")
absent <- setdiff(src_cols, names(merged_aki))
if (length(absent)) merged_aki[, (absent) := NA_character_]

# 2 ───── vector with death‐keywords (lower-case for cheap matching)
death_terms <- tolower(c("Expired","Died","Death","Deceased","DIED","DEAD/EXPIRED"))

is_dead <- function(x) tolower(trimws(x)) %chin% death_terms   # fast %chin%

# 3 ───── build the unified flag, honouring the stated hierarchy
merged_aki[, hospital_death :=
             fcase(                                           # highest → lowest
               !is.na(hospital_expire_flag),
               as.integer(hospital_expire_flag == 1L),
               
               is_dead(hospitaldischargestatus),
               1L,
               !is.na(hospitaldischargestatus),               # non-death, non-NA
               0L,
               
               is_dead(unitdischargestatus),
               1L,
               !is.na(unitdischargestatus),
               0L,
               
               is_dead(discharge_location),
               1L,
               !is.na(discharge_location),
               0L,
               
               default = 0L)]                                 # if no info → assume alive


###############################################################################
##  hours_to_death  (NA for survivors)
###############################################################################
# Make sure the needed columns are present
time_cols <- c("intime", "dod", "hospitaldischargeoffset")
absent <- setdiff(time_cols, names(merged_aki))
if (length(absent)) merged_aki[, (absent) := NA]

merged_aki[, hours_to_death := NA_real_]                       # initialise

# a) eICU :   offset is in minutes from ICU admission
merged_aki[dataset == "eicu" &
             hospital_death == 1L &
             !is.na(hospitaldischargeoffset),
           hours_to_death := hospitaldischargeoffset / 60]

# b) MIMIC-III / MIMIC-IV :   dod – intime    (both POSIXct)
merged_aki[dataset != "eicu" &
             hospital_death == 1L &
             !is.na(dod) & !is.na(intime),
           hours_to_death := as.numeric(difftime(dod, intime, units = "hours"))]










library(data.table)

setDT(merged_aki)
setorder(merged_aki, subject_id_std, hadm_id, intime)   # change `intime` if needed
merged_aki[,
           unitvisitnumber := fifelse(
             is.na(unitvisitnumber),
             seq_len(.N),
             unitvisitnumber
           ),
           by = .(subject_id_std, hadm_id)
]

























##############################################
##  SUMMARY TABLES – binary & non-binary    ##
##############################################
library(dplyr)
library(tidyr)
library(tibble)          # tibble::tibble()

##-------------------------------------------------
## 0. Dataset label  (mimic / eicu)
##-------------------------------------------------
dat <- aki_subset %>%                       # or merged_aki
  dplyr::mutate(dataset_grp = ifelse(dataset == "eicu",
                                     "eicu", "mimic"))

##-------------------------------------------------
## 1.  Binary vs. non-binary variables
##-------------------------------------------------
is_binary <- vapply(dat, function(x)
  length(unique(x[!is.na(x)])) <= 2, logical(1))

binary_vars     <- setdiff(names(dat)[is_binary],
                           c("dataset", "dataset_grp"))
non_binary_vars <- setdiff(names(dat),
                           c(binary_vars, "dataset", "dataset_grp"))

##-------------------------------------------------
## 2.  Helper: count + %
##-------------------------------------------------
count_pct <- function(vec)
{
  total_n <- length(vec)
  
  tibble::tibble(value = vec) %>%
    dplyr::mutate(value = ifelse(is.na(value), "NA",
                                 as.character(value))) %>%
    dplyr::group_by(value) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    tidyr::complete(value = c("0", "1", "NA"), fill = list(n = 0)) %>%
    dplyr::mutate(pct = round(100 * n / total_n, 1))
}

##-------------------------------------------------
## 3A.  Binary – overall & by dataset
##-------------------------------------------------
binary_summary <- dat %>%
  dplyr::select(dplyr::all_of(c(binary_vars, "dataset_grp"))) %>%
  tidyr::pivot_longer(-dataset_grp,
                      names_to  = "variable",
                      values_to = "value") %>%
  dplyr::group_by(variable, dataset_grp) %>%
  dplyr::group_modify(~ count_pct(.x$value)) %>%
  dplyr::ungroup() %>%
  dplyr::rename(dataset = dataset_grp)

# Overall rows
overall_bin <- dat %>%
  dplyr::select(dplyr::all_of(binary_vars)) %>%
  tidyr::pivot_longer(everything(),
                      names_to  = "variable",
                      values_to = "value") %>%
  dplyr::group_by(variable) %>%
  dplyr::group_modify(~ count_pct(.x$value)) %>%
  dplyr::mutate(dataset = "Overall")

binary_summary <- dplyr::bind_rows(overall_bin, binary_summary) %>%
  dplyr::arrange(variable,
                 factor(dataset, levels = c("Overall", "mimic", "eicu")),
                 value)

##-------------------------------------------------
## 3B.  Binary – split by AKI status
##-------------------------------------------------
binary_by_aki <- dat %>%
  dplyr::select(dplyr::all_of(c(binary_vars,
                                "dataset_grp", "aki_24h_to_7d"))) %>%
  tidyr::pivot_longer(-c(dataset_grp, aki_24h_to_7d),
                      names_to  = "variable",
                      values_to = "value") %>%
  dplyr::group_by(variable, aki_24h_to_7d, dataset_grp) %>%
  dplyr::group_modify(~ count_pct(.x$value)) %>%
  dplyr::ungroup() %>%
  dplyr::rename(dataset = dataset_grp)

# Overall rows within each AKI stratum
overall_aki <- dat %>%
  dplyr::select(dplyr::all_of(c(binary_vars, "aki_24h_to_7d"))) %>%
  tidyr::pivot_longer(-aki_24h_to_7d,
                      names_to  = "variable",
                      values_to = "value") %>%
  dplyr::group_by(variable, aki_24h_to_7d) %>%
  dplyr::group_modify(~ count_pct(.x$value)) %>%
  dplyr::mutate(dataset = "Overall")

binary_by_aki <- dplyr::bind_rows(overall_aki, binary_by_aki) %>%
  dplyr::arrange(variable, aki_24h_to_7d,
                 factor(dataset, levels = c("Overall", "mimic", "eicu")),
                 value)

##-------------------------------------------------
## 4.  Non-binary variables – % missing
##      (fix: coerce all to character inside pivot)
##-------------------------------------------------
na_summary <- dat %>%
  dplyr::select(dplyr::all_of(c(non_binary_vars, "dataset_grp"))) %>%
  tidyr::pivot_longer(
    -dataset_grp,
    names_to  = "variable",
    values_to = "value",
    values_transform = list(value = as.character)   # <-- key line
  ) %>%
  dplyr::group_by(variable, dataset_grp) %>%
  dplyr::summarise(total    = dplyr::n(),
                   n_miss   = sum(is.na(value)),
                   pct_miss = round(100 * n_miss / total, 1),
                   .groups  = "drop") %>%
  dplyr::rename(dataset = dataset_grp)

# Overall column
overall_na <- dat %>%
  dplyr::select(dplyr::all_of(non_binary_vars)) %>%
  tidyr::pivot_longer(everything(),
                      names_to  = "variable",
                      values_to = "value",
                      values_transform = list(value = as.character)) %>%
  dplyr::group_by(variable) %>%
  dplyr::summarise(total    = dplyr::n(),
                   n_miss   = sum(is.na(value)),
                   pct_miss = round(100 * n_miss / total, 1),
                   .groups  = "drop") %>%
  dplyr::mutate(dataset = "Overall")

na_summary <- dplyr::bind_rows(overall_na, na_summary) %>%
  dplyr::arrange(variable,
                 factor(dataset, levels = c("Overall", "mimic", "eicu")))

##-------------------------------------------------
## 5.  Resulting tables
##-------------------------------------------------
binary_summary   # counts / % of 0,1,NA (Overall + mimic + eicu)
binary_by_aki    # same table, stratified by aki_24h_to_7d
na_summary       # % missing for every non-binary variable









filtered_data <- binary_summary %>%
  filter(value == 1 & dataset == "Overall")

# 'filtered_data' will now contain only the rows that meet both conditions.













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
  "early_aki_exclude1",  "akd_aki", "first_aki_time_24h_to_7d",
  "aki_48h_to_7d", "first_aki_3_time_24h_to_7d",
  "first_aki_time_48h_to_7d", "late_aki_after_week1",
  
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













###############################################################################
##  Assumptions
##  1. Your working data-frame is called  `aki_subset`
##  2. The AKI outcome column is         `aki_24h_to_7d`
##  3. A binary variable is defined as   {0,1,NA}  (no other values)
###############################################################################

library(dplyr)
library(tidyr)

## ────────────────────────────────────────────────────────────────────────────
## 1. Identify binary columns
## ────────────────────────────────────────────────────────────────────────────

binary_vars <- aki_subset %>% 
  select_if(function(.x) {
    x <- na.omit(.x)
    length(x) > 0 && all(x %in% c(0, 1))
  }) %>% 
  names()

## ────────────────────────────────────────────────────────────────────────────
## 2. Replace NA with 0 in every binary column (incl. the outcome if needed)
## ────────────────────────────────────────────────────────────────────────────
aki_subset <- aki_subset %>% 
  mutate(across(all_of(binary_vars), ~ replace_na(as.numeric(.x), 0)))

aki_subset <- aki_subset %>%        # make sure the outcome itself has no NA
  mutate(aki_24h_to_7d = replace_na(as.numeric(aki_24h_to_7d), 0))

## ────────────────────────────────────────────────────────────────────────────
## 3. Table 1 – overall distribution of 0 / 1
## ────────────────────────────────────────────────────────────────────────────
table1 <- tibble(var_name = binary_vars) %>%
  mutate(
    perc_0 = sapply(var_name, function(v) mean(aki_subset[[v]] == 0, na.rm = TRUE) * 100),
    perc_1 = sapply(var_name, function(v) mean(aki_subset[[v]] == 1, na.rm = TRUE) * 100)
  )


## ────────────────────────────────────────────────────────────────────────────
## 4. Table 2 – distribution of “1” stratified by AKI and χ² p-value
## ────────────────────────────────────────────────────────────────────────────
##############################################################################
## TABLE 2 – % of “1” stratified by AKI status + χ² p-value  (base-R version)
##############################################################################
library(purrr)

vars2  <- setdiff(binary_vars, "aki_24h_to_7d")

table2 <- map_dfr(vars2, function(v) {
  x <- aki_subset[[v]]
  
  tibble(
    var_name    = v,
    perc1_aki1  = mean(x[aki_subset$aki_24h_to_7d == 1] == 1, na.rm = TRUE) * 100,
    perc1_aki0  = mean(x[aki_subset$aki_24h_to_7d == 0] == 1, na.rm = TRUE) * 100,
    p_value     = {
      tt <- table(x, aki_subset$aki_24h_to_7d)
      if (all(dim(tt) == c(2, 2))) chisq.test(tt, correct = FALSE)$p.value else NA_real_
    }
  )
})

## ────────────────────────────────────────────────────────────────────────────
## 5. Result
## ────────────────────────────────────────────────────────────────────────────
table1      # overall 0/1 percentages
table2      # stratified percentages + χ² p-values


























###############################################################################
##  CONTINUOUS VARIABLES  +  NA SUMMARY FOR **ALL** NON-BINARY VARIABLES
###############################################################################
library(dplyr)
library(tidyr)

## ---------------------------------------------------------------------------
## 0.  Work on a data.frame copy – immune to data.table subsetting traps
## ---------------------------------------------------------------------------
aki_df <- as.data.frame(aki_subset)      # one cheap, safe coercion

## ---------------------------------------------------------------------------
## 1.  CONTINUOUS = numeric & NOT restricted to {0,1}
## ---------------------------------------------------------------------------
continuous_vars <- names(
  Filter(function(col) {
    is.numeric(col) && !all(na.omit(col) %in% c(0, 1))
  },
  aki_df)
)

## ---------------------------------------------------------------------------
## 2.  NON-BINARY VARIABLES  (everything that is NOT in binary_vars)
## ---------------------------------------------------------------------------
non_binary_vars <- setdiff(names(aki_df), binary_vars)

## ---------------------------------------------------------------------------
## 3.  NA SUMMARY  (base-R column scan, no across(), no data.table joins)
## ---------------------------------------------------------------------------
na_summary <- tibble(
  var_name = non_binary_vars,
  n_NA     = vapply(aki_df[ , non_binary_vars, drop = FALSE],
                    function(x) sum(is.na(x)),
                    integer(1)),
  perc_NA  = vapply(aki_df[ , non_binary_vars, drop = FALSE],
                    function(x) mean(is.na(x)) * 100,
                    numeric(1))
)

## ---------------------------------------------------------------------------
## 4.  RESULTS
## ---------------------------------------------------------------------------
continuous_vars   # character vector of continuous variables
na_summary        # var_name | n_NA | perc_NA

colnames(aki_df)











###############################################################################
##  Assumptions
##  1. Your working data-frame is called  `aki_subset`
##  2. The AKI outcome column is         `aki_24h_to_7d`
##  3. A binary variable is defined as   {0,1,NA}  (no other values)
###############################################################################

library(dplyr)
library(tidyr)

## ────────────────────────────────────────────────────────────────────────────
## 1. Identify binary columns
## ────────────────────────────────────────────────────────────────────────────

binary_vars <- aki_subset %>% 
  select_if(function(.x) {
    x <- na.omit(.x)
    length(x) > 0 && all(x %in% c(0, 1))
  }) %>% 
  names()

## ────────────────────────────────────────────────────────────────────────────
## 2. Replace NA with 0 in every binary column (incl. the outcome if needed)
## ────────────────────────────────────────────────────────────────────────────
aki_subset <- aki_subset %>% 
  mutate(across(all_of(binary_vars), ~ replace_na(as.numeric(.x), 0)))

aki_subset <- aki_subset %>%        # make sure the outcome itself has no NA
  mutate(aki_24h_to_7d = replace_na(as.numeric(aki_24h_to_7d), 0))

## ────────────────────────────────────────────────────────────────────────────
## 3. Table 1 – overall distribution of 0 / 1
## ────────────────────────────────────────────────────────────────────────────
table1 <- tibble(var_name = binary_vars) %>%
  mutate(
    perc_0 = sapply(var_name, function(v) mean(aki_subset[[v]] == 0, na.rm = TRUE) * 100),
    perc_1 = sapply(var_name, function(v) mean(aki_subset[[v]] == 1, na.rm = TRUE) * 100)
  )


## ────────────────────────────────────────────────────────────────────────────
## 4. Table 2 – distribution of “1” stratified by AKI and χ² p-value
## ────────────────────────────────────────────────────────────────────────────
##############################################################################
## TABLE 2 – % of “1” stratified by AKI status + χ² p-value  (base-R version)
##############################################################################
library(purrr)

vars2  <- setdiff(binary_vars, "aki_24h_to_7d")

table2 <- map_dfr(vars2, function(v) {
  x <- aki_subset[[v]]
  
  tibble(
    var_name    = v,
    perc1_aki1  = mean(x[aki_subset$aki_24h_to_7d == 1] == 1, na.rm = TRUE) * 100,
    perc1_aki0  = mean(x[aki_subset$aki_24h_to_7d == 0] == 1, na.rm = TRUE) * 100,
    p_value     = {
      tt <- table(x, aki_subset$aki_24h_to_7d)
      if (all(dim(tt) == c(2, 2))) chisq.test(tt, correct = FALSE)$p.value else NA_real_
    }
  )
})

## ────────────────────────────────────────────────────────────────────────────
## 5. Result
## ────────────────────────────────────────────────────────────────────────────
table1      # overall 0/1 percentages
table2      # stratified percentages + χ² p-values


























###############################################################################
##  CONTINUOUS VARIABLES  +  NA SUMMARY FOR **ALL** NON-BINARY VARIABLES
###############################################################################
library(dplyr)
library(tidyr)

## ---------------------------------------------------------------------------
## 0.  Work on a data.frame copy – immune to data.table subsetting traps
## ---------------------------------------------------------------------------
aki_df <- as.data.frame(aki_subset)      # one cheap, safe coercion

## ---------------------------------------------------------------------------
## 1.  CONTINUOUS = numeric & NOT restricted to {0,1}
## ---------------------------------------------------------------------------
continuous_vars <- names(
  Filter(function(col) {
    is.numeric(col) && !all(na.omit(col) %in% c(0, 1))
  },
  aki_df)
)

## ---------------------------------------------------------------------------
## 2.  NON-BINARY VARIABLES  (everything that is NOT in binary_vars)
## ---------------------------------------------------------------------------
non_binary_vars <- setdiff(names(aki_df), binary_vars)

## ---------------------------------------------------------------------------
## 3.  NA SUMMARY  (base-R column scan, no across(), no data.table joins)
## ---------------------------------------------------------------------------
na_summary <- tibble(
  var_name = non_binary_vars,
  n_NA     = vapply(aki_df[ , non_binary_vars, drop = FALSE],
                    function(x) sum(is.na(x)),
                    integer(1)),
  perc_NA  = vapply(aki_df[ , non_binary_vars, drop = FALSE],
                    function(x) mean(is.na(x)) * 100,
                    numeric(1))
)

## ---------------------------------------------------------------------------
## 4.  RESULTS
## ---------------------------------------------------------------------------
continuous_vars   # character vector of continuous variables
na_summary        # var_name | n_NA | perc_NA

colnames(aki_df)















