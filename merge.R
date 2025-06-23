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

