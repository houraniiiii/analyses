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
