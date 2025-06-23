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
