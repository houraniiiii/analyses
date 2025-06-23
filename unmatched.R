## exclusion## -----------------------------------------------------------------
## 0.  put the three data.tables in a **named** list  (already done)
## -----------------------------------------------------------------
dts <- list(mimic_iii = mimic_iii,
            mimic_iv  = mimic_iv,
            eicu      = eicu)

## -----------------------------------------------------------------
## 1.  long layout: one row per (column, dataset) pair that exists
## -----------------------------------------------------------------
long <- rbindlist(
  lapply(names(dts), \(tag) 
         data.table(column  = names(dts[[tag]]),      # column name
                    dataset = tag)),                  # where it was found
  idcol = FALSE)

## -----------------------------------------------------------------
## 2.  wide presence matrix (1 = present, 0 = absent)
## -----------------------------------------------------------------
wide <- dcast(long,
              column ~ dataset,
              fun.aggregate = length,   # counts how many times the pair appears
              value.var      = "dataset")

## -----------------------------------------------------------------
## 3.  add a “how many datasets?” column
## -----------------------------------------------------------------
wide[, n_datasets := mimic_iii + mimic_iv + eicu]

## -----------------------------------------------------------------
## 4.  keep only the columns that are **not** present in all 3 tables
## -----------------------------------------------------------------
unmatched_cols <- wide[n_datasets < length(dts)]

## inspect
print(unmatched_cols[])











