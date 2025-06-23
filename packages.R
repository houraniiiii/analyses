if ("package:h2o" %in% search())  detach("package:h2o", unload=TRUE)
if ("h2o" %in% rownames(installed.packages())) remove.packages("h2o")


install.packages(c("RCurl","jsonlite"))


install.packages("h2o",
                 type = "source",
                 repos = c(
                   "https://h2o-release.s3.amazonaws.com/h2o/latest_stable_R",
                   getOption("repos")
                 )
)



install.packages("googledrive")






# Define package groups
imputation_pkgs <- c("mice", "missForest", "Amelia", "VIM")
manipulation_pkgs <- c("dplyr", "data.table", "tidyr", "plyr")
basic_stats_pkgs <- c("stats", "MASS", "psych", "car")
symbolic_regression_pkgs <- c("gramEvol", "GPareto")
genetic_algorithm_pkgs <- c("GA", "genalg", "rgenoud", "GenSA")
shap_pkgs <- c("iml", "fastshap", "shapper", "DALEX")
lime_pkgs <- c("lime", "DALEX", "iml", "breakDown")
auc_pkgs <- c("pROC", "ROCR", "PRROC", "caret")

# In your R startup (or at top of your script):
Sys.setenv(RGL_USE_NULL = "TRUE")
options(rgl.useNULL = TRUE)

# Combine all into one vector
all_pkgs <- c(imputation_pkgs,
              manipulation_pkgs,
              basic_stats_pkgs,
              symbolic_regression_pkgs,
              genetic_algorithm_pkgs,
              shap_pkgs,
              lime_pkgs,
              auc_pkgs)

# Install any missing packages
install_if_missing <- function(pkgs) {
  missing <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
  if (length(missing)) {
    install.packages(missing)
  }
}
install_if_missing(all_pkgs)

# Load all packages
lapply(all_pkgs, library, character.only = TRUE)



















library(googledrive)
library(h2o)
h2o.init()

h2o.shutdown()

