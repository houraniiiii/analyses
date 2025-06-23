drive_deauth()

files <- c(
  mimic_iv  = "1kckbOIuh9XFRGa9mdrEsijObZNu9ANHo",
  mimic_iii = "1viIpX2JWl0EOdjK39nZ20hHAEH9-5l5k",
  eicu      = "1dKsaujQi3Pr57e8Dj-KHAWaYqCHefOwn"
)

# Download 
lapply(names(files), function(name) {
  drive_download(
    as_id(files[name]),
    path      = paste0(name, ".csv"),
    overwrite = TRUE
  )
})

# Read them in
mimic_iv  <- read.csv("mimic_iv.csv")
mimic_iii <- read.csv("mimic_iii.csv")
eicu      <- read.csv("eicu.csv")


mimic_iv$intime




