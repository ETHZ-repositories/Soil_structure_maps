############################################################
# 1. DEPTH HARMONISATION (Equal-area spline using ithir)
############################################################

library(ithir)

data <- read.csv("D:/Soil_health/Analysis/soil_str_data_mapping_11_10_23.csv")

# ---------------------------
# SAND
# ---------------------------
All_profile_sand <- data[, c(2, 10, 11, 17)]

eaFit_sand <- ithir::ea_spline(
  obj = All_profile_sand,
  var.name = "SAND_pct",
  d = t(c(0, 5, 15, 30, 60, 100, 200)),
  lam = 0.1,
  vlow = 0,
  show.progress = FALSE
)

jhj1_sand <- eaFit_sand$harmonised

# ---------------------------
# CLAY
# ---------------------------
All_profile_clay <- data[, c(2, 10, 11, 15)]

eaFit_clay <- ithir::ea_spline(
  obj = All_profile_clay,
  var.name = "CLAY_pct",
  d = t(c(0, 5, 15, 30, 60, 100, 200)),
  lam = 0.1,
  vlow = 0,
  show.progress = FALSE
)

jhj1_clay <- eaFit_clay$harmonised

# ---------------------------
# SOC
# ---------------------------
All_profile_SOC <-data[, c(2, 10, 11, 53)]
All_profile_SOC <- All_profile_SOC[!is.na(All_profile_SOC$SOC), ]

eaFit_SOC <- ithir::ea_spline(
  obj = All_profile_SOC,
  var.name = "SOC",
  d = t(c(0, 5, 15, 30, 60, 100, 200)),
  lam = 0.1,
  vlow = 0,
  show.progress = FALSE
)

jhj1_SOC <- eaFit_SOC$harmonised

############################################################
# 3. EXTRACT 0–5 CM VALUES
############################################################

# Sand
jhj1_sand1 <- jhj1_sand[, c(1, 2)]
colnames(jhj1_sand1)[colnames(jhj1_sand1) == "0-5 cm"] <- "Sand_0_5cm"

# Clay
jhj1_clay1 <- jhj1_clay[, c(1, 2)]
colnames(jhj1_clay1)[colnames(jhj1_clay1) == "0-5 cm"] <- "Clay_0_5cm"

# SOC
jhj1_SOC1 <- jhj1_SOC[, c(1, 2)]
colnames(jhj1_SOC1)[colnames(jhj1_SOC1) == "0-5 cm"] <- "SOC_0_5cm"

############################################################
############################################################
Depth0_5cm <- merge(jhj1_sand1, jhj1_clay1, by = "id")
Depth0_5cm <- merge(Depth0_5cm, jhj1_SOC1, by = "id")