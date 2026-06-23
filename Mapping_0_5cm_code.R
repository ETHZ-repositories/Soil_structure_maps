library(dplyr)
library(ranger)
library(ithir)
library(ranger)
library(caret)
library(terra)

data <- read.csv("D:/Soil_health/Analysis/soil_str_data_mapping_11_10_23.csv")


#Soil structure class by overlap


get_structure_class <- function(profile,
                                target_top = 0,
                                target_bottom = 5) {
  
  profile <- profile %>%
    mutate(
      overlap = pmax(
        0,
        pmin(DEPTH_TO_cm.x, target_bottom) -
          pmax(DEPTH_FROM_cm.x, target_top)
      )
    ) %>%
    filter(overlap > 0)
  
  if (nrow(profile) == 0) {
    return(NA_character_)
  }
  
  x <- profile %>%
    group_by(SOIL_STRUCTURE_SIZE111) %>%
    summarise(
      total_overlap = sum(overlap),
      top_depth = min(DEPTH_FROM_cm.x),
      .groups = "drop"
    )
  
  x %>%
    arrange(desc(total_overlap), top_depth) %>%
    slice(1) %>%
    pull(SOIL_STRUCTURE_SIZE111)
}

#CREATE DEPTH-WISE SOIL STRUCTURE (0–5)

Soil_structure_0_5 <- data %>%
  group_by(ID) %>%
  summarise(
    Soil_structure_class =
      get_structure_class(
        cur_data(),
        target_top = 0,
        target_bottom = 5
      )
  )
table(Soil_structure_0_5$Soil_structure_class)

#LOAD soil texture and SOC data

Soil_structure<-read.csv("D:/Soil_structure_mapping/depth0_5cm.csv")

colnames(Soil_structure)

SS_0_5<- Soil_structure[,c(2,4,5,6)]

colnames(SS_0_5)[which(colnames(SS_0_5)%in% c("id"))]<- "ID"

Soil_structure_0_5<- merge(Soil_structure_0_5, SS_0_5, by = "ID")

#ADD COORDINATES

coords <- data %>%
  dplyr::select(
    ID,
    COORDINATE_X_E.x,
    COORDINATE_Y_N.x
  ) %>%
  distinct()

Soil_structure_0_5 <- merge(
  Soil_structure_0_5,
  coords,
  by="ID"
)


#Extract Environmental Covaraites

grid <- list.files(
  "E:/multispectral_Landsat8/soil_structure_cov/",
  pattern="*.tif$",
  full.names=TRUE
)

All_cov <- rast(grid)

pts <- Soil_structure_0_5[,c(
  "COORDINATE_X_E.x",
  "COORDINATE_Y_N.x"
)]

cov_values <- terra::extract(
  All_cov,
  pts
)

cov_values <- dplyr::select(
  cov_values,
  -ID
)

DSM_data <- cbind(
  Soil_structure_0_5,
  cov_values
)

DSM_data$Soil_structure_class <-
  as.factor(
    DSM_data$Soil_structure_class
  )

DSM_data <- DSM_data[
  complete.cases(DSM_data),
]

table(
  DSM_data$Soil_structure_class
)

colnames(DSM_data)

colnames(DSM_data)[which(colnames(DSM_data)%in% c("COORDINATE_X_E.x"))]<- "Long"

colnames(DSM_data)[which(colnames(DSM_data)%in% c("COORDINATE_Y_N.x"))]<- "Lat"

###77covariates

# DSM_data<-DSM_data[,c("Soil_structure_class",         "Clay_0_5cm",           "SOC_0_5cm",            "Long",
#                        "Lat",     "Clim_bio_13",          "Clim_bio_14",          "Clim_bio_4",           "Clim_bio_6",           "DEM1_lan2" ,
#                        "Facc_1_F",             "Hi_sed_dep_1_F",       "Hi_soil_sed_1_F",   "lith_lan2",
#                        "NDVI_1_2005",          "NDVI_1_2011",          "NDVI_1_2012",          "NDVI_1_2013",          "NDVI_1_2014",          "NDVI_1_2016" ,
#                        "NDVI_1_2017",          "NDVI_1_2018",          "NDVI_1_2019",          "NDVI_2_2005",          "NDVI_2_2011",          "NDVI_2_2012" ,
#                        "NDVI_2_2013",          "NDVI_2_2015",          "NDVI_2_2016" ,         "NDVI_2_2017",          "NDVI_2_2018" ,         "NDVI_2_2019" ,
#                        "NDVI_3_2005",          "NDVI_3_2011",          "NDVI_3_2012" ,         "NDVI_3_2013",          "NDVI_3_2014"  ,        "NDVI_3_2015" ,
#             "NDVI_3_2016",          "NDVI_3_2017",          "NDVI_3_2018",          "NDVI_3_2019",          "NDVI_4_2011" ,         "NDVI_4_2012" ,
#                        "NDVI_4_2013",          "NDVI_4_2014",          "NDVI_4_2015" ,         "NDVI_4_2016",          "NDVI_4_2017" ,         "NDVI_4_2018" ,
#                        "NDVI_4_2019" ,         "pl_cur_1_F",           "precp1_lan2",          "prof_cu_1_F",          "Sand_0_5cm"   ,        "SCOMP_B11_STD_Swiz",
#                        "SCOMP_B11_Swiz",       "SCOMP_B12_STD_Swiz",   "SCOMP_B12_Swiz",       "SCOMP_B2_STD_Swiz",    "SCOMP_B2_Swiz"  ,      "SCOMP_B3_STD_Swiz" ,
#             "SCOMP_B3_Swiz",        "SCOMP_B4_STD_Swiz",    "SCOMP_B4_Swiz"  ,      "SCOMP_B5_STD_Swiz",    "SCOMP_B6_STD_Swiz" ,   "SCOMP_B7_STD_Swiz" ,
#  "SCOMP_B7_Swiz",       "SCOMP_B8_Swiz",        "SCOMP_B8A_STD_Swiz" ,  "SCOMP_B8A_Swiz",       "slop_deg_1_F"   ,      "temp3_lan2",
#                        "Ter_roug_1_F" ,        "TPI_1_F" ,             "TRI_1_F" ,             "TWetnes_1_F"   )]


#####selected_20 covaraites
DSM_data<-DSM_data[,c("Soil_structure_class",         "Clay_0_5cm",           "SOC_0_5cm",            "Long",
                       "Lat",     "Clim_bio_13",          "Clim_bio_14",          "Clim_bio_4",           "Clim_bio_6",
                       "Sand_0_5cm","precp1_lan2","DEM1_lan2", "Hi_sed_dep_1_F", "temp3_lan2","SCOMP_B2_Swiz", "SCOMP_B4_Swiz",
                       "NDVI_1_2016", "Hi_soil_sed_1_F", "SCOMP_B3_Swiz","NDVI_1_2017", "SCOMP_B4_STD_Swiz")]

#5-Fold Cross Validation

set.seed(123)

k <- 5
n <- nrow(DSM_data)

folds <- sample(rep(1:k, length.out = n))

all_preds <- c()
all_obs <- c()

# store importance from each fold
importance_list <- list()

for (i in 1:k) {
  
  train_data <- DSM_data[folds != i, ]
  test_data  <- DSM_data[folds == i, ]
  
  rf_model <- ranger(
    Soil_structure_class ~ .,
    data = train_data,
    probability = TRUE,
    num.trees = 500,
    mtry = 6,
    importance = "permutation"
  )
  
  # predictions
  pred <- predict(rf_model, data = test_data)$predictions
  pred_class <- colnames(pred)[max.col(pred)]
  
  all_preds <- c(all_preds, pred_class)
  all_obs   <- c(all_obs, as.character(test_data$Soil_structure_class))
  
  # store importance
  importance_list[[i]] <- rf_model$variable.importance
}

#Model Performance

confusionMatrix(
  factor(all_preds),
  factor(all_obs)
)


# Average variable Importance

imp_df <- do.call(cbind, importance_list)

mean_importance <- rowMeans(imp_df, na.rm = TRUE)

importance_table <- data.frame(
  Variable = names(mean_importance),
  Importance = mean_importance
)

# sort descending
importance_table <- importance_table[order(-importance_table$Importance), ]

importance_table

table(test_data$Soil_structure_class)

#####################

# sort descending
importance_table <- importance_table[order(-importance_table$Importance), ]

# top 20 covariates
top20_importance <- importance_table[1:20, ]

top20_importance

###################################

grid <- list.files(
  "E:/multispectral_Landsat8/Top_20_covaraites_0_5cm/",
  pattern = "\\.tif$",
  full.names = TRUE
)

All_cov <- raster::stack(grid)

# fix names properly
names(All_cov) <- tools::file_path_sans_ext(basename(grid))

#Final Model

final_rf <- ranger(
  Soil_structure_class ~ .,
  data = DSM_data,
  probability = TRUE,
  num.trees = 500,
  importance = "permutation"
)

final_rf

# Check predictor names used by model
model_vars <- setdiff(names(DSM_data), "Soil_structure_class")

model_vars


# Compare with model predictors
setdiff(model_vars, names(All_cov))
setdiff(names(All_cov), model_vars)

# Prediction map

rf_pred_fun <- function(model, data) {
  
  pred <- predict(model, data = data)$predictions
  
  # class with highest probability
  class_id <- max.col(pred)
  
  return(class_id)
}

soil_structure_map <- raster::predict(
  All_cov,
  final_rf,
  fun = rf_pred_fun,
  progress = "text",
  filename = "soil_structure_class_map.tif",
  overwrite = TRUE
)

plot(soil_structure_map)


#writeRaster(soil_structure_map, "D:/Soil_health/Analysis/Soil_structure_0_5cm.tif")


#Probability + Uncertainty Map

All_cov_v <- rast(All_cov)

rf_prob_fun <- function(model, data, ...) {
  as.data.frame(predict(model, data)$predictions)
}

prob_maps <- terra::predict(
  All_cov_v,
  final_rf,
  fun = rf_prob_fun
)


nlyr(prob_maps)
names(prob_maps)


max_prob <- app(prob_maps, max)
uncertainty <- 1 - max_prob

plot(uncertainty)


writeRaster(uncertainty, "D:/Soil_health_christine/Analysis/Soil_structure_0_5cm_uncertainity.tif")


for (i in 1:nlyr(prob_maps)) {
  writeRaster(
    prob_maps[[i]],
    filename = paste0(
      "prob_0_5cm",
      names(prob_maps)[i],
      ".tif"
    ),
    overwrite = TRUE
  )
}
