#' ---
#' title: "Yield gap analysis - Cassava - Random forest - PUDT"
#' author: "Frédéric Baudron"
#' date: "7 September 2025"
#' ---


# CLEAR ENVIRONMENT-------------------------------------------------------------

rm(list = ls())


# LOADING NECESSARY PACKAGES----------------------------------------------------

library(openxlsx)
library(dplyr)
library(h2o)
library(ggplot2)
library(kernelshap)
library(shapviz)
library(terra)
library(sf)
library(tidyterra)
library(geodata)


# SETTING UP THE DIRECTORY & LOADING THE DATA-----------------------------------

setwd("D:\\Mes Donnees\\1. Cirad\\5. PUDT\\Enquête\\Analysis\\Yield gap\\1. Cassava")

data = read.xlsx("Data\\2. Datasets\\Data for YGA.xlsx", sheet = 1)

data1 = data

# Select relevant columns
data = data[, c("ch_id", "yield_manioc", "sarclages", "vegetation",
                 "duree_jachere", "tracteur_usage", "tubercules",
                 "cereales", "legumineuses","legumes",
                 "banane_plantain","arbres_nonfruit","surf_manioc",
                 "foncier","elevation","soc","sand","ph",
                 "prec","cover")]

# Convert categorical variables to factors
cat_cols = c("duree_jachere","tracteur_usage","sarclages","vegetation",
              "tubercules","cereales","legumineuses","legumes",
              "banane_plantain","arbres_nonfruit","foncier","cover")
data[cat_cols] = lapply(data[cat_cols], as.factor)

# Remove missing values

data = na.omit(data)


# Initialize H2O and split data-------------------------------------------------

set.seed(1234)
h2o.init()
h2o.removeAll()

data_h2o = as.h2o(data)
splits = h2o.splitFrame(data_h2o, ratios = 0.8, seed = 1234)
train = splits[[1]]
test  = splits[[2]]

response_col = "yield_manioc"
predictors = setdiff(names(data[, -c(1)]), response_col)


# Parameterization--------------------------------------------------------------

hyper_params = list(
  ntrees = c(100, 200, 300),
  max_depth = c(5, 10, 15, 20),
  sample_rate = c(0.7, 0.8, 1.0),
  mtries = c(-1, 5, 10)
)

# hyper_params = list(
#   ntrees = c(200, 400, 600),
#   max_depth = c(10, 15, 20, 30),
#   sample_rate = c(0.6, 0.8, 1.0),
#   mtries = c(-1, 5, 10, 20),
#   min_rows = c(1, 5, 10),
#   nbins_cats = c(50, 100, 200)
# )


rf_grid = h2o.grid(
  algorithm = "randomForest",
  grid_id = "grid_manioc_rdt",
  x = predictors,
  y = response_col,
  training_frame = train,
  nfolds = 5,
  keep_cross_validation_predictions = TRUE,  # needed for CV metrics
  seed = 1234,
  hyper_params = hyper_params,
  search_criteria = list(strategy = "RandomDiscrete", max_models = 30)
)


# Select best model by RMSE
sorted_grid = h2o.getGrid("grid_manioc_rdt", sort_by = "rmse", decreasing = FALSE)
best_model_id = sorted_grid@model_ids[[1]]
best_rf = h2o.getModel(best_model_id)


# Variable importance (CV model)------------------------------------------------

# Define groups
management_vars = c("sarclages", "vegetation", "duree_jachere", "tracteur_usage",
                     "tubercules", "cereales", "legumineuses", "legumes",
                     "banane_plantain", "arbres_nonfruit", "surf_manioc", "foncier")

geospatial_vars = c("elevation", "soc", "sand", "ph", "prec", "cover")

# Clean + group variable importance
vi_top20 <- as.data.frame(h2o.varimp(best_rf)) %>%
  filter(!is.na(variable) & variable != "") %>%
  mutate(group = case_when(
    variable %in% management_vars ~ "Pratiques",
    variable %in% geospatial_vars ~ "Geolocalisation",
    TRUE ~ "Other"
  )) %>%
  arrange(desc(relative_importance)) %>%
  slice(1:20)

# Plot
ggplot(vi_top20, aes(x = reorder(variable, relative_importance),
                     y = relative_importance,
                     fill = group)) +
  geom_col() +
  coord_flip() +
  labs(x = "Predicteurs", y = "Importance relative",
       title = "Variable Importance (Top 20 Predictors)",
       fill = "Type de variables") +
  scale_fill_manual(values = c("Pratiques" = "#33a02c",
                               "Geolocalisation" = "#1f78b4",
                               "Other" = "grey70")) +
  theme_bw() +
  scale_x_discrete(labels = c("céréales associées",
                              "banane/plantain associé",
                              "légumineuses associées",
                              "tubercules associés",
                              "arbres non-fruitiers associés",
                              "legumes associés",
                              "utilisation du tracteur",
                              "domaine",
                              "foncier",
                              "végétation initiale",
                              "sarclages",
                              "durée de jachère",
                              "surface",
                              "pH",
                              "carbone organique du sol",
                              "altitude",
                              "% sable du sol",
                              "précipitation annuelle")) +
  theme(plot.title = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold", margin = margin(0, 10, 0, 0)),
        legend.position = c(0.9, 0.1), legend.justification = c(0.9, 0.1),
        legend.background = element_rect(color = "black"),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        plot.margin = margin(10, 10, 10, 10))

ggsave("Graphs//vi.png", units = "cm", width = 20, height = 15, dpi = 320)


# Cross-validated metrics (holdout predictions)---------------------------------

cv_preds = as.vector(h2o.cross_validation_holdout_predictions(best_rf))
cv_actuals = as.vector(train[[response_col]])

cv_df = data.frame(
  predicted = cv_preds,
  actual = cv_actuals
)

# CV metrics
rmse_cv = sqrt(mean((cv_df$predicted - cv_df$actual)^2))
mae_cv  = mean(abs(cv_df$predicted - cv_df$actual))
r2_cv   = 1 - sum((cv_df$predicted - cv_df$actual)^2) / sum((cv_df$actual - mean(cv_df$actual))^2)

cat("Cross-validated RMSE:", round(rmse_cv,2), "\n")
cat("Cross-validated MAE :", round(mae_cv,2), "\n")
cat("Cross-validated R²  :", round(r2_cv,3), "\n")

# CV Predicted vs Observed Plot
ggplot(cv_df, aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.6, color = "#1f78b4", size = 2) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed", linewidth = 1) +
  geom_smooth(method = "lm", color = "red", se = FALSE, linetype = "dashed", linewidth = 1) +
  labs(
    x = "Rendements observés (t/ha)",
    y = "Rendements prédits (t/ha)",
    title = "Predicted vs Observed Yield (CV Holdouts)"
  ) +
  theme_bw() +
  theme(plot.title = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.margin = margin(10, 10, 10, 10))

ggsave("Graphs//predicted vs observed.png", units = "cm", width = 20, height = 15, dpi = 320)


# Test set performance
perf_test = h2o.performance(best_rf, newdata = test)
cat("Test set R²:", round(h2o.r2(perf_test),3), "\n")
cat("Test set RMSE:", round(h2o.rmse(perf_test),2), "\n")
cat("Test set MAE:", round(h2o.mae(perf_test),2), "\n")

# Final model trained on all data-----------------------------------------------

final_model = h2o.randomForest(
  x = predictors,
  y = response_col,
  training_frame = data_h2o,
  seed = 1,
  nfolds = 0
)

# Predictions on full data
train_preds = h2o.predict(final_model, data_h2o)
train_pred_vals = as.data.frame(train_preds)$predict
train_obs_vals  = as.data.frame(data_h2o[, response_col])[[1]]
train_plot_df = data.frame(Observed = train_obs_vals, Predicted = train_pred_vals)

# Training Predictions Plot (Full model)
ggplot(train_plot_df, aes(x = Observed, y = Predicted)) +
  geom_point(alpha = 0.5, color = "#e31a1c") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "blue") +
  labs(x = "Observed Yield (t/ha)",
       y = "Predicted Yield (t/ha)",
       title = "Training Predictions (Full Model)") +
  theme_minimal(base_size = 14)

# Metrics for final model
cat("Final model R²:", round(h2o.r2(final_model),3), "\n")
cat("Final model RMSE:", round(h2o.rmse(final_model),2), "\n")
cat("Final model MAE:", round(h2o.mae(final_model),2), "\n")

# Variable importance for final model
h2o.varimp_plot(final_model, num_of_features = 20)
summary(final_model)


# ANALYSIS OF SHAP VALUES-------------------------------------------------------

shap_values = h2o.predict_contributions(final_model, data_h2o)

shap_values_df = as.data.frame(shap_values)

names(shap_values_df) = c("sv_sarclages", "sv_vegetation", "sv_duree_jachere", "sv_tracteur_usage",
                          "sv_tubercules", "sv_cereales", "sv_legumineuses", "sv_legumes", "sv_banane_plantain",
                          "sv_arbres_nonfruit",	"sv_surf_manioc",	"sv_foncier",	"sv_elevation",	"sv_soc",
                          "sv_sand",	"sv_ph",	
                          "sv_prec", "sv_cover", "BiasTerm")


data_with_shap = cbind(data[, 1], shap_values_df)
names(data_with_shap)[1] = "ch_id"
data_with_shap = merge(data1, data_with_shap, by = "ch_id")

names(data_with_shap)

write.xlsx(data_with_shap, "Data//2. Datasets//Data with SHAP values 5 septembre 2025.xlsx", rowNames = F)

# https://www.r-bloggers.com/2022/07/shapviz-goes-h2o/
shp = shapviz(best_rf, X_pred = data)

sv_importance(shp, show_numbers = TRUE)
sv_importance(shp, show_numbers = TRUE, kind = "bee")
ggsave("Graphs//sv_importance 5 septembre 2025.png", units = "cm", width = 30, height = 20, dpi = 320)


shp$X$duree_jachere = factor(shp$X$duree_jachere, levels = c("Végétation naturelle", "1-2 ans", "3-5 ans", "6-11 ans", "> 11 ans"))

p1 = sv_dependence(shp, "duree_jachere", color_var = NULL, color = "tomato", size = 2.5, alpha = 0.5)

pc1 = p1 + ggtitle("Rendement manioc - durée de jachère") +
  theme_bw() + 
  # scale_x_discrete(labels = c('Non', 'Oui')) +
  theme(plot.title = element_text(size = 16, face="bold", hjust = 0, margin = margin(0, 0, 10, 0)),
        axis.text = element_text(size = 12, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(0, 10, 0, 0)),
        plot.margin = margin(10, 10, 10, 10))

pc1

# saveRDS(pc1, file = "D:\\Mes Donnees\\1. Cirad\\PUDT\\Livrable 6\\Analyses finales\\pc1.rds")


shp$X$sarclages = factor(shp$X$sarclages, levels = c("Pas de sarclage", "1 fois", "2 fois", "3 fois", "4 et +"))

p2 = sv_dependence(shp, "sarclages", color_var = NULL, color = "tomato", size = 2.5, alpha = 0.5)

pc2 = p2 + ggtitle("Rendement manioc - nombre de sarclages") +
  theme_bw() + 
  # scale_x_discrete(labels = c('Non', 'Oui')) +
  theme(plot.title = element_text(size = 16, face="bold", hjust = 0, margin = margin(0, 0, 10, 0)),
        axis.text = element_text(size = 12, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(0, 10, 0, 0)),
        plot.margin = margin(10, 10, 10, 10))

pc2

# saveRDS(pc2, file = "D:\\Mes Donnees\\1. Cirad\\PUDT\\Livrable 6\\Analyses finales\\pc2.rds")


p3 = sv_dependence(shp, "foncier", color_var = NULL, color = "tomato", size = 2.5, alpha = 0.5)

pc3 = p3 + ggtitle("Rendement manioc - foncier") +
  theme_bw() + 
  # scale_x_discrete(labels = c('Non', 'Oui')) +
  theme(plot.title = element_text(size = 16, face="bold", hjust = 0, margin = margin(0, 0, 10, 0)),
        axis.text = element_text(size = 12, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(0, 10, 0, 0)),
        plot.margin = margin(10, 10, 10, 10))

pc3

# saveRDS(pc3, file = "D:\\Mes Donnees\\1. Cirad\\PUDT\\Livrable 6\\Analyses finales\\pc3.rds")


p4 = sv_dependence(shp, "tracteur_usage", color_var = NULL, color = "tomato", size = 2.5, alpha = 0.5)

pc4 = p4 + ggtitle("Rendement manioc - utilisation de tracteur") +
  theme_bw() + 
  scale_x_discrete(labels = c('Non', 'Oui')) +
  theme(plot.title = element_text(size = 16, face="bold", hjust = 0, margin = margin(0, 0, 10, 0)),
        axis.text = element_text(size = 12, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(0, 10, 0, 0)),
        plot.margin = margin(10, 10, 10, 10))

pc4

# saveRDS(pc4, file = "D:\\Mes Donnees\\1. Cirad\\PUDT\\Livrable 6\\Analyses finales\\pc4.rds")


sv_dependence(shp, "prec", "sand", alpha = 0.5)
ggsave("Graphs//sv_dependence_prec.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "sand", "prec", alpha = 0.5)
ggsave("Graphs//sv_dependence_sand.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "soc", "prec", alpha = 0.5)
ggsave("Graphs//sv_dependence_soc.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "elevation", "prec", alpha = 0.5)
ggsave("Graphs//sv_dependence_elevation.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "ph", "prec", alpha = 0.5)
ggsave("Graphs//sv_dependence_ph.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "sarclages", "prec", alpha = 0.5)
ggsave("Graphs//sv_dependence_sarclages.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "duree_jachere", "prec", alpha = 0.5)
ggsave("Graphs//sv_dependence_duree_jachere.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "surf_manioc", "prec", alpha = 0.5)
ggsave("Graphs//sv_dependence_surf_manioc.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "vegetation", "prec", alpha = 0.5)
ggsave("Graphs//sv_dependence_vegetation.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "foncier", "prec", alpha = 0.5)
ggsave("Graphs//sv_dependence_foncier.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "arbres_nonfruit", "prec", alpha = 0.5)
ggsave("Graphs//sv_dependence_arbres_nonfruit.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "tracteur_usage", "prec", alpha = 0.5)
ggsave("Graphs//sv_dependence_tracteur_usage.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "cover", "prec", alpha = 0.5)
ggsave("Graphs//sv_dependence_cover.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "legumes", "sand", alpha = 0.5)
ggsave("Graphs//sv_dependence_legumes.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "tubercules", "sand", alpha = 0.5)
ggsave("Graphs//sv_dependence_tubercules.png", units = "cm", width = 20, height = 15, dpi = 320)


sv_force(shp, row_id = 1)
sv_waterfall(shp, row_id = 1)


# PREDICTIONS-------------------------------------------------------------------

# RENDEMENT

cog0 = terra::vect("D:\\Mes Donnees\\1. Cirad\\5. PUDT\\Geo\\input-data\\admin-regions\\limites administratives\\limites_departementales_cniaf_2023.gpkg")

# Load rasters
elevation = terra::rast("Data/1. Rasters esquisse/elevation.tiff")
soc       = terra::rast("Data/1. Rasters esquisse/soc.tiff")
sand      = terra::rast("Data/1. Rasters esquisse/sand.tiff")
ph        = terra::rast("Data/1. Rasters esquisse/ph.tiff")
tavg      = terra::rast("Data/1. Rasters esquisse/tavg.tiff")
prec      = terra::rast("Data/1. Rasters esquisse/prec.tiff")
esq       = terra::rast("Data/1. Rasters esquisse/esq_raster_3classes.tiff")

# Make sure all rasters are aligned to elevation
esq = terra::resample(esq, elevation, method = "near")
cls = data.frame(id = 1:3, cover = c("Savane", "Mosaïque", "Forêt"))
levels(esq) = cls

# Build raster stack
stacked = c(elevation, soc, sand, ph, tavg, prec, esq)
names(stacked) = c("elevation","soc","sand","ph","tavg","prec","cover")


# convert to dataframe
stack_df = as.data.frame(stacked, xy = TRUE, na.rm = FALSE)


# predict in chunk with h20 model
chunk_size = 2500000   # adjust depending on RAM
n = nrow(stack_df)
preds = numeric(n)

for (i in seq(1, n, by = chunk_size)) {
  cat("Processing rows", i, "to", min(i + chunk_size - 1, n), "\n")
  
  # select chunk
  chunk = stack_df[i:min(i + chunk_size - 1, n), ]
  
  # remove rows with NA predictors
  valid = complete.cases(chunk)
  chunk_preds = rep(NA, nrow(chunk))
  
  if (any(valid)) {
    chunk_h2o = as.h2o(chunk[valid, -(1:2)])  # drop x,y before prediction
    chunk_pred = as.data.frame(h2o.predict(final_model, chunk_h2o))$predict
    chunk_preds[valid] = chunk_pred
  }
  
  preds[i:min(i + chunk_size - 1, n)] = chunk_preds
}


# rebuild raster
stack_df$yield_pred = preds
pred_map = terra::rast(stack_df[, c("x", "y", "yield_pred")], type = "xyz", crs = crs(stacked))

terra::writeRaster(pred_map, "Data\\3. Rasters prediction\\predicted_cassava_yield.tiff", overwrite = T)



pred_df = as.data.frame(pred_map, xy = TRUE, na.rm = TRUE)

ggplot(pred_df) +
  geom_tile(aes(x = x, y = y, fill = yield_pred)) +
  geom_sf(data = sf::st_as_sf(cog0), fill = NA, color = "black", linewidth = 0.5) +
  scale_fill_viridis_c(option = "C", direction = -1, name = "Yield (kg/ha)") +
  theme_bw() +
  labs(title = "Predicted cassava yield in Congo") +
  theme(
    plot.title = element_text(size = 18, face = "bold", margin = margin(0,0,10,0))
  )

ggsave("Maps/Predictions/rendement_cassava_map.png", units = "cm", width = 23, height = 25, dpi = 320)

