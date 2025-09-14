#' ---
#' title: "Revenus agricoles - Random forest - PUDT"
#' author: "Frédéric Baudron"
#' date: "12 September 2025"
#' ---


# CLEAR ENVIRONMENT-------------------------------------------------------------

rm(list = ls())


# LOADING NECESSARY PACKAGES----------------------------------------------------

library(openxlsx)
library(dplyr)
library(h2o)
library(ggplot2)
library(ggthemes)
library(kernelshap)
library(shapviz)
library(terra)
library(sf)
library(tidyterra)
library(geodata)
library(egg)
library(grid)


# SETTING UP THE DIRECTORY & LOADING THE DATA-----------------------------------

setwd("D:\\Mes Donnees\\1. Cirad\\5. PUDT\\Enquête\\Analysis\\Yield gap\\1.2 Revenus ag")

data = read.xlsx("Data\\2. Datasets\\Data for RF farm-scale.xlsx", sheet = 1)

data$revenu_agricole = ifelse(is.na(data$revenu_agricole), 0, data$revenu_agricole)
data$revenu_res_nat = ifelse(is.na(data$revenu_res_nat), 0, data$revenu_res_nat)
data$revenu_autre = ifelse(is.na(data$revenu_autre), 0, data$revenu_autre)

data$revenu_total = data$revenu_agricole + data$revenu_res_nat + data$revenu_autre

# autre = végétation autre que cropland et forêt

data = data[, c("id", "revenu_agricole", "revenu_res_nat", "revenu_total",
                "manioc_production", "mais_production", "soja_production",
                "arachide_production", "safou_production", "volume_cacao", "volume_cafe", "plantain_production", "palmier_production",
                "taille_famille", "age_chef_fam", "cultivated_area", "diversite_total",
                "foyer_femme", "education_chef_fam", "indigene", "jardin", "verger", "jachere", "surface_louee",
                "entraide", "main_doeuvre_ext", "groupement", "contrat", "reprise", 
                "previous_vegetation", "fallow_duration_simplifie",
                "evolution_surf_cult_passe", "evolution_surf_cult_future", "statut_champ", "orientation",
                "tracteur_usage", "voit_kav_mot_usage", "porcs", "petits_ruminants",
                "elevation",
                "soc", "sand", "ph", "tavg",
                "prec",
                "distance_route", "cropland", "forest_all", "autre", "cover", "population")]


data$porcs = ifelse(data$porcs > 0, 1, 0)
data$petits_ruminants = ifelse(data$petits_ruminants > 0, 1, 0)
data$fallow_duration_simplifie
data$foyer_femme = as.factor(data$foyer_femme)
data$education_chef_fam = as.factor(data$education_chef_fam)
data$indigene = as.factor(data$indigene)
data$jardin = as.factor(data$jardin)
data$verger = as.factor(data$verger)
data$jachere = as.factor(data$jachere)
data$surface_louee = as.factor(data$surface_louee)
data$entraide = as.factor(data$entraide)
data$main_doeuvre_ext = as.factor(data$main_doeuvre_ext)
data$groupement = as.factor(data$groupement)
data$contrat = as.factor(data$contrat)
data$reprise = as.factor(data$reprise)
data$previous_vegetation = as.factor(data$previous_vegetation)
data$fallow_duration_simplifie = as.factor(data$fallow_duration_simplifie)
data$evolution_surf_cult_passe = as.factor(data$evolution_surf_cult_passe)
data$evolution_surf_cult_future = as.factor(data$evolution_surf_cult_future)
data$statut_champ = as.factor(data$statut_champ)
data$orientation = as.factor(data$orientation)
data$tracteur_usage = as.factor(data$tracteur_usage)
data$voit_kav_mot_usage = as.factor(data$voit_kav_mot_usage)
data$cover = as.factor(data$cover)
data$porcs = as.factor(data$porcs)
data$petits_ruminants = as.factor(data$petits_ruminants)

# Select relevant columns
data = data[, c("id", "revenu_agricole", "taille_famille", "age_chef_fam", "cultivated_area", "diversite_total",
                "foyer_femme", "education_chef_fam", "indigene", "jardin", "verger", "jachere", "surface_louee",
                "entraide", "main_doeuvre_ext", "groupement", "contrat", "reprise", 
                "previous_vegetation", "fallow_duration_simplifie",
                "evolution_surf_cult_passe", "evolution_surf_cult_future", "statut_champ", "orientation",
                "tracteur_usage", "voit_kav_mot_usage", "porcs", "petits_ruminants",
                "elevation", "soc", "sand", "ph", 
                # "tavg", 
                "prec",
                "distance_route", "cropland", "forest_all", "autre", "cover", "population")]

# Remove missing values

data1 = data

data = na.omit(data)

data = subset(data, revenu_agricole > 0)

hist(log(data$revenu_agricole))

data$revenu_agricole_log = log(data$revenu_agricole)


boxplot(data$revenu_agricole_log)
data =  data[data$revenu_agricole_log > 7,]
boxplot(data$revenu_agricole_log)


# Initialize H2O and split data-------------------------------------------------

set.seed(1234)
h2o.init()
h2o.removeAll()

data_h2o = as.h2o(data)
splits = h2o.splitFrame(data_h2o, ratios = 0.8, seed = 1234)
train = splits[[1]]
test  = splits[[2]]

response_col = "revenu_agricole_log"
predictors = setdiff(names(data[, -c(1, 2)]), response_col)


# Parameterization--------------------------------------------------------------

hyper_params = list(
  ntrees = c(400, 600),
  max_depth = c(15, 20),
  sample_rate = c(0.8, 1.0),
  mtries = c(-1, 10),
  min_rows = c(1, 5),
  nbins_cats = c(50, 100)
)

rf_grid = h2o.grid(
  algorithm = "randomForest",
  grid_id = "grid_rev_ag",
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
sorted_grid = h2o.getGrid("grid_rev_ag", sort_by = "rmse", decreasing = FALSE)
best_model_id = sorted_grid@model_ids[[1]]
best_rf = h2o.getModel(best_model_id)


# Variable importance (CV model)------------------------------------------------

# Define groups
management_vars = c("taille_famille", "age_chef_fam", "cultivated_area", "diversite_total",
                    "foyer_femme", "education_chef_fam", "indigene", "jardin", "verger", "jachere", "surface_louee",
                    "entraide", "main_doeuvre_ext", "groupement", "contrat", "reprise", 
                    "previous_vegetation", "fallow_duration_simplifie",
                    "evolution_surf_cult_passe", "evolution_surf_cult_future", "statut_champ", "orientation",
                    "tracteur_usage", "voit_kav_mot_usage", "porcs", "petits_ruminants")

geospatial_vars = c("elevation", "soc", "sand", "ph", "tavg", "prec",
                    "distance_route", "cropland", "forest_all", "autre", "cover", "population")

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
  scale_x_discrete(labels = c("domaine géographique",
                              "chef d'exploitation femme",
                              "distance à la route",
                              "végétation précédente",
                              "% forêt dans un rayon de 4 km",
                              "durée de jachère",
                              "moyen de transport motorisé",
                              "main d'oeuvre extérieure",
                              "densité de population",
                              "pH du sol",
                              "carbone organique du sol",
                              "% autre que forêt et zone cultivée\ndans un rayon de 4 km",
                              "précipitations annuelles",
                              "diversité cultivée totale",
                              "taille de famille",
                              "age du chef d'exploitation",
                              "% sable du sol",
                              "orientation vers le marché",
                              "altitude",
                              "surface cultivée")) +
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
    x = "Revenus observés (XAF/an, log)",
    y = "Revenus prédits (XAF/an, log)",
    title = "Revenus predits vs observés (CV Holdouts)"
  ) +
  theme_bw() +
  theme(plot.title = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold", margin = margin(0, 10, 0, 0)),
        legend.position = c(0.9, 0.1), legend.justification = c(0.9, 0.1),
        legend.background = element_rect(color = "black"),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
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
  geom_point(alpha = 0.5, color = "#e31a1c", size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
  labs(x = "Revenus observés (XAF/an, log)",
       y = "Revenus prédits (XAF/an, log)",
       title = "Predictions sur les données d'entrainement (model complet)") +
  theme_bw() +
  theme(plot.title = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold", margin = margin(0, 10, 0, 0)),
        legend.position = c(0.9, 0.1), legend.justification = c(0.9, 0.1),
        legend.background = element_rect(color = "black"),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        plot.margin = margin(10, 10, 10, 10))

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

names(shap_values_df)

names(shap_values_df) = c("sv_taille_famille", "sv_age_chef_fam", "sv_cultivated_area", "sv_diversite_total", "sv_foyer_femme",
                          "sv_education_chef_fam", "sv_indigene", "sv_jardin", "sv_verger", "sv_jachere",
                          "sv_surface_louee", "sv_entraide", "sv_main_doeuvre_ext", "sv_groupement",                
                          "sv_contrat", "sv_reprise", "sv_previous_vegetation",
                          "sv_fallow_duration_simplifie", "sv_evolution_surf_cult_passe",
                          "sv_evolution_surf_cult_future", "sv_statut_champ", "sv_orientation",               
                          "sv_tracteur_usage", "sv_voit_kav_mot_usage", "sv_porcs", "sv_petits_ruminants",           
                          "sv_elevation", "sv_soc", "sv_sand", "sv_ph", "sv_prec", "sv_distance_route",            
                          "sv_cropland", "sv_forest_all", "sv_autre", "sv_cover", "sv_population", "BiasTerm")


data_with_shap = cbind(data[, 1], shap_values_df)
names(data_with_shap)[1] = "id"
data_with_shap = merge(data1, data_with_shap, by = "id")

names(data_with_shap)

write.xlsx(data_with_shap, "Data//2. Datasets//Data with SHAP values 5 septembre 2025.xlsx")

data_with_shap[40:76] = lapply(data_with_shap[40:76], function(x) exp(data_with_shap$BiasTerm) * exp(x))


p1 = ggplot(data = data_with_shap, aes(x = cultivated_area, y = sv_cultivated_area)) +
         geom_point(color = "#1f78b4", size = 2.5, alpha = 0.5) +
         ggtitle("A") +
  xlab("Surfave cultivée (ha)") +
  ylab("SHAP revenus agricoles (XAF/an)") +
  theme_bw() + 
  theme(plot.title = element_text(size = 16, face="bold", hjust = 0, margin = margin(0, 0, 10, 0)),
        axis.text = element_text(size = 12, face = "bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(10, 0, 0, 0)),
        plot.margin = margin(10, 10, 10, 10))

p2 = ggplot(data = data_with_shap, aes(x = orientation, y = sv_orientation)) +
  geom_jitter(color = "#1f78b4", size = 2.5, alpha = 0.5, width = 0.25) +
  ggtitle("B") +
  xlab("Orientation vers le marché") +
  ylab("SHAP revenus agricoles (XAF/an)") +
  theme_bw() + 
  scale_x_discrete(labels = c('Non', 'Oui')) +
  theme(plot.title = element_text(size = 16, face="bold", hjust = 0, margin = margin(0, 0, 10, 0)),
        axis.text = element_text(size = 12, face = "bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(10, 0, 0, 0)),
        plot.margin = margin(10, 10, 10, 10))

p3 = ggplot(data = data_with_shap, aes(x = age_chef_fam, y = sv_cultivated_area)) +
  geom_point(color = "#1f78b4", size = 2.5, alpha = 0.5) +
  ggtitle("C") +
  xlab("Age du chef de famille") +
  ylab("SHAP revenus agricoles (XAF/an)") +
  theme_bw() + 
  theme(plot.title = element_text(size = 16, face="bold", hjust = 0, margin = margin(0, 0, 10, 0)),
        axis.text = element_text(size = 12, face = "bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(10, 0, 0, 0)),
        plot.margin = margin(10, 10, 10, 10))

p4 = ggplot(data = data_with_shap, aes(x = taille_famille, y = sv_taille_famille)) +
  geom_point(color = "#1f78b4", size = 2.5, alpha = 0.5) +
  ggtitle("D") +
  xlab("Taille de famille") +
  ylab("SHAP revenus agricoles (XAF/an)") +
  theme_bw() + 
  theme(plot.title = element_text(size = 16, face="bold", hjust = 0, margin = margin(0, 0, 10, 0)),
        axis.text = element_text(size = 12, face = "bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(10, 0, 0, 0)),
        plot.margin = margin(10, 10, 10, 10))

p5 = ggplot(data = data_with_shap, aes(x = diversite_total, y = sv_diversite_total)) +
  geom_point(color = "#1f78b4", size = 2.5, alpha = 0.5) +
  ggtitle("E") +
  xlab("Diversité cultivée totale") +
  ylab("SHAP revenus agricoles (XAF/an)") +
  theme_bw() + 
  theme(plot.title = element_text(size = 16, face="bold", hjust = 0, margin = margin(0, 0, 10, 0)),
        axis.text = element_text(size = 12, face = "bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(10, 0, 0, 0)),
        plot.margin = margin(10, 10, 10, 10))

p6 = ggplot(data = data_with_shap, aes(x = main_doeuvre_ext, y = sv_main_doeuvre_ext)) +
  geom_jitter(color = "#1f78b4", size = 2.5, alpha = 0.5, width = 0.25) +
  ggtitle("F") +
  xlab("Main d'oeuvre extérieure") +
  ylab("SHAP revenus agricoles (XAF/an)") +
  theme_bw() + 
  scale_x_discrete(labels = c('Non', 'Oui')) +
  theme(plot.title = element_text(size = 16, face="bold", hjust = 0, margin = margin(0, 0, 10, 0)),
        axis.text = element_text(size = 12, face = "bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(10, 0, 0, 0)),
        plot.margin = margin(10, 10, 10, 10))

FIGURE1 = ggarrange(p1, p2, p3, p4,
                    ncol = 2, nrow = 2, widths = c(1, 1), heights = c(1, 1))

y.grob = textGrob("SHAP revenus agricoles (XAF/an)", gp = gpar(fontface = "bold", fontsize = 16), rot = 90)

ggdraw(arrangeGrob(FIGURE1, left = y.grob)) +  
  theme(plot.background = element_rect(fill = "white"),
        plot.margin = margin(10, 10, 10, 10))

ggsave("Graphs//panel_sv.png", units = "cm", width = 25, height = 20, dpi = 320)


# https://www.r-bloggers.com/2022/07/shapviz-goes-h2o/
shp = shapviz(best_rf, X_pred = data)

sv_importance(shp, show_numbers = TRUE)
sv_importance(shp, show_numbers = TRUE, kind = "bee")
ggsave("Graphs//sv_importance.png", units = "cm", width = 30, height = 20, dpi = 320)



sv_dependence(shp, "cultivated_area", "orientation", alpha = 0.5)
ggsave("Graphs//sv_dependence_cultivated_area.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "elevation", "cultivated_area", alpha = 0.5)
ggsave("Graphs//sv_dependence_elevation.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "orientation", "cultivated_area", alpha = 0.5)
ggsave("Graphs//sv_dependence_orientation.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "sand", "cultivated_area", alpha = 0.5)
ggsave("Graphs//sv_dependence_sand.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "age_chef_fam", "cultivated_area", alpha = 0.5)
ggsave("Graphs//sv_dependence_age_chef_fam.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "taille_famille", "cultivated_area", alpha = 0.5)
ggsave("Graphs//sv_dependence_taille_famille.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "diversite_total", "cultivated_area", alpha = 0.5)
ggsave("Graphs//sv_dependence_diversite_total.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "prec", "cultivated_area", alpha = 0.5)
ggsave("Graphs//sv_dependence_prec.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "autre", "cultivated_area", alpha = 0.5)
ggsave("Graphs//sv_dependence_autre.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "soc", "cultivated_area", alpha = 0.5)
ggsave("Graphs//sv_dependence_soc.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "ph", "cultivated_area", alpha = 0.5)
ggsave("Graphs//sv_dependence_ph.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "population", "cultivated_area", alpha = 0.5)
ggsave("Graphs//sv_dependence_population.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "main_doeuvre_ext", "cultivated_area", alpha = 0.5)
ggsave("Graphs//sv_dependence_main_doeuvre_ext.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "voit_kav_mot_usage", "cultivated_area", alpha = 0.5)
ggsave("Graphs//sv_dependence_voit_kav_mot_usage.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "fallow_duration_simplifie", "cultivated_area", alpha = 0.5)
ggsave("Graphs//sv_dependence_fallow_duration_simplifie.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "forest_all", "cultivated_area", alpha = 0.5)
ggsave("Graphs//sv_dependence_forest_all.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "previous_vegetation", "cultivated_area", alpha = 0.5)
ggsave("Graphs//sv_dependence_previous_vegetation.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "distance_route", "cultivated_area", alpha = 0.5)
ggsave("Graphs//sv_dependence_distance_route.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "foyer_femme", "cultivated_area", alpha = 0.5)
ggsave("Graphs//sv_dependence_foyer_femme.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_dependence(shp, "cover", "cultivated_area", alpha = 0.5)
ggsave("Graphs//sv_dependence_cover.png", units = "cm", width = 20, height = 15, dpi = 320)

sv_force(shp, row_id = 1)
sv_waterfall(shp, row_id = 1)


# PREDICTIONS-------------------------------------------------------------------

# RENDEMENT

cog0 = terra::vect("D:\\Mes Donnees\\1. Cirad\\5. PUDT\\Geo\\input-data\\admin-regions\\limites administratives\\limites_departementales_cniaf_2023.gpkg")

# Load rasters
elevation = terra::rast("D:/Mes Donnees/1. Cirad/5. PUDT/Enquête/Analysis/Yield gap/1. Cassava/Data/1. Rasters esquisse/elevation.tiff")
soc       = terra::rast("D:/Mes Donnees/1. Cirad/5. PUDT/Enquête/Analysis/Yield gap/1. Cassava/Data/1. Rasters esquisse/soc.tiff")
sand      = terra::rast("D:/Mes Donnees/1. Cirad/5. PUDT/Enquête/Analysis/Yield gap/1. Cassava/Data/1. Rasters esquisse/sand.tiff")
ph        = terra::rast("D:/Mes Donnees/1. Cirad/5. PUDT/Enquête/Analysis/Yield gap/1. Cassava/Data/1. Rasters esquisse/ph.tiff")
tavg      = terra::rast("D:/Mes Donnees/1. Cirad/5. PUDT/Enquête/Analysis/Yield gap/1. Cassava/Data/1. Rasters esquisse/tavg.tiff")
prec      = terra::rast("D:/Mes Donnees/1. Cirad/5. PUDT/Enquête/Analysis/Yield gap/1. Cassava/Data/1. Rasters esquisse/prec.tiff")
esq       = terra::rast("D:/Mes Donnees/1. Cirad/5. PUDT/Enquête/Analysis/Yield gap/1. Cassava/Data/1. Rasters esquisse/esq_raster_3classes.tiff")

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
stack_df$pred = preds
pred_map = terra::rast(stack_df[, c("x", "y", "pred")], type = "xyz", crs = crs(stacked))

terra::writeRaster(pred_map, "Data\\3. Rasters prediction\\predicted_income.tiff", overwrite = T)



pred_df = as.data.frame(pred_map, xy = TRUE, na.rm = TRUE)

ggplot(pred_df) +
  geom_tile(aes(x = x, y = y, fill = exp(pred))) +
  geom_sf(data = sf::st_as_sf(cog0), fill = NA, color = "black", linewidth = 0.5) +
  scale_fill_viridis_c(option = "C", direction = -1, name = "Revenu agricole (XAF/an)") +
  theme_bw() +
  labs(title = "Revenu agricole prédit au Congo") +
  theme(
    plot.title = element_text(size = 18, face = "bold", margin = margin(0,0,10,0))
  )

ggsave("Maps/Predictions/ag_income_map.png", units = "cm", width = 25, height = 25, dpi = 320)

