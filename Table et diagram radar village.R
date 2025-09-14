#' ---
#' title: "Table villages"
#' author: "Frédéric Baudron"
#' date: "13 septembre 2025"
#' ---


# CLEAR ENVIRONMENT-------------------------------------------------------------

rm(list = ls())


# LOADING NECESSARY PACKAGES----------------------------------------------------

library(openxlsx)
library(janitor)
library(dplyr)
library(gtsummary)
library(gt)
library(ggradar)
library(ggplot2)
library(cowplot)
library(egg)

# SETTING UP THE DIRECTORY & LOADING THE DATA-----------------------------------

setwd("D:\\Mes Donnees\\1. Cirad\\5. PUDT\\Enquête village\\")

data = read.xlsx("Enquête village avec données spatiales.xlsx", sheet = 1)

data = clean_names(data)


# DATA MANIPULATION-------------------------------------------------------------

typo = data[, c(4:6, 8:10, 16:19, 24:33, 35:37, 39:40, 50:51, 53:61, 63:66, 68:70, 75, 88, 92, 100:104)]

names(typo)

names(typo)[c(4:44, 47:48, 50:51)] = c("latitude", "longitude", "altitude", "nb_cases", "nb_habitants", "nb_prop_terriens",
                                       "autochtones", "agriculture", "arboriculture", "pisciculture", "elevage", "charbon",
                                       "peche", "chasse", "sciage", "cueillette", "emploi", "mine", "briqueterie", "commerce",
                                       "neoruraux", "main_oeuvre_ext", "groupements", "nb_groupements", "intrants_equip",
                                       "mecanisation", "entraide", "savoir", "transformation", "commercialisation", "projets",
                                       "prod_agric", "tracteur", "acheteurs", "pieds", "kavaki", "voiture_camion", "route_vente",
                                       "marche", "brazza_pnoire", "projet_agric", "altitude", "pluvio", "carbone_sol", "sable")

typo$autochtones = as.numeric(typo$autochtones == "Oui")
typo$neoruraux = as.numeric(typo$neoruraux == "Oui")
typo$main_oeuvre_ext = as.numeric(typo$main_oeuvre_ext == "Oui")
typo$groupements = as.numeric(typo$groupements == "Oui")
typo$tracteur = as.numeric(typo$tracteur == "Oui")
typo$projet_agric = as.numeric(typo$projet_agric == "Oui")

typo$nb_groupements[is.na(typo$nb_groupements)] = 0


typo$nb_prop_terriens = typo$nb_prop_terriens / typo$nb_habitants

typo$type2 =  ifelse(typo$type == "défriche (10 - 30 %) sur couvert forestier discontinu", "Défriche dense sous couvert forestier discontinu", NA)
typo$type2 =  ifelse(typo$type == "défriche dense (+ de 30 %) sur couvert forestier discontinu", "Défriche dense sous couvert forestier discontinu", typo$type2)

typo$type2 =  ifelse(typo$type == "défriche forestière (25-50%)", "Défriche en zone forestière", typo$type2)
typo$type2 =  ifelse(typo$type == "défriche forestière dense (+ de 50%)", "Défriche en zone forestière", typo$type2)
typo$type2 =  ifelse(typo$type == "défriche forestière peu dense (- de 25%)", "Défriche en zone forestière", typo$type2)

typo$type2 =  ifelse(typo$type == "défriche marginale sur couvert forestier discontinu", "Défriche marginale sous couvert forestier discontinu", typo$type2)
typo$type2 =  ifelse(typo$type == "défriche peu dense (- de 10%) sur couvert forestier discontinu", "Défriche marginale sous couvert forestier discontinu", typo$type2)

typo$type2 =  ifelse(typo$type == "parcellaire mécanisé", "Agriculture de savane des Plateaux Batéké", typo$type2)
typo$type2 =  ifelse(typo$type == "parcellaire mécanisé récent (après 2015)", "Agriculture de savane des Plateaux Batéké", typo$type2)

typo$type2 =  ifelse(typo$type == "petit parcellaire dense", "Agriculture de savane de la vallée du Niari", typo$type2)

#Important, on réintégre les villages du district de Lékane dans le paysage Plateaux Batéké
typo$type2 = ifelse(typo$district == "Lékana", "Agriculture de savane des Plateaux Batéké", typo$type2)



typo$esquisse =  ifelse(typo$type2 == "Agriculture de savane des Plateaux Batéké", "Savanes", NA)
typo$esquisse =  ifelse(typo$type2 == "Agriculture de savane de la vallée du Niari", "Savanes", typo$esquisse)
typo$esquisse =  ifelse(typo$type2 == "Défriche en zone forestière", "Forêt", typo$esquisse)
typo$esquisse =  ifelse(typo$type2 == "Défriche dense sous couvert forestier discontinu", "Mosaïques", typo$esquisse)
typo$esquisse =  ifelse(typo$type2 == "Défriche marginale sous couvert forestier discontinu", "Mosaïques", typo$esquisse)

typo = typo[, c(45, 1:44, 47:51, 46, 52:53)]


tb = typo[, c("nb_cases", "nb_habitants", "nb_prop_terriens", "autochtones", "agriculture", 
                   "arboriculture", "pisciculture", "elevage", "charbon", "peche", "chasse", 
                   "sciage", "cueillette", "emploi", "mine", "briqueterie", "commerce", "neoruraux",
                   "main_oeuvre_ext", "groupements", "tracteur", "projet_agric", "altitude",
                   "pluvio", "route", "carbone_sol", "sable", "type2")] %>%
  tbl_summary(
    by = type2, 
    missing = "no",
    statistic = list(all_continuous() ~ "{median} [{p25}, {p75}]",
                     all_categorical() ~ "{p}%"),
    digits = all_continuous() ~ 1,
    label = list(nb_cases ~ "Nombre de cases",
                 nb_habitants ~ "Nombre d'habitants",
                 nb_prop_terriens ~ "Proportion de propriétaires terriens (%)",
                 autochtones ~ "Présence d'autochtones",
                 agriculture ~ "Pratique de l'agriculture",
                 arboriculture	 ~ "Pratique de l'arboriculture",
                 pisciculture ~ "Pratique de la pisciculture",
                 elevage ~ "Pratique de l'élevage",
                 charbon ~ "Pratique du charbonage",
                 peche ~ "Pratique de la pêche",
                 chasse ~ "Pratique de la chasse",
                 sciage ~ "Pratique du sciage",
                 cueillette ~ "Pratique de la cueillette",
                 emploi ~ "Présence d'emplois salariés",
                 mine ~ "Présence de mines",
                 briqueterie ~ "Pratique de la briquetrie",
                 commerce ~ "Présence de commerces",
                 neoruraux ~ "Présence de néoruraux",
                 main_oeuvre_ext ~ "Présence de main d'oeuvre externe",
                 groupements ~ "Présence de groupements",
                 tracteur ~ "Présence de tracteurs",
                 projet_agric ~ "Présence de projets agricoles",
                 altitude	 ~ "Altitude (m.a.s.l.)",
                 pluvio ~ "Pluviométrie annuelle",
                 route ~ "Distance à la route goudronnée (km)",
                 carbone_sol ~ "Carbone organique du sol (g/kg 0-30 cm profondeur)",
                 sable ~ "Teneur en sable du sol (%, 0-30 cm profondeur)"
    )) %>%
  add_p(test = all_continuous() ~ "kruskal.test") 

tb

write.xlsx(tb, "Table villages.xlsx")
gtsave(as_gt(tb), "Table villages.html")


activites = c("type2", "agriculture", "arboriculture", "pisciculture", "elevage",
               "charbon", "peche", "chasse", "sciage", "cueillette",
               "emploi", "mine", "briqueterie")

structures = c("type2", "autochtones", "commerce", "neoruraux", "main_oeuvre_ext",
               "groupements", "tracteur", "projet_agric")

activites = typo[ , activites]
structures = typo[ , structures]


activites = activites %>%     
  group_by(type2) %>%
  summarise_each(funs(mean))%>% 
  mutate(across(-type2, ~ .x * 100))

structures = structures %>%     
  group_by(type2) %>%
  summarise_each(funs(mean))%>% 
  mutate(across(-type2, ~ .x * 100))


group_levels = unique(activites$type2)
n_groups = length(group_levels)
base_colors = c("#558aa6", "#B1740F", "#D5006A", "#08585A", "#9449d2")
group_colours = rep(base_colors, length.out = n_groups)
names(group_colours) = group_levels

p1_noleg = p1 + theme(legend.position = "none")
p2_noleg = p2 + theme(legend.position = "none")

legend_df <- data.frame(
  name = factor(group_levels, levels = group_levels),
  x1 = seq_along(group_levels) - 0.3,
  x2 = seq_along(group_levels) + 0.3,
  x = seq_along(group_levels),
  y = 1)

legend_plot = ggplot(legend_df) +
  geom_segment(aes(x = x1, xend = x2, y = y, yend = y, colour = name), size = 1) +
  geom_point(aes(x = x, y = y, colour = name), size = 3) +
  geom_text(aes(x = x, y = y - 0.15, label = name), vjust = 1, size = 4.6) +
  scale_color_manual(values = group_colours) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.05))) +
  coord_cartesian(ylim = c(0.6, 1.05)) +
  theme_void() +
  theme(plot.margin = margin(5, 5, 5, 5),
        legend.position = "none")


plots_panel = cowplot::plot_grid(p1_noleg, p2_noleg, ncol = 2, align = "hv")
final_plot = cowplot::plot_grid(plots_panel, legend_plot, ncol = 1, rel_heights = c(1, 0.10))


ggdraw(final_plot) + 
  theme(plot.background = element_rect(fill = "white", colour = NA),
        plot.margin = margin(5, 5, 5, 5))

ggsave("Radars villages.png", units = "cm", width = 55, height = 25, dpi = 320)




