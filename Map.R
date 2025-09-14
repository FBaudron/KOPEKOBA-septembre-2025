#' ---
#' title: "Savane et sites KOPEKOBA/ROTATES"
#' author: "Frédéric Baudron"
#' date: "12 septembre 2025"
#' ---

library(terra)
library(geodata)
library(tidyterra)
library(ggspatial)
library(ggthemes)
library(ggrepel)

setwd("D:\\Mes Donnees\\1. Cirad\\4. ROTATES\\2. Countries\\Congo\\Diagnostic\\")


cog = vect("Geo\\admin-regions\\limites administratives\\limites_departementales_cniaf_2023.gpkg")
mosaic = rast('Geo\\mosaic\\MOSAIC_TMF_1990_COG.tif')
stations = vect("Geo\\ESSAIS KOPEKOBA.kml")

villages_df = data.frame(
  Name = c("Laka", "Kiossi", "Dieu le veut", "Imbama"),
  lon  = c(13.280300, 13.152955, 15.512763, 15.664521),
  lat  = c(-4.341589, -4.144004, -3.639392, -3.466903))

villages = vect(villages_df, geom = c("lon","lat"), crs = "EPSG:4326")

plot(mosaic)

savanes = ifel(mosaic == 34, 1, NA)
savanes = trim(savanes)
names(savanes) = "savanes"
# plot(savanes)

stations_filtered = stations[stations$Name != "Prefecture", ]
stations_filtered
centroids = centroids(stations_filtered)

plot(savanes)
plot(cog, add = T)
plot(centroids, add = T)
plot(villages, add = T)


savanes_df = as.data.frame(savanes, xy = TRUE)


villages_df = data.frame(crds(villages), Name = villages$Name)
names(villages_df)[1:2] = c("x", "y")


centroids_df = data.frame(crds(centroids), Name = centroids$Name)
centroids_df$Name = c("LOUDIMA", "ODZIBA")
names(centroids_df)[1:2] = c("x", "y")


labels_df = rbind(
  data.frame(x = centroids_df$x, y = centroids_df$y, Name = centroids_df$Name, type = "station"),
  data.frame(x = villages_df$x, y = villages_df$y, Name = villages_df$Name, type = "village"))


ggplot() + 
  ggtitle("Stations et villages KOPEKOBA/ROTATES en savanes") + 
  theme_bw() +
  geom_spatvector(data = cog, fill = "grey85", linewidth = 0.5, color = "black") +
  geom_tile(data = na.omit(savanes_df), aes(x = x, y = y), fill = "orange") +
  geom_spatvector(data = cog, fill = NA, linewidth = 0.5, color = "black") +
  geom_spatvector(data = centroids, size = 5, color = "red", alpha = 0.6) +
  geom_spatvector(data = villages, size = 5, color = "purple", alpha = 0.6) +
  geom_label_repel(data = labels_df, aes(x = x, y = y, label = Name, color = type, fill = type, segment.color = type),
    segment.size = 1, force = 10, force_pull = 0.1, box.padding = 1.0, point.padding = 0.5, min.segment.length = 0,
    max.overlaps = Inf, max.time = 5, size = 5, fontface = "bold") +
  scale_color_manual(values = c("station" = "red", "village" = "purple")) +
  scale_fill_manual(values = c("station" = "white", "village" = "white")) +
  scale_discrete_manual(aesthetics = "segment.color", 
                        values = c("station" = "red", "village" = "purple")) +
  theme(plot.title = element_text(hjust = 0, size = 16, face = "bold"),
        axis.title = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 12),
        axis.text.x = element_text(size = 12),
        legend.position = "none") +
  annotation_scale(location = "tl", width_hint = 0.2, height = unit(0.25, "cm"),
                   text_cex = 1, pad_x = unit(0.15, "cm"), pad_y = unit(0.15, "cm")) +
  annotation_north_arrow(location = "br", which_north = "true",
                         height = unit(1, "cm"), width = unit(1, "cm"),
                         pad_x = unit(0.15, "cm"), pad_y = unit(0.15, "cm"),
                         style = north_arrow_fancy_orienteering(text_size = 10)) +
  coord_sf(xlim = c(11.5, 16.5), ylim = c(-5.2, -0.5), expand = FALSE)

ggsave("Map.jpeg", units = "cm", width = 21, height = 20, dpi = 320)

sum(expanse(cog, unit = "km"))  
expanse(savanes, unit = "km")

expanse(savanes, unit = "km")/sum(expanse(cog, unit = "km"))

# environ 60,000 km2 de savanes
# 17% du pays en savanes
