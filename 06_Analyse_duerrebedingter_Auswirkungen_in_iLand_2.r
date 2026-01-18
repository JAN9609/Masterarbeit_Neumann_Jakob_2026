# Jakob Neumann 05.05.2025 #

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Initialisierung ############################################################################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

## Bibliotheken laden ####
library(tidyverse)
library(RSQLite)
library(dbplyr)
library(terra)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(scales)

## iLand-Eingabedateien einlesen ####

setwd("C:/Users/Jakob/OneDrive/OneDrive Dokumente/Universitaet/02_Master/Masterarbeit/iLand_SOSFOR/Iland_project/temp")

# Environment-Datei
(env.df <- read_delim("../gis/environment.txt"))

# DEM-Raster (10 x 10 m Auflösung)
dem.grid <- terra::rast("C:/Users/Jakob/OneDrive/OneDrive Dokumente/Universitaet/02_Master/Masterarbeit/iLand_SOSFOR/Iland_project/gis/dem.asc")

# Originalkoordinaten speichern
env.df$x_orig <- env.df$x
env.df$y_orig <- env.df$y

env.df <- env.df %>%
  mutate(
    x = 405400 + x * 100,
    y = 5277726 + y * 100
  )
# Höheninformationen hinzufügen
env.df$elevation <- terra::extract(
  terra::aggregate(dem.grid, fact = 10, fun = "mean"), 
  env.df[, c("x", "y")], ID = FALSE) %>% unlist()

# Artendatenbank
db.conn <- RSQLite::dbConnect(RSQLite::SQLite(),
                              dbname = "../database/all_species_database.sqlite")
species.df <- dplyr::tbl(db.conn, "species") %>% 
  collect()
RSQLite::dbDisconnect(db.conn); rm(db.conn)

# Target-Datei
target.df <- read.csv("C:/Users/Jakob/OneDrive/OneDrive Dokumente/Universitaet/02_Master/Masterarbeit/iLand_SOSFOR/AgentFiles/no_mgmt_eng_Muenstertal_ABE-lib_targets_v1.0.csv", sep=";")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# iLand-Ausgabedaten einlesen ################################################################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Funktion zum Verbinden mit SQLite-Datei
connect_to_sqlite <- function(filename, output_dir = "../output/") {
  db_path <- file.path(output_dir, filename)
  
  if (!file.exists(db_path)) {
    available_files <- list.files(output_dir, pattern = "\\.sqlite$")
    cat("Error: File not found:", db_path, "\n")
    cat("Available SQLite files in", output_dir, ":\n")
    if (length(available_files) > 0) {
      cat(paste("-", available_files, collapse = "\n"), "\n")
    } else {
      cat("No SQLite files found in the output directory.\n")
    }
    stop("Specified SQLite file does not exist: ", filename)
  }
  
  cat("Loading SQLite file:", filename, "\n")
  cat("Full path:", db_path, "\n")
  if (file.exists(db_path)) {
    cat("File created:", format(file.mtime(db_path), "%Y-%m-%d %H:%M:%S"), "\n")
  }
  
  return(db_path)
}

# SQLite-Datenbank verbinden und Daten laden
sqlite_filename <- "project_CSA_20251103_140033.sqlite"
selected_db <- connect_to_sqlite(sqlite_filename)
db.conn <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = selected_db)

RSQLite::dbListTables(db.conn)

# Tabellen laden
lscp <- dplyr::tbl(db.conn, "landscape") %>% 
  collect()

'dyn <- dplyr::tbl(db.conn, "dynamicstand") %>% 
  filter(year %in% c(0,100)) %>% # filter simulation years
  rename(id = rid) %>% 
  collect() %>% 
  # combine with environment file to get information like xy-coordinates or elevation for each Resource Unit
  left_join(env.df[, c("id", "x", "y", "elevation")], 
            by = join_by(id))
'

# wind <- dplyr::tbl(db.conn, "wind") %>% 
#  collect()

production_month <- dplyr::tbl(db.conn, "production_month") %>% 
  collect()

water <- dplyr::tbl(db.conn, "water") %>% 
  collect()

landscape_removed <- dplyr::tbl(db.conn, "landscape_removed") %>% 
  collect()

landscape <- dplyr::tbl(db.conn, "landscape") %>% 
  collect()

RSQLite::dbDisconnect(db.conn); rm(db.conn)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Analysen #####################################################################################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

## Daten aggregieren ####
cat("Aggregiere production_month Daten nach Jahr (nur Vegetationsperiode April-Oktober)...\n")

# Aggregiere zu jährlichen Mittelwerten (Vegetationsperiode: Monate 4-10)
production_yearly <- production_month %>%
  filter(month >= 4 & month <= 10) %>%
  group_by(year, ru, rid, species) %>%
  summarise(
    tempResponse = mean(tempResponse, na.rm = TRUE),
    waterResponse = mean(waterResponse, na.rm = TRUE),
    vpdResponse = mean(vpdResponse, na.rm = TRUE),
    co2Response = mean(co2Response, na.rm = TRUE),
    nitrogenResponse = mean(nitrogenResponse, na.rm = TRUE),
    .groups = 'drop'
  )

cat("Datengröße reduziert von", nrow(production_month), "auf", nrow(production_yearly), "Zeilen\n")

## Boxplots für Response-Variablen nach Jahr ####

# tempResponse
p_temp <- ggplot(production_yearly, aes(x = factor(year), y = tempResponse)) +
  geom_boxplot(fill = "lightcoral", alpha = 0.7, outlier.size = 0.5, width = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "darkred") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "darkred", size = 1, alpha = 0.8) +
  labs(
    x = "Jahr",
    y = "Temperature Response"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  ) +
  coord_cartesian(ylim = c(0, 1))

# waterResponse
p_water <- ggplot(production_yearly, aes(x = factor(year), y = waterResponse)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7, outlier.size = 0.5, width = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "darkblue") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "darkblue", size = 1, alpha = 0.8) +
  labs(
    x = "Jahr",
    y = "Water Response"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  ) +
  coord_cartesian(ylim = c(0, 1))

# vpdResponse
p_vpd <- ggplot(production_yearly, aes(x = factor(year), y = vpdResponse)) +
  geom_boxplot(fill = "lightgreen", alpha = 0.7, outlier.size = 0.5, width = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "darkgreen") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "darkgreen", size = 1, alpha = 0.8) +
  labs(
    x = "Jahr",
    y = "VPD Response"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  ) +
  coord_cartesian(ylim = c(0, 1))

# co2Response
p_co2 <- ggplot(production_yearly, aes(x = factor(year), y = co2Response)) +
  geom_boxplot(fill = "lightyellow", alpha = 0.7, outlier.size = 0.5, width = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "orange") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "orange", size = 1, alpha = 0.8) +
  labs(
    x = "Jahr",
    y = "CO2 Response"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  ) +
  coord_cartesian(ylim = c(0.5, 1.5))

# nitrogenResponse
p_nitrogen <- ggplot(production_yearly, aes(x = factor(year), y = nitrogenResponse)) +
  geom_boxplot(fill = "plum", alpha = 0.7, outlier.size = 0.5, width = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "purple") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "purple", size = 1, alpha = 0.8) +
  labs(
    x = "Jahr",
    y = "Nitrogen Response"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  ) +
  coord_cartesian(ylim = c(0.5, 1.5))

# Plots anzeigen
combined_plot <- grid.arrange(p_temp, p_co2,
                             p_water, p_nitrogen,
                             p_vpd, grid.rect(gp=gpar(col="white")),
                             ncol = 2, nrow = 3,
                             top = textGrob("Limitierende Faktoren\nÜber alle Arten (während Vegegationsperiode, April - Oktober)", 
                                          gp = gpar(fontsize = 16, fontface = "bold")))

print(combined_plot)

# ## Zusätzliche Analyse: 6 separate Plots für ausgewählte Arten ####
# cat("\nErstelle separate Plots für jede Art: fasy, qupe, piab, abal, psme, pisy\n")

# # Liste der ausgewählten Arten
# selected_species <- c("fasy", "qupe", "piab", "abal", "psme", "pisy")

# # Funktion zum Erstellen eines Plots für eine Art
# create_species_plot <- function(species_name) {
  
#   # Filter für die spezifische Art
#   species_data <- production_yearly %>%
#     filter(species == species_name)
  
#   cat("Erstelle Plot für", species_name, "- Anzahl Datenpunkte:", nrow(species_data), "\n")
  
#   # tempResponse
#   p_temp <- ggplot(species_data, aes(x = factor(year), y = tempResponse)) +
#     geom_boxplot(fill = "lightcoral", alpha = 0.7, outlier.size = 0.5, width = 0.6) +
#     stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "darkred") +
#     stat_summary(fun = mean, geom = "line", aes(group = 1), color = "darkred", size = 1, alpha = 0.8) +
#     labs(x = "Jahr", y = "Temperature Response") +
#     theme_minimal() +
#     theme(
#       panel.grid.major.x = element_blank(),
#       panel.grid.minor = element_blank(),
#       axis.text.x = element_text(angle = 0, hjust = 0.5),
#       axis.title.x = element_text(face = "bold"),
#       axis.title.y = element_text(face = "bold")
#     ) +
#     coord_cartesian(ylim = c(0, 1))
  
#   # waterResponse
#   p_water <- ggplot(species_data, aes(x = factor(year), y = waterResponse)) +
#     geom_boxplot(fill = "lightblue", alpha = 0.7, outlier.size = 0.5, width = 0.6) +
#     stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "darkblue") +
#     stat_summary(fun = mean, geom = "line", aes(group = 1), color = "darkblue", size = 1, alpha = 0.8) +
#     labs(x = "Jahr", y = "Water Response") +
#     theme_minimal() +
#     theme(
#       panel.grid.major.x = element_blank(),
#       panel.grid.minor = element_blank(),
#       axis.text.x = element_text(angle = 0, hjust = 0.5),
#       axis.title.x = element_text(face = "bold"),
#       axis.title.y = element_text(face = "bold")
#     ) +
#     coord_cartesian(ylim = c(0, 1))
  
#   # vpdResponse
#   p_vpd <- ggplot(species_data, aes(x = factor(year), y = vpdResponse)) +
#     geom_boxplot(fill = "lightgreen", alpha = 0.7, outlier.size = 0.5, width = 0.6) +
#     stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "darkgreen") +
#     stat_summary(fun = mean, geom = "line", aes(group = 1), color = "darkgreen", size = 1, alpha = 0.8) +
#     labs(x = "Jahr", y = "VPD Response") +
#     theme_minimal() +
#     theme(
#       panel.grid.major.x = element_blank(),
#       panel.grid.minor = element_blank(),
#       axis.text.x = element_text(angle = 0, hjust = 0.5),
#       axis.title.x = element_text(face = "bold"),
#       axis.title.y = element_text(face = "bold")
#     ) +
#     coord_cartesian(ylim = c(0, 1))
  
#   # co2Response
#   p_co2 <- ggplot(species_data, aes(x = factor(year), y = co2Response)) +
#     geom_boxplot(fill = "lightyellow", alpha = 0.7, outlier.size = 0.5, width = 0.6) +
#     stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "orange") +
#     stat_summary(fun = mean, geom = "line", aes(group = 1), color = "orange", size = 1, alpha = 0.8) +
#     labs(x = "Jahr", y = "CO2 Response") +
#     theme_minimal() +
#     theme(
#       panel.grid.major.x = element_blank(),
#       panel.grid.minor = element_blank(),
#       axis.text.x = element_text(angle = 0, hjust = 0.5),
#       axis.title.x = element_text(face = "bold"),
#       axis.title.y = element_text(face = "bold")
#     ) +
#     coord_cartesian(ylim = c(0.5, 1.5))
  
#   # nitrogenResponse
#   p_nitrogen <- ggplot(species_data, aes(x = factor(year), y = nitrogenResponse)) +
#     geom_boxplot(fill = "plum", alpha = 0.7, outlier.size = 0.5, width = 0.6) +
#     stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "purple") +
#     stat_summary(fun = mean, geom = "line", aes(group = 1), color = "purple", size = 1, alpha = 0.8) +
#     labs(x = "Jahr", y = "Nitrogen Response") +
#     theme_minimal() +
#     theme(
#       panel.grid.major.x = element_blank(),
#       panel.grid.minor = element_blank(),
#       axis.text.x = element_text(angle = 0, hjust = 0.5),
#       axis.title.x = element_text(face = "bold"),
#       axis.title.y = element_text(face = "bold")
#     ) +
#     coord_cartesian(ylim = c(0.5, 1.5))
  

#   # Kombinierter Plot für diese Art
#   combined_plot <- grid.arrange(p_temp, p_co2,
#                                p_water, p_nitrogen,
#                                p_vpd, grid.rect(gp=gpar(col="white")),
#                                ncol = 2, nrow = 3,
#                                top = textGrob(paste("Limitierende Faktoren -", toupper(species_name), "\n(Jahresmittel)"), 
#                                             gp = gpar(fontsize = 16, fontface = "bold")))
  
#   return(combined_plot)
# }

# # Erstelle und zeige Plots für alle 6 Arten
# for(species in selected_species) {
#   species_plot <- create_species_plot(species)
#   print(species_plot)
# } 


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Analyse limitierende Faktoren ###############################################################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

cat("\n=== Analyse des limitierenden Faktors ===\n")

## Daten filtern (Jahre 11-33 entsprechen 2000-2022) ####
production_filtered <- production_yearly %>%
  filter(year >= 11) %>%
  mutate(calendar_year = year - 11 + 2000)

## Jährliche Mediane berechnen ####
yearly_medians <- production_filtered %>%
  group_by(calendar_year) %>%
  summarise(
    Temp = median(tempResponse, na.rm = TRUE),
    Water = median(waterResponse, na.rm = TRUE),
    VPD = median(vpdResponse, na.rm = TRUE),
    Temp_Q25 = quantile(tempResponse, 0.25, na.rm = TRUE),
    Temp_Q75 = quantile(tempResponse, 0.75, na.rm = TRUE),
    Water_Q25 = quantile(waterResponse, 0.25, na.rm = TRUE),
    Water_Q75 = quantile(waterResponse, 0.75, na.rm = TRUE),
    VPD_Q25 = quantile(vpdResponse, 0.25, na.rm = TRUE),
    VPD_Q75 = quantile(vpdResponse, 0.75, na.rm = TRUE),
    .groups = 'drop'
  )

# Limitierenden Faktor bestimmen
yearly_medians <- yearly_medians %>%
  mutate(
    limiting_factor = case_when(
      Temp <= Water & Temp <= VPD ~ "Temp",
      Water <= Temp & Water <= VPD ~ "Water",
      TRUE ~ "VPD"
    )
  )

# Daten für Plotting vorbereiten
yearly_long <- yearly_medians %>%
  pivot_longer(
    cols = c(Temp, Water, VPD),
    names_to = "Factor",
    values_to = "Response"
  ) %>%
  left_join(
    yearly_medians %>%
      pivot_longer(cols = c(Temp_Q25, Water_Q25, VPD_Q25), 
                   names_to = "Factor_Q25", values_to = "Q25") %>%
      mutate(Factor = gsub("_Q25", "", Factor_Q25)) %>%
      select(calendar_year, Factor, Q25),
    by = c("calendar_year", "Factor")
  ) %>%
  left_join(
    yearly_medians %>%
      pivot_longer(cols = c(Temp_Q75, Water_Q75, VPD_Q75), 
                   names_to = "Factor_Q75", values_to = "Q75") %>%
      mutate(Factor = gsub("_Q75", "", Factor_Q75)) %>%
      select(calendar_year, Factor, Q75),
    by = c("calendar_year", "Factor")
  ) %>%
  mutate(Factor = case_when(
    Factor == "Temp" ~ "Temperatur",
    Factor == "Water" ~ "Bodenwasserpotential",
    Factor == "VPD" ~ "Dampfdruckdefizit"
  ))

# Farben definieren
factor_colors <- c("Temperatur" = "#E74C3C", "Bodenwasserpotential" = "#3498DB", "Dampfdruckdefizit" = "#27AE60")

## Plot A: Zeitreihe ####
plot_A <- ggplot(yearly_long, aes(x = calendar_year, y = Response, color = Factor, fill = Factor)) +
  geom_ribbon(aes(ymin = Q25, ymax = Q75), alpha = 0.2, color = NA) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  scale_color_manual(values = factor_colors, name = "") +
  scale_fill_manual(values = factor_colors, name = "") +
  labs(
    title = "A",
    x = "Jahr",
    y = "Limitierungs-Index"
  ) +
  theme_minimal(base_family = "Arial") +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 11),
    axis.text = element_text(size = 11),
    axis.text.x = element_text(size = 11),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(breaks = seq(2000, 2020, 5)) +
  scale_y_continuous(limits = c(0.5, 1.0), breaks = seq(0.5, 1.0, 0.1),
                     labels = scales::label_number(decimal.mark = ","))

## Plot B: Häufigkeit ####
all_factors <- data.frame(
  limiting_factor = c("Temp", "Water", "VPD"),
  factor_label = c("Temperatur", "Bodenwasser-\npotential", "Dampfdruck-\ndefizit")
)

limiting_freq <- yearly_medians %>%
  count(limiting_factor) %>%
  right_join(all_factors, by = "limiting_factor") %>%
  mutate(
    n = ifelse(is.na(n), 0, n),
    percentage = n / sum(n) * 100,
    label = paste0(round(percentage), "%\n(n=", n, ")")
  )

# Farben für Plot B
factor_colors_B <- c("Temp" = "#E74C3C", "Water" = "#3498DB", "VPD" = "#27AE60")

plot_B <- ggplot(limiting_freq, aes(x = factor_label, y = percentage, fill = limiting_factor)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = label), vjust = -0.3, size = 3.5, fontface = "bold", family = "Arial") +
  scale_fill_manual(values = factor_colors_B) +
  labs(
    title = "B",
    x = "",
    y = "Häufigkeit [%]"
  ) +
  theme_minimal(base_family = "Arial") +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 11),
    axis.text = element_text(size = 11),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  scale_y_continuous(limits = c(0, 120), breaks = c(0, 25, 50, 75, 100), expand = expansion(mult = c(0, 0.05))) +
  scale_x_discrete(limits = c("Temperatur", "Bodenwasser-\npotential", "Dampfdruck-\ndefizit"))

## Plot C: Boxplots ####
boxplot_data <- production_filtered %>%
  select(calendar_year, tempResponse, waterResponse, vpdResponse) %>%
  pivot_longer(
    cols = c(tempResponse, waterResponse, vpdResponse),
    names_to = "Factor",
    values_to = "Response"
  ) %>%
  mutate(
    Factor_orig = case_when(
      Factor == "tempResponse" ~ "Temp",
      Factor == "waterResponse" ~ "Water",
      Factor == "vpdResponse" ~ "VPD"
    ),
    Factor_label = case_when(
      Factor == "tempResponse" ~ "Temperatur",
      Factor == "waterResponse" ~ "Bodenwasser-\npotential",
      Factor == "vpdResponse" ~ "Dampfdruck-\ndefizit"
    )
  )

# Mittelwerte berechnen
boxplot_means <- boxplot_data %>%
  group_by(Factor_label, Factor_orig) %>%
  summarise(
    mean_val = mean(Response, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(mean_label = format(round(mean_val, 2), decimal.mark = ","))

# Farben für Plot C
factor_colors_C <- c("Temp" = "#E74C3C", "Water" = "#3498DB", "VPD" = "#27AE60")

plot_C <- ggplot(boxplot_data, aes(x = Factor_label, y = Response, fill = Factor_orig)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.5, width = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "black") +
  geom_text(data = boxplot_means, 
            aes(x = Factor_label, y = 1.02, label = mean_label),
            fontface = "bold", size = 5, family = "Arial") +
  scale_fill_manual(values = factor_colors_C) +
  labs(
    title = "C",
    x = "",
    y = "Index-Wert"
  ) +
  theme_minimal(base_family = "Arial") +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 11),
    panel.grid.major.x = element_blank()
  ) +
  scale_y_continuous(limits = c(0.5, 1.1), breaks = seq(0.5, 1.0, 0.1),
                     labels = scales::label_number(decimal.mark = ",")) +
  scale_x_discrete(limits = c("Temperatur", "Bodenwasser-\npotential", "Dampfdruck-\ndefizit"))

## Kombinierte Plots ####
library(patchwork)

# Plot A mit Legende
plot_A_with_legend <- plot_A + 
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 11),
    legend.key.size = unit(0.8, "cm"),
    legend.key.width = unit(1.2, "cm"),
    legend.spacing.x = unit(0.3, "cm")
  ) +
  guides(color = guide_legend(nrow = 1, override.aes = list(linewidth = 1, size = 2)))

# Plots kombinieren
combined_limiting_plot <- (plot_A_with_legend / plot_B) +
  plot_layout(heights = c(2, 1))

print(combined_limiting_plot)

# Plot speichern
ggsave(
  filename = "C:/Users/Jakob/Desktop/Limitierende_Faktoren_Analyse.png",
  plot = combined_limiting_plot,
  width = 15,
  height = 17,
  units = "cm",
  dpi = 300,
  bg = "white"
)
cat("\nPlot gespeichert unter: C:/Users/Jakob/Desktop/Limitierende_Faktoren_Analyse.png\n")

# Statistik ausgeben
cat("\n=== Zusammenfassung ===\n")
cat("Anzahl Jahre analysiert:", nrow(yearly_medians), "\n\n")
cat("Häufigkeit der Limitierung:\n")
print(limiting_freq)
cat("\nMittlere Response-Werte über alle Jahre:\n")
print(boxplot_means)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Baumartenspezifische Analysen ###############################################################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

cat("\n=== Baumartenspezifische Analyse ===\n")

# Artennamen
selected_species <- c("fasy", "qupe", "piab", "abal", "psme", "pisy")
species_names_de <- c(
  "fasy" = "Rotbuche (Fagus sylvatica)",
  "qupe" = "Traubeneiche (Quercus petraea)", 
  "piab" = "Fichte (Picea abies)",
  "abal" = "Weißtanne (Abies alba)",
  "psme" = "Douglasie (Pseudotsuga menziesii)",
  "pisy" = "Waldkiefer (Pinus sylvestris)"
)

# Funktion für artspezifischen Plot
create_species_limiting_plot <- function(species_code) {
  
  cat("\nErstelle Plot für:", species_names_de[species_code], "\n")
  
  # Daten für diese Art filtern
  species_filtered <- production_filtered %>%
    filter(species == species_code)
  
  if(nrow(species_filtered) == 0) {
    cat("  -> Keine Daten für", species_code, "vorhanden!\n")
    return(NULL)
  }
  
  cat("  -> Anzahl Datenpunkte:", nrow(species_filtered), "\n")
  
  # Jährliche Mediane berechnen
  species_yearly_medians <- species_filtered %>%
    group_by(calendar_year) %>%
    summarise(
      Temp = median(tempResponse, na.rm = TRUE),
      Water = median(waterResponse, na.rm = TRUE),
      VPD = median(vpdResponse, na.rm = TRUE),
      Temp_Q25 = quantile(tempResponse, 0.25, na.rm = TRUE),
      Temp_Q75 = quantile(tempResponse, 0.75, na.rm = TRUE),
      Water_Q25 = quantile(waterResponse, 0.25, na.rm = TRUE),
      Water_Q75 = quantile(waterResponse, 0.75, na.rm = TRUE),
      VPD_Q25 = quantile(vpdResponse, 0.25, na.rm = TRUE),
      VPD_Q75 = quantile(vpdResponse, 0.75, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Limitierenden Faktor bestimmen
  species_yearly_medians <- species_yearly_medians %>%
    mutate(
      limiting_factor = case_when(
        Temp <= Water & Temp <= VPD ~ "Temp",
        Water <= Temp & Water <= VPD ~ "Water",
        TRUE ~ "VPD"
      )
    )
  
  # Daten für Plotting vorbereiten
  species_yearly_long <- species_yearly_medians %>%
    pivot_longer(
      cols = c(Temp, Water, VPD),
      names_to = "Factor",
      values_to = "Response"
    ) %>%
    left_join(
      species_yearly_medians %>%
        pivot_longer(cols = c(Temp_Q25, Water_Q25, VPD_Q25), 
                     names_to = "Factor_Q25", values_to = "Q25") %>%
        mutate(Factor = gsub("_Q25", "", Factor_Q25)) %>%
        select(calendar_year, Factor, Q25),
      by = c("calendar_year", "Factor")
    ) %>%
    left_join(
      species_yearly_medians %>%
        pivot_longer(cols = c(Temp_Q75, Water_Q75, VPD_Q75), 
                     names_to = "Factor_Q75", values_to = "Q75") %>%
        mutate(Factor = gsub("_Q75", "", Factor_Q75)) %>%
        select(calendar_year, Factor, Q75),
      by = c("calendar_year", "Factor")
    ) %>%
    mutate(Factor = case_when(
      Factor == "Temp" ~ "Temperatur",
      Factor == "Water" ~ "Bodenwasserpotential",
      Factor == "VPD" ~ "Dampfdruckdefizit"
    ))
  
  # Plot A: Zeitreihe
  species_plot_A <- ggplot(species_yearly_long, aes(x = calendar_year, y = Response, color = Factor, fill = Factor)) +
    geom_ribbon(aes(ymin = Q25, ymax = Q75), alpha = 0.2, color = NA) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.8) +
    scale_color_manual(values = factor_colors, name = "") +
    scale_fill_manual(values = factor_colors, name = "") +
    labs(
      title = "A",
      x = "Jahr",
      y = "Limitierungs-Index"
    ) +
    theme_minimal(base_family = "Arial") +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 14),
      axis.title = element_text(face = "bold", size = 11),
      axis.text = element_text(size = 11),
      panel.grid.minor = element_blank()
    ) +
    scale_x_continuous(breaks = seq(2000, 2020, 5)) +
    scale_y_continuous(limits = c(0.5, 1.0), breaks = seq(0.5, 1.0, 0.1),
                       labels = scales::label_number(decimal.mark = ","))
  
  # Plot B
  species_limiting_freq <- species_yearly_medians %>%
    count(limiting_factor) %>%
    right_join(all_factors, by = "limiting_factor") %>%
    mutate(
      n = ifelse(is.na(n), 0, n),
      percentage = n / sum(n) * 100,
      label = paste0(round(percentage), "%\n(n=", n, ")")
    )
  
  species_plot_B <- ggplot(species_limiting_freq, aes(x = factor_label, y = percentage, fill = limiting_factor)) +
    geom_bar(stat = "identity", width = 0.6) +
    geom_text(aes(label = label), vjust = -0.3, size = 3.5, fontface = "bold", family = "Arial") +
    scale_fill_manual(values = factor_colors_B) +
    labs(
      title = "B",
      x = "",
      y = "Häufigkeit [%]"
    ) +
    theme_minimal(base_family = "Arial") +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 14),
      axis.title = element_text(face = "bold", size = 11),
      axis.text = element_text(size = 11),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank()
    ) +
    scale_y_continuous(limits = c(0, 120), breaks = c(0, 25, 50, 75, 100), expand = expansion(mult = c(0, 0.05))) +
    scale_x_discrete(limits = c("Temperatur", "Bodenwasser-\npotential", "Dampfdruck-\ndefizit"))
  
  # Plot A mit Legende
  species_plot_A_legend <- species_plot_A + 
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 11),
      legend.key.size = unit(0.8, "cm"),
      legend.key.width = unit(1.2, "cm"),
      legend.spacing.x = unit(0.3, "cm")
    ) +
    guides(color = guide_legend(nrow = 1, override.aes = list(linewidth = 1, size = 2)))
  
  # Plots kombinieren
  species_combined <- (species_plot_A_legend / species_plot_B) +
    plot_layout(heights = c(2, 1))
  
  # Speichern
  filename <- paste0("C:/Users/Jakob/Desktop/Limitierende_Faktoren_", toupper(species_code), ".png")
  ggsave(
    filename = filename,
    plot = species_combined,
    width = 15,
    height = 17,
    units = "cm",
    dpi = 300,
    bg = "white"
  )
  cat("  -> Gespeichert:", filename, "\n")
  
  # Statistik ausgeben
  cat("\n  Häufigkeit für", species_code, ":\n")
  print(species_limiting_freq[, c("limiting_factor", "n", "percentage")])
  
  return(species_combined)
}

# Plots erstellen
for(sp in selected_species) {
  species_plot <- create_species_limiting_plot(sp)
  if(!is.null(species_plot)) {
    print(species_plot)
  }
}

cat("\n=== Artspezifische Plots erstellt ===\n")

