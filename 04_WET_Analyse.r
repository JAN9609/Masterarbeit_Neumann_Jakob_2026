# WET-Analyse: Vergleich aller Risikostufen (gering, mittel, hoch)
# Autor: Neumann, J. (2025)


# Bibliotheken laden
library(tidyverse)
library(RSQLite)
library(dbplyr)
library(terra)
library(grid)
library(gridExtra)
library(zoo)
library(cowplot)
library(ggtext)
library(readxl)
library(writexl)


# Arbeitsverzeichnis und Pfade
setwd("C:/Users/Jakob/OneDrive/OneDrive Dokumente/Universitaet/02_Master/Masterarbeit/iLand_SOSFOR/Iland_project/temp")

# Environment-Datei laden
(env.df <- read_delim("../gis/environment.txt"))

# DEM raster
dem.grid <- terra::rast("C:/Users/Jakob/OneDrive/OneDrive Dokumente/Universitaet/02_Master/Masterarbeit/iLand_SOSFOR/Iland_project/gis/dem.asc")

# Koordinaten umrechnen
env.df$x_orig <- env.df$x
env.df$y_orig <- env.df$y
env.df <- env.df %>%
  mutate(
    x = 405400 + x * 100,
    y = 5277726 + y * 100
  )
env.df$elevation <- terra::extract(
  terra::aggregate(dem.grid, fact = 10, fun = "mean"), 
  env.df[, c("x", "y")], ID = FALSE) %>% unlist()

# Artendatenbank für Farben laden
db.conn <- RSQLite::dbConnect(RSQLite::SQLite(),
                              dbname = "../database/all_species_database.sqlite")
species.df <- dplyr::tbl(db.conn, "species") %>% 
  collect()
RSQLite::dbDisconnect(db.conn); rm(db.conn)

# Create color palette for species from species database
species_colors <- species.df %>% 
  arrange(shortName) %>% 
  mutate(displayColor = paste0("#", displayColor)) %>% 
  select(shortName, displayColor) %>%
  deframe()

# Definition der Risikostufen 

risk_levels <- list(
  gering = list(
    sqlite = "project_CSA_20251031_083643.sqlite",
    targets = "g_eng_Muenstertal_ABE-lib_targets_v1.0.csv",
    label = "geringes Risiko",
    color = "#2ecc71"
  ),
  mittel = list(
    sqlite = "project_CSA_20251101_110746.sqlite",
    targets = "m_eng_Muenstertal_ABE-lib_targets_v1.0.csv",
    label = "mittleres Risiko",
    color = "#f39c12"
  ),
  hoch = list(
    sqlite = "project_CSA_20251101_105114.sqlite",
    targets = "x_eng_Muenstertal_ABE-lib_targets_v1.0.csv",
    label = "hohes Risiko",
    color = "#e74c3c"
  )
)

# Spin-up Periode
SPINUP_YEARS <- 20

# WET-spezifische Konfiguration für Altersgrenzen und Altersklassen
wet_config <- list(
  "WET_bg" = list(max_age = 120, max_age_class = 6, age_class_names = c("I", "II", "III", "IV", "V", "VI")),
  "WET_bm" = list(max_age = 120, max_age_class = 6, age_class_names = c("I", "II", "III", "IV", "V", "VI")),
  "WET_bx" = list(max_age = 120, max_age_class = 6, age_class_names = c("I", "II", "III", "IV", "V", "VI")),
  "WET_eg_Schirm" = list(max_age = 180, max_age_class = 9, age_class_names = c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX")),
  "WET_em_Schirm" = list(max_age = 180, max_age_class = 9, age_class_names = c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX")),
  "WET_eg_Femel" = list(max_age = 180, max_age_class = 9, age_class_names = c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX")),
  "WET_em_Femel" = list(max_age = 180, max_age_class = 9, age_class_names = c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX")),
  "WET_fg" = list(max_age = 100, max_age_class = 5, age_class_names = c("I", "II", "III", "IV", "V")),
  "WET_fm" = list(max_age = 100, max_age_class = 5, age_class_names = c("I", "II", "III", "IV", "V")),
  "WET_fx" = list(max_age = 100, max_age_class = 5, age_class_names = c("I", "II", "III", "IV", "V")),
  "WET_tg" = list(max_age = 100, max_age_class = 5, age_class_names = c("I", "II", "III", "IV", "V")),
  "WET_tm" = list(max_age = 100, max_age_class = 5, age_class_names = c("I", "II", "III", "IV", "V")),
  "WET_tx" = list(max_age = 100, max_age_class = 5, age_class_names = c("I", "II", "III", "IV", "V")),
  "WET_dg" = list(max_age = 100, max_age_class = 5, age_class_names = c("I", "II", "III", "IV", "V")),
  "WET_dm" = list(max_age = 100, max_age_class = 5, age_class_names = c("I", "II", "III", "IV", "V")),
  "WET_kg" = list(max_age = 140, max_age_class = 7, age_class_names = c("I", "II", "III", "IV", "V", "VI", "VII")),
  "WET_km" = list(max_age = 140, max_age_class = 7, age_class_names = c("I", "II", "III", "IV", "V", "VI", "VII")),
  "WET_kx" = list(max_age = 140, max_age_class = 7, age_class_names = c("I", "II", "III", "IV", "V", "VI", "VII")),
  "WET_h2" = list(max_age = 120, max_age_class = 6, age_class_names = c("I", "II", "III", "IV", "V", "VI")),
  "WET_j" = list(max_age = 120, max_age_class = 6, age_class_names = c("I", "II", "III", "IV", "V", "VI"))
)

# Funktion zum Laden der Daten für eine Risikostufe 

load_risk_data <- function(risk_name, risk_config) {
  cat("\n=== Lade Daten für:", risk_config$label, "===\n")
  
  # Targets-Datei laden
  targets_path <- paste0("C:/Users/Jakob/OneDrive/OneDrive Dokumente/Universitaet/02_Master/Masterarbeit/iLand_SOSFOR/AgentFiles/", 
                        risk_config$targets)
  
  spinup_targets <- read.csv(targets_path, sep = ",") %>%
    select(id, stp, targetSpecies, U, wet, area_ha) %>%
    rename(standid = id) %>%
    mutate(
      U = as.numeric(U),
      area_ha = as.numeric(area_ha)
    )
  
  cat("Spinup targets loaded:", nrow(spinup_targets), "rows\n")
  
  # Verbindung zur SQLite-Datenbank
  db_path <- file.path("../output/", risk_config$sqlite)
  
  if (!file.exists(db_path)) {
    cat("ERROR: SQLite file not found:", db_path, "\n")
    return(NULL)
  }
  
  db.conn <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = db_path)
  
  # Daten laden (ohne Spin-up Jahre)
  abeStand <- dplyr::tbl(db.conn, "abeStand") %>% 
    filter(year > SPINUP_YEARS) %>%
    collect()
  
  abeStandDetail <- dplyr::tbl(db.conn, "abeStandDetail") %>% 
    filter(year > SPINUP_YEARS) %>%
    collect()
  
  abeStandRemoval <- dplyr::tbl(db.conn, "abeStandRemoval") %>% 
    filter(year > SPINUP_YEARS) %>%
    collect()
  
  RSQLite::dbDisconnect(db.conn)
  
  cat("Data loaded - abeStand:", nrow(abeStand), "rows\n")
  
  # abeStand zusammenführen und filtern
  abeStand_merged <- abeStand %>%
    left_join(spinup_targets, by = "standid") %>%
    filter(area >= 1) %>%
    mutate(risk_level = risk_config$label)
  
  # abeStandDetail zusammenführen
  abeStandDetail_merged <- abeStandDetail %>%
    left_join(spinup_targets, by = "standid") %>%
    filter(standid %in% unique(abeStand_merged$standid)) %>%
    mutate(risk_level = risk_config$label)
  
  # Altersinformation zu abeStandDetail hinzufügen
  abeStandDetail_merged <- abeStandDetail_merged %>%
    left_join(
      abeStand_merged %>% select(standid, year, age),
      by = c("standid", "year")
    )
  
  # abeStandRemoval zusammenführen (hat bereits age-Spalte aus SQLite)
  abeStandRemoval_merged <- abeStandRemoval %>%
    left_join(spinup_targets, by = "standid") %>%
    filter(standid %in% unique(abeStand_merged$standid)) %>%
    mutate(risk_level = risk_config$label)
  
  cat("  abeStandRemoval:", nrow(abeStandRemoval_merged), "rows\n")
  
  # Fläche pro WET berechnen
  area_per_wet <- spinup_targets %>%
    group_by(stp) %>%
    summarise(
      total_area_ha = sum(area_ha, na.rm = TRUE),
      n_stands = n(),
      .groups = 'drop'
    )
  
  cat("Merge completed - abeStand_merged:", nrow(abeStand_merged), "rows\n")
  
  return(list(
    abeStand = abeStand_merged,
    abeStandDetail = abeStandDetail_merged,
    abeStandRemoval = abeStandRemoval_merged,
    targets = spinup_targets,
    area_per_wet = area_per_wet
  ))
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# DATEN FÜR ALLE RISIKOSTUFEN LADEN ########################################################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("LADE DATEN FÜR ALLE RISIKOSTUFEN\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

all_data <- lapply(names(risk_levels), function(name) {
  load_risk_data(name, risk_levels[[name]])
})
names(all_data) <- names(risk_levels)

# Prüfen ob alle Daten erfolgreich geladen wurden
successful_loads <- sapply(all_data, function(x) !is.null(x))
if (!all(successful_loads)) {
  cat("WARNING: Some risk levels failed to load:\n")
  print(names(all_data)[!successful_loads])
}

# Daten aller Risikostufen kombinieren
combined_abeStand <- bind_rows(lapply(all_data[successful_loads], function(x) x$abeStand))
combined_abeStandDetail <- bind_rows(lapply(all_data[successful_loads], function(x) x$abeStandDetail))
combined_abeStandRemoval <- bind_rows(lapply(all_data[successful_loads], function(x) x$abeStandRemoval))

# Faktorreihenfolge festlegen
risk_order <- c("geringes Risiko", "mittleres Risiko", "hohes Risiko")
combined_abeStand$risk_level <- factor(combined_abeStand$risk_level, levels = risk_order)
combined_abeStandDetail$risk_level <- factor(combined_abeStandDetail$risk_level, levels = risk_order)
combined_abeStandRemoval$risk_level <- factor(combined_abeStandRemoval$risk_level, levels = risk_order)

cat("\n=== DATEN ALLER RISIKOSTUFEN GELADEN ===\n")
cat("Gesamt abeStand Zeilen:", nrow(combined_abeStand), "\n")
cat("Gesamt abeStandDetail Zeilen:", nrow(combined_abeStandDetail), "\n")
cat("Gesamt abeStandRemoval Zeilen:", nrow(combined_abeStandRemoval), "\n")

# Übersicht der Daten pro Risikostufe
cat("\nDaten pro Risikostufe:\n")
combined_abeStand %>%
  group_by(risk_level) %>%
  summarise(
    n_stands = n_distinct(standid),
    n_observations = n(),
    .groups = 'drop'
  ) %>%
  print()

# Ertragstafel laden (optional) 

# Ertragstafel configuration
ertragstafel_config <- list(
  "WET_bg" = list(sheet = "Buche", column = "OHB100_32,5", column_md = "OHB100_32,5_md"),
  "WET_eg_Schirm" = list(sheet = "Eiche", column = "OHB100_33", column_md = "OHB100_33_md"),
  "WET_eg_Femel" = list(sheet = "Eiche", column = "OHB100_33", column_md = "OHB100_33_md"),
  "WET_fg" = list(sheet = "Fichte", column = "OHB100_39", column_md = "OHB100_39_md"),
  "WET_tg" = list(sheet = "Fichte", column = "OHB100_39", column_md = "OHB100_39_md"),
  "WET_dg" = list(sheet = "Douglasie", column = "OHB100_45", column_md = "OHB100_45_md"),
  "WET_kg" = list(sheet = "Kiefer", column = "OHB100_29", column_md = "OHB100_29_md")
)

ertragstafel_path <- "C:/Users/Jakob/OneDrive/OneDrive Dokumente/Universitaet/02_Master/Masterarbeit/Auswertung/01_WET/Vorrat_NW-FVA.xlsx"

all_ertragstafel_data <- list()
all_ertragstafel_data_dbh <- list()

if (file.exists(ertragstafel_path)) {
  sheet_names <- readxl::excel_sheets(ertragstafel_path)
  
  for (stp_name in names(ertragstafel_config)) {
    config <- ertragstafel_config[[stp_name]]
    sheet_name <- config$sheet
    column_name <- config$column
    column_name_dbh <- config$column_md
    
    if (sheet_name %in% sheet_names) {
      ertragstafel_sheet <- readxl::read_excel(ertragstafel_path, sheet = sheet_name)
      
      age_col <- "Alter"
      
      # Vorratsdaten
      if (age_col %in% colnames(ertragstafel_sheet) && column_name %in% colnames(ertragstafel_sheet)) {
        ertragstafel_data <- ertragstafel_sheet %>%
          select(age = all_of(age_col), volume = all_of(column_name)) %>%
          filter(!is.na(age) & !is.na(volume)) %>%
          mutate(age = as.numeric(age), volume = as.numeric(volume)) %>%
          filter(!is.na(age) & !is.na(volume) & age >= 0 & age <= 200) %>%
          arrange(age)
        
        if (nrow(ertragstafel_data) > 0) {
          all_ertragstafel_data[[stp_name]] <- ertragstafel_data
        }
      }
      
      # BHD-Daten
      if (age_col %in% colnames(ertragstafel_sheet) && column_name_dbh %in% colnames(ertragstafel_sheet)) {
        ertragstafel_data_dbh <- ertragstafel_sheet %>%
          select(age = all_of(age_col), dbh = all_of(column_name_dbh)) %>%
          filter(!is.na(age) & !is.na(dbh)) %>%
          mutate(age = as.numeric(age), dbh = as.numeric(dbh)) %>%
          filter(!is.na(age) & !is.na(dbh) & age >= 0 & age <= 200) %>%
          arrange(age)
        
        if (nrow(ertragstafel_data_dbh) > 0) {
          all_ertragstafel_data_dbh[[stp_name]] <- ertragstafel_data_dbh
        }
      }
    }
  }
  cat("Ertragstafel data loaded for", length(all_ertragstafel_data), "WET types\n")
} else {
  cat("Ertragstafel file not found - continuing without reference curves\n")
}

# Ausgabeverzeichnis erstellen 

output_dir <- "C:/Users/Jakob/Desktop/Ergebnisse"

# Erstelle Ordner falls nicht vorhanden
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("Ausgabe-Ordner erstellt:", output_dir, "\n")
} else {
  cat("Ausgabe-Ordner existiert bereits:", output_dir, "\n")
}

# Theme-Definition 

# Define modern theme with white background
theme_risk_comparison <- theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5, color = "gray20"),
    plot.subtitle = element_text(size = 10, color = "gray40", hjust = 0.5),
    axis.title = element_text(size = 10, face = "bold", color = "gray20"),
    axis.text = element_text(size = 9, color = "gray30"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#E0E0E0", linewidth = 0.3),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(size = 11, face = "bold", color = "gray20"),
    legend.position = "bottom",
    legend.background = element_rect(fill = "white", color = NA)
  )

# WET-Gruppen definieren 

# WET-Gruppen: Jede Gruppe repräsentiert denselben Waldtyp über alle Risikostufen
# Format: Gruppenname = list(gering = "WET_Xg", mittel = "WET_Xm", hoch = "WET_Xx")
# available_risks: Welche Risikostufen für diesen WET verfügbar sind
#   - c("gering", "mittel", "hoch"): 3 Spalten
#   - c("gering", "mittel"): 2 Spalten  
#   - c("gering"): 1 Spalte (keine Risikoeinstufung)
wet_groups <- list(
  "Buchen-Mischwald" = list(
    gering = "WET_bg",
    mittel = "WET_bm", 
    hoch = "WET_bx",
    ertrag_key = "WET_bg",
    available_risks = c("gering", "mittel", "hoch")
  ),
  "Eichen-Schirmschlag" = list(
    gering = "WET_eg_Schirm",
    mittel = "WET_em_Schirm",
    hoch = NULL,
    ertrag_key = "WET_eg_Schirm",
    available_risks = c("gering", "mittel")
  ),
  "Eichen-Femelschlag" = list(
    gering = "WET_eg_Femel",
    mittel = "WET_em_Femel",
    hoch = NULL,
    ertrag_key = "WET_eg_Femel",
    available_risks = c("gering", "mittel")
  ),
  "Douglasien-Mischwald" = list(
    gering = "WET_dg",
    mittel = "WET_dm",
    hoch = NULL,
    ertrag_key = "WET_dg",
    available_risks = c("gering", "mittel")
  ),
  "Fichten-Mischwald" = list(
    gering = "WET_fg",
    mittel = "WET_fm",
    hoch = "WET_fx",
    ertrag_key = "WET_fg",
    available_risks = c("gering", "mittel", "hoch")
  ),
  "Buntlaubbaum-Mischwald frisch" = list(
    gering = "WET_h2",
    mittel = NULL,
    hoch = NULL,
    ertrag_key = NULL,
    available_risks = c("gering")
  ),
  "Buntlaubbaum-Mischwald trocken" = list(
    gering = "WET_j",
    mittel = NULL,
    hoch = NULL,
    ertrag_key = NULL,
    available_risks = c("gering")
  ),
  "Kiefern-Mischwald" = list(
    gering = "WET_kg",
    mittel = "WET_km",
    hoch = "WET_kx",
    ertrag_key = "WET_kg",
    available_risks = c("gering", "mittel", "hoch")
  ),
  "Tannen-Mischwald" = list(
    gering = "WET_tg",
    mittel = "WET_tm",
    hoch = "WET_tx",
    ertrag_key = "WET_tg",
    available_risks = c("gering", "mittel", "hoch")  # Alle 3 Risikostufen
  )
)

cat("\nDefinierte WET-Gruppen für Risikovergleich:\n")
for (group_name in names(wet_groups)) {
  group <- wet_groups[[group_name]]
  n_risks <- length(group$available_risks)
  cat("  ", group_name, "(", n_risks, "Risikostufen):", paste(group$available_risks, collapse=", "), "\n")
}

# Vordefinierte Statistiken pro WET-Gruppe (N und Fläche)
wet_statistics <- data.frame(
  wet_group = c("Buchen-Mischwald", "Fichten-Mischwald", "Douglasien-Mischwald", 
                "Tannen-Mischwald", "Eichen-Schirmschlag", "Eichen-Femelschlag",
                "Buntlaubbaum-Mischwald frisch", "Buntlaubbaum-Mischwald trocken", "Kiefern-Mischwald"),
  total_area_ha = c(1775, 1465, 993, 813, 103.2, 2.3, 51.7, 4, 15.7),
  n_stands = c(454, 396, 290, 207, 41, 10, 42, 1, 2),
  stringsAsFactors = FALSE
)

# Schleife über alle WET-Gruppen 

for (group_name in names(wet_groups)) {
  
  current_group <- wet_groups[[group_name]]
  
  # Bestimme verfügbare Risikostufen für diese WET-Gruppe
  available_risks <- current_group$available_risks
  n_risk_levels <- length(available_risks)
  
  cat("\n", paste(rep("=", 80), collapse = ""), "\n")
  cat("ERSTELLE VERGLEICHSPLOTS FÜR:", group_name, "(", n_risk_levels, "Risikostufen)\n")
  cat("  Verfügbare Risikostufen:", paste(available_risks, collapse = ", "), "\n")
  cat(paste(rep("=", 80), collapse = ""), "\n")
  
  # WET-spezifische Konfiguration laden
  current_wet_config <- wet_config[[current_group$gering]]
  max_age_limit <- if(!is.null(current_wet_config)) current_wet_config$max_age else 120
  max_age_class_num <- if(!is.null(current_wet_config)) current_wet_config$max_age_class else 6
  age_class_names <- if(!is.null(current_wet_config)) current_wet_config$age_class_names else c("I", "II", "III", "IV", "V", "VI")
  
  # Ertragstafel-Daten für diese WET-Gruppe laden
  ertrag_key <- current_group$ertrag_key
  current_ertragstafel_data <- if(!is.null(ertrag_key)) all_ertragstafel_data[[ertrag_key]] else NULL
  current_ertragstafel_data_dbh <- if(!is.null(ertrag_key)) all_ertragstafel_data_dbh[[ertrag_key]] else NULL
  
  # Daten für diese WET-Gruppe filtern
  risk_label_mapping <- c("gering" = "geringes Risiko", "mittel" = "mittleres Risiko", "hoch" = "hohes Risiko")
  
  # WET-Zuordnung erstellen
  wet_mapping <- data.frame(
    risk_level = factor(risk_label_mapping[available_risks], 
                       levels = c("geringes Risiko", "mittleres Risiko", "hohes Risiko")),
    wet_name = sapply(available_risks, function(r) current_group[[r]]),
    stringsAsFactors = FALSE
  ) %>% filter(!is.na(wet_name))
  
  # Daten filtern: für jede Risikostufe das entsprechende WET verwenden
  wet_data <- combined_abeStand %>%
    inner_join(wet_mapping, by = "risk_level") %>%
    filter(stp == wet_name & !is.na(age) & !is.na(volume)) %>%
    select(-wet_name)
  
  wet_detail_data <- combined_abeStandDetail %>%
    inner_join(wet_mapping, by = "risk_level") %>%
    filter(stp == wet_name & !is.na(age) & !is.na(species)) %>%
    select(-wet_name)
  
  wet_removal_data <- combined_abeStandRemoval %>%
    inner_join(wet_mapping, by = "risk_level") %>%
    filter(stp == wet_name & !is.na(age)) %>%
    mutate(
      total_removal_efm = (volumeThinning + volumeFinal + volumeSalvaged + volumeDisturbed) * 0.8
    ) %>%
    filter(total_removal_efm > 0) %>%
    select(-wet_name)
  
  if (nrow(wet_data) == 0) {
    cat("Keine Daten für WET-Gruppe:", group_name, "- überspringe\n")
    next
  }
  
  # Berechne N und Fläche pro Risikostufe für Titel
  stats_per_risk <- wet_data %>%
    group_by(risk_level) %>%
    summarise(
      n_stands = n_distinct(standid),
      .groups = 'drop'
    )
  
  # Übernehme N und Fläche aus vordefinierter Statistiktabelle
  wet_stats <- wet_statistics %>%
    filter(wet_group == group_name)
  
  if (nrow(wet_stats) > 0) {
    total_n_stands <- wet_stats$n_stands[1]
    total_area_ha <- wet_stats$total_area_ha[1]
  } else {
    # Fallback: aus Daten berechnen falls nicht in Tabelle
    total_n_stands <- wet_data %>%
      filter(risk_level == "geringes Risiko") %>%
      summarise(n = n_distinct(standid)) %>%
      pull(n)
    total_area_ha <- wet_data %>%
      filter(risk_level == "geringes Risiko") %>%
      group_by(stp) %>%
      summarise(total_area = sum(area_ha, na.rm = TRUE), .groups = 'drop') %>%
      pull(total_area)
    cat("  WARNUNG: Keine Statistik für", group_name, "gefunden - verwende berechnete Werte\n")
  }
  
  # Haupttitel für die Abbildung
  main_title <- paste0("WET ", group_name, " (N = ", format(total_n_stands, big.mark = ".", decimal.mark = ","), "; ", format(round(total_area_ha, 0), big.mark = ".", decimal.mark = ","), " ha)")
  
  # Gemeinsame Y-Achsen-Limits über alle Risikostufen berechnen
  
  # Maximaler Vorrat über alle Risikostufen (inkl. Ausreißer für vollständige Darstellung)
  y_max_vorrat <- wet_data %>%
    filter(!is.na(volume)) %>%
    summarise(max_val = max(volume, na.rm = TRUE)) %>%
    pull(max_val)
  y_max_vorrat <- ceiling(y_max_vorrat / 100) * 100
  
  # Maximaler BHD
  y_max_bhd <- wet_data %>%
    filter(!is.na(dbh)) %>%
    summarise(max_val = max(dbh, na.rm = TRUE)) %>%
    pull(max_val)
  y_max_bhd <- ceiling(y_max_bhd / 10) * 10
  
  # Maximales Erntevolumen
  y_max_removal <- wet_removal_data %>%
    mutate(total_removal_efm = (volumeThinning + volumeFinal + volumeSalvaged + volumeDisturbed) * 0.8) %>%
    filter(!is.na(total_removal_efm) & total_removal_efm > 0) %>%
    summarise(max_val = max(total_removal_efm, na.rm = TRUE)) %>%
    pull(max_val)
  y_max_removal <- if(length(y_max_removal) > 0 && !is.na(y_max_removal)) ceiling(y_max_removal / 100) * 100 else 500
  
  # Maximaler Baumartenanteil
  y_max_species <- 100
  
  cat("  Y-Limits: Vorrat =", y_max_vorrat, ", BHD =", y_max_bhd, ", Entnahme =", y_max_removal, "\n")
  
  # Plot 1: Vorrats- und BHD-Entwicklung (2x3 Grid) 
  
  # Funktion für Vorrat-Plot einer Risikostufe
  create_vorrat_plot <- function(data, risk_label, ertrag_data, max_age, y_limit, show_y_axis = TRUE, show_x_axis = TRUE, show_label = TRUE) {
    
    # Statistik für diese Risikostufe
    risk_stats <- stats_per_risk %>% filter(risk_level == risk_label)
    n_stands <- if(nrow(risk_stats) > 0) risk_stats$n_stands else 0
    
    plot_data <- data %>%
      filter(risk_level == risk_label) %>%
      mutate(age_group = round(age / 5) * 5) %>%
      filter(age_group >= 0 & age_group <= max_age)
    
    if (nrow(plot_data) == 0) return(NULL)
    
    # Calculate quantiles (exactly like old code)
    quantiles_data <- plot_data %>%
      group_by(age_group) %>%
      summarise(
        n = n(),
        median = median(volume, na.rm = TRUE),
        q25 = quantile(volume, 0.25, na.rm = TRUE),
        q75 = quantile(volume, 0.75, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(
        iqr = q75 - q25,
        lower_whisker = q25 - 1.5 * iqr,
        upper_whisker = q75 + 1.5 * iqr
      ) %>%
      filter(n >= 3)
    
    # Calculate min/max without outliers for each age group (exactly like old code)
    whiskers_data <- plot_data %>%
      left_join(quantiles_data %>% select(age_group, lower_whisker, upper_whisker), by = "age_group") %>%
      filter(!is.na(lower_whisker)) %>%
      filter(volume >= lower_whisker & volume <= upper_whisker) %>%
      group_by(age_group) %>%
      summarise(
        min_no_outliers = min(volume, na.rm = TRUE),
        max_no_outliers = max(volume, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Merge back with quantiles data
    quantiles_data <- quantiles_data %>%
      left_join(whiskers_data, by = "age_group")
    
    # Outliers: points outside the whisker range
    outliers_data <- plot_data %>%
      group_by(age_group) %>%
      filter(n() >= 3) %>%
      mutate(
        q25 = quantile(volume, 0.25, na.rm = TRUE),
        q75 = quantile(volume, 0.75, na.rm = TRUE),
        iqr = q75 - q25,
        lower_fence = q25 - 1.5 * iqr,
        upper_fence = q75 + 1.5 * iqr
      ) %>%
      filter(volume < lower_fence | volume > upper_fence) %>%
      select(age_group, volume) %>%
      ungroup()
    
    p <- ggplot(quantiles_data, aes(x = age_group)) +
      # Heller Bereich: Min-Max OHNE Ausreißer (Whiskers)
      geom_ribbon(aes(ymin = min_no_outliers, ymax = max_no_outliers), fill = "#E8E8E8", alpha = 0.6) +
      # Dunkler Bereich: IQR
      geom_ribbon(aes(ymin = q25, ymax = q75), fill = "#34495E", alpha = 0.7) +
      # Median-Linie
      geom_line(aes(y = median), color = "#E74C3C", linewidth = 2, alpha = 0.9)
    
    # Ausreißer hinzufügen
    if (nrow(outliers_data) > 0) {
      p <- p + geom_point(data = outliers_data, aes(x = age_group, y = volume), 
                         color = "#404040", size = 0.8, alpha = 0.6)
    }
    
    # Ertragstafel-Kurve hinzufügen falls verfügbar
    if (!is.null(ertrag_data) && nrow(ertrag_data) > 0) {
      p <- p + geom_line(data = ertrag_data, aes(x = age, y = volume), 
                        color = "#FF8C42", size = 1.5, alpha = 0.9, inherit.aes = FALSE)
    }
    
    p <- p +
      scale_x_continuous(limits = c(0, max_age), breaks = seq(0, max_age, 20)) +
      scale_y_continuous(
        labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
        breaks = scales::pretty_breaks(n = 6)
      ) +
      coord_cartesian(ylim = c(0, y_limit)) +
      labs(
        x = if(show_x_axis) "Alter [Jahre]" else NULL,
        y = NULL
      ) +
      theme_risk_comparison +
      theme(
        plot.title = element_text(size = 10, hjust = 0.5),
        plot.margin = margin(5, 5, 5, 5)
      )
    
    # Nur linker Plot zeigt Y-Achsen-Werte
    if (!show_y_axis) {
      p <- p + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
    }
    
    if (!show_x_axis) {
      p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())
    }
    
    return(p)
  }
  
  # Funktion für BHD-Plot einer Risikostufe
  create_bhd_plot <- function(data, risk_label, ertrag_data_dbh, max_age, y_limit, show_y_axis = TRUE, show_x_axis = TRUE) {
    
    # BHD verwendet 5-Jahres-Intervalle
    plot_data <- data %>%
      filter(risk_level == risk_label & !is.na(dbh)) %>%
      mutate(age_group = round(age / 5) * 5) %>%
      filter(age_group >= 0 & age_group <= max_age)
    
    if (nrow(plot_data) == 0) return(NULL)
    
    # Calculate quantiles (exactly like old code)
    quantiles_data <- plot_data %>%
      group_by(age_group) %>%
      summarise(
        n = n(),
        median = median(dbh, na.rm = TRUE),
        q25 = quantile(dbh, 0.25, na.rm = TRUE),
        q75 = quantile(dbh, 0.75, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(
        iqr = q75 - q25,
        lower_whisker = q25 - 1.5 * iqr,
        upper_whisker = q75 + 1.5 * iqr
      ) %>%
      filter(n >= 3)
    
    # Calculate min/max without outliers for each age group (exactly like old code)
    whiskers_data <- plot_data %>%
      left_join(quantiles_data %>% select(age_group, lower_whisker, upper_whisker), by = "age_group") %>%
      filter(!is.na(lower_whisker)) %>%
      filter(dbh >= lower_whisker & dbh <= upper_whisker) %>%
      group_by(age_group) %>%
      summarise(
        min_no_outliers = min(dbh, na.rm = TRUE),
        max_no_outliers = max(dbh, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Merge back with quantiles data
    quantiles_data <- quantiles_data %>%
      left_join(whiskers_data, by = "age_group")
    
    # Outliers: points outside the whisker range
    outliers_data <- plot_data %>%
      group_by(age_group) %>%
      filter(n() >= 3) %>%
      mutate(
        q25 = quantile(dbh, 0.25, na.rm = TRUE),
        q75 = quantile(dbh, 0.75, na.rm = TRUE),
        iqr = q75 - q25,
        lower_fence = q25 - 1.5 * iqr,
        upper_fence = q75 + 1.5 * iqr
      ) %>%
      filter(dbh < lower_fence | dbh > upper_fence) %>%
      select(age_group, dbh) %>%
      ungroup()
    
    p <- ggplot(quantiles_data, aes(x = age_group)) +
      # Heller Bereich: Min-Max OHNE Ausreißer (Whiskers)
      geom_ribbon(aes(ymin = min_no_outliers, ymax = max_no_outliers), fill = "#E8E8E8", alpha = 0.6) +
      # Dunkler Bereich: IQR
      geom_ribbon(aes(ymin = q25, ymax = q75), fill = "#34495E", alpha = 0.7) +
      # Median-Linie
      geom_line(aes(y = median), color = "#E74C3C", linewidth = 2, alpha = 0.9)
    
    # Add outliers
    if (nrow(outliers_data) > 0) {
      p <- p + geom_point(data = outliers_data, aes(x = age_group, y = dbh), 
                         color = "#404040", size = 1, alpha = 0.8, shape = 19)
    }
    
    # Add Ertragstafel curve if available
    if (!is.null(ertrag_data_dbh) && nrow(ertrag_data_dbh) > 0) {
      p <- p + geom_line(data = ertrag_data_dbh, aes(x = age, y = dbh), 
                        color = "#FF8C42", linewidth = 2, alpha = 0.9, inherit.aes = FALSE)
    }
    
    p <- p +
      scale_x_continuous(limits = c(0, max_age), breaks = seq(0, max_age, 20)) +
      scale_y_continuous(
        labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
        breaks = scales::pretty_breaks(n = 6)
      ) +
      coord_cartesian(ylim = c(0, y_limit)) +
      labs(
        x = if(show_x_axis) "Alter [Jahre]" else NULL,
        y = NULL
      ) +
      theme_risk_comparison +
      theme(
        plot.margin = margin(5, 5, 5, 5)
      )
    
    # Nur linker Plot zeigt Y-Achsen-Werte
    if (!show_y_axis) {
      p <- p + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
    }
    
    if (!show_x_axis) {
      p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())
    }
    
    return(p)
  }
  
  # ============================================================================
  # DYNAMISCHE PLOT-ERSTELLUNG BASIEREND AUF VERFÜGBAREN RISIKOSTUFEN
  # ============================================================================
  
  # Erstelle Vorrat-Plots nur für verfügbare Risikostufen
  vorrat_plots <- list()
  bhd_plots <- list()
  
  for (i in seq_along(available_risks)) {
    risk <- available_risks[i]
    risk_label <- risk_label_mapping[risk]
    show_y <- (i == 1)  # Nur erster Plot mit Y-Achse
    
    # Ertragstafel nur für ersten (geringes Risiko) Plot
    ertrag_vol <- if(i == 1) current_ertragstafel_data else NULL
    ertrag_dbh <- if(i == 1) current_ertragstafel_data_dbh else NULL
    
    vorrat_plots[[risk]] <- create_vorrat_plot(wet_data, risk_label, ertrag_vol, max_age_limit, y_max_vorrat, show_y, FALSE, TRUE)
    bhd_plots[[risk]] <- create_bhd_plot(wet_data, risk_label, ertrag_dbh, max_age_limit, y_max_bhd, show_y, TRUE)
  }
  
  # Prüfe ob mindestens ein gültiger Plot existiert
  valid_vorrat <- !sapply(vorrat_plots, is.null)
  valid_bhd <- !sapply(bhd_plots, is.null)
  
  if (any(valid_vorrat) && any(valid_bhd)) {
    
    # Haupttitel (zentriert über alle Spalten)
    main_title_grob <- textGrob(main_title, gp = gpar(fontsize = 14, fontface = "bold"))
    
    # Column headers - nur für verfügbare Risikostufen
    risk_titles <- list(
      "gering" = textGrob("geringes Risiko", gp = gpar(fontsize = 12, fontface = "bold")),
      "mittel" = textGrob("mittleres Risiko", gp = gpar(fontsize = 12, fontface = "bold")),
      "hoch" = textGrob("hohes Risiko", gp = gpar(fontsize = 12, fontface = "bold"))
    )
    
    # Y-Achsen-Labels als separate Elemente (gedreht um 90 Grad)
    y_label_vorrat <- textGrob(expression(bold("Vorrat [Vfm ha"^-1*"]")), rot = 90, gp = gpar(fontsize = 10, fontface = "bold"))
    y_label_bhd <- textGrob("BHD [cm]", rot = 90, gp = gpar(fontsize = 10, fontface = "bold"))
    
    # A und B Labels
    tag_a <- textGrob("A", gp = gpar(fontsize = 12, fontface = "bold"), hjust = 0.5)
    tag_b <- textGrob("B", gp = gpar(fontsize = 12, fontface = "bold"), hjust = 0.5)
    
    # Kombiniere A/B mit Y-Label - A/B oben, Y-Label zentriert
    left_col_row1 <- arrangeGrob(
      arrangeGrob(tag_a, nullGrob(), nrow = 2, heights = c(0.15, 0.85)),
      y_label_vorrat,
      ncol = 2, widths = c(0.4, 0.6)
    )
    left_col_row2 <- arrangeGrob(
      arrangeGrob(tag_b, nullGrob(), nrow = 2, heights = c(0.15, 0.85)),
      y_label_bhd,
      ncol = 2, widths = c(0.4, 0.6)
    )
    
    # Konvertiere alle verfügbaren Plots zu gtables
    g_vorrat <- lapply(vorrat_plots[valid_vorrat], ggplotGrob)
    g_bhd <- lapply(bhd_plots[valid_bhd], ggplotGrob)
    
    # Finde maximale Breite und setze alle gleich
    all_grobs <- c(g_vorrat, g_bhd)
    max_width_all <- Reduce(unit.pmax, lapply(all_grobs, function(g) g$widths))
    
    for (i in seq_along(g_vorrat)) g_vorrat[[i]]$widths <- max_width_all
    for (i in seq_along(g_bhd)) g_bhd[[i]]$widths <- max_width_all
    
    # Speichere als PNG - Dateiname aus group_name erzeugen (Leerzeichen durch Unterstriche ersetzen)
    filename_safe <- gsub(" ", "_", gsub("-", "_", group_name))
    filename_vorrat_bhd <- paste0(output_dir, "/", filename_safe, "_Vorrat_BHD_Risikovergleich.png")
    
    # Dynamisches Layout basierend auf Anzahl der Risikostufen
    if (n_risk_levels == 3) {
      # 3 Spalten Layout (original)
      png(filename_vorrat_bhd, width = 24, height = 14.5, units = "cm", res = 300)
      grid.arrange(
        main_title_grob,
        risk_titles[[available_risks[1]]], risk_titles[[available_risks[2]]], risk_titles[[available_risks[3]]],
        left_col_row1, g_vorrat[[1]], g_vorrat[[2]], g_vorrat[[3]],
        left_col_row2, g_bhd[[1]], g_bhd[[2]], g_bhd[[3]],
        nrow = 4, ncol = 4,
        heights = c(0.08, 0.08, 1, 1),
        widths = c(0.12, 1, 1, 1),
        layout_matrix = rbind(
          c(1, 1, 1, 1),
          c(NA, 2, 3, 4),
          c(5, 6, 7, 8),
          c(9, 10, 11, 12)
        )
      )
      dev.off()
    } else if (n_risk_levels == 2) {
      # 2 Spalten Layout
      png(filename_vorrat_bhd, width = 18, height = 14.5, units = "cm", res = 300)
      grid.arrange(
        main_title_grob,
        risk_titles[[available_risks[1]]], risk_titles[[available_risks[2]]],
        left_col_row1, g_vorrat[[1]], g_vorrat[[2]],
        left_col_row2, g_bhd[[1]], g_bhd[[2]],
        nrow = 4, ncol = 3,
        heights = c(0.08, 0.08, 1, 1),
        widths = c(0.15, 1, 1),
        layout_matrix = rbind(
          c(1, 1, 1),
          c(NA, 2, 3),
          c(4, 5, 6),
          c(7, 8, 9)
        )
      )
      dev.off()
    } else {
      # 1 Spalte Layout (keine Risikoeinstufung) - OHNE Risiko-Überschrift
      png(filename_vorrat_bhd, width = 14, height = 14.5, units = "cm", res = 300)
      grid.arrange(
        main_title_grob,
        left_col_row1, g_vorrat[[1]],
        left_col_row2, g_bhd[[1]],
        nrow = 3, ncol = 2,
        heights = c(0.08, 1, 1),
        widths = c(0.18, 1),
        layout_matrix = rbind(
          c(1, 1),
          c(2, 3),
          c(4, 5)
        )
      )
      dev.off()
    }
    
    cat("Vorrat-BHD Vergleichsplot gespeichert:", filename_vorrat_bhd, "\n")
  }
  
  # Plot 2: Entnahmemengen und Baumartenanteile (2x3 Grid) 
  
  # Funktion für Entnahme-Plot einer Risikostufe
  # WICHTIG: fixed_age_classes wird vom Baumarten-Plot übergeben für konsistente X-Achsen
  create_removal_plot <- function(data, risk_label, max_ac_num, ac_names, y_limit, show_y_axis = TRUE, show_x_axis = TRUE, fixed_age_classes = NULL) {
    
    plot_data <- data %>%
      filter(risk_level == risk_label) %>%
      mutate(
        age_class_num = case_when(
          age >= 1 & age <= 20 ~ 1,
          age >= 21 & age <= 40 ~ 2,
          age >= 41 & age <= 60 ~ 3,
          age >= 61 & age <= 80 ~ 4,
          age >= 81 & age <= 100 ~ 5,
          age >= 101 & age <= 120 ~ 6,
          age >= 121 & age <= 140 ~ 7,
          age >= 141 & age <= 160 ~ 8,
          age >= 161 & age <= 180 ~ 9,
          TRUE ~ NA_real_
        ),
        age_class = case_when(
          age_class_num == 1 & age_class_num <= max_ac_num ~ ac_names[1],
          age_class_num == 2 & age_class_num <= max_ac_num ~ ac_names[2],
          age_class_num == 3 & age_class_num <= max_ac_num ~ ac_names[3],
          age_class_num == 4 & age_class_num <= max_ac_num ~ ac_names[4],
          age_class_num == 5 & age_class_num <= max_ac_num ~ ac_names[5],
          age_class_num == 6 & age_class_num <= max_ac_num ~ if(length(ac_names) >= 6) ac_names[6] else NA_character_,
          age_class_num == 7 & age_class_num <= max_ac_num ~ if(length(ac_names) >= 7) ac_names[7] else NA_character_,
          age_class_num == 8 & age_class_num <= max_ac_num ~ if(length(ac_names) >= 8) ac_names[8] else NA_character_,
          age_class_num == 9 & age_class_num <= max_ac_num ~ if(length(ac_names) >= 9) ac_names[9] else NA_character_,
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(age_class)) %>%
      group_by(age_class) %>%
      filter(n() >= 3) %>%
      ungroup()
    
    # Verwende fixed_age_classes vom Baumarten-Plot falls verfügbar
    if (!is.null(fixed_age_classes) && length(fixed_age_classes) > 0) {
      age_class_levels <- fixed_age_classes
    } else {
      # Fallback: alle definierten Altersklassen verwenden
      age_class_levels <- ac_names[1:max_ac_num]
    }
    
    if (nrow(plot_data) == 0) {
      # Leeren Plot erstellen
      p <- ggplot() +
        scale_x_discrete(limits = age_class_levels, drop = FALSE) +
        scale_y_continuous(
          labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
          breaks = scales::pretty_breaks(n = 6)
        ) +
        coord_cartesian(ylim = c(0, y_limit)) +
        labs(
          x = if(show_x_axis) "Altersklasse" else NULL,
          y = NULL
        ) +
        theme_risk_comparison +
        theme(plot.margin = margin(5, 5, 5, 5))
      
      if (!show_y_axis) {
        p <- p + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
      }
      if (!show_x_axis) {
        p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())
      }
      
      return(p)
    }
    
    # Nur Altersklassen filtern, die im Baumarten-Plot vorhanden sind
    plot_data <- plot_data %>%
      filter(age_class %in% age_class_levels) %>%
      mutate(age_class = factor(age_class, levels = age_class_levels))
    
    if (nrow(plot_data) == 0) {
      # Erstelle leeren Plot mit den Altersklassen vom Baumarten-Plot
      p <- ggplot() +
        scale_x_discrete(limits = age_class_levels, drop = FALSE) +
        scale_y_continuous(
          labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
          breaks = scales::pretty_breaks(n = 6)
        ) +
        coord_cartesian(ylim = c(0, y_limit)) +
        labs(
          x = if(show_x_axis) "Altersklasse" else NULL,
          y = NULL
        ) +
        theme_risk_comparison +
        theme(plot.margin = margin(5, 5, 5, 5))
      
      if (!show_y_axis) {
        p <- p + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
      }
      if (!show_x_axis) {
        p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())
      }
      
      return(p)
    }
    
    p <- ggplot(plot_data, aes(x = age_class, y = total_removal_efm)) +
      geom_boxplot(fill = "#E8E8E8", alpha = 0.8, color = "#404040", linewidth = 0.5,
                  outlier.size = 1, outlier.alpha = 0.6, outlier.color = "#404040") +
      stat_boxplot(geom = "errorbar", width = 0.15, color = "#404040", linewidth = 0.5) +
      scale_x_discrete(drop = FALSE) +
      scale_y_continuous(
        labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
        breaks = scales::pretty_breaks(n = 6)
      ) +
      coord_cartesian(ylim = c(0, y_limit)) +
      labs(
        x = if(show_x_axis) "Altersklasse" else NULL,
        y = NULL
      ) +
      theme_risk_comparison +
      theme(plot.margin = margin(5, 5, 5, 5))
    
    # Nur linker Plot zeigt Y-Achsen-Werte
    if (!show_y_axis) {
      p <- p + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
    }
    
    if (!show_x_axis) {
      p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())
    }
    
    return(p)
  }
  
  # Funktion für Baumartenanteile-Plot einer Risikostufe
  create_species_plot <- function(data, risk_label, max_ac_num, ac_names, y_limit, show_y_axis = TRUE, show_x_axis = TRUE, show_legend = FALSE) {
    
    plot_data <- data %>%
      filter(risk_level == risk_label & !is.na(relBasalarea)) %>%
      mutate(relBasalarea_percent = relBasalarea * 100) %>%
      mutate(
        age_class_num = case_when(
          age >= 1 & age <= 20 ~ 1,
          age >= 21 & age <= 40 ~ 2,
          age >= 41 & age <= 60 ~ 3,
          age >= 61 & age <= 80 ~ 4,
          age >= 81 & age <= 100 ~ 5,
          age >= 101 & age <= 120 ~ 6,
          age >= 121 & age <= 140 ~ 7,
          age >= 141 & age <= 160 ~ 8,
          age >= 161 & age <= 180 ~ 9,
          TRUE ~ NA_real_
        ),
        age_class = case_when(
          age_class_num == 1 & age_class_num <= max_ac_num ~ ac_names[1],
          age_class_num == 2 & age_class_num <= max_ac_num ~ ac_names[2],
          age_class_num == 3 & age_class_num <= max_ac_num ~ ac_names[3],
          age_class_num == 4 & age_class_num <= max_ac_num ~ ac_names[4],
          age_class_num == 5 & age_class_num <= max_ac_num ~ ac_names[5],
          age_class_num == 6 & age_class_num <= max_ac_num ~ if(length(ac_names) >= 6) ac_names[6] else NA_character_,
          age_class_num == 7 & age_class_num <= max_ac_num ~ if(length(ac_names) >= 7) ac_names[7] else NA_character_,
          age_class_num == 8 & age_class_num <= max_ac_num ~ if(length(ac_names) >= 8) ac_names[8] else NA_character_,
          age_class_num == 9 & age_class_num <= max_ac_num ~ if(length(ac_names) >= 9) ac_names[9] else NA_character_,
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(age_class))
    
    if (nrow(plot_data) == 0) return(NULL)
    
    # Calculate species proportions
    species_summary <- plot_data %>%
      group_by(age_class, age_class_num, species) %>%
      summarise(
        n_observations = n(),
        median_percentage = median(relBasalarea_percent, na.rm = TRUE),
        q25_percentage = quantile(relBasalarea_percent, 0.25, na.rm = TRUE),
        q75_percentage = quantile(relBasalarea_percent, 0.75, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      filter(n_observations >= 3) %>%
      # Group minor species
      group_by(species) %>%
      mutate(overall_median = median(median_percentage, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(species_grouped = ifelse(overall_median < 5, "Sonstige", species)) %>%
      group_by(age_class, age_class_num, species_grouped) %>%
      summarise(
        median_percentage = sum(median_percentage, na.rm = TRUE),
        q25_percentage = sum(q25_percentage, na.rm = TRUE),
        q75_percentage = sum(q75_percentage, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      rename(species = species_grouped)
    
    if (nrow(species_summary) == 0) return(NULL)
    
    # Farben zuweisen
    present_species <- unique(species_summary$species)
    current_colors <- species_colors[present_species]
    missing_species <- present_species[!present_species %in% names(species_colors)]
    if (length(missing_species) > 0) {
      additional_colors <- rainbow(length(missing_species))
      names(additional_colors) <- missing_species
      current_colors <- c(current_colors, additional_colors)
    }
    if ("Sonstige" %in% present_species) {
      current_colors["Sonstige"] <- "#808080"
    }
    
    p <- ggplot(species_summary, aes(x = factor(age_class, levels = ac_names[1:max_ac_num]), 
                                     y = median_percentage, fill = species)) +
      geom_col(position = "dodge", alpha = 0.8, color = "white", linewidth = 0.2) +
      geom_errorbar(aes(ymin = q25_percentage, ymax = q75_percentage),
                   position = position_dodge(width = 0.9), width = 0.2, 
                   color = "#404040", linewidth = 0.4) +
      scale_fill_manual(values = current_colors, name = "Baumarten") +
      scale_y_continuous(
        labels = scales::comma,
        breaks = scales::pretty_breaks(n = 6)
      ) +
      coord_cartesian(ylim = c(0, y_limit)) +
      labs(
        x = if(show_x_axis) "Altersklasse" else NULL,
        y = NULL
      ) +
      theme_risk_comparison +
      theme(
        legend.position = if(show_legend) "bottom" else "none",
        plot.margin = margin(5, 5, 5, 5)
      )
    
    # Nur linker Plot zeigt Y-Achsen-Werte
    if (!show_y_axis) {
      p <- p + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
    }
    
    if (!show_x_axis) {
      p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())
    }
    
    return(p)
  }
  
  # Dynamische Entnahme/Baumarten Plot-Erstellung
  
  # Hilfsfunktion: vorhandene Altersklassen für eine Risikostufe ermitteln
  get_present_age_classes <- function(data, risk_label, max_ac_num, ac_names) {
    plot_data <- data %>%
      filter(risk_level == risk_label & !is.na(relBasalarea)) %>%
      mutate(
        age_class_num = case_when(
          age >= 1 & age <= 20 ~ 1,
          age >= 21 & age <= 40 ~ 2,
          age >= 41 & age <= 60 ~ 3,
          age >= 61 & age <= 80 ~ 4,
          age >= 81 & age <= 100 ~ 5,
          age >= 101 & age <= 120 ~ 6,
          age >= 121 & age <= 140 ~ 7,
          age >= 141 & age <= 160 ~ 8,
          age >= 161 & age <= 180 ~ 9,
          TRUE ~ NA_real_
        )
      ) %>%
      filter(!is.na(age_class_num) & age_class_num <= max_ac_num) %>%
      # Mindestens 3 Beobachtungen pro Altersklasse
      group_by(age_class_num) %>%
      filter(n() >= 3) %>%
      ungroup()
    
    if (nrow(plot_data) == 0) return(character(0))
    
    # Altersklassen-Namen zurückgeben
    present_nums <- sort(unique(plot_data$age_class_num))
    return(ac_names[present_nums])
  }
  
  # Schritt 1: Baumarten-Plots erstellen (bestimmen die X-Achsen-Struktur)
  species_plots <- list()
  present_age_classes_per_risk <- list()
  
  for (i in seq_along(available_risks)) {
    risk <- available_risks[i]
    risk_label <- risk_label_mapping[risk]
    show_y <- (i == 1)
    
    # Altersklassen im Baumarten-Plot ermitteln
    present_age_classes_per_risk[[risk]] <- get_present_age_classes(wet_detail_data, risk_label, max_age_class_num, age_class_names)
    
    species_plots[[risk]] <- create_species_plot(wet_detail_data, risk_label, max_age_class_num, age_class_names, y_max_species, show_y, TRUE, FALSE)
  }
  
  valid_species <- !sapply(species_plots, is.null)
  
  # Schritt 2: Entnahme-Plots mit gleichen Altersklassen erstellen
  removal_plots <- list()
  
  for (i in seq_along(available_risks)) {
    risk <- available_risks[i]
    risk_label <- risk_label_mapping[risk]
    show_y <- (i == 1)
    
    # Altersklassen vom Baumarten-Plot für konsistente X-Achsen
    fixed_ac <- present_age_classes_per_risk[[risk]]
    removal_plots[[risk]] <- create_removal_plot(wet_removal_data, risk_label, max_age_class_num, age_class_names, y_max_removal, show_y, FALSE, fixed_ac)
  }
  
  valid_removal <- !sapply(removal_plots, is.null)
  
  if (any(valid_removal) && any(valid_species)) {
    
    # Haupttitel
    main_title_grob2 <- textGrob(main_title, gp = gpar(fontsize = 14, fontface = "bold"))
    
    # Spaltenüberschriften
    risk_titles2 <- list(
      "gering" = textGrob("geringes Risiko", gp = gpar(fontsize = 12, fontface = "bold")),
      "mittel" = textGrob("mittleres Risiko", gp = gpar(fontsize = 12, fontface = "bold")),
      "hoch" = textGrob("hohes Risiko", gp = gpar(fontsize = 12, fontface = "bold"))
    )
    
    # Y-Achsen-Labels
    y_label_removal <- textGrob(expression(bold("Erntevolumen [Efm ha"^-1*"]")), rot = 90, gp = gpar(fontsize = 10, fontface = "bold"))
    y_label_species <- textGrob("Anteil [%]", rot = 90, gp = gpar(fontsize = 10, fontface = "bold"))
    
    # A und B Labels
    tag_a2 <- textGrob("A", gp = gpar(fontsize = 12, fontface = "bold"), hjust = 0.5)
    tag_b2 <- textGrob("B", gp = gpar(fontsize = 12, fontface = "bold"), hjust = 0.5)
    
    # A/B mit Y-Label kombinieren
    left_col_row1_2 <- arrangeGrob(
      arrangeGrob(tag_a2, nullGrob(), nrow = 2, heights = c(0.15, 0.85)),
      y_label_removal,
      ncol = 2, widths = c(0.4, 0.6)
    )
    left_col_row2_2 <- arrangeGrob(
      arrangeGrob(tag_b2, nullGrob(), nrow = 2, heights = c(0.15, 0.85)),
      y_label_species,
      ncol = 2, widths = c(0.4, 0.6)
    )
    
    # Plots zu gtables konvertieren
    g_removal <- lapply(removal_plots[valid_removal], ggplotGrob)
    g_species <- lapply(species_plots[valid_species], ggplotGrob)
    
    # Baumarten-Plots bestimmen Breiten, Entnahme-Plots übernehmen diese
    max_width_species <- Reduce(unit.pmax, lapply(g_species, function(g) g$widths))
    
    for (i in seq_along(g_species)) g_species[[i]]$widths <- max_width_species
    
    # Breiten auf Removal-Plots übertragen
    for (i in seq_along(g_removal)) g_removal[[i]]$widths <- max_width_species
    
    # Sammle alle Baumarten aus verfügbaren Risikostufen für kombinierte Legende
    get_species_for_risk <- function(data, risk_label) {
      plot_data <- data %>%
        filter(risk_level == risk_label & !is.na(relBasalarea)) %>%
        mutate(relBasalarea_percent = relBasalarea * 100)
      
      if (nrow(plot_data) == 0) return(character(0))
      
      species_summary <- plot_data %>%
        group_by(species) %>%
        summarise(overall_median = median(relBasalarea_percent, na.rm = TRUE), .groups = 'drop') %>%
        mutate(species_grouped = ifelse(overall_median < 5, "Sonstige", species)) %>%
        pull(species_grouped) %>%
        unique()
      
      return(species_summary)
    }
    
    # Baumarten aus allen verfügbaren Risikostufen sammeln
    all_species_lists <- lapply(available_risks, function(r) {
      get_species_for_risk(wet_detail_data, risk_label_mapping[r])
    })
    
    # Alle Baumarten vereinigen
    all_species_data <- unique(unlist(all_species_lists))
    all_species_data <- c(all_species_data[all_species_data != "Sonstige"], 
                          if ("Sonstige" %in% all_species_data) "Sonstige" else NULL)
    
    # Farben zuweisen
    legend_colors <- species_colors[all_species_data[all_species_data %in% names(species_colors)]]
    missing_species <- all_species_data[!all_species_data %in% names(species_colors)]
    if (length(missing_species) > 0) {
      additional_colors <- rainbow(length(missing_species))
      names(additional_colors) <- missing_species
      legend_colors <- c(legend_colors, additional_colors)
    }
    if ("Sonstige" %in% all_species_data) {
      legend_colors["Sonstige"] <- "#808080"
    }
    
    # Erstelle Dummy-Plot mit allen Baumarten für kombinierte Legende
    dummy_data <- data.frame(
      age_class = factor(rep("I", length(all_species_data))),
      species = factor(all_species_data, levels = all_species_data),
      median_percentage = rep(10, length(all_species_data))
    )
    
    legend_plot <- ggplot(dummy_data, aes(x = age_class, y = median_percentage, fill = species)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = legend_colors, name = "Baumarten", drop = FALSE) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 9),
        legend.key.size = unit(0.4, "cm"),
        legend.spacing.x = unit(0.2, "cm"),
        legend.box.background = element_blank()
      ) +
      guides(fill = guide_legend(nrow = 1, title.position = "left"))
    
    # Extrahiere Legende mit ggplot_gtable
    legend_gtable <- ggplot_gtable(ggplot_build(legend_plot))
    legend_idx <- which(sapply(legend_gtable$grobs, function(x) x$name) == "guide-box")
    if (length(legend_idx) > 0) {
      legend <- legend_gtable$grobs[[legend_idx[1]]]
    } else {
      legend <- nullGrob()
    }
    
    # Speichere als PNG - Dateiname aus group_name erzeugen
    filename_removal_species <- paste0(output_dir, "/", filename_safe, "_Entnahme_Baumarten_Risikovergleich.png")
    
    # Dynamisches Layout basierend auf Anzahl der Risikostufen
    if (n_risk_levels == 3) {
      # 3 Spalten Layout (original)
      png(filename_removal_species, width = 24, height = 14.5, units = "cm", res = 300)
      grid.arrange(
        main_title_grob2,
        risk_titles2[[available_risks[1]]], risk_titles2[[available_risks[2]]], risk_titles2[[available_risks[3]]],
        left_col_row1_2, g_removal[[1]], g_removal[[2]], g_removal[[3]],
        left_col_row2_2, g_species[[1]], g_species[[2]], g_species[[3]],
        legend,
        nrow = 5, ncol = 4,
        heights = c(0.08, 0.08, 1, 1, 0.15),
        widths = c(0.12, 1, 1, 1),
        layout_matrix = rbind(
          c(1, 1, 1, 1),
          c(NA, 2, 3, 4),
          c(5, 6, 7, 8),
          c(9, 10, 11, 12),
          c(NA, 13, 13, 13)
        )
      )
      dev.off()
    } else if (n_risk_levels == 2) {
      # 2 Spalten Layout
      png(filename_removal_species, width = 18, height = 14.5, units = "cm", res = 300)
      grid.arrange(
        main_title_grob2,
        risk_titles2[[available_risks[1]]], risk_titles2[[available_risks[2]]],
        left_col_row1_2, g_removal[[1]], g_removal[[2]],
        left_col_row2_2, g_species[[1]], g_species[[2]],
        legend,
        nrow = 5, ncol = 3,
        heights = c(0.08, 0.08, 1, 1, 0.15),
        widths = c(0.15, 1, 1),
        layout_matrix = rbind(
          c(1, 1, 1),
          c(NA, 2, 3),
          c(4, 5, 6),
          c(7, 8, 9),
          c(NA, 10, 10)
        )
      )
      dev.off()
    } else {
      # 1 Spalte Layout (keine Risikoeinstufung) - OHNE Risiko-Überschrift
      png(filename_removal_species, width = 14, height = 14.5, units = "cm", res = 300)
      grid.arrange(
        main_title_grob2,
        left_col_row1_2, g_removal[[1]],
        left_col_row2_2, g_species[[1]],
        legend,
        nrow = 4, ncol = 2,
        heights = c(0.08, 1, 1, 0.15),
        widths = c(0.18, 1),
        layout_matrix = rbind(
          c(1, 1),
          c(2, 3),
          c(4, 5),
          c(NA, 6)
        )
      )
      dev.off()
    }
    
    cat("Entnahme-Baumarten Vergleichsplot gespeichert:", filename_removal_species, "\n")
  }
  
  cat("Vergleichsplots für", group_name, "erfolgreich erstellt!\n")
}

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("ALLE VERGLEICHSPLOTS ERFOLGREICH ERSTELLT!\n")
cat("Dateien gespeichert auf dem Desktop\n")
cat(paste(rep("=", 80), collapse = ""), "\n")


# Tabellarische Auswertung

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("ERSTELLE TABELLEN FÜR VORRATSENTWICKLUNG\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

# Funktion zur Berechnung der Statistiken für Vorratsentwicklung
calculate_volume_statistics <- function(data, risk_label, max_age) {
  
  # Daten in 5-Jahres-Intervalle gruppieren
  plot_data <- data %>%
    filter(risk_level == risk_label) %>%
    mutate(age_group = round(age / 5) * 5) %>%
    filter(age_group >= 0 & age_group <= max_age & !is.na(volume))
  
  if (nrow(plot_data) == 0) {
    cat("  Keine Daten für", risk_label, "\n")
    return(NULL)
  }
  
  # Berechne Statistiken pro Altersklasse
  stats_table <- plot_data %>%
    group_by(age_group) %>%
    summarise(
      n = n(),
      Median = median(volume, na.rm = TRUE),
      Q25 = quantile(volume, 0.25, na.rm = TRUE),
      Q75 = quantile(volume, 0.75, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(
      IQR = Q75 - Q25,
      lower_fence = Q25 - 1.5 * IQR,
      upper_fence = Q75 + 1.5 * IQR
    ) %>%
    filter(n >= 3)  # Mindestens 3 Beobachtungen pro Altersklasse
  
  # Berechne tatsächliche Whisker-Werte (Min/Max ohne Ausreißer)
  whiskers <- plot_data %>%
    left_join(stats_table %>% select(age_group, lower_fence, upper_fence), by = "age_group") %>%
    filter(!is.na(lower_fence)) %>%
    filter(volume >= lower_fence & volume <= upper_fence) %>%
    group_by(age_group) %>%
    summarise(
      Lower_Whisker = min(volume, na.rm = TRUE),
      Upper_Whisker = max(volume, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Kombiniere Statistiken mit Whiskern
  final_table <- stats_table %>%
    left_join(whiskers, by = "age_group") %>%
    select(Alter = age_group, n, Median, Q25, Q75, Lower_Whisker, Upper_Whisker) %>%
    arrange(Alter)
  
  return(final_table)
}

# Schleife über alle WET-Gruppen und erstelle Tabellen
all_volume_tables <- list()

for (group_name in names(wet_groups)) {
  
  current_group <- wet_groups[[group_name]]
  available_risks <- current_group$available_risks
  risk_label_mapping <- c("gering" = "geringes Risiko", "mittel" = "mittleres Risiko", "hoch" = "hohes Risiko")
  
  cat("\n--- Erstelle Tabellen für:", group_name, "(", length(available_risks), "Risikostufen) ---\n")
  
  # Get WET-specific configuration (use the "gering" version as reference)
  current_wet_config <- wet_config[[current_group$gering]]
  max_age_limit <- if(!is.null(current_wet_config)) current_wet_config$max_age else 120
  
  # Filter data for this WET GROUP - nur verfügbare Risikostufen
  wet_mapping <- data.frame(
    risk_level = factor(risk_label_mapping[available_risks], 
                       levels = c("geringes Risiko", "mittleres Risiko", "hohes Risiko")),
    wet_name = sapply(available_risks, function(r) current_group[[r]]),
    stringsAsFactors = FALSE
  ) %>% filter(!is.na(wet_name))
  
  wet_data <- combined_abeStand %>%
    inner_join(wet_mapping, by = "risk_level") %>%
    filter(stp == wet_name & !is.na(age) & !is.na(volume)) %>%
    select(-wet_name)
  
  if (nrow(wet_data) == 0) {
    cat("  Keine Daten für WET-Gruppe:", group_name, "- überspringe\n")
    next
  }
  
  # Tabellen für verfügbare Risikostufen erstellen
  volume_tables <- list()
  
  for (risk in risk_label_mapping[available_risks]) {
    table_data <- calculate_volume_statistics(wet_data, risk, max_age_limit)
    
    if (!is.null(table_data)) {
      volume_tables[[risk]] <- table_data
      cat("  ", risk, ": ", nrow(table_data), " Altersklassen\n")
    }
  }
  
  all_volume_tables[[group_name]] <- volume_tables
  
  # Alle Risikostufen in einer Excel-Datei speichern
  if (length(volume_tables) > 0) {
    
    # Erstelle sicheren Dateinamen
    group_safe <- gsub("-", "_", gsub(" ", "_", group_name))
    
    # Erstelle Ausgabeverzeichnis falls nicht vorhanden
    volume_output_dir <- "C:/Users/Jakob/Desktop/Ergebnisse/Tab_Vorratsentwicklung"
    if (!dir.exists(volume_output_dir)) {
      dir.create(volume_output_dir, recursive = TRUE)
    }
    
    # Excel-Datei erstellen
    excel_filename <- file.path(volume_output_dir, paste0("Vorrat_", group_safe, ".xlsx"))
    
    # Erstelle eine Liste für writexl mit benannten Sheets
    sheets_list <- list()
    sheet_names <- c("geringes Risiko" = "Gering", 
                     "mittleres Risiko" = "Mittel", 
                     "hohes Risiko" = "Hoch")
    
    for (risk in names(volume_tables)) {
      if (!is.null(volume_tables[[risk]])) {
        sheet_name <- sheet_names[risk]
        sheets_list[[sheet_name]] <- volume_tables[[risk]]
      }
    }
    
    # Füge NW-FVA Ertragstafel hinzu, falls vorhanden
    ertrag_key <- current_group$ertrag_key
    if (!is.null(ertrag_key) && !is.null(all_ertragstafel_data[[ertrag_key]])) {
      ertragstafel_data <- all_ertragstafel_data[[ertrag_key]] %>%
        filter(age %% 10 == 0) %>%
        rename(Alter = age, Vorrat = volume) %>%
        arrange(Alter)
      
      sheets_list[["NW-FVA"]] <- ertragstafel_data
      cat("    NW-FVA Ertragstafel hinzugefügt\n")
    }
    
    # Speichere als Excel mit mehreren Sheets
    writexl::write_xlsx(sheets_list, path = excel_filename)
    cat("    Gespeichert:", basename(excel_filename), "mit", length(sheets_list), "Sheets\n")
  }
}

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("ALLE VORRATS-TABELLEN ERFOLGREICH ERSTELLT!\n")
cat("Dateien gespeichert in: C:/Users/Jakob/Desktop/Ergebnisse/Tab_Vorratsentwicklung\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

# Zeige Beispiel einer Tabelle
cat("\nBeispiel: Vorratsentwicklung für", names(all_volume_tables)[1], "- geringes Risiko:\n")
if (length(all_volume_tables) > 0 && length(all_volume_tables[[1]]) > 0) {
  print(head(all_volume_tables[[1]][["geringes Risiko"]], 10))
}

# Tabellarische Auswertung: BHD-Entwicklung

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("ERSTELLE TABELLEN FÜR BHD-ENTWICKLUNG\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

# Funktion zur Berechnung der Statistiken für BHD-Entwicklung
calculate_dbh_statistics <- function(data, risk_label, max_age) {
  
  # Gruppiere Daten in 5-Jahres-Intervalle mit round()
  plot_data <- data %>%
    filter(risk_level == risk_label) %>%
    mutate(age_group = round(age / 5) * 5) %>%
    filter(age_group >= 0 & age_group <= max_age & !is.na(dbh))
  
  if (nrow(plot_data) == 0) {
    cat("  Keine Daten für", risk_label, "\n")
    return(NULL)
  }
  
  # Berechne Statistiken pro Altersklasse
  stats_table <- plot_data %>%
    group_by(age_group) %>%
    summarise(
      n = n(),
      Median = median(dbh, na.rm = TRUE),
      Q25 = quantile(dbh, 0.25, na.rm = TRUE),
      Q75 = quantile(dbh, 0.75, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(
      IQR = Q75 - Q25,
      lower_fence = Q25 - 1.5 * IQR,
      upper_fence = Q75 + 1.5 * IQR
    ) %>%
    filter(n >= 3)  # Mindestens 3 Beobachtungen pro Altersklasse
  
  # Berechne tatsächliche Whisker-Werte (Min/Max ohne Ausreißer)
  whiskers <- plot_data %>%
    left_join(stats_table %>% select(age_group, lower_fence, upper_fence), by = "age_group") %>%
    filter(!is.na(lower_fence)) %>%
    filter(dbh >= lower_fence & dbh <= upper_fence) %>%
    group_by(age_group) %>%
    summarise(
      Lower_Whisker = min(dbh, na.rm = TRUE),
      Upper_Whisker = max(dbh, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Kombiniere Statistiken mit Whiskern
  final_table <- stats_table %>%
    left_join(whiskers, by = "age_group") %>%
    select(Alter = age_group, n, Median, Q25, Q75, Lower_Whisker, Upper_Whisker) %>%
    arrange(Alter)
  
  return(final_table)
}

# Schleife über alle WET-Gruppen und erstelle Tabellen
all_dbh_tables <- list()

for (group_name in names(wet_groups)) {
  
  current_group <- wet_groups[[group_name]]
  available_risks <- current_group$available_risks
  risk_label_mapping <- c("gering" = "geringes Risiko", "mittel" = "mittleres Risiko", "hoch" = "hohes Risiko")
  
  cat("\n--- Erstelle Tabellen für:", group_name, "(", length(available_risks), "Risikostufen) ---\n")
  
  # Get WET-specific configuration (use the "gering" version as reference)
  current_wet_config <- wet_config[[current_group$gering]]
  max_age_limit <- if(!is.null(current_wet_config)) current_wet_config$max_age else 120
  
  # Filter data for this WET GROUP - nur verfügbare Risikostufen
  wet_mapping <- data.frame(
    risk_level = factor(risk_label_mapping[available_risks], 
                       levels = c("geringes Risiko", "mittleres Risiko", "hohes Risiko")),
    wet_name = sapply(available_risks, function(r) current_group[[r]]),
    stringsAsFactors = FALSE
  ) %>% filter(!is.na(wet_name))
  
  wet_data <- combined_abeStand %>%
    inner_join(wet_mapping, by = "risk_level") %>%
    filter(stp == wet_name & !is.na(age) & !is.na(dbh)) %>%
    select(-wet_name)
  
  if (nrow(wet_data) == 0) {
    cat("  Keine Daten für WET-Gruppe:", group_name, "- überspringe\n")
    next
  }
  
  # Erstelle Tabellen nur für verfügbare Risikostufen
  dbh_tables <- list()
  
  for (risk in risk_label_mapping[available_risks]) {
    table_data <- calculate_dbh_statistics(wet_data, risk, max_age_limit)
    
    if (!is.null(table_data)) {
      dbh_tables[[risk]] <- table_data
      cat("  ", risk, ": ", nrow(table_data), " Altersklassen\n")
    }
  }
  
  all_dbh_tables[[group_name]] <- dbh_tables
  
  # Speichere alle Risikostufen in einer Excel-Datei mit mehreren Sheets
  if (length(dbh_tables) > 0) {
    
    # Erstelle sicheren Dateinamen
    group_safe <- gsub("-", "_", gsub(" ", "_", group_name))
    
    # Erstelle Ausgabeverzeichnis falls nicht vorhanden
    dbh_output_dir <- "C:/Users/Jakob/Desktop/Ergebnisse/Tab_BHD-Entwicklung"
    if (!dir.exists(dbh_output_dir)) {
      dir.create(dbh_output_dir, recursive = TRUE)
    }
    
    # Excel-Datei mit mehreren Sheets erstellen
    excel_filename <- file.path(dbh_output_dir, paste0("BHD_", group_safe, ".xlsx"))
    
    # Erstelle eine Liste für writexl mit benannten Sheets
    sheets_list <- list()
    sheet_names <- c("geringes Risiko" = "Gering", 
                     "mittleres Risiko" = "Mittel", 
                     "hohes Risiko" = "Hoch")
    
    for (risk in names(dbh_tables)) {
      if (!is.null(dbh_tables[[risk]])) {
        sheet_name <- sheet_names[risk]
        sheets_list[[sheet_name]] <- dbh_tables[[risk]]
      }
    }
    
    # Füge NW-FVA Ertragstafel hinzu, falls vorhanden
    ertrag_key <- current_group$ertrag_key
    if (!is.null(ertrag_key) && !is.null(all_ertragstafel_data_dbh[[ertrag_key]])) {
      ertragstafel_data <- all_ertragstafel_data_dbh[[ertrag_key]] %>%
        filter(age %% 5 == 0) %>%
        rename(Alter = age, BHD = dbh) %>%
        arrange(Alter)
      
      sheets_list[["NW-FVA"]] <- ertragstafel_data
      cat("    NW-FVA Ertragstafel hinzugefügt\n")
    }
    
    # Speichere als Excel mit mehreren Sheets
    writexl::write_xlsx(sheets_list, path = excel_filename)
    cat("    Gespeichert:", basename(excel_filename), "mit", length(sheets_list), "Sheets\n")
  }
}

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("ALLE BHD-TABELLEN ERFOLGREICH ERSTELLT!\n")
cat("Dateien gespeichert in: C:/Users/Jakob/Desktop/Ergebnisse/Tab_BHD-Entwicklung\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

# Zeige Beispiel einer Tabelle
cat("\nBeispiel: BHD-Entwicklung für", names(all_dbh_tables)[1], "- geringes Risiko:\n")
if (length(all_dbh_tables) > 0 && length(all_dbh_tables[[1]]) > 0) {
  print(head(all_dbh_tables[[1]][["geringes Risiko"]], 10))
}

# Tabellarische Auswertung: Entnahmemengen

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("ERSTELLE TABELLEN FÜR ENTNAHMEMENGEN\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

# Funktion zur Berechnung der Statistiken für Entnahmemengen
calculate_removal_statistics <- function(data, risk_label, max_age, max_age_class, age_class_names) {
  
  # Altersklassen-Breite berechnen
  age_class_width <- max_age / max_age_class
  
  # Gruppiere Daten nach Altersklassen
  plot_data <- data %>%
    filter(risk_level == risk_label) %>%
    mutate(
      age_class_num = ceiling(age / age_class_width),
      age_class_num = pmin(age_class_num, max_age_class)
    ) %>%
    filter(age_class_num >= 1 & age_class_num <= max_age_class & !is.na(total_removal_efm) & total_removal_efm > 0)
  
  if (nrow(plot_data) == 0) {
    cat("  Keine Daten für", risk_label, "\n")
    return(NULL)
  }
  
  # Berechne Statistiken pro Altersklasse
  stats_table <- plot_data %>%
    group_by(age_class_num) %>%
    summarise(
      n = n(),
      Median = median(total_removal_efm, na.rm = TRUE),
      Q25 = quantile(total_removal_efm, 0.25, na.rm = TRUE),
      Q75 = quantile(total_removal_efm, 0.75, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(
      IQR = Q75 - Q25,
      lower_fence = Q25 - 1.5 * IQR,
      upper_fence = Q75 + 1.5 * IQR
    ) %>%
    filter(n >= 3)  # Mindestens 3 Beobachtungen pro Altersklasse
  
  # Berechne tatsächliche Whisker-Werte (Min/Max ohne Ausreißer)
  whiskers <- plot_data %>%
    left_join(stats_table %>% select(age_class_num, lower_fence, upper_fence), by = "age_class_num") %>%
    filter(!is.na(lower_fence)) %>%
    filter(total_removal_efm >= lower_fence & total_removal_efm <= upper_fence) %>%
    group_by(age_class_num) %>%
    summarise(
      Lower_Whisker = min(total_removal_efm, na.rm = TRUE),
      Upper_Whisker = max(total_removal_efm, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Statistiken mit Whiskern kombinieren und Altersklassen-Namen hinzufügen
  final_table <- stats_table %>%
    left_join(whiskers, by = "age_class_num") %>%
    mutate(Altersstufe = age_class_names[age_class_num]) %>%
    select(Altersstufe, n, Median, Q25, Q75, Lower_Whisker, Upper_Whisker) %>%
    arrange(match(Altersstufe, age_class_names))
  
  return(final_table)
}

# Schleife über alle WET-Gruppen und erstelle Tabellen
all_removal_tables <- list()

for (group_name in names(wet_groups)) {
  
  current_group <- wet_groups[[group_name]]
  available_risks <- current_group$available_risks
  risk_label_mapping <- c("gering" = "geringes Risiko", "mittel" = "mittleres Risiko", "hoch" = "hohes Risiko")
  
  cat("\n--- Erstelle Tabellen für:", group_name, "(", length(available_risks), "Risikostufen) ---\n")
  
  # Get WET-specific configuration (use the "gering" version as reference)
  current_wet_config <- wet_config[[current_group$gering]]
  max_age_limit <- if(!is.null(current_wet_config)) current_wet_config$max_age else 120
  max_age_class_num <- if(!is.null(current_wet_config)) current_wet_config$max_age_class else 6
  age_class_names <- if(!is.null(current_wet_config)) current_wet_config$age_class_names else c("I", "II", "III", "IV", "V", "VI")
  
  # Filter data for this WET GROUP - nur verfügbare Risikostufen
  wet_mapping <- data.frame(
    risk_level = factor(risk_label_mapping[available_risks], 
                       levels = c("geringes Risiko", "mittleres Risiko", "hohes Risiko")),
    wet_name = sapply(available_risks, function(r) current_group[[r]]),
    stringsAsFactors = FALSE
  ) %>% filter(!is.na(wet_name))
  
  wet_removal_data <- combined_abeStandRemoval %>%
    inner_join(wet_mapping, by = "risk_level") %>%
    filter(stp == wet_name & !is.na(age)) %>%
    mutate(
      total_removal_efm = (volumeThinning + volumeFinal + volumeSalvaged + volumeDisturbed) * 0.8
    ) %>%
    filter(total_removal_efm > 0) %>%
    select(-wet_name)
  
  if (nrow(wet_removal_data) == 0) {
    cat("  Keine Daten für WET-Gruppe:", group_name, "- überspringe\n")
    next
  }
  
  # Erstelle Tabellen nur für verfügbare Risikostufen
  removal_tables <- list()
  
  for (risk in risk_label_mapping[available_risks]) {
    table_data <- calculate_removal_statistics(wet_removal_data, risk, max_age_limit, max_age_class_num, age_class_names)
    
    if (!is.null(table_data)) {
      removal_tables[[risk]] <- table_data
      cat("  ", risk, ": ", nrow(table_data), " Altersklassen\n")
    }
  }
  
  all_removal_tables[[group_name]] <- removal_tables
  
  # Speichere alle Risikostufen in einer Excel-Datei mit mehreren Sheets
  if (length(removal_tables) > 0) {
    
    # Erstelle sicheren Dateinamen
    group_safe <- gsub("-", "_", gsub(" ", "_", group_name))
    
    # Erstelle Ausgabeverzeichnis falls nicht vorhanden
    removal_output_dir <- "C:/Users/Jakob/Desktop/Ergebnisse/Tab_Entnahmemenge"
    if (!dir.exists(removal_output_dir)) {
      dir.create(removal_output_dir, recursive = TRUE)
    }
    
    # Excel-Datei mit mehreren Sheets erstellen
    excel_filename <- file.path(removal_output_dir, paste0("Entnahme_", group_safe, ".xlsx"))
    
    # Erstelle eine Liste für writexl mit benannten Sheets
    sheets_list <- list()
    sheet_names <- c("geringes Risiko" = "Gering", 
                     "mittleres Risiko" = "Mittel", 
                     "hohes Risiko" = "Hoch")
    
    for (risk in names(removal_tables)) {
      if (!is.null(removal_tables[[risk]])) {
        sheet_name <- sheet_names[risk]
        sheets_list[[sheet_name]] <- removal_tables[[risk]]
      }
    }
    
    # Speichere als Excel mit mehreren Sheets
    writexl::write_xlsx(sheets_list, path = excel_filename)
    cat("    Gespeichert:", basename(excel_filename), "mit", length(sheets_list), "Sheets\n")
  }
}

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("ALLE ENTNAHME-TABELLEN ERFOLGREICH ERSTELLT!\n")
cat("Dateien gespeichert in: C:/Users/Jakob/Desktop/Ergebnisse/Tab_Entnahmemenge\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

# Zeige Beispiel einer Tabelle
cat("\nBeispiel: Entnahmemengen für", names(all_removal_tables)[1], "- geringes Risiko:\n")
if (length(all_removal_tables) > 0 && length(all_removal_tables[[1]]) > 0) {
  print(all_removal_tables[[1]][["geringes Risiko"]])
}

# Tabellarische Auswertung: Baumartenanteile

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("ERSTELLE TABELLEN FÜR BAUMARTENANTEILE\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

# Funktion zur Berechnung der Baumartenanteile pro Altersklasse
calculate_species_statistics <- function(data, risk_label, max_age, max_age_class, age_class_names) {
  
  # Berechne Altersklassen-Breite
  age_class_width <- max_age / max_age_class
  
  # Gruppiere Daten nach Altersklassen und berechne Prozent
  plot_data <- data %>%
    filter(risk_level == risk_label & !is.na(relBasalarea)) %>%
    mutate(
      relBasalarea_percent = relBasalarea * 100,
      age_class_num = ceiling(age / age_class_width),
      age_class_num = pmin(age_class_num, max_age_class)
    ) %>%
    filter(age_class_num >= 1 & age_class_num <= max_age_class)
  
  if (nrow(plot_data) == 0) {
    cat("  Keine Daten für", risk_label, "\n")
    return(NULL)
  }
  
  # Berechne Statistiken pro Altersklasse und Baumart
  species_summary <- plot_data %>%
    group_by(age_class_num, species) %>%
    summarise(
      n_observations = n(),
      Median = median(relBasalarea_percent, na.rm = TRUE),
      Q25 = quantile(relBasalarea_percent, 0.25, na.rm = TRUE),
      Q75 = quantile(relBasalarea_percent, 0.75, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    filter(n_observations >= 3)
  
  if (nrow(species_summary) == 0) {
    cat("  Keine Daten mit n>=3 für", risk_label, "\n")
    return(NULL)
  }
  
  # Kleine Baumarten zu "Sonstige" gruppieren (< 5% Median)
  species_overall <- species_summary %>%
    group_by(species) %>%
    summarise(overall_median = median(Median, na.rm = TRUE), .groups = 'drop') %>%
    mutate(species_grouped = ifelse(overall_median < 5, "Sonstige", species))
  
  # Gruppierung hinzufügen
  species_summary <- species_summary %>%
    left_join(species_overall %>% select(species, species_grouped), by = "species") %>%
    group_by(age_class_num, species_grouped) %>%
    summarise(
      n_observations = sum(n_observations),
      Median = sum(Median, na.rm = TRUE),
      Q25 = sum(Q25, na.rm = TRUE),
      Q75 = sum(Q75, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    rename(species = species_grouped)
  
  # Whisker berechnen (Lower = Q25, Upper = Q75)
  final_table <- species_summary %>%
    mutate(
      Altersstufe = age_class_names[age_class_num],
      Lower_Whisker = Q25,
      Upper_Whisker = Q75
    ) %>%
    select(Altersstufe, Baumart = species, n = n_observations, Median, Q25, Q75, Lower_Whisker, Upper_Whisker) %>%
    arrange(match(Altersstufe, age_class_names), Baumart)
  
  return(final_table)
}

# Schleife über alle WET-Gruppen und erstelle Tabellen
all_species_tables <- list()

for (group_name in names(wet_groups)) {
  
  current_group <- wet_groups[[group_name]]
  available_risks <- current_group$available_risks
  risk_label_mapping <- c("gering" = "geringes Risiko", "mittel" = "mittleres Risiko", "hoch" = "hohes Risiko")
  
  cat("\n--- Erstelle Tabellen für:", group_name, "(", length(available_risks), "Risikostufen) ---\n")
  
  # Get WET-specific configuration (use the "gering" version as reference)
  current_wet_config <- wet_config[[current_group$gering]]
  max_age_limit <- if(!is.null(current_wet_config)) current_wet_config$max_age else 120
  max_age_class_num <- if(!is.null(current_wet_config)) current_wet_config$max_age_class else 6
  age_class_names <- if(!is.null(current_wet_config)) current_wet_config$age_class_names else c("I", "II", "III", "IV", "V", "VI")
  
  # Filter data for this WET GROUP - nur verfügbare Risikostufen
  wet_mapping <- data.frame(
    risk_level = factor(risk_label_mapping[available_risks], 
                       levels = c("geringes Risiko", "mittleres Risiko", "hohes Risiko")),
    wet_name = sapply(available_risks, function(r) current_group[[r]]),
    stringsAsFactors = FALSE
  ) %>% filter(!is.na(wet_name))
  
  wet_detail_data <- combined_abeStandDetail %>%
    inner_join(wet_mapping, by = "risk_level") %>%
    filter(stp == wet_name & !is.na(age) & !is.na(species) & !is.na(relBasalarea)) %>%
    select(-wet_name)
  
  if (nrow(wet_detail_data) == 0) {
    cat("  Keine Daten für WET-Gruppe:", group_name, "- überspringe\n")
    next
  }
  
  # Erstelle Tabellen nur für verfügbare Risikostufen
  species_tables <- list()
  
  for (risk in risk_label_mapping[available_risks]) {
    table_data <- calculate_species_statistics(wet_detail_data, risk, max_age_limit, max_age_class_num, age_class_names)
    
    if (!is.null(table_data)) {
      species_tables[[risk]] <- table_data
      cat("  ", risk, ": ", nrow(table_data), " Zeilen (Altersstufe x Baumart)\n")
    }
  }
  
  all_species_tables[[group_name]] <- species_tables
  
  # Speichere alle Risikostufen in einer Excel-Datei mit mehreren Sheets
  if (length(species_tables) > 0) {
    
    # Erstelle sicheren Dateinamen
    group_safe <- gsub("-", "_", gsub(" ", "_", group_name))
    
    # Erstelle Ausgabeverzeichnis falls nicht vorhanden
    species_output_dir <- "C:/Users/Jakob/Desktop/Ergebnisse/Tab_Baumartenanteile"
    if (!dir.exists(species_output_dir)) {
      dir.create(species_output_dir, recursive = TRUE)
    }
    
    # Excel-Datei mit mehreren Sheets erstellen
    excel_filename <- file.path(species_output_dir, paste0("Baumartenanteile_", group_safe, ".xlsx"))
    
    # Erstelle eine Liste für writexl mit benannten Sheets
    sheets_list <- list()
    sheet_names <- c("geringes Risiko" = "Gering", 
                     "mittleres Risiko" = "Mittel", 
                     "hohes Risiko" = "Hoch")
    
    for (risk in names(species_tables)) {
      if (!is.null(species_tables[[risk]])) {
        sheet_name <- sheet_names[risk]
        sheets_list[[sheet_name]] <- species_tables[[risk]]
      }
    }
    
    # Speichere als Excel mit mehreren Sheets
    writexl::write_xlsx(sheets_list, path = excel_filename)
    cat("    Gespeichert:", basename(excel_filename), "mit", length(sheets_list), "Sheets\n")
  }
}

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("ALLE BAUMARTENANTEILE-TABELLEN ERFOLGREICH ERSTELLT!\n")
cat("Dateien gespeichert in: C:/Users/Jakob/Desktop/Ergebnisse/Tab_Baumartenanteile\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

# Zeige Beispiel einer Tabelle
cat("\nBeispiel: Baumartenanteile für", names(all_species_tables)[1], "- geringes Risiko:\n")
if (length(all_species_tables) > 0 && length(all_species_tables[[1]]) > 0) {
  print(head(all_species_tables[[1]][["geringes Risiko"]], 15))
} 


# Mittlere Gesamterntemenge pro WET und Risikostufe

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("BERECHNE MITTLERE GESAMTERNTEMENGE PRO WET UND RISIKOSTUFE\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

# Sammle alle Erntedaten über alle WET-Gruppen
harvest_summary <- data.frame()

for (group_name in names(wet_groups)) {
  
  current_group <- wet_groups[[group_name]]
  available_risks <- current_group$available_risks
  risk_label_mapping <- c("gering" = "geringes Risiko", "mittel" = "mittleres Risiko", "hoch" = "hohes Risiko")
  
  cat("\nVerarbeite WET-Gruppe:", group_name, "(", length(available_risks), "Risikostufen)\n")
  
  # Mapping für diese WET-Gruppe
  wet_mapping <- data.frame(
    risk_level = factor(risk_label_mapping[available_risks], 
                       levels = c("geringes Risiko", "mittleres Risiko", "hohes Risiko")),
    wet_name = sapply(available_risks, function(r) current_group[[r]]),
    stringsAsFactors = FALSE
  ) %>% filter(!is.na(wet_name))
  
  # Removal-Daten für diese WET-Gruppe filtern
  wet_removal_data <- combined_abeStandRemoval %>%
    inner_join(wet_mapping, by = "risk_level") %>%
    filter(stp == wet_name) %>%
    mutate(
      total_removal_efm = (volumeThinning + volumeFinal + volumeSalvaged + volumeDisturbed) * 0.8
    ) %>%
    select(-wet_name)
  
  if (nrow(wet_removal_data) == 0) {
    cat("  Keine Erntedaten für WET-Gruppe:", group_name, "- überspringe\n")
    next
  }
  
  # Gesamternte pro Bestand berechnen (Rohdaten bereits pro Hektar)
  total_harvest_per_stand <- wet_removal_data %>%
    group_by(standid, stp, risk_level) %>%
    summarise(
      n_harvests = n(),  # Anzahl Ernteereignisse
      total_harvest_efm_ha = sum(total_removal_efm, na.rm = TRUE),  # Gesamternte [Efm/ha]
      .groups = 'drop'
    )
  
  # Statistiken pro Risikostufe aggregieren
  group_stats <- total_harvest_per_stand %>%
    group_by(risk_level) %>%
    summarise(
      n_stands = n(),
      mean_harvest_efm_ha = mean(total_harvest_efm_ha, na.rm = TRUE),
      median_harvest_efm_ha = median(total_harvest_efm_ha, na.rm = TRUE),
      sd_harvest_efm_ha = sd(total_harvest_efm_ha, na.rm = TRUE),
      min_harvest_efm_ha = min(total_harvest_efm_ha, na.rm = TRUE),
      max_harvest_efm_ha = max(total_harvest_efm_ha, na.rm = TRUE),
      sum_harvest_efm_ha = sum(total_harvest_efm_ha, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(wet_group = group_name)
  
  harvest_summary <- bind_rows(harvest_summary, group_stats)
  
  cat("  Risikostufen verarbeitet:", nrow(group_stats), "\n")
}

# Spalten ordnen und formatieren
harvest_summary <- harvest_summary %>%
  select(wet_group, risk_level, n_stands, mean_harvest_efm_ha, median_harvest_efm_ha, 
         sd_harvest_efm_ha, min_harvest_efm_ha, max_harvest_efm_ha, sum_harvest_efm_ha) %>%
  arrange(wet_group, risk_level)

# Ausgabe
cat("\n", paste(rep("-", 80), collapse = ""), "\n")
cat("ERGEBNIS: Mittlere Gesamterntemenge pro WET und Risikostufe [Efm/ha]\n")
cat(paste(rep("-", 80), collapse = ""), "\n\n")
print(as.data.frame(harvest_summary))

# Als Excel speichern
harvest_output_dir <- "C:/Users/Jakob/Desktop/Ergebnisse"
if (!dir.exists(harvest_output_dir)) dir.create(harvest_output_dir, recursive = TRUE)

writexl::write_xlsx(
  harvest_summary,
  path = file.path(harvest_output_dir, "Gesamterntemenge_pro_WET_Risikostufe.xlsx")
)

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("GESAMTERNTEMENGE-TABELLE ERFOLGREICH ERSTELLT!\n")
cat("Datei gespeichert: C:/Users/Jakob/Desktop/Ergebnisse/Gesamterntemenge_pro_WET_Risikostufe.xlsx\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

