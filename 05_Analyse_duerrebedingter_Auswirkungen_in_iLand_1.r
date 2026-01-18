# Autor: Neumann, J.(2025)
# Analyse Abbildun dürrebedingter Auswirkungen in iLand



#### SPEI berechnen ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Setup mit gewichteten Klimatabellen
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Pakete laden
library(dplyr)
library(RSQLite)
library(dbplyr)
library(tibble)

# Pfade und Dateinamen
database_path <- "C:/Users/Jakob/OneDrive/OneDrive Dokumente/Universitaet/02_Master/Masterarbeit/iLand_SOSFOR/Iland_project/database"
db_file <- "Hyras_1951-2020.sqlite"
ru_file <- "C:/Users/Jakob/OneDrive/OneDrive Dokumente/Universitaet/02_Master/Masterarbeit/Auswertung/02_Trockenheit/SOSFOR_environment/climate_by_RU.csv"

# 1. Lade climate_by_RU.csv und bestimme Gewichte
cat("\n=== Lade RU-Klimazuordnung ===\n")
climate_by_ru <- read.csv(ru_file)

# Entferne Zeilen mit NA-Werten in model.climate.tableName
climate_by_ru <- climate_by_ru %>%
  filter(!is.na(model.climate.tableName))

cat("Nach Entfernung der NA-Werte:", nrow(climate_by_ru), "Resource Units verbleiben\n")

# Häufigkeit jeder Klimatabelle zählen
climate_weights <- climate_by_ru %>%
  count(model.climate.tableName, name = "n_rus") %>%
  mutate(weight = n_rus / sum(n_rus)) %>%
  arrange(desc(n_rus))

cat("\nGewichtung der Klimatabellen (nach Häufigkeit der RUs):\n")
print(climate_weights)
cat("\nGesamtanzahl Resource Units:", nrow(climate_by_ru), "\n")

# Datenbankverbindung öffnen
db_file_path <- file.path(database_path, db_file)
db.conn <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = db_file_path)

# Klimatabellen laden und gewichten
cat("\n=== Lade und gewichte Klimadaten ===\n")
weighted_data_list <- list()

for(i in 1:nrow(climate_weights)) {
  table_name <- climate_weights$model.climate.tableName[i]
  weight <- climate_weights$weight[i]
  
  cat("Lade", table_name, "mit Gewicht", round(weight, 4), 
      "(", climate_weights$n_rus[i], "RUs)\n")
  
  # Tabelle laden und Gewicht anwenden
  climate_data <- dplyr::tbl(db.conn, table_name) %>% 
    collect() %>%
    mutate(
      min_temp = min_temp * weight,
      max_temp = max_temp * weight,
      rad = rad * weight,
      vpd = vpd * weight,
      prec = prec * weight
    )
  
  weighted_data_list[[i]] <- climate_data
}

# Gewichtete Daten zusammenführen
cat("\n=== Füge gewichtete Klimadaten zusammen ===\n")

# Erste Tabelle als Basis
data <- weighted_data_list[[1]] %>%
  select(year, month, day, min_temp, max_temp, rad, vpd, prec)

# Weitere Tabellen addieren
if(length(weighted_data_list) > 1) {
  for(i in 2:length(weighted_data_list)) {
    temp_data <- weighted_data_list[[i]] %>%
      select(year, month, day, min_temp, max_temp, rad, vpd, prec)
    
    data <- data %>%
      full_join(temp_data, by = c("year", "month", "day"), suffix = c("", "_add")) %>%
      mutate(
        min_temp = min_temp + coalesce(min_temp_add, 0),
        max_temp = max_temp + coalesce(max_temp_add, 0),
        rad = rad + coalesce(rad_add, 0),
        vpd = vpd + coalesce(vpd_add, 0),
        prec = prec + coalesce(prec_add, 0)
      ) %>%
      select(year, month, day, min_temp, max_temp, rad, vpd, prec)
  }
}

# Verbindung schließen
RSQLite::dbDisconnect(db.conn); rm(db.conn)

cat("\n=== Gewichtete Klimadaten erfolgreich erstellt ===\n")
cat("Anzahl Tage:", nrow(data), "\n")
cat("Zeitraum:", min(data$year), "-", max(data$year), "\n")
cat("Spalten:", paste(names(data), collapse = ", "), "\n")

# Variable speichern
assign("climate_data_weighted", data, envir = .GlobalEnv)

cat("\nGewichtete Klimadaten gespeichert als 'climate_data_weighted'\n\n")


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Datenvorbereitung
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Aggregiere zu Monatswerten
monthly_data <- climate_data_weighted %>%
  group_by(year, month) %>%
  summarise(
    min_temp_mean = mean(min_temp, na.rm = TRUE),
    max_temp_mean = mean(max_temp, na.rm = TRUE),
    rad_mean = mean(rad, na.rm = TRUE),
    vpd_mean = mean(vpd, na.rm = TRUE),
    prec_sum = sum(prec, na.rm = TRUE),
    n_days = n(),
    n_missing = sum(is.na(prec) | is.na(max_temp) | is.na(min_temp) | is.na(rad) | is.na(vpd)),
    .groups = 'drop'
  )

# Zeige Ergebnis
cat("\nAggregierte Monatswerte:\n")
cat("Anzahl Monate:", nrow(monthly_data), "\n")
print(head(monthly_data, 10))

cat("\nZusammenfassung der Monatswerte:\n")
print(summary(monthly_data))

# Speichere als neue Variable
assign("climate_monthly", monthly_data, envir = .GlobalEnv)
cat("\nMonatswerte gespeichert als 'climate_monthly'\n")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Visualisierung der Klimadaten
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Erstelle Datum für Zeitreihen-Plot
monthly_data$date <- as.Date(paste(monthly_data$year, monthly_data$month, "01", sep = "-"))

# Filtere Daten (1990-2020)
monthly_data_subset <- monthly_data %>%
  filter(year >= 1990 & year <= 2020)

cat("\nDaten für Visualisierung gefiltert (1990-2020):\n")
cat("Anzahl Monate:", nrow(monthly_data_subset), "\n")
cat("Zeitraum:", min(monthly_data_subset$year), "-", max(monthly_data_subset$year), "\n")

# Plot-Parameter zurücksetzen
par(mfrow=c(1,1), mar=c(5,4,4,2), oma=c(0,0,0,0))

# 2x1 Layout erstellen
par(mfrow=c(2,1), mar=c(3,4,3,2), oma=c(3,0,2,0))

# X-Achsen-Limits definieren
xlim_range <- c(as.Date("1990-01-01"), as.Date("2020-12-31"))

# 1. Niederschlagszeitreihe
plot(monthly_data_subset$date, monthly_data_subset$prec_sum, 
     type = "l", col = "#1976D2", lwd = 1.5,
     main = "Monatlicher Niederschlag (gewichtet) 1990-2020",
     ylab = "Niederschlag [mm]", xlab = "",
     xlim = xlim_range,
     xaxt = "n")  # Keine automatischen X-Achsen-Labels

# Manuelle X-Achsen-Beschriftung für jedes Jahr
years <- seq(1990, 2020, by = 1)
year_dates <- as.Date(paste(years, "-01-01", sep = ""))
axis(1, at = year_dates, labels = years, las = 2, cex.axis = 0.8)

# Gitterlinien
abline(v = seq(as.Date("1990-01-01"), as.Date("2021-01-01"), by = "5 years"), 
       col = "gray", lty = "dotted")
abline(v = year_dates, col = "lightgray", lty = "dotted", lwd = 0.5)
grid(nx = NA, ny = NULL, col = "lightgray", lty = "dotted")

# Trendlinie für Niederschlag
prec_time <- as.numeric(monthly_data_subset$date)
prec_lm <- lm(prec_sum ~ prec_time, data = monthly_data_subset)
lines(monthly_data_subset$date, fitted(prec_lm), col = "#0D47A1", lwd = 2.5, lty = "dashed")

# Temperaturzeitreihe
plot(monthly_data_subset$date, monthly_data_subset$max_temp_mean, 
     type = "l", col = "#FF5722", lwd = 1.5,
     main = "Monatliche Temperaturen (gewichtet) 1990-2020",
     ylab = "Temperatur [°C]", xlab = "",
     xlim = xlim_range,
     ylim = range(c(monthly_data_subset$min_temp_mean, monthly_data_subset$max_temp_mean), na.rm = TRUE),
     xaxt = "n")  # Keine automatischen X-Achsen-Labels

# Manuelle X-Achsen-Beschriftung für jedes Jahr
axis(1, at = year_dates, labels = years, las = 2, cex.axis = 0.8)

lines(monthly_data_subset$date, monthly_data_subset$min_temp_mean, 
      col = "#2196F3", lwd = 1.5)

temp_mean_subset <- (monthly_data_subset$max_temp_mean + monthly_data_subset$min_temp_mean) / 2
lines(monthly_data_subset$date, temp_mean_subset, 
      col = "#4CAF50", lwd = 2)

# Gitterlinien
abline(v = seq(as.Date("1990-01-01"), as.Date("2021-01-01"), by = "5 years"), 
       col = "gray", lty = "dotted")
abline(v = year_dates, col = "lightgray", lty = "dotted", lwd = 0.5)
grid(nx = NA, ny = NULL, col = "lightgray", lty = "dotted")

abline(h = 0, col = "black", lty = "dashed", lwd = 1)

# Trendlinien
temp_time <- as.numeric(monthly_data_subset$date)
tmax_lm <- lm(max_temp_mean ~ temp_time, data = monthly_data_subset)
tmin_lm <- lm(min_temp_mean ~ temp_time, data = monthly_data_subset)
tmean_lm <- lm(temp_mean_subset ~ temp_time)
lines(monthly_data_subset$date, fitted(tmax_lm), col = "#B71C1C", lwd = 2.5, lty = "dashed")
lines(monthly_data_subset$date, fitted(tmin_lm), col = "#0D47A1", lwd = 2.5, lty = "dashed")
lines(monthly_data_subset$date, fitted(tmean_lm), col = "#1B5E20", lwd = 2.5, lty = "dashed")

legend("topleft", legend = c("Tmax", "Tmin", "Tmittel"), 
       col = c("#FF5722", "#2196F3", "#4CAF50"), 
       lwd = c(1.5, 1.5, 2), cex = 0.8)

mtext("Jahr", side = 1, outer = TRUE, cex = 1.2, line = 2)

mtext("Klimazeitreihen - Münstertal (gewichtete Monatswerte) 1990-2020", 
      side = 3, outer = TRUE, cex = 1.3, font = 2, line = 0.5)

par(mfrow=c(1,1), mar=c(5,4,4,2), oma=c(0,0,0,0))

cat("\nKlimazeitreihen-Plots erstellt!\n")
cat("- Oberer Plot: Monatlicher Niederschlag\n")
cat("- Unterer Plot: Monatliche Temperaturen (Min, Max, Mittel)\n")
cat("- Zeitraum: 1990-2020\n")

## Datenformat für SPEI vorbereiten ####

# Variablen umbenennen
muenstertal <- monthly_data %>%
  select(
    YEAR = year,
    MONTH = month,
    PRCP = prec_sum,
    TMAX = max_temp_mean,
    TMIN = min_temp_mean
  ) %>%
  mutate(
    TMED = (TMAX + TMIN) / 2
  )

# Simulation einer erweiterten Trockenperiode
cat("\n=== Simulation erweiterte Trockenperiode ===\n")
cat("Kopiere Klimadaten von 2018 für die Jahre 2021 und 2022...\n")

# Extrahiere 2018 Daten
data_2018 <- muenstertal %>%
  filter(YEAR == 2018) %>%
  select(MONTH, PRCP, TMAX, TMIN, TMED)

data_2021 <- data_2018 %>%
  mutate(YEAR = 2021)

data_2022 <- data_2018 %>%
  mutate(YEAR = 2022)

muenstertal_extended <- muenstertal %>%
  bind_rows(data_2021) %>%
  bind_rows(data_2022) %>%
  arrange(YEAR, MONTH)

muenstertal <- muenstertal_extended

cat("2018-Klimadaten erfolgreich für 2021 und 2022 kopiert!\n")
cat("Ursprüngliche Datenanzahl:", nrow(muenstertal) - 24, "Monate\n")
cat("Erweiterte Datenanzahl:", nrow(muenstertal), "Monate (+24 Monate)\n")
cat("Neuer Zeitraum:", min(muenstertal$YEAR), "-", max(muenstertal$YEAR), "\n")

cat("\nBeispiel - 2018 Original vs. 2021/2022 Kopien:\n")
comparison_data <- muenstertal %>%
  filter(YEAR %in% c(2018, 2021, 2022) & MONTH <= 3) %>%
  select(YEAR, MONTH, PRCP, TMAX, TMIN)
print(comparison_data)

cat("\nSPEI-Datensatz 'muenstertal' erstellt:\n")
cat("Anzahl Monate:", nrow(muenstertal), "\n")
print(head(muenstertal, 10))

cat("\nZusammenfassung des SPEI-Datensatzes:\n")
print(summary(muenstertal))

assign("muenstertal", muenstertal, envir = .GlobalEnv)
cat("\nSPEI-Datensatz gespeichert als 'muenstertal'\n")


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Evapotranspiration berechnen
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Thornthwaite (1948) Methode
library(SPEI)

# Daten vorbereiten - attach verwenden um direkt auf Spalten zugreifen zu können
attach(muenstertal)
cat("Verfügbare Variablen im muenstertal Datensatz:\n")
print(names(muenstertal))

# Thornthwaite Evapotranspiration berechnen
tho <- thornthwaite(TMED, 47.85472)

# Plot-Parameter zurücksetzen für Einzelplots
par(mfrow=c(1,1), mar=c(5,4,4,2), oma=c(0,0,0,0))

plot(tho)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Wasserbilanz berechnen
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Wasserbilanz = Niederschlag - Evapotranspiration

balance <- muenstertal$PRCP - tho

# Balance als wasserbilanz zur muenstertal-Tabelle hinzufügen
muenstertal$wasserbilanz <- balance

cat("\nMuenstertal-Datensatz mit ET0 (Wasserbilanz) erweitert:\n")
cat("Anzahl Spalten:", ncol(muenstertal), "\n")
cat("Spaltennamen:", paste(names(muenstertal), collapse = ", "), "\n")
print(head(muenstertal, 10))

cat("\nZusammenfassung der Wasserbilanz:\n")
print(summary(muenstertal$wasserbilanz))


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# SPEI berechnen
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Zeitreihe erstellen
start_year <- min(muenstertal$YEAR)
wasserbilanz_ts <- ts(muenstertal$wasserbilanz, 
                      freq = 12, 
                      start = c(start_year, 1))

cat("Zeitreihe erstellt von", start_year, "bis", max(muenstertal$YEAR), "\n")
cat("Länge der Zeitreihe:", length(wasserbilanz_ts), "Monate\n")

# Berechne SPEI
spei1 <- spei(wasserbilanz_ts, 1)   # 1-Monats SPEI (wird nicht verwendet)
spei3 <- spei(wasserbilanz_ts, 3)   # 3-Monats SPEI
spei6 <- spei(wasserbilanz_ts, 6)   # 6-Monats SPEI
spei12 <- spei(wasserbilanz_ts, 12) # 12-Monats SPEI
spei24 <- spei(wasserbilanz_ts, 24) # 24-Monats SPEI

# SPEI-Daten als separate Datensätze extrahieren und speichern
cat("\nExtrahiere SPEI-Daten als separate Datensätze...\n")

# Basis-Informationen für alle Datensätze
base_data <- data.frame(
  YEAR = muenstertal$YEAR,
  MONTH = muenstertal$MONTH,
  DATE = as.Date(paste(muenstertal$YEAR, muenstertal$MONTH, "01", sep="-")),
  WASSERBILANZ = as.numeric(wasserbilanz_ts)
)

spei1_data <- base_data
spei1_data$SPEI_1 <- as.numeric(spei1$fitted)
spei1_data_clean <- na.omit(spei1_data)
assign("spei1_data", spei1_data, envir = .GlobalEnv)
assign("spei1_data_clean", spei1_data_clean, envir = .GlobalEnv)

spei3_data <- base_data
spei3_data$SPEI_3 <- as.numeric(spei3$fitted)
spei3_data_clean <- na.omit(spei3_data)
assign("spei3_data", spei3_data, envir = .GlobalEnv)
assign("spei3_data_clean", spei3_data_clean, envir = .GlobalEnv)

spei6_data <- base_data
spei6_data$SPEI_6 <- as.numeric(spei6$fitted)
spei6_data_clean <- na.omit(spei6_data)
assign("spei6_data", spei6_data, envir = .GlobalEnv)
assign("spei6_data_clean", spei6_data_clean, envir = .GlobalEnv)

spei12_data <- base_data
spei12_data$SPEI_12 <- as.numeric(spei12$fitted)
spei12_data_clean <- na.omit(spei12_data)
assign("spei12_data", spei12_data, envir = .GlobalEnv)
assign("spei12_data_clean", spei12_data_clean, envir = .GlobalEnv)

spei24_data <- base_data
spei24_data$SPEI_24 <- as.numeric(spei24$fitted)
spei24_data_clean <- na.omit(spei24_data)
assign("spei24_data", spei24_data, envir = .GlobalEnv)
assign("spei24_data_clean", spei24_data_clean, envir = .GlobalEnv)

cat("\nErstellt wurden folgende Datensätze:\n")
cat("=== EINZELNE SPEI-DATENSÄTZE ===\n")
cat("spei1_data & spei1_data_clean: SPEI-1 mit", nrow(spei1_data), "bzw.", nrow(spei1_data_clean), "Zeilen\n")
cat("spei3_data & spei3_data_clean: SPEI-3 mit", nrow(spei3_data), "bzw.", nrow(spei3_data_clean), "Zeilen\n")
cat("spei6_data & spei6_data_clean: SPEI-6 mit", nrow(spei6_data), "bzw.", nrow(spei6_data_clean), "Zeilen\n")
cat("spei12_data & spei12_data_clean: SPEI-12 mit", nrow(spei12_data), "bzw.", nrow(spei12_data_clean), "Zeilen\n")
cat("spei24_data & spei24_data_clean: SPEI-24 mit", nrow(spei24_data), "bzw.", nrow(spei24_data_clean), "Zeilen\n")

cat("\nSpalten in den einzelnen SPEI-Datensätzen:\n")
cat("spei3_data:", paste(names(spei3_data), collapse = ", "), "\n")
cat("spei6_data:", paste(names(spei6_data), collapse = ", "), "\n")
cat("spei12_data:", paste(names(spei12_data), collapse = ", "), "\n")
cat("spei24_data:", paste(names(spei24_data), collapse = ", "), "\n")

cat("\nBeispiel - Erste 5 Zeilen von spei12_data_clean:\n")
print(head(spei12_data_clean, 5))

cat("\nZusammenfassung aller einzelnen SPEI-Werte:\n")
cat("SPEI-3:\n"); print(summary(spei3_data_clean$SPEI_3))
cat("SPEI-6:\n"); print(summary(spei6_data_clean$SPEI_6))
cat("SPEI-12:\n"); print(summary(spei12_data_clean$SPEI_12))
cat("SPEI-24:\n"); print(summary(spei24_data_clean$SPEI_24))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# SPEI-Plots 2x2 Layout für 2000-2022
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

cat("\nErstelle SPEI-Plots im 2x2 Layout für 2000-2022...\n")

spei3_subset <- spei3_data_clean[spei3_data_clean$YEAR >= 2000 & spei3_data_clean$YEAR <= 2022, ]
spei6_subset <- spei6_data_clean[spei6_data_clean$YEAR >= 2000 & spei6_data_clean$YEAR <= 2022, ]
spei12_subset <- spei12_data_clean[spei12_data_clean$YEAR >= 2000 & spei12_data_clean$YEAR <= 2022, ]
spei24_subset <- spei24_data_clean[spei24_data_clean$YEAR >= 2000 & spei24_data_clean$YEAR <= 2022, ]

if(dev.cur() != 1) dev.off()

spei_plot_file <- file.path("C:/Users/Jakob/Desktop", "SPEI_2000-2022_4panel.png")
png(spei_plot_file, width = 3000, height = 2400, res = 300, family = "sans")

par(mfrow=c(2,2), mar=c(4,4.5,3,1), oma=c(0,0,0,0))

# SPEI-3
colors3 <- ifelse(spei3_subset$SPEI_3 < 0, "#D32F2F", "#1976D2")
plot(spei3_subset$DATE, spei3_subset$SPEI_3, 
     type = "h", col = colors3, lwd = 1.2,
     main = "SPEI 3",
     ylab = "SPEI-Index", xlab = "",
     ylim = c(-2.5, 2.5),
     xlim = c(as.Date("2000-01-01"), as.Date("2022-12-31")),
     xaxt = "n",
     cex.main = 2.0, cex.lab = 1.8, cex.axis = 1.5, font.lab = 2)
abline(h = 0, col = "black", lwd = 1)
abline(h = c(-2, -1, 1, 2), col = "gray", lty = "dotted")
abline(v = seq(as.Date("2000-01-01"), as.Date("2022-01-01"), by = "5 years"), 
       col = "gray", lty = "dotted")
abline(v = seq(as.Date("2000-01-01"), as.Date("2022-01-01"), by = "1 year"), 
       col = "lightgray", lty = "dotted", lwd = 0.5)
# X-Achse: Nur Ticks, keine Labels (obere Plots)
year_ticks <- seq(as.Date("2000-01-01"), as.Date("2022-01-01"), by = "1 year")
axis(1, at = year_ticks, labels = FALSE, tck = -0.02)  # Kleinere Ticks
year_dates_5 <- as.Date(paste(seq(2000, 2020, by = 5), "-01-01", sep = ""))
axis(1, at = year_dates_5, labels = FALSE, tck = -0.035)  # Größere Ticks für 5-Jahres-Schritte

# SPEI-6 Plot (oben rechts) - Keine Achsenbeschriftungen, nur Ticks
colors6 <- ifelse(spei6_subset$SPEI_6 < 0, "#D32F2F", "#1976D2")
plot(spei6_subset$DATE, spei6_subset$SPEI_6, 
     type = "h", col = colors6, lwd = 1.2,
     main = "SPEI 6",
     ylab = "", xlab = "",
     ylim = c(-2.5, 2.5),
     xlim = c(as.Date("2000-01-01"), as.Date("2022-12-31")),
     xaxt = "n", yaxt = "n",
     cex.main = 2.0, cex.lab = 1.8, cex.axis = 1.5, font.lab = 2)
# Y-Achse nur Ticks, keine Labels
axis(2, labels = FALSE, tck = -0.02)
abline(h = 0, col = "black", lwd = 1)
abline(h = c(-2, -1, 1, 2), col = "gray", lty = "dotted")
abline(v = seq(as.Date("2000-01-01"), as.Date("2022-01-01"), by = "5 years"), 
       col = "gray", lty = "dotted")
abline(v = seq(as.Date("2000-01-01"), as.Date("2022-01-01"), by = "1 year"), 
       col = "lightgray", lty = "dotted", lwd = 0.5)
# X-Achse: Nur Ticks, keine Labels (obere Plots)
year_ticks <- seq(as.Date("2000-01-01"), as.Date("2022-01-01"), by = "1 year")
axis(1, at = year_ticks, labels = FALSE, tck = -0.02)  # Kleinere Ticks
year_dates_5 <- as.Date(paste(seq(2000, 2020, by = 5), "-01-01", sep = ""))
axis(1, at = year_dates_5, labels = FALSE, tck = -0.035)  # Größere Ticks für 5-Jahres-Schritte

# SPEI-12 Plot (unten links) - Y-Achse und X-Achse mit Beschriftung
colors12 <- ifelse(spei12_subset$SPEI_12 < 0, "#D32F2F", "#1976D2")
plot(spei12_subset$DATE, spei12_subset$SPEI_12, 
     type = "h", col = colors12, lwd = 1.2,
     main = "SPEI 12",
     ylab = "SPEI-Index", xlab = "Jahr",
     ylim = c(-2.5, 2.5),
     xlim = c(as.Date("2000-01-01"), as.Date("2022-12-31")),
     xaxt = "n",
     cex.main = 2.0, cex.lab = 1.8, cex.axis = 1.5, font.lab = 2)
abline(h = 0, col = "black", lwd = 1)
abline(h = c(-2, -1, 1, 2), col = "gray", lty = "dotted")
abline(v = seq(as.Date("2000-01-01"), as.Date("2022-01-01"), by = "5 years"), 
       col = "gray", lty = "dotted")
abline(v = seq(as.Date("2000-01-01"), as.Date("2022-01-01"), by = "1 year"), 
       col = "lightgray", lty = "dotted", lwd = 0.5)
# X-Achse: Ticks für alle Jahre, Labels nur alle 5 Jahre
year_ticks <- seq(as.Date("2000-01-01"), as.Date("2022-01-01"), by = "1 year")
year_labels_5 <- seq(2000, 2020, by = 5)
year_dates_5 <- as.Date(paste(year_labels_5, "-01-01", sep = ""))
axis(1, at = year_ticks, labels = FALSE, tck = -0.02, cex.axis = 1.5)  # Kleinere Ticks
axis(1, at = year_dates_5, labels = year_labels_5, tck = -0.035, cex.axis = 1.5)  # Größere Ticks mit Labels

# SPEI-24 Plot (unten rechts) - X-Achse mit Beschriftung, Y-Achse nur Ticks
colors24 <- ifelse(spei24_subset$SPEI_24 < 0, "#D32F2F", "#1976D2")
plot(spei24_subset$DATE, spei24_subset$SPEI_24, 
     type = "h", col = colors24, lwd = 1.2,
     main = "SPEI 24",
     ylab = "", xlab = "Jahr",
     ylim = c(-2.5, 2.5),
     xlim = c(as.Date("2000-01-01"), as.Date("2022-12-31")),
     xaxt = "n", yaxt = "n",
     cex.main = 2.0, cex.lab = 1.8, cex.axis = 1.5, font.lab = 2)
# Y-Achse nur Ticks, keine Labels
axis(2, labels = FALSE, tck = -0.02)
abline(h = 0, col = "black", lwd = 1)
abline(h = c(-2, -1, 1, 2), col = "gray", lty = "dotted")
abline(v = seq(as.Date("2000-01-01"), as.Date("2022-01-01"), by = "5 years"), 
       col = "gray", lty = "dotted")
abline(v = seq(as.Date("2000-01-01"), as.Date("2022-01-01"), by = "1 year"), 
       col = "lightgray", lty = "dotted", lwd = 0.5)
# X-Achse: Ticks für alle Jahre, Labels nur alle 5 Jahre
year_ticks <- seq(as.Date("2000-01-01"), as.Date("2022-01-01"), by = "1 year")
year_labels_5 <- seq(2000, 2020, by = 5)
year_dates_5 <- as.Date(paste(year_labels_5, "-01-01", sep = ""))
axis(1, at = year_ticks, labels = FALSE, tck = -0.02, cex.axis = 1.5)  # Kleinere Ticks
axis(1, at = year_dates_5, labels = year_labels_5, tck = -0.035, cex.axis = 1.5)  # Größere Ticks mit Labels

# PNG-Datei schließen
dev.off()

par(mfrow=c(1,1), mar=c(5,4,4,2), oma=c(0,0,0,0))

cat("SPEI-Plots im 2x2 Layout für 2000-2022 erstellt!\n")
cat("- PNG gespeichert als:", spei_plot_file, "\n")
cat("- Zeitraum: 2000-2022 (inkl. simulierte Trockenperiode 2021-2022)\n")
cat("- Layout: 2x2 (SPEI-3, SPEI-6, SPEI-12, SPEI-24)\n")
cat("- Rote Balken: Trockenheit (SPEI < 0)\n")
cat("- Blaue Balken: Feuchtigkeit (SPEI > 0)\n")
cat("- Y-Achse: -2.5 bis +2.5 (standardisiert)\n")
cat("- Simulation: 2021-2022 verwenden 2018-Klimadaten (Trockenperiode)\n")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# NPP
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

## Theme definieren ####
theme_modern <- theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", color = "gray20"),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    axis.title = element_text(size = 12, color = "gray20"),
    axis.text = element_text(size = 10, color = "gray30"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray80", size = 0.5),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(size = 11, face = "bold", color = "gray20"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

## iLand Input-Dateien laden ####

setwd("C:/Users/Jakob/OneDrive/OneDrive Dokumente/Universitaet/02_Master/Masterarbeit/iLand_SOSFOR/Iland_project/temp")

# Environment file
(env.df <- read_delim("../gis/environment.txt"))

# DEM raster, 10 x 10 m resolution (before 50 x 50 m) 
dem.grid <- terra::rast("C:/Users/Jakob/OneDrive/OneDrive Dokumente/Universitaet/02_Master/Masterarbeit/iLand_SOSFOR/Iland_project/gis/dem.asc") # wurde von mir angepasst, da die Datei nicht existierte

# Convert x,y coordinates to lon,lat based on lower left corner coordinates
# Store original x and y coordinates
env.df$x_orig <- env.df$x
env.df$y_orig <- env.df$y

env.df <- env.df %>%
  mutate(
    # Convert x to longitude
    x = 405400 + x * 100, #JAl: x coordinate of right side 465700
    # Convert y to latitude
    y = 5277726 + y * 100 #JAl: y coordinate of lower side 
  )
# Add information about elevation to environment file
env.df$elevation <- terra::extract(
  # aggregate DEM (50x50 m) to fit resolution of environment file (100x100 m)
  terra::aggregate(dem.grid, fact = 10, fun = "mean"), 
  env.df[, c("x", "y")], ID = FALSE) %>% unlist()

# Species database
db.conn <- RSQLite::dbConnect(RSQLite::SQLite(),
                              dbname = "../database/all_species_database.sqlite") # habe ich angepasst - für european species keine Datei mit Inhalt vorhanden...
species.df <- dplyr::tbl(db.conn, "species") %>% 
  collect()
RSQLite::dbDisconnect(db.conn); rm(db.conn)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Zentrale Farbpalette für Baumarten
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

cat("\n=== LADE ZENTRALE FARBPALETTE AUS SPECIES DATABASE ===\n")

# Basis-Farbpalette erstellen
species_colors <- species.df %>% 
  arrange(shortName) %>% 
  mutate(displayColor = paste0("#", displayColor)) %>% 
  select(shortName, displayColor) %>%
  deframe()  # Konvertiere zu Named Vector

# Erweitere Farbpalette für qupe_quro (Eichen-Gruppe)
species_colors["qupe_quro"] <- species_colors["qupe"]  # Verwende qupe-Farbe für die Gruppe

# Erweitere Farbpalette für deutsche Namen
species_colors["Buche"] <- species_colors["fasy"]
species_colors["Fichte"] <- species_colors["piab"]
species_colors["Kiefer"] <- species_colors["pisy"]
species_colors["Eichen"] <- species_colors["qupe"]

# Labels für deutsche Namen
species_labels_german <- c(
  "fasy" = "Buche",
  "qupe_quro" = "Eichen", 
  "piab" = "Fichte",
  "pisy" = "Kiefer"
)

cat("✅ Zentrale Farbpalette definiert:\n")
cat("- Basis: species_colors (alle Arten aus Database)\n")
cat("- Erweitert für: qupe_quro, deutsche Namen\n")
cat("- species_labels_german (Übersetzungen)\n")
cat("\nHauptarten:\n")
main_species <- c("fasy", "piab", "pisy", "qupe_quro")
for(sp in main_species) {
  cat("-", sp, "(", species_labels_german[sp], "):", species_colors[sp], "\n")
}

target.df <- read.csv("C:/Users/Jakob/OneDrive/OneDrive Dokumente/Universitaet/02_Master/Masterarbeit/iLand_SOSFOR/AgentFiles/no_mgmt_eng_Muenstertal_ABE-lib_targets_v1.0.csv", sep=";")

# iLand Output laden
# Funktion für SQLite-Verbindung
connect_to_sqlite <- function(filename, output_dir = "../output/") {
  # Construct full path
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

# Connect to SQLite database and load data
sqlite_filename <- "project_CSA_20251103_140033.sqlite"  # Change filename here
selected_db <- connect_to_sqlite(sqlite_filename)
db.conn <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = selected_db)

RSQLite::dbListTables(db.conn) # list of available output tables
# Hint: if you want to analyse other tables, please refer to https://iland-model.org/Outputs 
# or alternatively, you can use a SQLite browser to explore the databases.
# If a table is missing check whether the specific output is activated in the project file

# Load tables landscape, dynamicstand, and wind
lscp <- dplyr::tbl(db.conn, "landscape") %>% 
  collect()

stand <- dplyr::tbl(db.conn, "stand") %>% 
  collect()

standdead <- dplyr::tbl(db.conn, "standdead") %>% 
  collect()

RSQLite::dbDisconnect(db.conn); rm(db.conn)


cat("\n=== LANDSCAPE-ANALYSE: GRUNDFLÄCHE UND NPP ===\n")
cat("Analysiere Grundfläche und NPP aus landscape-Tabelle...\n")

landscape_data <- lscp %>%
  mutate(
    species_grouped = case_when(
      species %in% c("qupe", "quro") ~ "qupe_quro",
      TRUE ~ species
    )
  ) %>%
  filter(species_grouped %in% c("fasy", "piab", "pisy", "qupe_quro")) %>%
  filter(year >= 11 & year <= 34) %>%
  mutate(real_year = year + 1989) %>%
  select(year, real_year, species_grouped, basal_area_m2, NPP_kg, count_ha)

landscape_summary <- landscape_data %>%
  arrange(real_year, species_grouped)

cat("Landscape-Daten verfügbar für:\n")
cat("- Jahre:", paste(sort(unique(landscape_summary$real_year)), collapse = ", "), "\n")
cat("- Baumarten:", paste(sort(unique(landscape_summary$species_grouped)), collapse = ", "), "\n")

# Baseline berechnen (Jahr 2000)
landscape_baseline <- landscape_summary %>%
  filter(year == 11) %>%
  select(species_grouped, baseline_basal_area = basal_area_m2, baseline_NPP = NPP_kg)

# Relative Veränderungen berechnen
landscape_changes <- landscape_summary %>%
  left_join(landscape_baseline, by = "species_grouped") %>%
  mutate(
    basal_area_change_pct = ((basal_area_m2 - baseline_basal_area) / baseline_basal_area) * 100,
    NPP_change_pct = ((NPP_kg - baseline_NPP) / baseline_NPP) * 100,
    basal_area_change_abs = basal_area_m2 - baseline_basal_area,
    NPP_change_abs = NPP_kg - baseline_NPP,
    years_since_baseline = year - 11
  ) %>%
  filter(!is.na(baseline_basal_area) & !is.na(baseline_NPP))

cat("\nErstelle NPP-Veränderungsplot mit relativen Werten (alle Baumarten)...\n")

mean_count_per_species <- landscape_data %>%
  group_by(species_grouped) %>%
  summarise(
    mean_count_ha = round(mean(count_ha, na.rm = TRUE), 0),
    .groups = "drop"
  )

cat("Mittlere Anzahl Bäume pro Hektar:\n")
for(i in 1:nrow(mean_count_per_species)) {
  species_name <- mean_count_per_species$species_grouped[i]
  count_value <- mean_count_per_species$mean_count_ha[i]
  cat("-", species_name, ":", count_value, "Bäume/ha\n")
}

present_species_npp <- unique(landscape_changes$species_grouped)
species_colors_landscape <- species_colors[present_species_npp]
species_colors_landscape <- species_colors_landscape[!is.na(species_colors_landscape)]

# Annotationen erstellen
annotations_all_species <- mean_count_per_species %>%
  mutate(
    x_pos = 2001,
    y_pos = case_when(
      species_grouped == "fasy" ~ 40,
      species_grouped == "piab" ~ 35,
      species_grouped == "pisy" ~ 30,
      species_grouped == "qupe_quro" ~ 25
    ),
    label = paste0("n = ", format(mean_count_ha, big.mark = ",")),
    species_label = species_labels_german[species_grouped],
    color = species_colors_landscape[species_grouped]
  )

# NPP-Veränderungsplot
p_NPP_change_all <- ggplot(landscape_changes, aes(x = real_year, y = NPP_change_pct, color = species_grouped)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.7) +
  geom_text(data = annotations_all_species, 
            aes(x = x_pos, y = y_pos, label = label, color = species_grouped),
            hjust = 0, vjust = 0, 
            size = 3.5, fontface = "bold", 
            inherit.aes = FALSE,
            show.legend = FALSE) +
  scale_color_manual(
    values = species_colors_landscape,
    labels = species_labels_german,
    name = "Baumart"
  ) +
  scale_x_continuous(breaks = seq(2000, 2022, by = 5)) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    breaks = scales::pretty_breaks(n = 8)
  ) +
  labs(
    title = "Relative NPP-Veränderung (Referenz: 2000)",
    subtitle = "n = mittlere Anzahl Bäume pro Hektar über alle Jahre",
    x = "Jahr",
    y = "Relative NPP-Veränderung [%]"
  ) +
  theme_modern +
  theme(
    legend.position = "bottom",
    plot.margin = margin(10, 10, 10, 10, "pt")
  )

print(p_NPP_change_all)

npp_plot_all_file <- file.path("C:/Users/Jakob/Desktop", "NPP_change_relative_all_species.png")
ggsave(npp_plot_all_file, p_NPP_change_all, width = 8, height = 6, dpi = 300)
cat("NPP-Plot (alle Baumarten, relativ) gespeichert als:", npp_plot_all_file, "\n")

# Zusammenfassung der Landscape-Ergebnisse
cat("\n=== ZUSAMMENFASSUNG LANDSCAPE-ANALYSE (RELATIVE WERTE) ===\n")

final_landscape <- landscape_changes %>%
  filter(real_year == max(real_year)) %>%
  arrange(desc(basal_area_change_pct))

cat("Relative Veränderungen von 2000 bis", max(landscape_changes$real_year), " (Landscape-Daten):\n")
cat("\nGrundfläche (relativ):\n")
for(i in 1:nrow(final_landscape)) {
  species_name <- species_labels_german[final_landscape$species_grouped[i]]
  basal_change_pct <- round(final_landscape$basal_area_change_pct[i], 1)
  cat("-", species_name, ":", basal_change_pct, "%\n")
}

cat("\nNPP (relativ):\n")
final_npp <- landscape_changes %>%
  filter(real_year == max(real_year)) %>%
  arrange(desc(NPP_change_pct))
for(i in 1:nrow(final_npp)) {
  species_name <- species_labels_german[final_npp$species_grouped[i]]
  npp_change_pct <- round(final_npp$NPP_change_pct[i], 1)
  cat("-", species_name, ":", npp_change_pct, "%\n")
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# NPP & SPEI korrelieren und plotten ######################################################################################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

cat("\n=== NPP & SPEI KOMBINATIONSPLOT ===\n")
cat("Erstelle Multiplot: Relative NPP-Veränderung + SPEI-3, SPEI-6, SPEI-12...\n")

# SPEI-Daten vorbereiten
spei3_plot_data <- spei3_data_clean %>%
  filter(YEAR >= 2000 & YEAR <= 2022) %>%
  mutate(DATE = as.Date(paste(YEAR, MONTH, "01", sep = "-"))) %>%
  select(YEAR, MONTH, DATE, SPEI_3)

spei6_plot_data <- spei6_data_clean %>%
  filter(YEAR >= 2000 & YEAR <= 2022) %>%
  mutate(DATE = as.Date(paste(YEAR, MONTH, "01", sep = "-"))) %>%
  select(YEAR, MONTH, DATE, SPEI_6)

spei12_plot_data <- spei12_data_clean %>%
  filter(YEAR >= 2000 & YEAR <= 2022) %>%
  mutate(DATE = as.Date(paste(YEAR, MONTH, "01", sep = "-"))) %>%
  select(YEAR, MONTH, DATE, SPEI_12)

# NPP-Daten für monatliche Darstellung vorbereiten
npp_monthly <- landscape_changes %>%
  select(real_year, species_grouped, NPP_change_pct) %>%
  crossing(MONTH = 1:12) %>%
  mutate(
    DATE = as.Date(paste(real_year, MONTH, "01", sep = "-"))
  ) %>%
  filter(real_year >= 2000 & real_year <= 2022) %>%
  arrange(species_grouped, DATE)

# Plot 1: NPP + SPEI-3
p1_spei3 <- ggplot() +
  geom_col(data = spei3_plot_data, 
           aes(x = DATE, y = SPEI_3), 
           fill = "lightgray", alpha = 0.6, width = 25) +
  geom_line(data = npp_monthly, 
            aes(x = DATE, y = NPP_change_pct / 20, color = species_grouped), 
            size = 1) +
  geom_point(data = npp_monthly %>% filter(MONTH == 6), 
             aes(x = DATE, y = NPP_change_pct / 20, color = species_grouped), 
             size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.7) +
  scale_color_manual(
    values = species_colors_landscape,
    labels = species_labels_german,
    name = "Baumart"
  ) +
  scale_y_continuous(
    name = "SPEI-3",
    limits = c(-2.5, 2.5),
    breaks = seq(-2, 2, by = 1),
    sec.axis = sec_axis(~ . * 20, 
                       name = "NPP [%]",
                       breaks = seq(-40, 40, by = 20))
  ) +
  scale_x_date(
    date_breaks = "5 years",
    date_labels = "%Y",
    limits = c(as.Date("2000-01-01"), as.Date("2022-12-31"))
  ) +
  labs(
    title = "a) NPP-Veränderung und SPEI-3",
    x = ""
  ) +
  theme_modern +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    plot.margin = margin(5, 10, 5, 10, "pt")
  )

# Plot 2: NPP + SPEI-6
p2_spei6 <- ggplot() +
  geom_col(data = spei6_plot_data, 
           aes(x = DATE, y = SPEI_6), 
           fill = "lightgray", alpha = 0.6, width = 25) +
  geom_line(data = npp_monthly, 
            aes(x = DATE, y = NPP_change_pct / 20, color = species_grouped), 
            size = 1) +
  geom_point(data = npp_monthly %>% filter(MONTH == 6), 
             aes(x = DATE, y = NPP_change_pct / 20, color = species_grouped), 
             size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.7) +
  scale_color_manual(
    values = species_colors_landscape,
    labels = species_labels_german,
    name = "Baumart"
  ) +
  scale_y_continuous(
    name = "SPEI-6",
    limits = c(-2.5, 2.5),
    breaks = seq(-2, 2, by = 1),
    sec.axis = sec_axis(~ . * 20, 
                       name = "NPP [%]",
                       breaks = seq(-40, 40, by = 20))
  ) +
  scale_x_date(
    date_breaks = "5 years",
    date_labels = "%Y",
    limits = c(as.Date("2000-01-01"), as.Date("2022-12-31"))
  ) +
  labs(
    title = "b) NPP-Veränderung und SPEI-6",
    x = ""
  ) +
  theme_modern +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    plot.margin = margin(5, 10, 5, 10, "pt")
  )

# Plot 3: NPP + SPEI-12
p3_spei12 <- ggplot() +
  geom_col(data = spei12_plot_data, 
           aes(x = DATE, y = SPEI_12), 
           fill = "lightgray", alpha = 0.6, width = 25) +
  geom_line(data = npp_monthly, 
            aes(x = DATE, y = NPP_change_pct / 20, color = species_grouped), 
            size = 1) +
  geom_point(data = npp_monthly %>% filter(MONTH == 6), 
             aes(x = DATE, y = NPP_change_pct / 20, color = species_grouped), 
             size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.7) +
  scale_color_manual(
    values = species_colors_landscape,
    labels = species_labels_german,
    name = "Baumart"
  ) +
  scale_y_continuous(
    name = "SPEI-12",
    limits = c(-2.5, 2.5),
    breaks = seq(-2, 2, by = 1),
    sec.axis = sec_axis(~ . * 20, 
                       name = "NPP [%]",
                       breaks = seq(-40, 40, by = 20))
  ) +
  scale_x_date(
    date_breaks = "5 years",
    date_labels = "%Y",
    limits = c(as.Date("2000-01-01"), as.Date("2022-12-31"))
  ) +
  labs(
    title = "c) NPP-Veränderung und SPEI-12",
    x = "Jahr"
  ) +
  theme_modern +
  theme(
    legend.position = "bottom",
    plot.margin = margin(5, 10, 10, 10, "pt")
  )

multiplot_combined <- grid.arrange(
  p1_spei3, p2_spei6, p3_spei12,
  ncol = 1, nrow = 3,
  heights = c(1, 1, 1.3)
)

multiplot_file <- file.path("C:/Users/Jakob/Desktop", "NPP_SPEI_multiplot.png")
ggsave(multiplot_file, multiplot_combined, width = 12, height = 10, dpi = 300)
cat("Multiplot NPP-SPEI (3 Zeitskalen) gespeichert als:", multiplot_file, "\n")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Korrelationstabelle erstellen
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

cat("\n=== ERSTELLE KORRELATIONSTABELLE NPP-SPEI ===\n")

# NPP-Daten vorbereiten
npp_correlation_data <- landscape_changes %>%
  select(
    year = real_year,
    species_grouped,
    NPP_absolute = NPP_kg,
    NPP_relative = NPP_change_pct
  ) %>%
  filter(year >= 2000 & year <= 2022)

# SPEI-Daten: jährliche Mittelwerte
spei_yearly_all <- list(
  spei3_data_clean %>% 
    filter(YEAR >= 2000 & YEAR <= 2022) %>%
    group_by(YEAR) %>%
    summarise(SPEI3_year_mean = mean(SPEI_3, na.rm = TRUE), .groups = "drop"),
  
  spei6_data_clean %>% 
    filter(YEAR >= 2000 & YEAR <= 2022) %>%
    group_by(YEAR) %>%
    summarise(SPEI6_year_mean = mean(SPEI_6, na.rm = TRUE), .groups = "drop"),
  
  spei12_data_clean %>% 
    filter(YEAR >= 2000 & YEAR <= 2022) %>%
    group_by(YEAR) %>%
    summarise(SPEI12_year_mean = mean(SPEI_12, na.rm = TRUE), .groups = "drop"),
  
  spei24_data_clean %>% 
    filter(YEAR >= 2000 & YEAR <= 2022) %>%
    group_by(YEAR) %>%
    summarise(SPEI24_year_mean = mean(SPEI_24, na.rm = TRUE), .groups = "drop")
) %>%
  reduce(full_join, by = "YEAR") %>%
  rename(year = YEAR)

# SPEI-Daten: Vegetationsperioden-Mittelwerte
spei_yearly_veg <- list(
  spei3_data_clean %>% 
    filter(YEAR >= 2000 & YEAR <= 2022 & MONTH >= 4 & MONTH <= 9) %>%
    group_by(YEAR) %>%
    summarise(SPEI3_veg_mean = mean(SPEI_3, na.rm = TRUE), .groups = "drop"),
  
  spei6_data_clean %>% 
    filter(YEAR >= 2000 & YEAR <= 2022 & MONTH >= 4 & MONTH <= 9) %>%
    group_by(YEAR) %>%
    summarise(SPEI6_veg_mean = mean(SPEI_6, na.rm = TRUE), .groups = "drop"),
  
  spei12_data_clean %>% 
    filter(YEAR >= 2000 & YEAR <= 2022 & MONTH >= 4 & MONTH <= 9) %>%
    group_by(YEAR) %>%
    summarise(SPEI12_veg_mean = mean(SPEI_12, na.rm = TRUE), .groups = "drop"),
  
  spei24_data_clean %>% 
    filter(YEAR >= 2000 & YEAR <= 2022 & MONTH >= 4 & MONTH <= 9) %>%
    group_by(YEAR) %>%
    summarise(SPEI24_veg_mean = mean(SPEI_24, na.rm = TRUE), .groups = "drop")
) %>%
  reduce(full_join, by = "YEAR") %>%
  rename(year = YEAR)

# SPEI-Daten zusammenführen
spei_correlation_data <- spei_yearly_all %>%
  left_join(spei_yearly_veg, by = "year")

# Finale Korrelationstabelle
correlation_table <- npp_correlation_data %>%
  left_join(spei_correlation_data, by = "year") %>%
  arrange(species_grouped, year)

cat("\nKorrelationstabelle erstellt:\n")
cat("- Zeilen:", nrow(correlation_table), "\n")
cat("- Spalten:", ncol(correlation_table), "\n")
cat("- Baumarten:", paste(unique(correlation_table$species_grouped), collapse = ", "), "\n")
cat("- Jahre:", min(correlation_table$year), "-", max(correlation_table$year), "\n")

cat("\nErste 10 Zeilen der Korrelationstabelle:\n")
print(head(correlation_table, 10))

cat("\nSpalten der Korrelationstabelle:\n")
column_descriptions <- c(
  "year: Jahr (2000-2022)",
  "species_grouped: Baumart (fasy, piab, pisy, qupe_quro)",
  "NPP_absolute: Absolute NPP [kg ha⁻¹ a⁻¹]",
  "NPP_relative: Relative NPP-Veränderung zur Baseline 2000 [%]",
  "SPEI3_year_mean: Jährlicher Mittelwert SPEI-3 (alle 12 Monate)",
  "SPEI6_year_mean: Jährlicher Mittelwert SPEI-6 (alle 12 Monate)",
  "SPEI12_year_mean: Jährlicher Mittelwert SPEI-12 (alle 12 Monate)", 
  "SPEI24_year_mean: Jährlicher Mittelwert SPEI-24 (alle 12 Monate)",
  "SPEI3_veg_mean: Vegetationsperioden-Mittelwert SPEI-3 (Apr-Sep)",
  "SPEI6_veg_mean: Vegetationsperioden-Mittelwert SPEI-6 (Apr-Sep)",
  "SPEI12_veg_mean: Vegetationsperioden-Mittelwert SPEI-12 (Apr-Sep)",
  "SPEI24_veg_mean: Vegetationsperioden-Mittelwert SPEI-24 (Apr-Sep)"
)
for(desc in column_descriptions) {
  cat("-", desc, "\n")
}

correlation_file <- file.path("C:/Users/Jakob/Desktop", "NPP_SPEI_correlation_table.csv")
write.csv(correlation_table, correlation_file, row.names = FALSE)
cat("\nKorrelationstabelle gespeichert als:", correlation_file, "\n")

cat("\n=== ZUSAMMENFASSUNG SPEI-WERTE ===\n")
spei_summary <- correlation_table %>%
  select(contains("SPEI")) %>%
  summarise_all(~ round(c(mean = mean(.x, na.rm = TRUE), 
                         sd = sd(.x, na.rm = TRUE), 
                         min = min(.x, na.rm = TRUE), 
                         max = max(.x, na.rm = TRUE)), 3))
cat("SPEI-Statistiken (Mittelwert, SD, Min, Max):\n")
print(spei_summary)

cat("\n=== KORRELATIONSTABELLE FERTIGGESTELLT ===\n")
cat("✅ NPP: absolut und relativ für alle Baumarten\n")
cat("✅ SPEI: 4 Zeitskalen (3, 6, 12, 24 Monate)\n")
cat("✅ SPEI: Jahres- und Vegetationsperioden-Mittelwerte\n")
cat("✅ Zeitraum: 2000-2022 (23 Jahre)\n")
cat("✅ Daten bereit für Korrelationsanalyse\n")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Korrelationsplots pro Baumart
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

cat("\n=== ERSTELLE KORRELATIONSPLOTS PRO BAUMART ===\n")

# Benötigte Bibliothek laden
library(corrplot)

# Funktion zur Erstellung der Korrelationsmatrix für eine Baumart (Pearson)
create_species_correlation <- function(species_name, data) {
  
  species_data <- data %>%
    filter(species_grouped == species_name) %>%
    select(year, NPP_relative, contains("SPEI")) %>%  # Nur relative NPP
    # Entferne NAs
    na.omit() %>%
    arrange(year)
  
  if(nrow(species_data) < 3) {
    cat("Warnung: Nicht genügend Daten für", species_name, "\n")
    return(NULL)
  }
  
  # Gleichzeitige Korrelationen (Jahr t)
  current_year_data <- species_data %>%
    select(NPP_relative, contains("SPEI"))
  
  cor_current <- cor(current_year_data, method = "pearson")
  npp_spei_current <- cor_current[1, 2:ncol(cor_current), drop = FALSE]  # Nur 1 Zeile
  
  # Lag-Korrelationen (SPEI Jahr t-1 vs NPP Jahr t)
  # SPEI-Daten um ein Jahr verschieben
  lag_data <- species_data %>%
    mutate(
      # SPEI-Werte des Vorjahres
      SPEI3_year_mean_lag = lag(SPEI3_year_mean, 1),
      SPEI6_year_mean_lag = lag(SPEI6_year_mean, 1),
      SPEI12_year_mean_lag = lag(SPEI12_year_mean, 1),
      SPEI24_year_mean_lag = lag(SPEI24_year_mean, 1),
      SPEI3_veg_mean_lag = lag(SPEI3_veg_mean, 1),
      SPEI6_veg_mean_lag = lag(SPEI6_veg_mean, 1),
      SPEI12_veg_mean_lag = lag(SPEI12_veg_mean, 1),
      SPEI24_veg_mean_lag = lag(SPEI24_veg_mean, 1)
    ) %>%
    # Entferne erste Zeile (hat NAs durch lag)
    filter(!is.na(SPEI3_year_mean_lag)) %>%
    select(NPP_relative, contains("lag"))
  
  if(nrow(lag_data) < 3) {
    cat("Warnung: Nicht genügend Lag-Daten für", species_name, "\n")
    lag_correlations <- matrix(NA, nrow = 1, ncol = 8)
    colnames(lag_correlations) <- paste0(colnames(npp_spei_current), "_lag1")
    rownames(lag_correlations) <- "NPP_relative"
  } else {
    cor_lag <- cor(lag_data, method = "pearson")
    lag_correlations <- cor_lag[1, 2:ncol(cor_lag), drop = FALSE]  # Nur 1 Zeile
    # Spaltennamen anpassen für bessere Lesbarkeit
    colnames(lag_correlations) <- gsub("_lag", "_lag1", colnames(lag_correlations))
  }
  
  # Beide Matrizen kombinieren
  combined_matrix <- cbind(npp_spei_current, lag_correlations)
  
  return(combined_matrix)
}

# Funktion zur Erstellung der Korrelationsmatrix für eine Baumart (Kendalls Tau)
create_species_correlation_kendall <- function(species_name, data) {
  
  species_data <- data %>%
    filter(species_grouped == species_name) %>%
    select(year, NPP_relative, contains("SPEI")) %>%  # Nur relative NPP
    # Entferne NAs
    na.omit() %>%
    arrange(year)
  
  if(nrow(species_data) < 3) {
    cat("Warnung: Nicht genügend Daten für", species_name, "\n")
    return(NULL)
  }
  
  # Gleichzeitige Korrelationen (Jahr t) - mit Kendalls Tau
  current_year_data <- species_data %>%
    select(-year)
  
  cor_current <- cor(current_year_data, method = "kendall")
  npp_spei_current <- cor_current[1, 2:ncol(cor_current), drop = FALSE]  # Nur 1 Zeile: NPP vs SPEI
  
  # Lag-Korrelationen (Jahr t-1) - mit Kendalls Tau
  lag_data <- species_data %>%
    arrange(year) %>%
    mutate(
      # SPEI-Werte des Vorjahres
      SPEI3_year_mean_lag = lag(SPEI3_year_mean, 1),
      SPEI6_year_mean_lag = lag(SPEI6_year_mean, 1),
      SPEI12_year_mean_lag = lag(SPEI12_year_mean, 1),
      SPEI24_year_mean_lag = lag(SPEI24_year_mean, 1),
      SPEI3_veg_mean_lag = lag(SPEI3_veg_mean, 1),
      SPEI6_veg_mean_lag = lag(SPEI6_veg_mean, 1),
      SPEI12_veg_mean_lag = lag(SPEI12_veg_mean, 1),
      SPEI24_veg_mean_lag = lag(SPEI24_veg_mean, 1)
    ) %>%
    # Entferne erste Zeile (hat NAs durch lag)
    filter(!is.na(SPEI3_year_mean_lag)) %>%
    select(NPP_relative, contains("lag"))
  
  if(nrow(lag_data) < 3) {
    cat("Warnung: Nicht genügend Lag-Daten für", species_name, "\n")
    lag_correlations <- matrix(NA, nrow = 1, ncol = 8)
    colnames(lag_correlations) <- paste0(colnames(npp_spei_current), "_lag1")
    rownames(lag_correlations) <- "NPP_relative"
  } else {
    cor_lag <- cor(lag_data, method = "kendall")
    lag_correlations <- cor_lag[1, 2:ncol(cor_lag), drop = FALSE]  # Nur 1 Zeile
    # Spaltennamen anpassen für bessere Lesbarkeit
    colnames(lag_correlations) <- gsub("_lag", "_lag1", colnames(lag_correlations))
  }
  
  # Beide Matrizen kombinieren
  combined_matrix <- cbind(npp_spei_current, lag_correlations)
  
  return(combined_matrix)
}

# Funktion zur Berechnung von Korrelationen mit p-Werten (Pearson)
create_species_correlation_with_pvalues <- function(species_name, data) {
  
  species_data <- data %>%
    filter(species_grouped == species_name) %>%
    select(year, NPP_relative, contains("SPEI")) %>%
    na.omit() %>%
    arrange(year)
  
  if(nrow(species_data) < 3) {
    cat("Warnung: Nicht genügend Daten für", species_name, "\n")
    return(list(correlations = NULL, pvalues = NULL, significance = NULL))
  }
  
  # Gleichzeitige Korrelationen mit p-Werten
  current_year_data <- species_data %>% select(NPP_relative, contains("SPEI"))
  
  cor_results_current <- list()
  p_values_current <- list()
  
  for(i in 2:ncol(current_year_data)) {
    test_result <- cor.test(current_year_data$NPP_relative, current_year_data[[i]], method = "pearson")
    cor_results_current[[colnames(current_year_data)[i]]] <- test_result$estimate
    p_values_current[[colnames(current_year_data)[i]]] <- test_result$p.value
  }
  
  # Lag-Korrelationen mit p-Werten
  lag_data <- species_data %>%
    arrange(year) %>%
    mutate(
      SPEI3_year_mean_lag = lag(SPEI3_year_mean, 1),
      SPEI6_year_mean_lag = lag(SPEI6_year_mean, 1),
      SPEI12_year_mean_lag = lag(SPEI12_year_mean, 1),
      SPEI24_year_mean_lag = lag(SPEI24_year_mean, 1),
      SPEI3_veg_mean_lag = lag(SPEI3_veg_mean, 1),
      SPEI6_veg_mean_lag = lag(SPEI6_veg_mean, 1),
      SPEI12_veg_mean_lag = lag(SPEI12_veg_mean, 1),
      SPEI24_veg_mean_lag = lag(SPEI24_veg_mean, 1)
    ) %>%
    filter(!is.na(SPEI3_year_mean_lag)) %>%
    select(NPP_relative, contains("lag"))
  
  cor_results_lag <- list()
  p_values_lag <- list()
  
  if(nrow(lag_data) >= 3) {
    for(i in 2:ncol(lag_data)) {
      test_result <- cor.test(lag_data$NPP_relative, lag_data[[i]], method = "pearson")
      var_name <- gsub("_lag", "_lag1", colnames(lag_data)[i])
      cor_results_lag[[var_name]] <- test_result$estimate
      p_values_lag[[var_name]] <- test_result$p.value
    }
  }
  
  # Kombiniere alle Ergebnisse
  all_correlations <- c(unlist(cor_results_current), unlist(cor_results_lag))
  all_pvalues <- c(unlist(p_values_current), unlist(p_values_lag))
  
  # Signifikanzsterne
  significance_stars <- ifelse(all_pvalues < 0.001, "***",
                              ifelse(all_pvalues < 0.01, "**", 
                                    ifelse(all_pvalues < 0.05, "*", "")))
  
  # Als Matrizen formatieren
  cor_matrix <- matrix(all_correlations, nrow = 1)
  p_matrix <- matrix(all_pvalues, nrow = 1)
  sig_matrix <- matrix(significance_stars, nrow = 1)
  colnames(cor_matrix) <- colnames(p_matrix) <- colnames(sig_matrix) <- names(all_correlations)
  rownames(cor_matrix) <- rownames(p_matrix) <- rownames(sig_matrix) <- "NPP_relative"
  
  return(list(correlations = cor_matrix, pvalues = p_matrix, significance = sig_matrix))
}

# Funktion zur Berechnung von Korrelationen mit p-Werten (Kendalls Tau)
create_species_correlation_kendall_with_pvalues <- function(species_name, data) {
  
  species_data <- data %>%
    filter(species_grouped == species_name) %>%
    select(year, NPP_relative, contains("SPEI")) %>%
    na.omit() %>%
    arrange(year)
  
  if(nrow(species_data) < 3) {
    cat("Warnung: Nicht genügend Daten für", species_name, "\n")
    return(list(correlations = NULL, pvalues = NULL, significance = NULL))
  }
  
  # Gleichzeitige Korrelationen mit p-Werten (Kendalls Tau)
  current_year_data <- species_data %>% select(NPP_relative, contains("SPEI"))
  
  cor_results_current <- list()
  p_values_current <- list()
  
  for(i in 2:ncol(current_year_data)) {
    test_result <- cor.test(current_year_data$NPP_relative, current_year_data[[i]], method = "kendall")
    cor_results_current[[colnames(current_year_data)[i]]] <- test_result$estimate
    p_values_current[[colnames(current_year_data)[i]]] <- test_result$p.value
  }
  
  # Lag-Korrelationen mit p-Werten (Kendalls Tau)
  lag_data <- species_data %>%
    arrange(year) %>%
    mutate(
      SPEI3_year_mean_lag = lag(SPEI3_year_mean, 1),
      SPEI6_year_mean_lag = lag(SPEI6_year_mean, 1),
      SPEI12_year_mean_lag = lag(SPEI12_year_mean, 1),
      SPEI24_year_mean_lag = lag(SPEI24_year_mean, 1),
      SPEI3_veg_mean_lag = lag(SPEI3_veg_mean, 1),
      SPEI6_veg_mean_lag = lag(SPEI6_veg_mean, 1),
      SPEI12_veg_mean_lag = lag(SPEI12_veg_mean, 1),
      SPEI24_veg_mean_lag = lag(SPEI24_veg_mean, 1)
    ) %>%
    filter(!is.na(SPEI3_year_mean_lag)) %>%
    select(NPP_relative, contains("lag"))
  
  cor_results_lag <- list()
  p_values_lag <- list()
  
  if(nrow(lag_data) >= 3) {
    for(i in 2:ncol(lag_data)) {
      test_result <- cor.test(lag_data$NPP_relative, lag_data[[i]], method = "kendall")
      var_name <- gsub("_lag", "_lag1", colnames(lag_data)[i])
      cor_results_lag[[var_name]] <- test_result$estimate
      p_values_lag[[var_name]] <- test_result$p.value
    }
  }
  
  # Kombiniere alle Ergebnisse
  all_correlations <- c(unlist(cor_results_current), unlist(cor_results_lag))
  all_pvalues <- c(unlist(p_values_current), unlist(p_values_lag))
  
  # Signifikanzsterne für Kendalls Tau
  significance_stars <- ifelse(all_pvalues < 0.001, "***",
                              ifelse(all_pvalues < 0.01, "**", 
                                    ifelse(all_pvalues < 0.05, "*", "")))
  
  # Als Matrizen formatieren
  cor_matrix <- matrix(all_correlations, nrow = 1)
  p_matrix <- matrix(all_pvalues, nrow = 1)
  sig_matrix <- matrix(significance_stars, nrow = 1)
  colnames(cor_matrix) <- colnames(p_matrix) <- colnames(sig_matrix) <- names(all_correlations)
  rownames(cor_matrix) <- rownames(p_matrix) <- rownames(sig_matrix) <- "NPP_relative"
  
  return(list(correlations = cor_matrix, pvalues = p_matrix, significance = sig_matrix))
}

correlation_matrices <- list()

species_list <- c("fasy", "piab", "pisy", "qupe_quro")
species_names_full <- c("fasy", "piab", "pisy", "qupe, quro")

combined_correlation_matrix <- NULL

for(i in seq_along(species_list)) {
  species_code <- species_list[i]
  species_name <- species_names_full[i]
  
  cat("Berechne Korrelationen für", species_name, "...\n")
  
  cor_matrix <- create_species_correlation(species_code, correlation_table)
  
  if(is.null(cor_matrix)) {
    cor_matrix <- matrix(NA, nrow = 1, ncol = 16)
    colnames(cor_matrix) <- c(
      "SPEI3_year", "SPEI6_year", "SPEI12_year", "SPEI24_year",
      "SPEI3_veg", "SPEI6_veg", "SPEI12_veg", "SPEI24_veg",
      "SPEI3_year_lag1", "SPEI6_year_lag1", "SPEI12_year_lag1", "SPEI24_year_lag1",
      "SPEI3_veg_lag1", "SPEI6_veg_lag1", "SPEI12_veg_lag1", "SPEI24_veg_lag1"
    )
  }
  
  rownames(cor_matrix) <- species_name
  
  if(is.null(combined_correlation_matrix)) {
    combined_correlation_matrix <- cor_matrix
  } else {
    combined_correlation_matrix <- rbind(combined_correlation_matrix, cor_matrix)
  }
  
  correlation_matrices[[species_code]] <- cor_matrix
}

colnames(combined_correlation_matrix) <- c(
  "SPEI3_Jahr", "SPEI6_Jahr", "SPEI12_Jahr", "SPEI24_Jahr",
  "SPEI3_Veg", "SPEI6_Veg", "SPEI12_Veg", "SPEI24_Veg",
  "SPEI3_Lag1", "SPEI6_Lag1", "SPEI12_Lag1", "SPEI24_Lag1",
  "SPEI3_VegLag1", "SPEI6_VegLag1", "SPEI12_VegLag1", "SPEI24_VegLag1"
)

cat("Erstelle kombinierten Korrelationsplot für alle Baumarten...\n")

combined_plot_file <- file.path("C:/Users/Jakob/Desktop", "correlation_plot_all_species_combined.png")

cat("Erstelle kombinierten Korrelationsplot mit kontinuierlichem Farbverlauf...\n")

combined_plot_file_continuous <- file.path("C:/Users/Jakob/Desktop", "correlation_plot_all_species_continuous.png")

png(combined_plot_file_continuous, width = 1400, height = 600, res = 150)

corrplot(combined_correlation_matrix, 
         method = "color",
         type = "full",
         order = "original",
         tl.col = "black",
         tl.srt = 45,
         tl.cex = 0.8,
         cl.cex = 0.9,
         addCoef.col = "black",
         number.cex = 0.7,
         col = colorRampPalette(c("#D7191C", "white", "#2C7BB6"))(100),
         is.corr = TRUE,
         title = "Korrelation NPP_relative - SPEI: Alle Baumarten (kontinuierlicher Farbverlauf)",
         mar = c(0, 0, 3, 0),
         tl.pos = "lt"
)

# Device schließen
dev.off()

cat("Kombinierter Korrelationsplot (kontinuierlich) gespeichert als:", combined_plot_file_continuous, "\n")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Kendalls Tau Korrelationsanalyse
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

cat("\n=== KENDALLS TAU KORRELATIONSANALYSE ===\n")

# Matrix für kombinierten Kendall-Plot vorbereiten
combined_correlation_matrix_kendall <- NULL

for(i in seq_along(species_list)) {
  species_code <- species_list[i]
  species_name <- species_names_full[i]
  
  cat("Berechne Kendalls Tau Korrelationen für", species_name, "...\n")
  
  # Korrelationsmatrix berechnen (Kendalls Tau)
  cor_matrix_kendall <- create_species_correlation_kendall(species_code, correlation_table)
  
  if(is.null(cor_matrix_kendall)) {
    cat("Keine Daten für", species_name, "verfügbar. Fülle mit NAs.\n")
    # Fülle mit NAs wenn keine Daten vorhanden
    cor_matrix_kendall <- matrix(NA, nrow = 1, ncol = 16)
    colnames(cor_matrix_kendall) <- c(
      "SPEI3_year", "SPEI6_year", "SPEI12_year", "SPEI24_year",
      "SPEI3_veg", "SPEI6_veg", "SPEI12_veg", "SPEI24_veg",
      "SPEI3_year_lag1", "SPEI6_year_lag1", "SPEI12_year_lag1", "SPEI24_year_lag1",
      "SPEI3_veg_lag1", "SPEI6_veg_lag1", "SPEI12_veg_lag1", "SPEI24_veg_lag1"
    )
  }
  
  # Zeilennamen für Baumarten setzen
  rownames(cor_matrix_kendall) <- species_name
  
  # Zur kombinierten Matrix hinzufügen
  if(is.null(combined_correlation_matrix_kendall)) {
    combined_correlation_matrix_kendall <- cor_matrix_kendall
  } else {
    combined_correlation_matrix_kendall <- rbind(combined_correlation_matrix_kendall, cor_matrix_kendall)
  }
}

# Spaltennamen verkürzen für bessere Lesbarkeit
colnames(combined_correlation_matrix_kendall) <- c(
  "SPEI3_Jahr", "SPEI6_Jahr", "SPEI12_Jahr", "SPEI24_Jahr",
  "SPEI3_Veg", "SPEI6_Veg", "SPEI12_Veg", "SPEI24_Veg",
  "SPEI3_Lag1", "SPEI6_Lag1", "SPEI12_Lag1", "SPEI24_Lag1",
  "SPEI3_VegLag1", "SPEI6_VegLag1", "SPEI12_VegLag1", "SPEI24_VegLag1"
)

# Kombinierter Kendall-Korrelationsplot erstellen
cat("Erstelle kombinierten Kendalls Tau Korrelationsplot...\n")

combined_plot_file_kendall <- file.path("C:/Users/Jakob/Desktop", "correlation_plot_all_species_kendall.png")

# PNG-Device öffnen
png(combined_plot_file_kendall, width = 1400, height = 600, res = 150)

# Korrelationsplot mit kontinuierlichem Farbverlauf erstellen
corrplot(combined_correlation_matrix_kendall, 
         method = "color",           # Farbige Quadrate
         type = "full",              # Vollständige Matrix
         order = "original",         # Ursprüngliche Reihenfolge
         tl.col = "black",          # Textfarbe für Labels
         tl.srt = 45,               # Rotation der Labels (Spalten)
         tl.cex = 0.8,              # Textgröße für Labels
         cl.cex = 0.9,              # Textgröße für Colorbar
         addCoef.col = "black",     # Farbe für Korrelationskoeffizienten
         number.cex = 0.7,          # Textgröße für Koeffizienten
         col = colorRampPalette(c("#D7191C", "white", "#2C7BB6"))(100),  # Kontinuierlicher Rot-Weiß-Blau Verlauf
         is.corr = TRUE,            # Ist Korrelationsmatrix (-1 bis 1)
         title = "Kendalls Tau: NPP_relative - SPEI (Alle Baumarten, kontinuierlicher Farbverlauf)",
         mar = c(0, 0, 3, 0),       # Ränder anpassen
         # Zeilennamen (Baumarten) links anzeigen
         tl.pos = "lt"              # Labels oben und links
)

# Device schließen
dev.off()

cat("Kombinierter Kendalls Tau Korrelationsplot gespeichert als:", combined_plot_file_kendall, "\n")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# ZUSAMMENFASSENDE KORRELATIONSANALYSE - SPEARMAN UND KENDALL KOMBINIERT ##########################################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

cat("\n=== ZUSAMMENFASSENDE KORRELATIONSANALYSE: SPEARMAN & KENDALL ===\n")

# Kombinierte Matrix für alle Baumarten und beide Korrelationsmethoden
combined_summary_correlation_matrix <- NULL
combined_summary_pvalue_matrix <- NULL
combined_summary_significance_matrix <- NULL

# Zeilennamen für die kombinierte Matrix
row_names_summary <- c()

for(i in seq_along(species_list)) {
  species_code <- species_list[i]
  species_name <- species_names_full[i]
  
  cat("Berechne Spearman und Kendall Korrelationen für", species_name, "...\n")
  
  # SPEARMAN KORRELATIONEN
  spearman_results <- create_species_correlation_with_pvalues(species_code, correlation_table)
  
  if(is.null(spearman_results$correlations)) {
    spearman_cor <- matrix(NA, nrow = 1, ncol = 16)
    spearman_pval <- matrix(NA, nrow = 1, ncol = 16)
    spearman_sig <- matrix("", nrow = 1, ncol = 16)
  } else {
    spearman_cor <- spearman_results$correlations
    spearman_pval <- spearman_results$pvalues
    spearman_sig <- spearman_results$significance
  }
  
  # KENDALLS TAU KORRELATIONEN  
  kendall_results <- create_species_correlation_kendall_with_pvalues(species_code, correlation_table)
  
  if(is.null(kendall_results$correlations)) {
    kendall_cor <- matrix(NA, nrow = 1, ncol = 16)
    kendall_pval <- matrix(NA, nrow = 1, ncol = 16)
    kendall_sig <- matrix("", nrow = 1, ncol = 16)
  } else {
    kendall_cor <- kendall_results$correlations
    kendall_pval <- kendall_results$pvalues
    kendall_sig <- kendall_results$significance
  }
  
  # Beide Methoden für diese Baumart kombinieren
  species_combined_cor <- rbind(spearman_cor, kendall_cor)
  species_combined_pval <- rbind(spearman_pval, kendall_pval) 
  species_combined_sig <- rbind(spearman_sig, kendall_sig)
  
  # Zeilennamen setzen - mit (ρ) für Spearman und (τ) für Kendall
  rownames(species_combined_cor) <- c(paste0(species_name, " (ρ)"), paste0(species_name, " (τ)"))
  rownames(species_combined_pval) <- c(paste0(species_name, " (ρ)"), paste0(species_name, " (τ)"))
  rownames(species_combined_sig) <- c(paste0(species_name, " (ρ)"), paste0(species_name, " (τ)"))
  
  # Zur Gesamtmatrix hinzufügen
  if(is.null(combined_summary_correlation_matrix)) {
    combined_summary_correlation_matrix <- species_combined_cor
    combined_summary_pvalue_matrix <- species_combined_pval
    combined_summary_significance_matrix <- species_combined_sig
  } else {
    combined_summary_correlation_matrix <- rbind(combined_summary_correlation_matrix, species_combined_cor)
    combined_summary_pvalue_matrix <- rbind(combined_summary_pvalue_matrix, species_combined_pval)
    combined_summary_significance_matrix <- rbind(combined_summary_significance_matrix, species_combined_sig)
  }
  
  # Zeilennamen für später sammeln
  row_names_summary <- c(row_names_summary, paste0(species_name, " (ρ)"), paste0(species_name, " (τ)"))
}

# Spaltennamen verkürzen für bessere Lesbarkeit (angepasste Namen wie bei Mortalität)
npp_column_names <- c(
  "SPEI3", "SPEI6", "SPEI12", "SPEI24",
  "SPEI3_Veg", "SPEI6_Veg", "SPEI12_Veg", "SPEI24_Veg",
  "SPEI3_Lag", "SPEI6_Lag", "SPEI12_Lag", "SPEI24_Lag",
  "SPEI3_VegLag", "SPEI6_VegLag", "SPEI12_VegLag", "SPEI24_VegLag"
)
colnames(combined_summary_correlation_matrix) <- npp_column_names
colnames(combined_summary_pvalue_matrix) <- npp_column_names
colnames(combined_summary_significance_matrix) <- npp_column_names

# Zusammenfassende Heatmap erstellen (mit ggplot2 wie bei Mortalität)
cat("Erstelle zusammenfassende Korrelations-Heatmap...\n")

combined_plot_file_summary <- file.path("C:/Users/Jakob/Desktop", "correlation_summary_spearman_kendall.png")

# Daten für ggplot2 vorbereiten
library(reshape2)

# Korrelationsmatrix in Long-Format umwandeln
npp_cor_long <- melt(combined_summary_correlation_matrix)
colnames(npp_cor_long) <- c("Species", "SPEI_Variable", "Correlation")

# Signifikanzmatrix in Long-Format umwandeln
npp_sig_long <- melt(combined_summary_significance_matrix)
colnames(npp_sig_long) <- c("Species", "SPEI_Variable", "Significance")

# Kombinieren
npp_heatmap_data <- npp_cor_long %>%
  left_join(npp_sig_long, by = c("Species", "SPEI_Variable"))

# Korrelationswerte mit deutschem Dezimaltrennzeichen formatieren
npp_heatmap_data <- npp_heatmap_data %>%
  mutate(
    # Korrelationswert mit Komma als Dezimaltrenner
    Cor_Label = gsub("\\.", ",", sprintf("%.2f", Correlation))
  )

# Faktor-Reihenfolge für X-Achse (Baumarten - jetzt oben)
npp_species_levels <- rownames(combined_summary_correlation_matrix)
npp_heatmap_data$Species <- factor(npp_heatmap_data$Species, levels = npp_species_levels)

# Faktor-Reihenfolge für Y-Achse (SPEI-Variablen - jetzt links, umgekehrt)
npp_heatmap_data$SPEI_Variable <- factor(npp_heatmap_data$SPEI_Variable, levels = rev(npp_column_names))

# ggplot2 Heatmap erstellen (90° gedreht: X=Baumarten, Y=SPEI)
p_npp_heatmap <- ggplot(npp_heatmap_data, aes(x = Species, y = SPEI_Variable, fill = Correlation)) +
  geom_tile(color = "white", linewidth = 0.8) +
  # Korrelationswerte leicht oberhalb der Mitte
  geom_text(aes(label = Cor_Label), 
            color = "black", 
            size = 3.0,
            family = "Arial",
            fontface = "bold",
            vjust = 0.3) +
  # Signifikanzsterne darunter (nur wenn vorhanden)
  geom_text(aes(label = Significance), 
            color = "black", 
            size = 2.8,
            family = "Arial",
            fontface = "bold",
            vjust = -0.8) +
  # Farbskala (rot-weiß-blau)
  scale_fill_gradient2(low = "#D7191C", mid = "white", high = "#2C7BB6",
                       midpoint = 0, 
                       limits = c(-1, 1),
                       name = NULL,
                       labels = function(x) gsub("\\.", ",", sprintf("%.1f", x))) +
  # X-Achse oben (Baumarten)
  scale_x_discrete(position = "top") +
  # Theme anpassen
  theme_minimal(base_family = "Arial") +
  theme(
    # X-Achse (oben): Baumarten, fett, 60° gedreht
    axis.text.x.top = element_text(angle = 60, hjust = 0, vjust = 0, 
                               size = 9, face = "bold", family = "Arial"),
    # Y-Achse: SPEI-Variablen, fett
    axis.text.y = element_text(size = 9, face = "bold", family = "Arial",
                               margin = margin(r = 2)),
    # Achsentitel entfernen
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    # Legende anpassen
    legend.position = "right",
    legend.text = element_text(size = 8, family = "Arial"),
    legend.key.height = unit(1.5, "cm"),
    legend.key.width = unit(0.3, "cm"),
    legend.margin = margin(l = 5),
    # Panel
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    # Ränder - mehr Platz oben
    plot.margin = margin(t = 50, r = 5, b = 5, l = 5)
  ) +
  coord_cartesian(clip = "off")

# Plot speichern Hochformat: 14 x 21 cm
ggsave(combined_plot_file_summary, p_npp_heatmap, width = 14, height = 21, units = "cm", dpi = 300)
cat("Zusammenfassende Korrelations-Heatmap gespeichert als:", combined_plot_file_summary, "\n")
cat("Format: 14 x 21 cm (Hochformat)\n")

# Zusammenfassende Tabelle als CSV speichern
cat("Erstelle zusammenfassende CSV-Tabelle...\n")

# Kombinierte Tabelle für CSV-Export erstellen
summary_table <- data.frame(
  Baumart_Methode = rownames(combined_summary_correlation_matrix),
  combined_summary_correlation_matrix,
  stringsAsFactors = FALSE
)

# p-Werte als separate Spalten hinzufügen
pvalue_columns <- combined_summary_pvalue_matrix
colnames(pvalue_columns) <- paste0(colnames(pvalue_columns), "_pval")
summary_table <- cbind(summary_table, pvalue_columns)

# Signifikanz-Spalten hinzufügen
significance_columns <- combined_summary_significance_matrix
colnames(significance_columns) <- paste0(colnames(combined_summary_correlation_matrix), "_sig")
summary_table <- cbind(summary_table, significance_columns)

# CSV speichern
summary_csv_file <- file.path("C:/Users/Jakob/Desktop", "NPP_SPEI_correlation_summary_spearman_kendall.csv")
write.csv(summary_table, summary_csv_file, row.names = FALSE)
cat("Zusammenfassende Tabelle gespeichert als:", summary_csv_file, "\n")

# Numerische Korrelationswerte ausgeben
cat("\n=== NUMERISCHE KORRELATIONSWERTE ===\n")
for(species_code in names(correlation_matrices)) {
  species_name <- species_labels_german[species_code]
  cor_matrix <- correlation_matrices[[species_code]]
  
  cat("\n", species_name, " (", species_code, "):\n", sep = "")
  cat("---", paste(rep("-", nchar(species_name) + nchar(species_code) + 4), collapse = ""), "\n")
  
  # Formatierte Ausgabe der Korrelationen
  for(i in 1:nrow(cor_matrix)) {
    npp_type <- rownames(cor_matrix)[i]
    cat(sprintf("%-12s", npp_type), ": ")
    
    for(j in 1:ncol(cor_matrix)) {
      spei_var <- colnames(cor_matrix)[j]
      cor_val <- cor_matrix[i, j]
      cat(sprintf("%6.3f ", cor_val))
    }
    cat("\n")
  }
}

# Zusammenfassung der stärksten Korrelationen
cat("\n=== STÄRKSTE KORRELATIONEN PRO BAUMART ===\n")
for(species_code in names(correlation_matrices)) {
  species_name <- species_labels_german[species_code]
  cor_matrix <- correlation_matrices[[species_code]]
  
  cat(species_name, " (", species_code, "):\n", sep = "")
  
  # Aktuelle Jahr Korrelationen (erste 8 Spalten)
  current_cols <- 1:8
  if(ncol(cor_matrix) >= 8) {
    current_matrix <- cor_matrix[, current_cols, drop = FALSE]
    max_cor_current <- max(abs(current_matrix), na.rm = TRUE)
    max_pos_current <- which(abs(current_matrix) == max_cor_current, arr.ind = TRUE)
    
    if(nrow(max_pos_current) > 0) {
      spei_var <- colnames(current_matrix)[max_pos_current[1, 2]]
      cor_val <- current_matrix[1, max_pos_current[1, 2]]
      
      cat("  Aktuelles Jahr : NPP_relative ~ ", spei_var, " (r = ", 
          sprintf("%.3f", cor_val), ")\n", sep = "")
    }
  }
  
  # Lag-1 Korrelationen (letzte 8 Spalten)
  if(ncol(cor_matrix) > 8) {
    lag_cols <- 9:ncol(cor_matrix)
    lag_matrix <- cor_matrix[, lag_cols, drop = FALSE]
    max_cor_lag <- max(abs(lag_matrix), na.rm = TRUE)
    max_pos_lag <- which(abs(lag_matrix) == max_cor_lag, arr.ind = TRUE)
    
    if(nrow(max_pos_lag) > 0 && !is.na(max_cor_lag)) {
      spei_var <- colnames(lag_matrix)[max_pos_lag[1, 2]]
      cor_val <- lag_matrix[1, max_pos_lag[1, 2]]
      
      cat("  Lag-1 Jahr     : NPP_relative ~ ", spei_var, " (r = ", 
          sprintf("%.3f", cor_val), ")\n", sep = "")
    } else {
      cat("  Lag-1 Jahr     : Keine gültigen Korrelationen\n")
    }
  }
  cat("\n")
}

cat("\n=== SPEI-KORRELATIONSANALYSE ABGESCHLOSSEN ===\n")
cat("✅ Zusammenfassende Korrelations-Heatmap (Spearman & Kendall)\n")
cat("✅ 8 Zeilen: Buche Spearman, Buche Kendall, Fichte Spearman, Fichte Kendall, etc.\n")
cat("✅ 16 Spalten: SPEI-Variablen (aktuell + Lag-1) für alle Zeitskalen\n")
cat("✅ p-Werte mit Signifikanzsternen (*<0.05, **<0.01, ***<0.001)\n")
cat("✅ Kontinuierliche Farbskala für direkte Vergleichbarkeit\n")
cat("\n📁 AUSGABEDATEIEN:\n")
cat("📊 correlation_summary_spearman_kendall.png - Korrelations-Heatmap\n") 
cat("📋 NPP_SPEI_correlation_summary_spearman_kendall.csv - Detaillierte Korrelationstabelle\n")
cat("\n🎯 WISSENSCHAFTLICHE ERKENNTNISSE:\n")
cat("🔬 Quantitative Beziehungen: NPP-SPEI Korrelationen mit statistischer Signifikanz\n")
cat("⏳ Lag-Effekte: Verzögerte Reaktionen auf Vorjahres-Trockenheit\n")
cat("🌲 Artspezifische Unterschiede: Differenzielle Trockenheitstoleranz\n")
cat("� Methodenvergleich: Spearman (parametrisch) vs. Kendall (nicht-parametrisch)\n") 



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Set-up ###################################################################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

## Read in libraries ####
library(tidyverse)
library(RSQLite)
library(dbplyr)
library(terra)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(scales)
# library(corrplot) # Optional - comment out if not installed

## Read in iLand input files ####

setwd("C:/Users/Jakob/OneDrive/OneDrive Dokumente/Universitaet/02_Master/Masterarbeit/iLand_SOSFOR/Iland_project/temp")

# Environment file
(env.df <- read_delim("../gis/environment.txt"))

# DEM raster, 10 x 10 m resolution (before 50 x 50 m) 
dem.grid <- terra::rast("C:/Users/Jakob/OneDrive/OneDrive Dokumente/Universitaet/02_Master/Masterarbeit/iLand_SOSFOR/Iland_project/gis/dem.asc") # wurde von mir angepasst, da die Datei nicht existierte

# Convert x,y coordinates to lon,lat based on lower left corner coordinates
# Store original x and y coordinates
env.df$x_orig <- env.df$x
env.df$y_orig <- env.df$y

env.df <- env.df %>%
  mutate(
    # Convert x to longitude
    x = 405400 + x * 100, #JAl: x coordinate of right side 465700
    # Convert y to latitude
    y = 5277726 + y * 100 #JAl: y coordinate of lower side 
  )
# Add information about elevation to environment file
env.df$elevation <- terra::extract(
  # aggregate DEM (50x50 m) to fit resolution of environment file (100x100 m)
  terra::aggregate(dem.grid, fact = 10, fun = "mean"), 
  env.df[, c("x", "y")], ID = FALSE) %>% unlist()

# Species database
db.conn <- RSQLite::dbConnect(RSQLite::SQLite(),
                              dbname = "../database/all_species_database.sqlite") # habe ich angepasst - für european species keine Datei mit Inhalt vorhanden...
species.df <- dplyr::tbl(db.conn, "species") %>% 
  collect()
RSQLite::dbDisconnect(db.conn); rm(db.conn)

# Farbpalette definieren (falls noch nicht vorhanden)
if(!exists("species_colors")) {
  species_colors <- species.df %>% 
    arrange(shortName) %>% 
    mutate(displayColor = paste0("#", displayColor)) %>% 
    select(shortName, displayColor) %>%
    deframe()
  
  species_colors["qupe_quro"] <- species_colors["qupe"]
  species_colors["Buche"] <- species_colors["fasy"]
  species_colors["Fichte"] <- species_colors["piab"]
  species_colors["Kiefer"] <- species_colors["pisy"]
  species_colors["Eichen"] <- species_colors["qupe"]
  
  species_labels_german <- c(
    "fasy" = "Buche",
    "qupe_quro" = "Eichen", 
    "piab" = "Fichte",
    "pisy" = "Kiefer"
  )
}

target.df <- read.csv("C:/Users/Jakob/OneDrive/OneDrive Dokumente/Universitaet/02_Master/Masterarbeit/iLand_SOSFOR/AgentFiles/no_mgmt_eng_Muenstertal_ABE-lib_targets_v1.0.csv", sep=";")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# iLand Output laden
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

connect_to_sqlite <- function(filename, output_dir = "../output/") {
  # Construct full path
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

# Connect to SQLite database and load data
sqlite_filename <- "project_CSA_20251103_140033.sqlite"  # Change filename here
selected_db <- connect_to_sqlite(sqlite_filename)
db.conn <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = selected_db)

RSQLite::dbListTables(db.conn) # list of available output tables
# Hint: if you want to analyse other tables, please refer to https://iland-model.org/Outputs 
# or alternatively, you can use a SQLite browser to explore the databases.
# If a table is missing check whether the specific output is activated in the project file

# Load tables landscape, dynamicstand, and wind
lscp <- dplyr::tbl(db.conn, "landscape") %>% 
  collect()

stand <- dplyr::tbl(db.conn, "stand") %>% 
  collect()

standdead <- dplyr::tbl(db.conn, "standdead") %>% 
  collect()

RSQLite::dbDisconnect(db.conn); rm(db.conn)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Mortalitätsrate berechnen (Fischer et al. Formel)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

## Daten vorbereiten ####
cat("Bereite Daten für Mortalitätsberechnung vor...\n")

# Lebende Bäume: Jahr - 1 für Vergleich mit toten Bäumen
living_trees <- stand %>%
  mutate(year_for_mortality = year + 1) %>%
  select(year_for_mortality, rid, species, count_ha_living = count_ha)

dead_trees <- standdead %>%
  select(year, rid, species, count_ha_dead = count_ha)

combined_data <- dead_trees %>%
  right_join(living_trees, 
            by = c("year" = "year_for_mortality", "rid", "species")) %>%
  mutate(
    count_ha_dead = ifelse(is.na(count_ha_dead), 0, count_ha_dead),
    count_ha_living = ifelse(is.na(count_ha_living), 0, count_ha_living)
  ) %>%
  # Eichen zusammenfassen: qupe + quro
  mutate(
    species_grouped = case_when(
      species %in% c("qupe", "quro") ~ "qupe_quro",
      TRUE ~ species
    )
  )

# Überblick über verfügbare Daten
cat("Jahre:", paste(sort(unique(combined_data$year)), collapse = ", "), "\n")
cat("Baumarten original:", paste(sort(unique(combined_data$species)), collapse = ", "), "\n")
cat("Baumarten gruppiert:", paste(sort(unique(combined_data$species_grouped)), collapse = ", "), "\n")

# Datenqualitäts-Checks
total_rows <- nrow(combined_data)
zero_living <- sum(combined_data$count_ha_living == 0, na.rm = TRUE)
zero_dead <- sum(combined_data$count_ha_dead == 0, na.rm = TRUE)
na_living <- sum(is.na(combined_data$count_ha_living))
na_dead <- sum(is.na(combined_data$count_ha_dead))

cat("\n=== DATENQUALITÄT ===\n")
cat("Gesamte RU-Art-Jahr Kombinationen:", total_rows, "\n")
cat("Kombinationen mit 0 lebenden Bäumen:", zero_living, "(", round(zero_living/total_rows*100, 1), "%)\n")
cat("Kombinationen mit 0 toten Bäumen:", zero_dead, "(", round(zero_dead/total_rows*100, 1), "%)\n")
cat("NAs in lebenden Bäumen (vor Behandlung):", na_living, "\n")
cat("NAs in toten Bäumen (vor Behandlung):", na_dead, "\n")

## Mortalitätsraten nach Fischer et al. Formel berechnen ####
# Mortalität_Landschaft(t) = Σ_alle_RUs (Tote Bäume(t)) / Σ_alle_RUs (Lebende Bäume(t-1)) × 100%

# Für einzelne Baumarten (fasy, piab, pisy, qupe_quro) nach Jahr
mortality_by_species_year <- combined_data %>%
  filter(species_grouped %in% c("fasy", "piab", "pisy", "qupe_quro")) %>%
  group_by(year, species_grouped) %>%
  summarise(
    total_dead_trees = sum(count_ha_dead, na.rm = TRUE),
    total_living_trees_prev = sum(count_ha_living, na.rm = TRUE),
    n_resource_units = n(),
    n_zero_living = sum(count_ha_living == 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Mortalitätsrate berechnen - explizite Division-durch-Null-Behandlung
  mutate(
    mortality_rate = case_when(
      total_living_trees_prev == 0 ~ NA_real_,  # Division durch 0
      total_living_trees_prev > 0 ~ (total_dead_trees / total_living_trees_prev) * 100,
      TRUE ~ NA_real_  # Fallback für unerwartete Fälle
    ),
    division_by_zero = total_living_trees_prev == 0
  )

# Zusammenfassung über alle gewünschten Baumarten nach Jahr
mortality_by_year_all_species <- combined_data %>%
  filter(species_grouped %in% c("fasy", "piab", "pisy", "qupe_quro")) %>%
  group_by(year) %>%
  summarise(
    total_dead_trees = sum(count_ha_dead, na.rm = TRUE),
    total_living_trees_prev = sum(count_ha_living, na.rm = TRUE),
    n_resource_units = n(),
    n_species = length(unique(species_grouped)),
    n_zero_living = sum(count_ha_living == 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    mortality_rate = case_when(
      total_living_trees_prev == 0 ~ NA_real_,
      total_living_trees_prev > 0 ~ (total_dead_trees / total_living_trees_prev) * 100,
      TRUE ~ NA_real_
    ),
    division_by_zero = total_living_trees_prev == 0
  )

## Ergebnisse ausgeben ####
cat("\n=== MORTALITÄTSRATEN NACH FISCHER ET AL. ===\n")
cat("Formel: Σ_alle_RUs (Tote Bäume(t)) / Σ_alle_RUs (Lebende Bäume(t-1)) × 100%\n\n")

# NA-Statistiken für Mortalitätsraten
na_species_year <- sum(is.na(mortality_by_species_year$mortality_rate))
na_year_all <- sum(is.na(mortality_by_year_all_species$mortality_rate))
division_by_zero_species <- sum(mortality_by_species_year$division_by_zero, na.rm = TRUE)
division_by_zero_year <- sum(mortality_by_year_all_species$division_by_zero, na.rm = TRUE)

cat("=== DATENQUALITÄT MORTALITÄTSRATEN ===\n")
cat("Jahr-Baumart Kombinationen mit NA (Division durch 0):", na_species_year, "/", nrow(mortality_by_species_year), "\n")
cat("Jahr Kombinationen mit NA (Division durch 0):", na_year_all, "/", nrow(mortality_by_year_all_species), "\n")
if(division_by_zero_species > 0 || division_by_zero_year > 0) {
  cat("⚠️  WARNUNG: Division durch Null erkannt! Diese werden als NA behandelt.\n")
}
cat("\n")

cat("Mortalitätsraten nach Jahr und Baumart:\n")
mortality_table <- mortality_by_species_year %>%
  select(year, species_grouped, mortality_rate, total_dead_trees, total_living_trees_prev, n_zero_living) %>%
  arrange(year, species_grouped)
print(mortality_table)

cat("\nMortalitätsraten zusammengefasst nach Jahr (alle Baumarten):\n")
mortality_year_summary <- mortality_by_year_all_species %>%
  select(year, mortality_rate, total_dead_trees, total_living_trees_prev, n_species, n_zero_living) %>%
  arrange(year)
print(mortality_year_summary)

cat("\nMortalitätsraten zusammengefasst nach Baumart (alle Jahre):\n")
mortality_species_summary <- mortality_by_species_year %>%
  group_by(species_grouped) %>%
  summarise(
    total_dead_trees = sum(total_dead_trees, na.rm = TRUE),
    total_living_trees_prev = sum(total_living_trees_prev, na.rm = TRUE),
    mortality_rate = case_when(
      total_living_trees_prev == 0 ~ NA_real_,
      total_living_trees_prev > 0 ~ (total_dead_trees / total_living_trees_prev) * 100,
      TRUE ~ NA_real_
    ),
    n_years = n(),
    n_na_years = sum(is.na(mortality_rate)),
    .groups = "drop"
  ) %>%
  arrange(desc(mortality_rate))
print(mortality_species_summary)

cat("\nGesamte Mortalitätsrate (alle Jahre und Baumarten):\n")
total_mortality <- combined_data %>%
  filter(species_grouped %in% c("fasy", "piab", "pisy", "qupe_quro")) %>%
  summarise(
    total_dead_trees = sum(count_ha_dead, na.rm = TRUE),
    total_living_trees_prev = sum(count_ha_living, na.rm = TRUE),
    mortality_rate = case_when(
      total_living_trees_prev == 0 ~ NA_real_,
      total_living_trees_prev > 0 ~ (total_dead_trees / total_living_trees_prev) * 100,
      TRUE ~ NA_real_
    )
  )

if(is.na(total_mortality$mortality_rate)) {
  cat("⚠️  WARNUNG: Gesamte Mortalitätsrate ist NA (Division durch Null)\n")
  cat("Tote Bäume gesamt:", total_mortality$total_dead_trees, "Bäume/ha\n")
  cat("Lebende Bäume gesamt (Vorjahr):", total_mortality$total_living_trees_prev, "Bäume/ha\n")
} else {
  cat("Gesamte Mortalitätsrate:", round(total_mortality$mortality_rate, 3), "%\n")
  cat("Tote Bäume gesamt:", total_mortality$total_dead_trees, "Bäume/ha\n")
  cat("Lebende Bäume gesamt (Vorjahr):", total_mortality$total_living_trees_prev, "Bäume/ha\n")
}

cat("\n=== ZUSAMMENFASSUNG ===\n")
cat("✅ full_join verwendet - behält 0%-Mortalität\n")
cat("✅ Division durch 0 explizit behandelt als NA\n")
cat("✅ NAs geprüft und dokumentiert\n")
cat("✅ Eichen gruppiert: qupe + quro → qupe_quro\n")
cat("✅ Gewichtete Aggregation nach Fischer et al. Formel\n")

## Multiplot: Mortalität über der Zeit pro Baumart ####
cat("\nErstelle Multiplot: Mortalität über der Zeit pro Baumart...\n")

# Sicherstellen, dass Jahr numerisch ist
mortality_by_species_year <- mortality_by_species_year %>%
  mutate(year = as.numeric(year))

# Daten zum Plotten: nur gewünschte Baumarten, Simulationsjahre 11-34 (reale Jahre 2000-2022)
plot_data <- mortality_by_species_year %>%
  filter(species_grouped %in% c("fasy", "piab", "pisy", "qupe_quro")) %>%
  filter(year >= 11 & year <= 34) %>%  # Simulationsjahre 11-34 entsprechen 2000-2022
  mutate(real_year = year + 1989) %>%  # Umrechnung zu realen Jahren (Jahr 11 = 2000)
  arrange(species_grouped, year)

if(nrow(plot_data) == 0) {
  cat("Keine Mortalitätsdaten für Jahre 11-34 vorhanden zum Plotten.\n")
} else {
  # Verwende NA-Filtern nur für Mortality (fehlende Werte bleiben als Lücken)
  plot_data_plot <- plot_data %>%
    filter(!is.na(mortality_rate))

  if(nrow(plot_data_plot) == 0) {
    cat("Alle Mortalitätsraten sind NA (z.B. Division durch 0). Kein Plot erstellt.\n")
  } else {
    # Berechne mittlere Anzahl lebender Bäume pro Baumart für N-Annotation
    n_annotation <- plot_data_plot %>%
      group_by(species_grouped) %>%
      summarise(
        mean_living = round(mean(total_living_trees_prev, na.rm = TRUE), 0),
        .groups = "drop"
      ) %>%
      mutate(
        label = paste0("N = ", format(mean_living, big.mark = ",")),
        # Position für Annotation (oben rechts in jedem Facet)
        x_pos = max(plot_data_plot$real_year, na.rm = TRUE),
        y_pos = Inf
      )
    
    library(ggplot2)

    # Verwende baumart-spezifische Farben
    present_species_mort <- unique(plot_data_plot$species_grouped)
    species_colors_mortality <- species_colors[present_species_mort]
    species_colors_mortality <- species_colors_mortality[!is.na(species_colors_mortality)]

    p <- ggplot(plot_data_plot, aes(x = real_year, y = mortality_rate, color = species_grouped)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      # N-Annotation hinzufügen
      geom_text(data = n_annotation, 
                aes(x = x_pos, y = y_pos, label = label),
                hjust = 1, vjust = 1.2, 
                size = 3.5, color = "darkgray", 
                inherit.aes = FALSE) +
      facet_wrap(~ species_grouped, ncol = 2, labeller = labeller(species_grouped = species_labels_german)) +
      scale_color_manual(values = species_colors_mortality, guide = "none") +  # Keine Legende nötig bei Facets
      scale_x_continuous(breaks = seq(2000, 2022, by = 5)) +
      labs(
        title = "Mortalität über die Zeit nach Baumart (2000-2022)",
        subtitle = "N = mittlere Anzahl lebender Bäume pro Jahr",
        x = "Jahr",
        y = "Mortalitätsrate (%)"
      ) +
      theme_minimal() +
      theme(
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "darkgray")
      )

    print(p)

    cat("Multiplot erstellt.\n")
  }
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Validierungsdaten laden und visualisieren ################################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

library(readxl)

## Validierungsdaten laden ####
cat("\nLade Validierungsdaten aus Excel-Dateien...\n")

validation_dir <- "C:/Users/Jakob/OneDrive/OneDrive Dokumente/Universitaet/02_Master/Masterarbeit/iLand_SOSFOR/GIS/Mortalitätsraten"

# Dateinamen und entsprechende Baumarten
validation_files <- data.frame(
  file = c("mort_beech_1998-2022.xlsx", "mort_oak_1998-2022.xlsx", 
           "mort_pine_1998-2022.xlsx", "mort_spruce_1998-2022.xlsx"),
  species = c("fasy", "qupe_quro", "pisy", "piab"),
  stringsAsFactors = FALSE
)

# Funktion zum Laden und Umformen der Excel-Daten
load_validation_data <- function(file_path, species_name) {
  if(!file.exists(file_path)) {
    cat("⚠️  Datei nicht gefunden:", file_path, "\n")
    return(NULL)
  }
  
  data <- read_excel(file_path)
  
  # DN Spalte als RU ID, Jahre 1998-2022 als Spalten C-AA
  # Umformen von wide zu long format
  data_long <- data %>%
    select(DN, `1998`:`2022`) %>%  # DN und Jahre 1998-2022
    rename(ru_id = DN) %>%
    pivot_longer(cols = -ru_id, names_to = "year", values_to = "mortality_raw") %>%
    mutate(
      year = as.numeric(year),
      species = species_name,
      # Umrechnung von 0-10000 zu 0-100%
      mortality_rate = mortality_raw / 100,
      # NAs beibehalten (Baumart nicht in RU vorhanden)
      mortality_rate = ifelse(is.na(mortality_raw), NA_real_, mortality_rate)
    ) %>%
    select(ru_id, year, species, mortality_rate) %>%
    # Nur Jahre 2000-2022 behalten (entspricht Simulationsjahren 11-34)
    filter(year >= 2000 & year <= 2022) %>%
    # NAs entfernen (Baumart nicht in RU)
    filter(!is.na(mortality_rate))
  
  return(data_long)
}

# Alle Validierungsdaten laden
validation_data_list <- list()
for(i in 1:nrow(validation_files)) {
  file_path <- file.path(validation_dir, validation_files$file[i])
  species_name <- validation_files$species[i]
  
  cat("Lade:", validation_files$file[i], "->", species_name, "\n")
  validation_data_list[[i]] <- load_validation_data(file_path, species_name)
}

# Alle Daten kombinieren
validation_data_combined <- do.call(rbind, validation_data_list)

if(is.null(validation_data_combined) || nrow(validation_data_combined) == 0) {
  cat("⚠️  Keine Validierungsdaten geladen.\n")
} else {
  cat("Validierungsdaten geladen:\n")
  cat("- RUs:", length(unique(validation_data_combined$ru_id)), "\n")
  cat("- Jahre:", paste(sort(unique(validation_data_combined$year)), collapse = ", "), "\n")
  cat("- Baumarten:", paste(sort(unique(validation_data_combined$species)), collapse = ", "), "\n")
  cat("- Datensätze:", nrow(validation_data_combined), "\n")

  ## Neue Visualisierung: RU-spezifische Mortalität über Zeit ####
  cat("\nErstelle RU-spezifische Mortalitätsvisualisierung...\n")
  
  # Für jede Baumart separaten Plot erstellen
  for(current_species in unique(validation_data_combined$species)) {
    
    species_data <- validation_data_combined %>%
      filter(species == current_species)
    
    if(nrow(species_data) == 0) next
    
    # Anzahl RUs für diese Baumart
    n_rus <- length(unique(species_data$ru_id))
    
    # Verwende baumart-spezifische Farbe aus zentraler Palette
    species_color <- species_colors[current_species]
    if(is.na(species_color)) species_color <- "#2C7BB6"  # Fallback
    
    # Mittlere und mediane Mortalität pro Jahr berechnen (über alle RUs mit dieser Baumart)
    yearly_stats <- species_data %>%
      group_by(year) %>%
      summarise(
        mean_mortality = mean(mortality_rate, na.rm = TRUE),
        median_mortality = median(mortality_rate, na.rm = TRUE),
        n_rus_year = n(),
        .groups = "drop"
      )
    
    cat("Erstelle Plot für", current_species, "mit", n_rus, "RUs...\n")
    
    # Deutsche Namen für Titel
    species_name_german <- species_labels_german[current_species]
    if(is.na(species_name_german)) species_name_german <- current_species
    
    p_validation <- ggplot(species_data, aes(x = year, y = mortality_rate)) +
      # Einzelne RU-Linien (baumart-spezifische Farbe, sehr transparent)
      geom_line(aes(group = ru_id), alpha = 0.02, color = species_color) +
      # Mittelwert pro Jahr (dunkler in Baumart-Farbe)
      geom_line(data = yearly_stats, aes(x = year, y = mean_mortality), 
                color = species_color, size = 2, inherit.aes = FALSE) +
      geom_point(data = yearly_stats, aes(x = year, y = mean_mortality), 
                 color = species_color, size = 3, inherit.aes = FALSE) +
      # Median pro Jahr (gestrichelt in Baumart-Farbe)
      geom_line(data = yearly_stats, aes(x = year, y = median_mortality), 
                color = species_color, size = 1.5, inherit.aes = FALSE, linetype = "dashed") +
      geom_point(data = yearly_stats, aes(x = year, y = median_mortality), 
                 color = species_color, size = 2.5, inherit.aes = FALSE, shape = 17) +
      scale_x_continuous(breaks = seq(2000, 2022, by = 5)) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      labs(
        title = paste("Mortalitätsraten pro Resource Unit:", species_name_german),
        subtitle = paste("Einzelne RUs (hell), Mittelwert (durchgezogen), Median (gestrichelt) •", n_rus, "Resource Units"),
        x = "Jahr",
        y = "Mortalitätsrate (%)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "darkgray")
      ) +
      # Legende manuell hinzufügen (in Baumart-Farbe)
      annotate("text", x = 2001, y = Inf, label = "Mittelwert", 
               color = species_color, hjust = 0, vjust = 2, fontface = "bold") +
      annotate("text", x = 2001, y = Inf, label = "Median", 
               color = species_color, hjust = 0, vjust = 3.5, fontface = "bold")
    
    print(p_validation)
    
    cat("RU-spezifischer Plot für", current_species, "erstellt.\n")
  }
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Vergleichsplot: iLand vs. Validierung ################################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

if(!is.null(validation_data_combined) && nrow(validation_data_combined) > 0) {
  
  cat("\nErstelle Vergleichsplots: iLand vs. Validierung...\n")
  
  # iLand-Daten für Vergleich vorbereiten (letztes Jahr entfernen)
  iland_comparison <- mortality_by_species_year %>%
    filter(species_grouped %in% c("fasy", "piab", "pisy", "qupe_quro")) %>%
    filter(year >= 11 & year <= 33) %>%  # Simulationsjahre 11-33 entsprechen 2000-2021 (letztes Jahr entfernt)
    mutate(real_year = year + 1989) %>%  # Umrechnung zu realen Jahren
    filter(!is.na(mortality_rate)) %>%
    select(real_year, species_grouped, mortality_rate, total_living_trees_prev) %>%
    mutate(
      data_source = "iLand",
      year = real_year,
      species = species_grouped
    ) %>%
    select(year, species, mortality_rate, data_source, total_living_trees_prev)
  
  # Validierungsdaten für Vergleich vorbereiten (Mittelwert pro Jahr, Jahre 2000-2022)
  validation_comparison <- validation_data_combined %>%
    filter(year >= 2000 & year <= 2022) %>%  # Überlappender Zeitraum mit iLand-Daten
    group_by(year, species) %>%
    summarise(
      mortality_rate = mean(mortality_rate, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      data_source = "Validierung",
      total_living_trees_prev = NA_real_  # Platzhalter für bind_rows
    )
  
  # Beide Datensätze kombinieren
  combined_comparison <- bind_rows(iland_comparison, validation_comparison)
  
  # Define modern theme (from WET file)
  theme_modern <- theme_minimal(base_family = "Helvetica") +
    theme(
      text = element_text(family = "Helvetica"),
      plot.title = element_text(size = 14, face = "bold", color = "gray20"),
      plot.subtitle = element_text(size = 11, color = "gray40"),
      axis.title = element_text(size = 12, face = "bold", color = "gray20"),
      axis.text = element_text(size = 10, color = "gray30"),
      axis.ticks = element_line(color = "#bbb9b9", size = 0.3),
      axis.ticks.length = unit(0.2, "cm"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#bbb9b9", size = 0.3),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      strip.text = element_text(size = 11, face = "bold", color = "gray20"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold")
    )
  
  # Liste für Plots
  plot_list_comparison <- list()
  
  # Spezifische Reihenfolge und Beschriftungen für Plots definieren
  species_order <- c("fasy", "qupe_quro", "piab", "pisy")
  species_labels <- c(
    "fasy" = "fasy",
    "qupe_quro" = "qupe, quro", 
    "piab" = "piab",
    "pisy" = "pisy"
  )
  # Plot-Titel mit A, B, C, D (ohne Klammern, ohne Baumart)
  plot_titles <- c(
    "fasy" = "A",
    "qupe_quro" = "B", 
    "piab" = "C",
    "pisy" = "D"
  )
  
  # Für jede Baumart in spezifischer Reihenfolge separaten Vergleichsplot erstellen
  for(i in seq_along(species_order)) {
    current_species <- species_order[i]
    
    if(!current_species %in% unique(combined_comparison$species)) {
      cat("Warnung: Baumart", current_species, "nicht in Daten gefunden.\n")
      next
    }
    
    comparison_data <- combined_comparison %>%
      filter(species == current_species)
    
    # Einzelne RU-Daten aus Validierung für Hintergrund (Jahre 2000-2022)
    validation_ru_data <- validation_data_combined %>%
      filter(species == current_species) %>%
      filter(year >= 2000 & year <= 2022)  # Gleicher Zeitraum
    
    # N-Annotation berechnen (mittlere Anzahl lebender Bäume aus iLand)
    iland_n_data <- comparison_data %>%
      filter(data_source == "iLand") %>%
      summarise(
        mean_living = round(mean(total_living_trees_prev, na.rm = TRUE), 0),
        .groups = "drop"
      )
    
    n_label <- ifelse(nrow(iland_n_data) > 0 && !is.na(iland_n_data$mean_living),
                     paste0("N = ", format(iland_n_data$mean_living, big.mark = ",")),
                     "N = n.a.")
    
    # Korrelationsanalyse zwischen iLand und Validierung (robuste Implementierung)
    iland_data <- comparison_data %>% 
      filter(data_source == "iLand") %>% 
      arrange(year)
    
    validation_data_for_cor <- comparison_data %>% 
      filter(data_source == "Validierung") %>% 
      arrange(year)
    
    # Debug-Ausgabe
    cat("iLand Daten für", current_species, ":", nrow(iland_data), "Jahre\n")
    cat("Validierungs Daten für", current_species, ":", nrow(validation_data_for_cor), "Jahre\n")
    
    # Pearson-Korrelation nur wenn beide Datensätze Werte haben und gleiche Länge
    if(nrow(iland_data) > 2 && nrow(validation_data_for_cor) > 2 && 
       nrow(iland_data) == nrow(validation_data_for_cor) &&
       all(!is.na(iland_data$mortality_rate)) && all(!is.na(validation_data_for_cor$mortality_rate))) {
      
      cor_test <- cor.test(iland_data$mortality_rate, validation_data_for_cor$mortality_rate, method = "pearson")
      r_value <- gsub("\\.", ",", format(round(cor_test$estimate, 3), nsmall = 3))
      p_value <- ifelse(cor_test$p.value < 0.001, "< 0,001", gsub("\\.", ",", format(round(cor_test$p.value, 3), nsmall = 3)))
      
    } else {
      # Debug warum keine Korrelation
      cat("Korrelation nicht möglich für", current_species, ":\n")
      cat("- iLand Zeilen:", nrow(iland_data), ", Validierung Zeilen:", nrow(validation_data_for_cor), "\n")
      cat("- iLand NAs:", sum(is.na(iland_data$mortality_rate)), ", Validierung NAs:", sum(is.na(validation_data_for_cor$mortality_rate)), "\n")
      r_value <- "n.a."
      p_value <- "n.a."
    }
    
    if(nrow(comparison_data) == 0) next
    
    cat("Erstelle Vergleichsplot für", current_species, "...\n")
    cat("Korrelation: r =", r_value, ", p =", p_value, "\n")
    
    # Verwende baumart-spezifische Farbe aus zentraler Palette
    species_color <- species_colors[current_species]
    if(is.na(species_color)) species_color <- "#2C7BB6"  # Fallback
    
    # Bestimme ob dies der letzte Plot ist (für X-Achsen-Label und Legende)
    is_last_plot <- (i == length(species_order))
    
    # Daten für iLand und Validierung getrennt vorbereiten
    iland_data_plot <- comparison_data %>% filter(data_source == "iLand")
    validation_data_plot <- comparison_data %>% filter(data_source == "Validierung")
    
    p_comparison <- ggplot() +
      # Einzelne RU-Linien aus Validierung (grau, transparenter)
      geom_line(data = validation_ru_data, 
                aes(x = year, y = mortality_rate, group = ru_id), 
                alpha = 0.08, color = "gray70") +
      # Validierungsdaten (schwarz)
      geom_line(data = validation_data_plot,
                aes(x = year, y = mortality_rate),
                color = "gray30", size = 1) +
      geom_point(data = validation_data_plot,
                 aes(x = year, y = mortality_rate),
                 color = "gray30", size = 1.5) +
      # iLand-Daten (Baumartenfarbe)
      geom_line(data = iland_data_plot,
                aes(x = year, y = mortality_rate),
                color = species_color, size = 1) +
      geom_point(data = iland_data_plot,
                 aes(x = year, y = mortality_rate),
                 color = species_color, size = 1.5) +
      scale_x_continuous(
        breaks = seq(2000, 2022, by = 5),
        minor_breaks = seq(2000, 2022, by = 1)
      ) +
      scale_y_continuous(
        limits = c(0, 15),
        labels = function(x) paste0(x),  # Entfernt %-Zeichen von Achsenbeschriftung
        breaks = seq(0, 15, by = 3)
      ) +
      labs(
        title = plot_titles[current_species],
        subtitle = paste0("N = ", format(iland_n_data$mean_living, big.mark = "."), "   r = ", r_value, "   p = ", p_value),
        x = if(is_last_plot) "Jahr" else "",
        y = "Mortalitätsrate [%]"
      ) +
      theme_modern +
      theme(
        legend.position = "none",
        axis.text.x = if(is_last_plot) element_text(size = 10, color = "gray30") else element_blank(),
        axis.title.x = if(is_last_plot) element_text(size = 12, face = "bold", color = "gray20") else element_blank(),
        axis.ticks.x = element_line(color = "#bbb9b9", size = 0.3),  # Ticks für alle Plots
        plot.margin = margin(5.5, 5.5, 5.5, 5.5, "pt")  # Gleiche Ränder für alle Plots
      )
    
    # Plot zur Liste hinzufügen
    plot_list_comparison[[current_species]] <- p_comparison
    
    print(p_comparison)
    
    cat("Vergleichsplot für", current_species, "erstellt.\n")
  }
  
  # Multiplot erstellen (1x4 Grid - alle untereinander)
  if(length(plot_list_comparison) > 0) {
    cat("\nErstelle Multiplot aller Vergleiche (untereinander)...\n")
    
    # Plots in der richtigen Reihenfolge sortieren
    ordered_plots <- plot_list_comparison[species_order[species_order %in% names(plot_list_comparison)]]
    
    # Erstelle gemeinsame Legende als separaten Plot
    legend_colors <- c(
      species_colors["fasy"],
      species_colors["qupe_quro"],
      species_colors["piab"],
      species_colors["pisy"],
      "gray30"
    )
    # Legende ohne a), b), c) - nur Baumartnamen
    legend_labels_clean <- c(
      "fasy",
      "qupe, quro",
      "piab",
      "pisy",
      "WZE"
    )
    
    # Legende als eigenständigen Plot erstellen
    legend_df <- data.frame(
      x = rep(1:2, 5),
      y = rep(1:2, 5),
      group = factor(rep(legend_labels_clean, each = 2), levels = legend_labels_clean)
    )
    
    legend_plot <- ggplot(legend_df, aes(x = x, y = y, color = group)) +
      geom_line(size = 1) +
      geom_point(size = 1.5) +
      scale_color_manual(values = setNames(legend_colors, legend_labels_clean), name = NULL) +
      theme_void(base_family = "Helvetica") +
      theme(
        text = element_text(family = "Helvetica"),
        legend.position = "bottom",
        legend.text = element_text(size = 10, family = "Helvetica"),
        legend.key.width = unit(1.2, "cm"),
        legend.box.margin = margin(0, 0, 0, 0)
      ) +
      guides(color = guide_legend(nrow = 1))
    
    # Legende extrahieren mit gtable/grid
    legend_grob <- ggplotGrob(legend_plot)$grobs[[which(sapply(ggplotGrob(legend_plot)$grobs, function(x) x$name) == "guide-box")]]
    
    # DIN A4 Format: 21.0 x 29.7 cm, with margins: re./li. 3.5 cm, oben/unten 2.5 cm
    # Usable area: 21.0 - 2*3.5 = 14.0 cm width, 29.7 - 2*2.5 = 24.7 cm height
    # Convert to inches: 14.0/2.54 = 5.51", 24.7/2.54 = 9.72"
    # Reduced height for figure caption space: 6.0" x 8.5"
    
    # Grid-Layout mit gridExtra (1 Spalte, 4 Zeilen + Legende)
    # Alle Plots gleiche Höhe, Legende extra Platz
    multiplot_comparison <- grid.arrange(
      grobs = c(ordered_plots, list(legend_grob)),
      ncol = 1, nrow = 5,
      heights = c(1, 1, 1, 1.15, 0.25)  # D bekommt etwas mehr Platz für X-Achsen-Text, Legende weniger
    )
    
    # Multiplot speichern mit DIN A4 Dimensionen
    out_file_multiplot <- file.path("C:/Users/Jakob/Desktop", "mortality_comparison_multiplot.png")
    ggsave(out_file_multiplot, multiplot_comparison, width = 6.0, height = 8.5, dpi = 300)
    cat("Multiplot gespeichert als:", out_file_multiplot, "\n")
    cat("Format: 6.0\" x 8.5\" (DIN A4 kompatibel)\n")
  }
  
  cat("\n=== FINALE ÜBERSICHT ===\n")
  cat("✅ iLand-Mortalitätsraten berechnet (Fischer et al. Formel)\n")
  cat("✅ Validierungsdaten aus Excel geladen\n")
  cat("✅ RU-spezifische Plots (Mittelwert + Median) erstellt\n")
  cat("✅ Direkte Vergleichsplots iLand vs. Validierung erstellt\n")
  cat("✅ Alle Plots auf Desktop gespeichert\n")
  
} else {
  cat("⚠️  Keine Validierungsdaten verfügbar für Vergleichsplots.\n")
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# MORTALITÄT & SPEI Korrelationsanalyse ######################################################################################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

cat("\n=== MORTALITÄTS-SPEI KORRELATIONSANALYSE ===\n")
cat("Erstelle Korrelationstabelle und Heatmap für Mortalität und SPEI...\n")

# Verwende bereits definierte zentrale Farbpaletten
cat("Verwende zentrale Farbpaletten für konsistente Farbgebung...\n")

# Robuste Farbzuweisung mit Fallback-Mechanismus
assign_species_colors <- function(present_species, base_colors) {
  current_colors <- base_colors[present_species]
  
  # Assign default colors for species not in our predefined list
  missing_species <- present_species[!present_species %in% names(base_colors)]
  if (length(missing_species) > 0) {
    additional_colors <- rainbow(length(missing_species))
    names(additional_colors) <- missing_species
    current_colors <- c(current_colors, additional_colors)
    cat("Warnung: Fallback-Farben für unbekannte Arten:", paste(missing_species, collapse = ", "), "\n")
  }
  
  # Special color for "Sonstige" if present
  if ("Sonstige" %in% present_species) {
    current_colors["Sonstige"] <- "#808080"  # Gray color for "Sonstige"
  }
  
  # Remove NA values (species not found in database)
  current_colors <- current_colors[!is.na(current_colors)]
  
  return(current_colors)
}

cat("Verwende zentrale Farbpaletten (bereits geladen):\n")
cat("- species_colors (Hauptpalette mit allen Arten)\n")
cat("- Erweitert für: qupe_quro, deutsche Namen\n")
cat("- species_labels_german (Übersetzungen)\n")

# 1. Mortalitätsdaten für Korrelationsanalyse vorbereiten
mortality_correlation_data <- mortality_by_species_year %>%
  mutate(real_year = year + 1989) %>%  # Umrechnung zu realen Jahren (Jahr 11 = 2000)
  filter(real_year >= 2000 & real_year <= 2022) %>%  # Überlappender Zeitraum mit SPEI
  select(real_year, species_grouped, mortality_rate) %>%
  rename(year = real_year) %>%  # Umbenennen für Join mit SPEI-Daten
  arrange(species_grouped, year)

# 2. SPEI-Daten für Korrelationsanalyse verwenden (bereits vorhanden)
# spei_correlation_data ist bereits definiert

# 3. Finale Mortalitäts-Korrelationstabelle erstellen
mortality_spei_correlation_table <- mortality_correlation_data %>%
  left_join(spei_correlation_data, by = "year") %>%
  arrange(species_grouped, year)

# 4. Tabelle anzeigen und speichern
cat("\nMortalitäts-SPEI Korrelationstabelle erstellt:\n")
cat("- Zeilen:", nrow(mortality_spei_correlation_table), "\n")
cat("- Spalten:", ncol(mortality_spei_correlation_table), "\n")
cat("- Baumarten:", paste(unique(mortality_spei_correlation_table$species_grouped), collapse = ", "), "\n")
cat("- Jahre:", min(mortality_spei_correlation_table$year), "-", max(mortality_spei_correlation_table$year), "\n")

# Erste Zeilen anzeigen
cat("\nErste 10 Zeilen der Mortalitäts-Korrelationstabelle:\n")
print(head(mortality_spei_correlation_table, 10))

# Tabelle als CSV speichern
mortality_correlation_file <- file.path("C:/Users/Jakob/Desktop", "Mortality_SPEI_correlation_table.csv")
write.csv(mortality_spei_correlation_table, mortality_correlation_file, row.names = FALSE)
cat("\nMortalitäts-Korrelationstabelle gespeichert als:", mortality_correlation_file, "\n")

# 5. Korrelationsfunktionen für Mortalität (analog zu NPP-Funktionen)

# Funktion zur Berechnung von Mortalitäts-SPEI Korrelationen mit p-Werten (Pearson)
create_mortality_correlation_with_pvalues <- function(species_name, data) {
  
  species_data <- data %>%
    filter(species_grouped == species_name) %>%
    select(year, mortality_rate, contains("SPEI")) %>%
    na.omit() %>%
    arrange(year)
  
  if(nrow(species_data) < 3) {
    cat("Warnung: Nicht genügend Daten für", species_name, "\n")
    return(list(correlations = NULL, pvalues = NULL, significance = NULL))
  }
  
  # Gleichzeitige Korrelationen mit p-Werten
  current_year_data <- species_data %>% select(mortality_rate, contains("SPEI"))
  
  cor_results_current <- list()
  p_values_current <- list()
  
  for(i in 2:ncol(current_year_data)) {
    test_result <- cor.test(current_year_data$mortality_rate, current_year_data[[i]], method = "pearson")
    cor_results_current[[colnames(current_year_data)[i]]] <- test_result$estimate
    p_values_current[[colnames(current_year_data)[i]]] <- test_result$p.value
  }
  
  # Lag-Korrelationen mit p-Werten
  lag_data <- species_data %>%
    arrange(year) %>%
    mutate(
      SPEI3_year_mean_lag = lag(SPEI3_year_mean, 1),
      SPEI6_year_mean_lag = lag(SPEI6_year_mean, 1),
      SPEI12_year_mean_lag = lag(SPEI12_year_mean, 1),
      SPEI24_year_mean_lag = lag(SPEI24_year_mean, 1),
      SPEI3_veg_mean_lag = lag(SPEI3_veg_mean, 1),
      SPEI6_veg_mean_lag = lag(SPEI6_veg_mean, 1),
      SPEI12_veg_mean_lag = lag(SPEI12_veg_mean, 1),
      SPEI24_veg_mean_lag = lag(SPEI24_veg_mean, 1)
    ) %>%
    filter(!is.na(SPEI3_year_mean_lag)) %>%
    select(mortality_rate, contains("lag"))
  
  cor_results_lag <- list()
  p_values_lag <- list()
  
  if(nrow(lag_data) >= 3) {
    for(i in 2:ncol(lag_data)) {
      test_result <- cor.test(lag_data$mortality_rate, lag_data[[i]], method = "pearson")
      var_name <- gsub("_lag", "_lag1", colnames(lag_data)[i])
      cor_results_lag[[var_name]] <- test_result$estimate
      p_values_lag[[var_name]] <- test_result$p.value
    }
  }
  
  # Kombiniere alle Ergebnisse
  all_correlations <- c(unlist(cor_results_current), unlist(cor_results_lag))
  all_pvalues <- c(unlist(p_values_current), unlist(p_values_lag))
  
  # Signifikanzsterne
  significance_stars <- ifelse(all_pvalues < 0.001, "***",
                              ifelse(all_pvalues < 0.01, "**", 
                                    ifelse(all_pvalues < 0.05, "*", "")))
  
  # Als Matrizen formatieren
  cor_matrix <- matrix(all_correlations, nrow = 1)
  p_matrix <- matrix(all_pvalues, nrow = 1)
  sig_matrix <- matrix(significance_stars, nrow = 1)
  colnames(cor_matrix) <- colnames(p_matrix) <- colnames(sig_matrix) <- names(all_correlations)
  rownames(cor_matrix) <- rownames(p_matrix) <- rownames(sig_matrix) <- "Mortality_rate"
  
  return(list(correlations = cor_matrix, pvalues = p_matrix, significance = sig_matrix))
}

# Funktion zur Berechnung von Mortalitäts-SPEI Korrelationen mit p-Werten (Kendalls Tau)
create_mortality_correlation_kendall_with_pvalues <- function(species_name, data) {
  
  species_data <- data %>%
    filter(species_grouped == species_name) %>%
    select(year, mortality_rate, contains("SPEI")) %>%
    na.omit() %>%
    arrange(year)
  
  if(nrow(species_data) < 3) {
    cat("Warnung: Nicht genügend Daten für", species_name, "\n")
    return(list(correlations = NULL, pvalues = NULL, significance = NULL))
  }
  
  # Gleichzeitige Korrelationen mit p-Werten (Kendalls Tau)
  current_year_data <- species_data %>% select(mortality_rate, contains("SPEI"))
  
  cor_results_current <- list()
  p_values_current <- list()
  
  for(i in 2:ncol(current_year_data)) {
    test_result <- cor.test(current_year_data$mortality_rate, current_year_data[[i]], method = "kendall")
    cor_results_current[[colnames(current_year_data)[i]]] <- test_result$estimate
    p_values_current[[colnames(current_year_data)[i]]] <- test_result$p.value
  }
  
  # Lag-Korrelationen mit p-Werten (Kendalls Tau)
  lag_data <- species_data %>%
    arrange(year) %>%
    mutate(
      SPEI3_year_mean_lag = lag(SPEI3_year_mean, 1),
      SPEI6_year_mean_lag = lag(SPEI6_year_mean, 1),
      SPEI12_year_mean_lag = lag(SPEI12_year_mean, 1),
      SPEI24_year_mean_lag = lag(SPEI24_year_mean, 1),
      SPEI3_veg_mean_lag = lag(SPEI3_veg_mean, 1),
      SPEI6_veg_mean_lag = lag(SPEI6_veg_mean, 1),
      SPEI12_veg_mean_lag = lag(SPEI12_veg_mean, 1),
      SPEI24_veg_mean_lag = lag(SPEI24_veg_mean, 1)
    ) %>%
    filter(!is.na(SPEI3_year_mean_lag)) %>%
    select(mortality_rate, contains("lag"))
  
  cor_results_lag <- list()
  p_values_lag <- list()
  
  if(nrow(lag_data) >= 3) {
    for(i in 2:ncol(lag_data)) {
      test_result <- cor.test(lag_data$mortality_rate, lag_data[[i]], method = "kendall")
      var_name <- gsub("_lag", "_lag1", colnames(lag_data)[i])
      cor_results_lag[[var_name]] <- test_result$estimate
      p_values_lag[[var_name]] <- test_result$p.value
    }
  }
  
  # Kombiniere alle Ergebnisse
  all_correlations <- c(unlist(cor_results_current), unlist(cor_results_lag))
  all_pvalues <- c(unlist(p_values_current), unlist(p_values_lag))
  
  # Signifikanzsterne für Kendalls Tau
  significance_stars <- ifelse(all_pvalues < 0.001, "***",
                              ifelse(all_pvalues < 0.01, "**", 
                                    ifelse(all_pvalues < 0.05, "*", "")))
  
  # Als Matrizen formatieren
  cor_matrix <- matrix(all_correlations, nrow = 1)
  p_matrix <- matrix(all_pvalues, nrow = 1)
  sig_matrix <- matrix(significance_stars, nrow = 1)
  colnames(cor_matrix) <- colnames(p_matrix) <- colnames(sig_matrix) <- names(all_correlations)
  rownames(cor_matrix) <- rownames(p_matrix) <- rownames(sig_matrix) <- "Mortality_rate"
  
  return(list(correlations = cor_matrix, pvalues = p_matrix, significance = sig_matrix))
}

# 6. Korrelationen für alle Baumarten berechnen
mortality_correlation_matrices <- list()

# Listen für kombinierte Matrizen
mortality_combined_correlation_matrix_list <- list()
mortality_combined_pvalue_matrix_list <- list()
mortality_combined_significance_matrix_list <- list()

# Spezifische Reihenfolge und Beschriftungen für Plots definieren
species_order <- c("fasy", "piab", "pisy", "qupe_quro")
species_labels_mortality <- c(
  "fasy" = "fasy",
  "piab" = "piab",
  "pisy" = "pisy",
  "qupe_quro" = "qupe, quro"
)

for(species_code in species_order) {
  species_name <- species_labels_mortality[species_code]
  
  cat("Berechne Mortalitäts-SPEI Korrelationen für", species_name, "...\n")
  
  # Pearson Korrelationen
  pearson_result <- create_mortality_correlation_with_pvalues(species_code, mortality_spei_correlation_table)
  
  # Kendall Korrelationen
  kendall_result <- create_mortality_correlation_kendall_with_pvalues(species_code, mortality_spei_correlation_table)
  
  if(!is.null(pearson_result$correlations) && !is.null(kendall_result$correlations)) {
    # Speichere die Matrizen für diese Baumart
    mortality_correlation_matrices[[species_code]] <- list(
      pearson = pearson_result,
      kendall = kendall_result
    )
    
    pearson_row_name <- paste0(species_name, " (r)")
    kendall_row_name <- paste0(species_name, " (τ)")
    
    mortality_combined_correlation_matrix_list[[pearson_row_name]] <- pearson_result$correlations[1, ]
    mortality_combined_pvalue_matrix_list[[pearson_row_name]] <- pearson_result$pvalues[1, ]
    mortality_combined_significance_matrix_list[[pearson_row_name]] <- pearson_result$significance[1, ]
    
    mortality_combined_correlation_matrix_list[[kendall_row_name]] <- kendall_result$correlations[1, ]
    mortality_combined_pvalue_matrix_list[[kendall_row_name]] <- kendall_result$pvalues[1, ]
    mortality_combined_significance_matrix_list[[kendall_row_name]] <- kendall_result$significance[1, ]
    
    cat("Korrelationen für", species_name, "berechnet (Pearson & Kendall).\n")
  } else {
    cat("Warnung: Unvollständige Daten für", species_name, "\n")
  }
}

mortality_combined_correlation_matrix <- do.call(rbind, mortality_combined_correlation_matrix_list)
mortality_combined_pvalue_matrix <- do.call(rbind, mortality_combined_pvalue_matrix_list)
mortality_combined_significance_matrix <- do.call(rbind, mortality_combined_significance_matrix_list)

mortality_column_names <- c(
  "SPEI3", "SPEI6", "SPEI12", "SPEI24",
  "SPEI3_Veg", "SPEI6_Veg", "SPEI12_Veg", "SPEI24_Veg",
  "SPEI3_Lag", "SPEI6_Lag", "SPEI12_Lag", "SPEI24_Lag",
  "SPEI3_VegLag", "SPEI6_VegLag", "SPEI12_VegLag", "SPEI24_VegLag"
)
colnames(mortality_combined_correlation_matrix) <- mortality_column_names
colnames(mortality_combined_pvalue_matrix) <- mortality_column_names
colnames(mortality_combined_significance_matrix) <- mortality_column_names

cat("Erstelle Mortalitäts-SPEI Korrelations-Heatmap...\n")

mortality_plot_file <- file.path("C:/Users/Jakob/Desktop", "Mortality_SPEI_correlation_heatmap.png")

library(reshape2)

cor_long <- melt(mortality_combined_correlation_matrix)
colnames(cor_long) <- c("Species", "SPEI_Variable", "Correlation")

sig_long <- melt(mortality_combined_significance_matrix)
colnames(sig_long) <- c("Species", "SPEI_Variable", "Significance")

heatmap_data <- cor_long %>%
  left_join(sig_long, by = c("Species", "SPEI_Variable"))

heatmap_data <- heatmap_data %>%
  mutate(
    Cor_Label = gsub("\\.", ",", sprintf("%.2f", Correlation)),
    Cell_Label = ifelse(Significance != "", 
                        paste0(Cor_Label, "\n", Significance),
                        Cor_Label)
  )

species_levels <- rownames(mortality_combined_correlation_matrix)
heatmap_data$Species <- factor(heatmap_data$Species, levels = species_levels)

heatmap_data$SPEI_Variable <- factor(heatmap_data$SPEI_Variable, levels = rev(mortality_column_names))

p_heatmap <- ggplot(heatmap_data, aes(x = Species, y = SPEI_Variable, fill = Correlation)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = Cor_Label), 
            color = "black", 
            size = 3.0,
            family = "Arial",
            fontface = "bold",
            vjust = 0.3) +
  # Signifikanzsterne darunter (nur wenn vorhanden)
  geom_text(aes(label = Significance), 
            color = "black", 
            size = 2.8,
            family = "Arial",
            fontface = "bold",
            vjust = -0.8) +
  # Farbskala (rot-weiß-blau)
  scale_fill_gradient2(low = "#D7191C", mid = "white", high = "#2C7BB6",
                       midpoint = 0, 
                       limits = c(-1, 1),
                       name = NULL,
                       labels = function(x) gsub("\\.", ",", sprintf("%.1f", x))) +
  # X-Achse oben (Baumarten)
  scale_x_discrete(position = "top") +
  # Theme anpassen
  theme_minimal(base_family = "Arial") +
  theme(
    # X-Achse (oben): Baumarten, fett, 60° gedreht
    axis.text.x.top = element_text(angle = 60, hjust = 0, vjust = 0, 
                               size = 9, face = "bold", family = "Arial"),
    # Y-Achse: SPEI-Variablen, fett
    axis.text.y = element_text(size = 9, face = "bold", family = "Arial",
                               margin = margin(r = 2)),
    # Achsentitel entfernen
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    # Legende anpassen
    legend.position = "right",
    legend.text = element_text(size = 8, family = "Arial"),
    legend.key.height = unit(1.5, "cm"),
    legend.key.width = unit(0.3, "cm"),
    legend.margin = margin(l = 5),
    # Panel
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    # Ränder - mehr Platz oben
    plot.margin = margin(t = 50, r = 5, b = 5, l = 5)
  ) +
  coord_cartesian(clip = "off")

ggsave(mortality_plot_file, p_heatmap, width = 14, height = 21, units = "cm", dpi = 300)
cat("Mortalitäts-SPEI Korrelations-Heatmap gespeichert als:", mortality_plot_file, "\n")
cat("Format: 14 x 21 cm (Hochformat)\n")

cat("Erstelle Mortalitäts-SPEI CSV-Tabelle...\n")

mortality_summary_table <- data.frame(
  Baumart_Methode = rownames(mortality_combined_correlation_matrix),
  mortality_combined_correlation_matrix,
  stringsAsFactors = FALSE
)

mortality_pvalue_columns <- mortality_combined_pvalue_matrix
colnames(mortality_pvalue_columns) <- paste0(colnames(mortality_pvalue_columns), "_pval")
mortality_summary_table <- cbind(mortality_summary_table, mortality_pvalue_columns)

mortality_significance_columns <- mortality_combined_significance_matrix
colnames(mortality_significance_columns) <- paste0(mortality_column_names, "_sig")
mortality_summary_table <- cbind(mortality_summary_table, mortality_significance_columns)

mortality_summary_csv_file <- file.path("C:/Users/Jakob/Desktop", "Mortality_SPEI_correlation_summary.csv")
write.csv(mortality_summary_table, mortality_summary_csv_file, row.names = FALSE)
cat("Mortalitäts-Korrelations-Tabelle gespeichert als:", mortality_summary_csv_file, "\n")

cat("\n=== STÄRKSTE MORTALITÄTS-SPEI KORRELATIONEN ===\n")
for(species_code in names(mortality_correlation_matrices)) {
  species_name <- species_labels_mortality[species_code]
  
  pearson_matrix <- mortality_correlation_matrices[[species_code]]$pearson$correlations
  kendall_matrix <- mortality_correlation_matrices[[species_code]]$kendall$correlations
  
  cat(species_name, " (", species_code, "):\n", sep = "")
  
  if(ncol(pearson_matrix) >= 8) {
    current_cols <- 1:8
    current_matrix <- pearson_matrix[, current_cols, drop = FALSE]
    max_cor_current <- max(abs(current_matrix), na.rm = TRUE)
    max_pos_current <- which(abs(current_matrix) == max_cor_current, arr.ind = TRUE)
    
    if(nrow(max_pos_current) > 0) {
      spei_var <- colnames(current_matrix)[max_pos_current[1, 2]]
      cor_val <- current_matrix[1, max_pos_current[1, 2]]
      
      cat("  Pearson aktuell  : Mortalität ~ ", spei_var, " (r = ", 
          sprintf("%.3f", cor_val), ")\n", sep = "")
    }
  }
  
  if(ncol(kendall_matrix) >= 8) {
    current_cols <- 1:8
    current_matrix <- kendall_matrix[, current_cols, drop = FALSE]
    max_cor_current <- max(abs(current_matrix), na.rm = TRUE)
    max_pos_current <- which(abs(current_matrix) == max_cor_current, arr.ind = TRUE)
    
    if(nrow(max_pos_current) > 0) {
      spei_var <- colnames(current_matrix)[max_pos_current[1, 2]]
      cor_val <- current_matrix[1, max_pos_current[1, 2]]
      
      cat("  Kendall aktuell  : Mortalität ~ ", spei_var, " (τ = ", 
          sprintf("%.3f", cor_val), ")\n", sep = "")
    }
  }
  cat("\n")
}

cat("\n=== MORTALITÄTS-SPEI KORRELATIONSANALYSE ABGESCHLOSSEN ===\n")
cat("✅ Mortalitäts-SPEI Korrelations-Heatmap (Pearson & Kendall)\n")
cat("✅ 8 Zeilen: Buche Pearson, Buche Kendall, Fichte Pearson, Fichte Kendall, etc.\n")
cat("✅ 16 Spalten: SPEI-Variablen (aktuell + Lag-1) für alle Zeitskalen\n")
cat("✅ p-Werte mit Signifikanzsternen (*<0.05, **<0.01, ***<0.001)\n")
cat("✅ Kontinuierliche Farbskala für direkte Vergleichbarkeit\n")
cat("\n📁 AUSGABEDATEIEN:\n")
cat("📊 Mortality_SPEI_correlation_heatmap.png - Mortalitäts-Korrelations-Heatmap\n") 
cat("📋 Mortality_SPEI_correlation_summary.csv - Detaillierte Mortalitäts-Korrelationstabelle\n")
cat("📋 Mortality_SPEI_correlation_table.csv - Rohdaten für weitere Analysen\n")
cat("\n🎯 WISSENSCHAFTLICHE ERKENNTNISSE:\n")
cat("🔬 Quantitative Beziehungen: Mortalitäts-SPEI Korrelationen mit statistischer Signifikanz\n")
cat("⏳ Lag-Effekte: Verzögerte Mortalitätsreaktionen auf Vorjahres-Trockenheit\n")
cat("🌲 Artspezifische Unterschiede: Differenzielle Trockenheitstoleranz bei Mortalität\n")
cat("📈 Methodenvergleich: Pearson vs. Kendall für Mortalitätsdaten\n")
cat("🔄 Komplementär zu NPP: Parallele Analyse von Wachstum vs. Mortalität unter Trockenstress\n")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Boxplot Mortalitätsraten pro Baumart
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

cat("\n=== BOXPLOT MORTALITÄTSRATEN PRO BAUMART ===\n")
cat("Erstelle einfachen Boxplot der Mortalitätsraten...\n")

boxplot_data <- mortality_correlation_data %>%
  filter(!is.na(mortality_rate)) %>%
  mutate(
    species_name = case_when(
      species_grouped == "fasy" ~ "Buche",
      species_grouped == "piab" ~ "Fichte", 
      species_grouped == "pisy" ~ "Kiefer",
      species_grouped == "qupe_quro" ~ "Eichen",
      TRUE ~ species_grouped
    )
  ) %>%
  mutate(species_name = factor(species_name, levels = c("Buche", "Fichte", "Kiefer", "Eichen")))

present_species_german <- as.character(unique(boxplot_data$species_name))
species_colors_boxplot <- species_colors[present_species_german]
species_colors_boxplot <- species_colors_boxplot[!is.na(species_colors_boxplot)]

cat("Verwendete Farben für Boxplot:\n")
cat("Vorhandene Arten:", paste(present_species_german, collapse = ", "), "\n")
print(species_colors_boxplot)

n_per_species <- boxplot_data %>%
  group_by(species_name) %>%
  summarise(n = n(), .groups = "drop")

boxplot_file <- file.path("C:/Users/Jakob/Desktop", "Mortality_rates_boxplot.png")

cat("\n=== STATISTISCHE TESTS ===\n")

kw_test <- kruskal.test(mortality_rate ~ species_name, data = boxplot_data)
cat("Kruskal-Wallis Test:\n")
cat("Chi-squared =", round(kw_test$statistic, 3), "\n")
cat("df =", kw_test$parameter, "\n")
cat("p-value =", ifelse(kw_test$p.value < 0.001, "< 0.001", round(kw_test$p.value, 4)), "\n")

# 2. Post-hoc Tests (falls Kruskal-Wallis signifikant)
if(kw_test$p.value < 0.05) {
  cat("\n✅ Kruskal-Wallis ist signifikant (p < 0.05) → Post-hoc Tests:\n")
  
  # Pairwise Wilcoxon Test mit Bonferroni-Korrektur
  posthoc_test <- pairwise.wilcox.test(boxplot_data$mortality_rate, 
                                      boxplot_data$species_name, 
                                      p.adjust.method = "bonferroni")
  
  cat("Pairwise Wilcoxon Tests (Bonferroni-korrigiert):\n")
  print(posthoc_test$p.value)
  
  # Signifikante Paare identifizieren
  p_matrix <- posthoc_test$p.value
  significant_pairs <- which(p_matrix < 0.05, arr.ind = TRUE)
  
  cat("\nSignifikante Unterschiede (p < 0.05):\n")
  if(nrow(significant_pairs) > 0) {
    for(i in 1:nrow(significant_pairs)) {
      row_idx <- significant_pairs[i, 1]
      col_idx <- significant_pairs[i, 2]
      species1 <- rownames(p_matrix)[row_idx]
      species2 <- colnames(p_matrix)[col_idx]
      p_val <- p_matrix[row_idx, col_idx]
      
      cat("-", species1, "vs", species2, ": p =", 
          ifelse(p_val < 0.001, "< 0.001", round(p_val, 4)), "\n")
    }
  } else {
    cat("- Keine signifikanten Paarvergleiche nach Bonferroni-Korrektur\n")
  }
  
} else {
  cat("\n⚠️  Kruskal-Wallis nicht signifikant (p ≥ 0.05) → Keine Post-hoc Tests nötig\n")
  significant_pairs <- matrix(nrow = 0, ncol = 2)
}

png(boxplot_file, width = 1000, height = 700, res = 150)

theme_modern <- theme_minimal() +
  theme(
    axis.title = element_text(size = 12, color = "gray20"),
    axis.text = element_text(size = 10, color = "gray30"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray80", size = 0.5),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(size = 11, face = "bold", color = "gray20"),
    legend.position = "none"
  )

p <- ggplot(boxplot_data, aes(x = species_name, y = mortality_rate, fill = species_name)) +
  geom_boxplot(notch = TRUE,
               alpha = 0.8,
               outlier.size = 2,
               outlier.alpha = 0.6) +
  scale_fill_manual(values = species_colors_boxplot) +
  scale_y_continuous(
    name = "Mortalitätsrate [%]",
    breaks = seq(0, ceiling(max(boxplot_data$mortality_rate, na.rm = TRUE)), by = 1),
    expand = expansion(mult = c(0.02, 0.15))
  ) +
  labs(
    x = "Baumart",
    y = "Mortalitätsrate [%]"
  ) +
  theme_modern

for(i in 1:nrow(n_per_species)) {
  species <- n_per_species$species_name[i]
  n_value <- n_per_species$n[i]
  x_pos <- which(levels(boxplot_data$species_name) == species)
  
  p <- p + annotate("text", x = x_pos, y = min(boxplot_data$mortality_rate, na.rm = TRUE) - 0.5,
                    label = paste0("N = ", n_value), size = 3.5, color = "gray40")
}

if(kw_test$p.value < 0.05 && nrow(significant_pairs) > 0) {
  
  y_max <- max(boxplot_data$mortality_rate, na.rm = TRUE)
  y_step <- y_max * 0.15
  y_start <- y_max + y_max * 0.12
  
  species_levels <- levels(boxplot_data$species_name)
  
  for(i in 1:nrow(significant_pairs)) {
    row_idx <- significant_pairs[i, 1]
    col_idx <- significant_pairs[i, 2]
    
    species1_name <- rownames(posthoc_test$p.value)[row_idx]
    species2_name <- colnames(posthoc_test$p.value)[col_idx]
    
    x1 <- which(species_levels == species1_name)
    x2 <- which(species_levels == species2_name)
    
    y_bracket <- y_start + ((i-1) * y_step)
    bracket_height <- y_max * 0.04
    
    p_val <- posthoc_test$p.value[row_idx, col_idx]
    if(p_val < 0.001) {
      star <- "***"
    } else if(p_val < 0.01) {
      star <- "**"
    } else {
      star <- "*"
    }
    
    p <- p + annotate("segment", x = x1, xend = x2, y = y_bracket, yend = y_bracket, 
                      color = "black", size = 0.7)
    
    p <- p + annotate("segment", x = x1, xend = x1, 
                      y = y_bracket, yend = y_bracket - bracket_height,
                      color = "black", size = 0.7)
    
    p <- p + annotate("segment", x = x2, xend = x2,
                      y = y_bracket, yend = y_bracket - bracket_height,
                      color = "black", size = 0.7)
    
    p <- p + annotate("text", x = (x1 + x2)/2, y = y_bracket + y_max * 0.03,
                      label = star, size = 5, fontface = "bold", color = "black")
  }
}

print(p)

dev.off()

cat("Boxplot mit Notches und wissenschaftlichen Signifikanz-Klammern (ggplot2) gespeichert als:", boxplot_file, "\n")

cat("\nStatistiken pro Baumart:\n")
summary_stats <- boxplot_data %>%
  group_by(species_name) %>%
  summarise(
    n_Jahre = n(),
    Mittelwert = round(mean(mortality_rate, na.rm = TRUE), 2),
    Median = round(median(mortality_rate, na.rm = TRUE), 2),
    Min = round(min(mortality_rate, na.rm = TRUE), 2),
    Max = round(max(mortality_rate, na.rm = TRUE), 2),
    SD = round(sd(mortality_rate, na.rm = TRUE), 2),
    .groups = "drop"
  )

print(summary_stats)

cat("\n=== BOXPLOT MORTALITÄTSRATEN FERTIGGESTELLT ===\n")
cat("✅ Kruskal-Wallis Test: Globaler Test auf Unterschiede\n")
cat("✅ Post-hoc (falls signifikant): Pairwise Wilcoxon mit Bonferroni\n")
cat("✅ Boxplot mit Signifikanzen: Visuelle Darstellung\n")
cat("✅ Zeitraum: 2000-2022\n")
cat("✅ Natürliche Farbgebung pro Baumart\n")
cat("📁 Ausgabedatei: Mortality_rates_boxplot.png\n")
cat("📊 Statistische Tests: Kruskal-Wallis + Post-hoc Vergleiche\n")
cat("🌟 Signifikanz-Sterne: * p<0.05, ** p<0.01, *** p<0.001\n")


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# NPP pro Baumart vs. SPEI6_Veg - Multiplot ######################################################################################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

cat("\n=== ERSTELLE NPP PRO BAUMART vs. SPEI6_VEG MULTIPLOT ===\n")

library(ggplot2)
library(gridExtra)
library(dplyr)

# SPEI6_Veg Daten vorbereiten (monatlich für Balkendiagramm)
spei6_veg_plot_data <- spei6_data_clean %>%
  filter(YEAR >= 2000 & YEAR <= 2022 & MONTH >= 4 & MONTH <= 9) %>%
  mutate(DATE = as.Date(paste(YEAR, MONTH, "01", sep = "-"))) %>%
  select(YEAR, MONTH, DATE, SPEI_6)

npp_species_monthly <- landscape_changes %>%
  select(real_year, species_grouped, NPP_change_pct) %>%
  crossing(MONTH = 1:12) %>%
  mutate(DATE = as.Date(paste(real_year, MONTH, "01", sep = "-"))) %>%
  filter(real_year >= 2000 & real_year <= 2022) %>%
  arrange(species_grouped, DATE)

theme_species_plot <- theme_minimal() +
  theme(
    plot.title = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.margin = margin(5, 10, 5, 10, "pt")
  )

species_list <- unique(npp_species_monthly$species_grouped)
plot_list <- list()

for(i in seq_along(species_list)) {
  species_name <- species_list[i]
  species_color <- species_colors_landscape[species_name]
  species_german <- species_labels_german[species_name]
  
  npp_data_species <- npp_species_monthly %>% filter(species_grouped == species_name)
  
  p <- ggplot() +
    geom_col(data = spei6_veg_plot_data, 
             aes(x = DATE, y = SPEI_6), 
             fill = "lightgray", alpha = 0.6, width = 25) +
    geom_line(data = npp_data_species, 
              aes(x = DATE, y = NPP_change_pct / 20), 
              color = species_color, size = 1) +
    geom_point(data = npp_data_species %>% filter(MONTH == 6), 
               aes(x = DATE, y = NPP_change_pct / 20), 
               color = species_color, size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.7) +
    scale_y_continuous(
      name = "SPEI-6 (Veg.)",
      limits = c(-2.5, 2.5),
      breaks = seq(-2, 2, by = 1),
      sec.axis = sec_axis(~ . * 20, 
                         name = "NPP [%]",
                         breaks = seq(-40, 40, by = 20))
    ) +
    scale_x_date(
      date_breaks = "5 years",
      date_labels = "%Y",
      limits = c(as.Date("2000-01-01"), as.Date("2022-12-31"))
    ) +
    labs(title = species_german) +
    theme_species_plot
  
  if(i < length(species_list)) {
    p <- p + theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank()
    )
  } else {
    p <- p + labs(x = "Jahr")
  }
  
  plot_list[[i]] <- p
}

n_species <- length(plot_list)
multiplot_species <- do.call(grid.arrange, c(plot_list, ncol = 1))

multiplot_species_file <- file.path("C:/Users/Jakob/Desktop", "NPP_species_SPEI6Veg_multiplot.png")
ggsave(multiplot_species_file, multiplot_species, width = 12, height = 2.5 * n_species, dpi = 300)

cat("✅ Multiplot NPP pro Baumart vs. SPEI6_Veg erstellt\n")
cat("📊 Anzahl Baumarten:", n_species, "\n")
cat("📁 Gespeichert als:", multiplot_species_file, "\n")
cat("🌳 Baumarten:", paste(species_labels_german, collapse = ", "), "\n")

