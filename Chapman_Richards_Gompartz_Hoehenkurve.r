# Chapman-Richards und Gompertz Oberhöhenkurven
# Autor: Neumann, J.
# Datum finaler Version: Januar 2026

# Bibliotheken laden
library(tidyverse)
library(RSQLite)
library(dbplyr)
library(terra)
library(grid)
library(gridExtra)

# Arbeitsverzeichnis setzen
setwd("C:/Users/Jakob/OneDrive/OneDrive Dokumente/Universitaet/02_Master/Masterarbeit/iLand_SOSFOR/Iland_project/temp")

# Umgebungsdatei laden
(env.df <- read_delim("../gis/environment.txt"))

# DEM Raster laden (10 x 10 m Auflösung)
dem.grid <- terra::rast("C:/Users/Jakob/OneDrive/OneDrive Dokumente/Universitaet/02_Master/Masterarbeit/iLand_SOSFOR/Iland_project/gis/dem.asc")

# Koordinaten umrechnen und ursprüngliche Werte speichern
env.df$x_orig <- env.df$x
env.df$y_orig <- env.df$y

env.df <- env.df %>%
  mutate(
    x = 405400 + x * 100,
    y = 5277726 + y * 100
  )
# Höheninformation hinzufügen
env.df$elevation <- terra::extract(
  terra::aggregate(dem.grid, fact = 10, fun = "mean"), 
  env.df[, c("x", "y")], ID = FALSE) %>% unlist()

# Artendatenbank laden
db.conn <- RSQLite::dbConnect(RSQLite::SQLite(),
                              dbname = "../database/all_species_database.sqlite")
species.df <- dplyr::tbl(db.conn, "species") %>% 
  collect()
RSQLite::dbDisconnect(db.conn); rm(db.conn)

# Target-Datei laden
target.df <- read.csv("C:/Users/Jakob/OneDrive/OneDrive Dokumente/Universitaet/02_Master/Masterarbeit/iLand_SOSFOR/AgentFiles/g_eng_Muenstertal_ABE-lib_targets_v1.0.csv", sep=";")



# iLand Ausgabedaten laden
# Funktion zum Verbinden mit SQLite-Datei
connect_to_sqlite <- function(filename, output_dir = "../output/") {
  # Pfad erstellen
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

# SQLite-Datenbank verbinden und Baumdaten laden
sqlite_filename <- "project_CSA_20251031_105519.sqlite"
selected_db <- connect_to_sqlite(sqlite_filename)
db.conn <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = selected_db)

RSQLite::dbListTables(db.conn)

# Landschafts- und Baumdaten laden
lscp <- dplyr::tbl(db.conn, "landscape") %>% 
  collect()

# Sampling direkt in der Datenbank
target_species <- c("fasy", "piab", "pisy", "psme", "qupe", "quro", "abal", "frex")
max_trees_per_species <- 50000

# Lade jede Art einzeln mit optimierter Rate
initial_sample_rate <- 0.02

cat("Lade Baumdaten mit Sampling pro Art...\n")

# Lade für jede Art separat mit adaptivem Sampling
tree_list <- list()
for (sp in target_species) {
  cat("Lade Art:", sp, "...")
  
  # Erste Runde mit moderater Sampling-Rate
  sp_data <- dplyr::tbl(db.conn, "tree") %>%
    filter(species == sp) %>%
    filter(height > 0, age > 0, height <= 60, age <= 300) %>%
    select(species, height, age, dbh) %>%
    filter(sql(paste0("RANDOM() < ", initial_sample_rate))) %>%
    collect()
  
  # Bei zu wenig Daten: Sampling-Rate erhöhen
  if (nrow(sp_data) < 1000 && nrow(sp_data) > 0) {
    cat(" wenige Daten, erhöhe Sampling-Rate...")
    
    sp_data <- dplyr::tbl(db.conn, "tree") %>%
      filter(species == sp) %>%
      filter(height > 0, age > 0, height <= 60, age <= 300) %>%
      select(species, height, age, dbh) %>%
      filter(sql(paste0("RANDOM() < ", 0.1))) %>%
      collect()
  }
  
  # Bei sehr wenig Daten: weiteres Sample laden
  if (nrow(sp_data) < 500 && nrow(sp_data) > 0) {
    cat(" sehr wenige Daten, nehme größeres Sample...")
    
    sp_data <- dplyr::tbl(db.conn, "tree") %>%
      filter(species == sp) %>%
      filter(height > 0, age > 0, height <= 60, age <= 300) %>%
      select(species, height, age, dbh) %>%
      filter(sql(paste0("RANDOM() < ", 0.3))) %>%
      collect()
  }
  
  # Bei zu vielen Daten: auf Maximum reduzieren
  if (nrow(sp_data) > max_trees_per_species) {
    cat(" zu viele Daten, reduziere auf", max_trees_per_species, "...")
    sp_data <- sp_data[sample(nrow(sp_data), max_trees_per_species), ]
  }
  
  if (nrow(sp_data) > 0) {
    tree_list[[sp]] <- sp_data
    cat(" ", nrow(sp_data), "Bäume geladen\n")
  } else {
    cat(" keine Daten gefunden\n")
  }
}

# Kombiniere alle Arten
if (length(tree_list) > 0) {
  tree <- do.call(rbind, tree_list)
  cat("Gesamt geladen:", nrow(tree), "Bäume aus", length(tree_list), "Arten\n")
} else {
  stop("Keine Baumdaten gefunden!")
}




# Chapman-Richards Oberhöhenkurven-Analyse

# growthmodels Paket installieren falls nötig
if (!require(growthmodels, quietly = TRUE)) {
  install.packages("growthmodels")
  library(growthmodels)
}

# Datenaufbereitung
target_species <- c("fasy", "piab", "pisy", "psme", "qupe", "quro", "abal", "frex")

# Baumdaten bereinigen und filtern
tree_clean <- tree %>%
  filter(!is.na(height), !is.na(age), !is.na(species)) %>%
  filter(height > 0, age > 0) %>%
  filter(height <= 60, age <= 300) %>% 
  filter(species %in% target_species) %>%
  mutate(species = ifelse(species %in% c("qupe", "quro"), "qupe, quro", species))

# Verfügbare Arten prüfen
cat("Available target species in dataset:\n")
print(unique(tree_clean$species))
cat("\nNumber of trees per species:\n")
print(table(tree_clean$species))

# Daten samplen zur Reduktion der Rechenzeit
set.seed(123)
max_trees_per_species <- 10000

# Funktion zum Samplen der Daten je Art
sample_species_data <- function(data, max_n) {
  if (nrow(data) <= max_n) {
    return(data)
  } else {
    return(data[sample(nrow(data), max_n), ])
  }
}

# Sampling anwenden
tree_sampled <- tree_clean %>%
  split(.$species) %>%
  map_dfr(~sample_species_data(.x, max_trees_per_species))

cat("\nAfter sampling (max", max_trees_per_species, "trees per species):\n")
print(table(tree_sampled$species))

# Funktion zur Auswahl der obersten 20% je BHD-Klasse
select_top_dbh_per_class <- function(data) {
  # BHD-Klassen erstellen
  data <- data %>%
    mutate(dbh_class = ceiling(dbh / 2)) %>%
    filter(dbh >= 1)
  
  # Je BHD-Klasse: oberste 20% nach Höhe
  data_filtered <- data %>%
    group_by(dbh_class) %>%
    arrange(desc(height)) %>%
    slice_head(prop = 0.2) %>%
    ungroup()
  
  return(data_filtered)
}

# BHD-Klassenfilterung anwenden
tree_filtered <- tree_sampled %>%
  split(.$species) %>%
  map_dfr(~select_top_dbh_per_class(.x))

cat("\nAfter diameter class filtering (top 20% per class):\n")
print(table(tree_filtered$species))

# BHD-Klassenverteilung als Beispiel anzeigen
if (nrow(tree_filtered) > 0) {
  first_species <- names(table(tree_filtered$species))[1]
  example_data <- tree_filtered %>% filter(species == first_species)
  cat("\nDiameter class distribution for", first_species, ":\n")
  print(table(example_data$dbh_class))
}

# Funktion zum sicheren Anpassen mit mehreren Fallbacks
fit_chapman_richards <- function(data) {
  species_name <- unique(data$species)[1]
  
  # Versuch 1: Chapman-Richards
  chapman_model <- fit_chapman_richards_only(data)
  if (!is.null(chapman_model)) {
    return(list(model = chapman_model, type = "Chapman-Richards"))
  }
  
  # Versuch 2: Gompertz
  cat("  Chapman-Richards failed, trying Gompertz fallback...\n")
  gompertz_model <- fit_gompertz_model(data)
  if (!is.null(gompertz_model)) {
    return(list(model = gompertz_model, type = "Gompertz"))
  }
  
  # Versuch 3: Lineares Modell
  cat("  Gompertz failed, trying linear model...\n")
  linear_model <- fit_linear_model(data)
  if (!is.null(linear_model)) {
    return(list(model = linear_model, type = "Linear"))
  }
  
  cat("  All models failed for this species\n")
  return(NULL)
}

# Funktion zum Anpassen des Chapman-Richards-Modells
fit_chapman_richards_only <- function(data) {
  tryCatch({
    # Chapman-Richards Modell: H = A * (1 - exp(-k * age))^p
    
    # Datencharakteristik für bessere Startwerte
    max_height <- max(data$height, na.rm = TRUE)
    min_height <- min(data$height, na.rm = TRUE)
    mean_height <- mean(data$height, na.rm = TRUE)
    max_age <- max(data$age, na.rm = TRUE)
    min_age <- min(data$age, na.rm = TRUE)
    mean_age <- mean(data$age, na.rm = TRUE)
    
    # Datencharakteristik ausgeben
    cat("  Data characteristics: n =", nrow(data), 
        ", height range:", round(min_height, 1), "-", round(max_height, 1),
        ", age range:", round(min_age, 1), "-", round(max_age, 1), "\n")
    
    # Startwerte für Parameter
    # A: Asymptote (etwas höher als max. Höhe)
    A_values <- c(
      max_height * c(1.1, 1.2, 1.3, 1.5),
      30, 40, 50, 60
    )
    
    # k: Wachstumsrate
    k_values <- c(0.005, 0.01, 0.02, 0.03, 0.05, 0.08)
    
    # p: Kurvenform
    p_values <- c(0.8, 1.0, 1.5, 2.0, 2.5)
    
    # Alle Kombinationen erstellen
    start_sets <- list()
    counter <- 1
    
    for (A in A_values) {
      for (k in k_values) {
        for (p in p_values) {
          start_sets[[counter]] <- list(A = A, k = k, p = p)
          counter <- counter + 1
        }
      }
    }
    
    # Empirisch gute Startwerte hinzufügen
    empirical_starts <- list(
      # Standardmuster
      list(A = max_height * 1.1, k = 0.02, p = 1.0),
      list(A = max_height * 1.2, k = 0.015, p = 1.5),
      list(A = max_height * 1.3, k = 0.01, p = 2.0),
      
      # Nadelbäume
      list(A = 45, k = 0.025, p = 1.2),
      list(A = 50, k = 0.015, p = 2.0),
      list(A = 40, k = 0.030, p = 1.0),
      
      # Laubbäume
      list(A = 35, k = 0.03, p = 1.5),
      list(A = 30, k = 0.04, p = 1.0),
      
      # Datenadaptive Muster
      list(A = max_height * 1.5, k = 0.01, p = 2.2),
      list(A = max_height * 1.4, k = 0.012, p = 1.6)
    )
    
    # Empirische und systematische Startwerte kombinieren
    all_starts <- c(empirical_starts, start_sets)
    
    cat("  Trying Chapman-Richards with", length(all_starts), "starting value combinations...\n")
    
    # Alle Startwerte durchprobieren
    for (i in seq_along(all_starts)) {
      start_vals <- all_starts[[i]]
      
      # Ungültige Kombinationen überspringen
      if (start_vals$A <= 0 || start_vals$k <= 0 || start_vals$p <= 0) {
        next
      }
      
      model <- tryCatch({
        nls(height ~ A * (1 - exp(-k * age))^p,
            data = data,
            start = start_vals,
            control = nls.control(maxiter = 1000, minFactor = 1/4096))
      }, error = function(e) NULL)
      
      if (!is.null(model)) {
        # Prüfen ob Parameter plausibel sind
        params <- coef(model)
        if (params["A"] > 0 && params["A"] < 150 &&
            params["k"] > 0 && params["k"] < 2 &&
            params["p"] > 0 && params["p"] < 15) {
          cat("  Chapman-Richards successful with start set", i, 
              ": A =", round(start_vals$A, 2),
              ", k =", round(start_vals$k, 4),
              ", p =", round(start_vals$p, 2), "\n")
          return(model)
        }
      }
      
      # Fortschritt alle 100 Versuche ausgeben
      if (i %% 100 == 0) {
        cat("  Tried", i, "Chapman-Richards combinations so far...\n")
      }
    }
    
    cat("  All Chapman-Richards approaches failed\n")
    return(NULL)
    
  }, error = function(e) {
    cat("Error fitting Chapman-Richards model:", e$message, "\n")
    return(NULL)
  })
}

# Funktion zum Anpassen des Gompertz-Modells als Fallback
fit_gompertz_model <- function(data) {
  tryCatch({
    # Gompertz Modell: H = alpha * exp(-beta * exp(-k * age))
    
    max_height <- max(data$height, na.rm = TRUE)
    
    # Startwerte für Gompertz-Parameter
    # alpha: Asymptote
    alpha_values <- c(
      max_height * c(1.05, 1.1, 1.15, 1.2, 1.25, 1.3, 1.4, 1.5, 1.6, 1.8, 2.0),
      seq(25, 80, by = 10)
    )
    
    # beta: Wachstumsverschiebung
    beta_values <- c(
      0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9,
      1.0, 1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4, 2.6, 2.8,
      3.0, 3.5, 4.0, 4.5, 5.0, 6.0, 7.0, 8.0, 10.0
    )
    
    # k: Wachstumsrate
    k_values <- c(
      0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008, 0.009,
      0.01, 0.012, 0.015, 0.018, 0.02, 0.025, 0.03, 0.035,
      0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1, 0.12, 0.15, 0.2
    )
    
    # Empirische Startwerte für Gompertz
    empirical_gompertz <- list(
      # Standardmuster
      list(alpha = max_height * 1.2, beta = 1.5, k = 0.02),
      list(alpha = max_height * 1.3, beta = 2.0, k = 0.015),
      list(alpha = max_height * 1.1, beta = 1.0, k = 0.03),
      
      # Nadelbäume
      list(alpha = 50, beta = 1.8, k = 0.025),
      list(alpha = 60, beta = 2.5, k = 0.015),
      list(alpha = 40, beta = 1.2, k = 0.03),
      
      # Laubbäume
      list(alpha = 35, beta = 1.0, k = 0.04),
      list(alpha = 30, beta = 0.8, k = 0.05),
      
      # Spezielle Muster für schwierige Arten
      list(alpha = 30, beta = 0.5, k = 0.08),
      list(alpha = 25, beta = 0.3, k = 0.1),
      list(alpha = 35, beta = 0.7, k = 0.06),
      list(alpha = 28, beta = 0.4, k = 0.12),
      
      list(alpha = 65, beta = 3.0, k = 0.008),
      list(alpha = 70, beta = 4.0, k = 0.006),
      list(alpha = 55, beta = 2.5, k = 0.012),
      
      # Randfälle
      list(alpha = max_height * 1.5, beta = 0.2, k = 0.15),
      list(alpha = max_height * 2.0, beta = 5.0, k = 0.005),
      list(alpha = max_height * 0.9, beta = 0.6, k = 0.07),
      
      # Weitere Kombinationen
      list(alpha = 26, beta = 0.1, k = 0.2),
      list(alpha = 32, beta = 0.15, k = 0.18),
      list(alpha = 29, beta = 0.25, k = 0.14),
      list(alpha = 24, beta = 0.35, k = 0.16)
    )
    
    # Systematische Kombinationen
    gompertz_starts <- empirical_gompertz
    
    # Strategische Kombinationen hinzufügen
    selected_alphas <- c(max_height * c(1.1, 1.2, 1.3), 25, 30, 35, 50)
    selected_betas <- c(0.2, 0.5, 1.0, 1.5, 2.0, 3.0)
    selected_ks <- c(0.01, 0.02, 0.03, 0.05, 0.08, 0.12)
    
    for (alpha in selected_alphas) {
      for (beta in selected_betas) {
        for (k in selected_ks) {
          gompertz_starts[[length(gompertz_starts) + 1]] <- list(
            alpha = alpha, beta = beta, k = k
          )
        }
      }
    }
    
    cat("  Trying Gompertz with", length(gompertz_starts), "starting value combinations...\n")
    
    # Alle Startwerte für Gompertz durchprobieren
    for (i in seq_along(gompertz_starts)) {
      start_vals <- gompertz_starts[[i]]
      
      # Ungültige Kombinationen überspringen
      if (start_vals$alpha <= 0 || start_vals$beta <= 0 || start_vals$k <= 0) {
        next
      }
      
      model <- tryCatch({
        nls(height ~ alpha * exp(-beta * exp(-k * age)),
            data = data,
            start = start_vals,
            control = nls.control(maxiter = 1000, minFactor = 1/4096))
      }, error = function(e) NULL)
      
      if (!is.null(model)) {
        # Prüfen ob Parameter plausibel sind
        params <- coef(model)
        if (params["alpha"] > 0 && params["alpha"] < 200 &&
            params["beta"] > 0 && params["beta"] < 20 &&
            params["k"] > 0 && params["k"] < 5) {
          cat("  Gompertz successful with start set", i, 
              ": alpha =", round(params["alpha"], 2),
              ", beta =", round(params["beta"], 3),
              ", k =", round(params["k"], 4), "\n")
          return(model)
        }
      }
      
      # Fortschritt alle 50 Versuche ausgeben
      if (i %% 50 == 0) {
        cat("  Tried", i, "Gompertz combinations so far...\n")
      }
    }
    
    cat("  All Gompertz approaches failed\n")
    return(NULL)
    
  }, error = function(e) {
    cat("Error fitting Gompertz model:", e$message, "\n")
    return(NULL)
  })
}

# Funktion zum Anpassen eines linearen Modells als Fallback
fit_linear_model <- function(data) {
  cat("  Trying linear model...\n")
  
  tryCatch({
    # Einfache lineare Regression: H = a + b * age
    linear_model <- lm(height ~ age, data = data)
    
    # Prüfen ob das Modell akzeptabel ist
    r_squared <- summary(linear_model)$r.squared
    
    if (r_squared > 0.1) {
      cat("  Linear model successful: R² =", round(r_squared, 3), "\n")
      
      # Koeffizienten ausgeben
      coeffs <- coef(linear_model)
      cat("  Linear parameters: a =", round(coeffs[1], 2), 
          ", b =", round(coeffs[2], 4), "\n")
      
      return(linear_model)
    } else {
      cat("  Linear model has poor fit (R² =", round(r_squared, 3), ")\n")
      return(NULL)
    }
    
  }, error = function(e) {
    cat("Error fitting linear model:", e$message, "\n")
    return(NULL)
  })
}

# Modelle für alle Arten anpassen
species_models <- list()
model_results <- data.frame()

for (sp in unique(tree_filtered$species)) {
  cat("Fitting height-age model for species:", sp, "\n")
  
  species_data <- tree_filtered %>% filter(species == sp)
  
  # Mindestanzahl Beobachtungen prüfen
  if (nrow(species_data) < 10) {
    cat("  Skipping", sp, "- insufficient data (n =", nrow(species_data), ")\n")
    next
  }
  
  result <- fit_chapman_richards(species_data)
  
  if (!is.null(result)) {
    species_models[[sp]] <- result
    
    # Parameter je nach Modelltyp extrahieren
    if (result$type == "Chapman-Richards") {
      params <- coef(result$model)
      model_results <- rbind(model_results, data.frame(
        species = sp,
        model_type = "Chapman-Richards",
        A = params["A"],
        k = params["k"],
        p = params["p"],
        alpha = NA,
        beta = NA,
        n_trees = nrow(species_data),
        rmse = sqrt(mean(residuals(result$model)^2))
      ))
      
      cat("  Success! Chapman-Richards: A =", round(params["A"], 2), 
          ", k =", round(params["k"], 4), 
          ", p =", round(params["p"], 2), "\n")
          
    } else if (result$type == "Gompertz") {
      params <- coef(result$model)
      model_results <- rbind(model_results, data.frame(
        species = sp,
        model_type = "Gompertz",
        A = NA,
        k = params["k"],
        p = NA,
        alpha = params["alpha"],
        beta = params["beta"],
        n_trees = nrow(species_data),
        rmse = sqrt(mean(residuals(result$model)^2))
      ))
      
      cat("  Success! Gompertz: alpha =", round(params["alpha"], 2), 
          ", beta =", round(params["beta"], 3), 
          ", k =", round(params["k"], 4), "\n")
          
    } else if (result$type == "Linear") {
      params <- coef(result$model)
      model_results <- rbind(model_results, data.frame(
        species = sp,
        model_type = "Linear",
        A = NA,
        k = NA,
        p = NA,
        alpha = NA,
        beta = NA,
        n_trees = nrow(species_data),
        rmse = sqrt(mean(residuals(result$model)^2))
      ))
      
      cat("  Success! Linear: a =", round(params[1], 2), 
          ", b =", round(params[2], 4), "\n")
          
    } else if (result$type == "Weibull") {
      params <- coef(result$model)
      model_results <- rbind(model_results, data.frame(
        species = sp,
        model_type = "Weibull",
        A = params["A"],
        k = params["k"],
        p = params["c"],  # Weibull shape parameter stored as p
        alpha = NA,
        beta = NA,
        n_trees = nrow(species_data),
        rmse = sqrt(mean(residuals(result$model)^2))
      ))
      
      cat("  Success! Weibull: A =", round(params["A"], 2), 
          ", k =", round(params["k"], 4),
          ", c =", round(params["c"], 2), "\n")
    }
  } else {
    cat("  Both models failed for", sp, "\n")
  }
}

print(model_results)

# Plots für alle Arten erstellen
# Funktion zum Erstellen der Höhenkurven-Plots
plot_height_curve <- function(species_name, model_result, data, show_x_label = TRUE, show_y_label = TRUE) {
  
  model <- model_result$model
  model_type <- model_result$type
  
  # Vorhersagewerte für glatte Kurve
  age_seq <- seq(10, 150, length.out = 100)
  params <- coef(model)
  
  # Vorhergesagte Höhe je nach Modelltyp berechnen
  if (model_type == "Chapman-Richards") {
    predicted_height <- params["A"] * (1 - exp(-params["k"] * age_seq))^params["p"]
    param_text <- paste0("A = ", round(params["A"], 1),
                        ", k = ", round(params["k"], 4),
                        ", p = ", round(params["p"], 2))
  } else if (model_type == "Gompertz") {
    predicted_height <- params["alpha"] * exp(-params["beta"] * exp(-params["k"] * age_seq))
    param_text <- paste0("α = ", round(params["alpha"], 1),
                        ", β = ", round(params["beta"], 3),
                        ", k = ", round(params["k"], 4))
  } else if (model_type == "Linear") {
    predicted_height <- params[1] + params[2] * age_seq
    param_text <- paste0("a = ", round(params[1], 2),
                        ", b = ", round(params[2], 4))
  } else if (model_type == "Weibull") {
    predicted_height <- params["A"] * (1 - exp(-(params["k"] * age_seq)^params["c"]))
    param_text <- paste0("A = ", round(params["A"], 1),
                        ", k = ", round(params["k"], 4),
                        ", c = ", round(params["c"], 2))
  }
  
  pred_data <- data.frame(age = age_seq, height = predicted_height)
  
  # R² berechnen
  fitted_values <- predict(model)
  observed_values <- data$height
  ss_res <- sum((observed_values - fitted_values)^2)
  ss_tot <- sum((observed_values - mean(observed_values))^2)
  r_squared <- 1 - (ss_res / ss_tot)
  
  # Plot erstellen
  p <- ggplot() +
    # Beobachtete Datenpunkte
    geom_point(data = data, aes(x = age, y = height), 
               alpha = 0.6, color = "darkblue", size = 0.5) +
    # Angepasste Kurve
    geom_line(data = pred_data, aes(x = age, y = height), 
              color = "red", size = 0.8) +
    # Beschriftung
    labs(title = paste0(species_name, " (", model_type, ")"),
         subtitle = paste0(param_text,
                          "\nR² = ", round(r_squared, 3),
                          ", n = ", nrow(data)),
         x = if(show_x_label) "Alter [Jahre]" else "",
         y = if(show_y_label) "Oberhöhe [m]" else "") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
          plot.subtitle = element_text(hjust = 0.5, size = 9),
          axis.title.x = element_text(face = "bold", size = 10, margin = margin(t = 5)),
          axis.title.y = element_text(face = "bold", size = 10, margin = margin(r = 5)),
          axis.text = element_text(size = 8),
          plot.margin = margin(8, 8, 8, 8)) +
    scale_x_continuous(limits = c(10, 150), breaks = seq(20, 140, 20)) +
    scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, 10))
  
  return(p)
}

# Individuelle Plots für jede Art erstellen
plot_list <- list()

# Plots für Gruppe 1: Nadelbäume
group1_species <- c("piab", "abal", "psme", "pisy")
group1_plots <- list()

for (i in seq_along(group1_species)) {
  sp <- group1_species[i]
  if (sp %in% names(species_models)) {
    species_data <- tree_filtered %>% filter(species == sp)
    
    # Achsenbeschriftungen je nach Position im 2x2 Raster
    show_y_label <- i %in% c(1, 3)
    show_x_label <- i %in% c(3, 4)
    
    group1_plots[[sp]] <- plot_height_curve(sp, species_models[[sp]], species_data, 
                                          show_x_label, show_y_label)
  }
}

# Plots für Gruppe 2: Laubbäume
group2_species <- c("fasy", "qupe, quro", "frex")
group2_plots <- list()

for (i in seq_along(group2_species)) {
  sp <- group2_species[i]
  if (sp %in% names(species_models)) {
    species_data <- tree_filtered %>% filter(species == sp)
    
    # Achsenbeschriftungen für 2x2 Raster mit leerem Feld
    show_y_label <- i %in% c(1, 3)
    show_x_label <- i == 3
    
    group2_plots[[sp]] <- plot_height_curve(sp, species_models[[sp]], species_data, 
                                          show_x_label, show_y_label)
  }
}

# Plots anzeigen
if (length(group1_plots) > 0 || length(group2_plots) > 0) {
  cat("\nCreating plots for", length(group1_plots) + length(group2_plots), "species in two groups...\n")
  
  # Grafik-Device zurücksetzen
  if (dev.cur() != 1) {
    dev.off()
  }
  
  # Einzelne Plots anzeigen (optional, zur Vorschau)
  for (sp in names(group1_plots)) {
    tryCatch({
      print(group1_plots[[sp]])
      Sys.sleep(0.5)  # Brief pause to allow plot to render
    }, error = function(e) {
      cat("Error plotting", sp, ":", e$message, "\n")
    })
  }
  
  for (sp in names(group2_plots)) {
    tryCatch({
      print(group2_plots[[sp]])
      Sys.sleep(0.5)  # Brief pause to allow plot to render
    }, error = function(e) {
      cat("Error plotting", sp, ":", e$message, "\n")
    })
  }
  
  # Gruppe 1: Nadelbäume als 2x2 Raster erstellen
  if (length(group1_plots) > 0) {
    # Plots in 2x2 Raster anordnen
    ordered_group1_plots <- list()
    group1_order <- c("piab", "abal", "psme", "pisy")

    for (sp in group1_order) {
      if (sp %in% names(group1_plots)) {
        ordered_group1_plots[[length(ordered_group1_plots) + 1]] <- group1_plots[[sp]]
      }
    }
    
    if (length(ordered_group1_plots) > 0) {
      tryCatch({
        # Als hochauflösendes PNG speichern
        png("C:/Users/Jakob/Desktop/Height_Age_Models_Group1_Conifers.png", 
            width = 6.0, height = 6.5, units = "in", res = 300)
        
        grid.arrange(grobs = ordered_group1_plots, 
                     ncol = 2, nrow = 2,
                     heights = c(1, 1),
                     widths = c(1, 1))
        dev.off()
        
        cat("Group 1 PNG file saved as: C:/Users/Jakob/Desktop/Height_Age_Models_Group1_Conifers.png\n")
        cat("Contains: piab, abal, psme, pisy (Nadelbäume)\n")
        
      }, error = function(e) {
        cat("Error creating Group 1 plot:", e$message, "\n")
        if (dev.cur() != 1) {
          dev.off()
        }
      })
    }
  }
  
  # Gruppe 2: Laubbäume als 2x2 Raster erstellen
  if (length(group2_plots) > 0) {
    # Plots in 2x2 Raster anordnen
    ordered_group2_plots <- list()
    group2_order <- c("fasy", "qupe, quro", "frex")

    for (sp in group2_order) {
      if (sp %in% names(group2_plots)) {
        ordered_group2_plots[[length(ordered_group2_plots) + 1]] <- group2_plots[[sp]]
      }
    }
    
    # Add empty plot for 4th position if we have exactly 3 plots
    if (length(ordered_group2_plots) == 3) {
      # Create empty plot
      empty_plot <- ggplot() + 
        theme_void() + 
        theme(panel.background = element_rect(fill = "white", color = NA))
      ordered_group2_plots[[4]] <- empty_plot
    }
    
    if (length(ordered_group2_plots) > 0) {
      tryCatch({
        # Save Group 2 as high-resolution PNG
        # Gleiche optimierte Dimensionen: Breite gleich, Höhe reduziert
        png("C:/Users/Jakob/Desktop/Height_Age_Models_Group2_Deciduous.png", 
            width = 6.0, height = 6.5, units = "in", res = 300)
        
        grid.arrange(grobs = ordered_group2_plots, 
                     ncol = 2, nrow = 2,
                     heights = c(1, 1),
                     widths = c(1, 1))
        dev.off()
        
        cat("Group 2 PNG file saved as: C:/Users/Jakob/Desktop/Height_Age_Models_Group2_Deciduous.png\n")
        cat("Contains: fasy, qupe+quro, frex (Laubbäume)\n")
        
      }, error = function(e) {
        cat("Error creating Group 2 plot:", e$message, "\n")
        if (dev.cur() != 1) {
          dev.off()
        }
      })
    }
  }
  
  cat("High-resolution PNG files (300 DPI) created successfully!\n")
  cat("Format: DIN A4 with margins, optimized for figure caption space\n")
  cat("Contains both Chapman-Richards and Gompertz models as needed\n")
  
} else {
  cat("No models could be fitted successfully.\n")
} 


#### Tabelle Eingriffszeitpunkt ####

# Bibliothek für Excel-Import laden
if (!require(readxl, quietly = TRUE)) {
  install.packages("readxl")
  library(readxl)
}

# Durchforstungsdaten laden
df_eingriffe <- read_excel("C:/Users/Jakob/Desktop/Df-EIngriffe_nach_OH.xlsx")

# Geladene Daten anzeigen
cat("Loaded thinning intervention data:\n")
print(df_eingriffe)

# Funktion zum Vorhersagen des Alters aus der Höhe
predict_age_from_height <- function(height, model_result, max_age = 200) {
  model <- model_result$model
  model_type <- model_result$type
  params <- coef(model)
  
  # Funktion zur Minimierung definieren
  if (model_type == "Chapman-Richards") {
    height_function <- function(age) {
      params["A"] * (1 - exp(-params["k"] * age))^params["p"] - height
    }
  } else if (model_type == "Gompertz") {
    height_function <- function(age) {
      params["alpha"] * exp(-params["beta"] * exp(-params["k"] * age)) - height
    }
  } else if (model_type == "Linear") {
    # Lineare Modell direkt lösen
    if (params[2] != 0) {
      age_direct <- (height - params[1]) / params[2]
      return(round(max(1, age_direct), 1))
    } else {
      return(NA)
    }
  } else if (model_type == "Weibull") {
    height_function <- function(age) {
      params["A"] * (1 - exp(-(params["k"] * age)^params["c"])) - height
    }
  }
  
  # Alter finden mittels numerischer Optimierung
  tryCatch({
    age_result <- uniroot(height_function, interval = c(1, max_age), extendInt = "yes")
    return(round(age_result$root, 1))
  }, error = function(e) {
    cat("Warning: Could not find age for height", height, "in model type", model_type, "\n")
    return(NA)
  })
}

# Funktion zur Umwandlung von WET-Codes in Beschreibungen
get_wet_description <- function(wet_code) {
  wet_descriptions <- list(
    # Buche
    "bg" = "Buche ger. Risiko",
    "bm" = "Buche mittl. Risiko", 
    "bx" = "Buche hoh. Risiko",
    
    # Eiche
    "eg" = "Eiche ger. Risiko",
    "em" = "Eiche mittl. Risiko",
    
    # Buntlaub
    "h1" = "Buntlaub frisch (einphasig)",
    "h2" = "Buntlaub frisch (zweiphasig)",
    "j" = "Buntlaub trocken",
    
    # Fichte
    "fg" = "Fichte ger. Risiko",
    "fm" = "Fichte mittl. Risiko",
    "fx" = "Fichte hoh. Risiko",
    
    # Tanne
    "tg" = "Tanne ger. Risiko",
    "tm" = "Tanne mittl. Risiko",
    "tx" = "Tanne hoh. Risiko",
    
    # Douglasie
    "dg" = "Douglasie ger. Risiko",
    "dm" = "Douglasie mittl. Risiko",
    
    # Kiefer
    "kg" = "Kiefer ger. Risiko",
    "km" = "Kiefer mittl. Risiko",
    "kx" = "Kiefer hoh. Risiko"
  )
  
  # Beschreibung zurückgeben oder Unbekannt
  if (wet_code %in% names(wet_descriptions)) {
    return(wet_descriptions[[wet_code]])
  } else {
    return(paste("Unbekannt:", wet_code))
  }
}

# Funktion zum Erstellen der Durchforstungstabelle
create_thinning_schedule <- function() {
  # Maximale Anzahl Eingriffe ermitteln
  max_interventions <- 0
  all_heights_list <- list()
  
  for (i in 1:nrow(df_eingriffe)) {
    df_start_height <- df_eingriffe$Df_Start[i]
    df_end_height <- df_eingriffe$Df_Ende[i]
    thinning_heights <- seq(df_start_height, df_end_height, by = 3)
    all_heights_list[[i]] <- thinning_heights
    max_interventions <- max(max_interventions, length(thinning_heights))
  }
  
  cat("Maximum number of interventions needed:", max_interventions, "\n")
  
  # Spaltennamen für alle Eingriffe erstellen
  base_cols <- c("WET_Bezeichnung", "WET_Code")
  height_cols <- paste0("h", 1:max_interventions)
  age_cols <- paste0("t", 1:max_interventions)
  all_cols <- c(base_cols, as.vector(rbind(height_cols, age_cols)))
  
  # Ergebnis-Dataframe initialisieren
  thinning_schedule <- data.frame(matrix(NA, nrow = 0, ncol = length(all_cols)))
  names(thinning_schedule) <- all_cols
  
  for (i in 1:nrow(df_eingriffe)) {
    wet_code <- df_eingriffe$WET[i]
    wet_description <- get_wet_description(wet_code)
    df_start_height <- df_eingriffe$Df_Start[i]
    df_end_height <- df_eingriffe$Df_Ende[i]
    oh_function <- df_eingriffe$OH_Funktion[i]
    
    cat("Processing WET:", wet_code, "(", wet_description, ") with species:", oh_function)
    
    # Zeile mit NA-Werten initialisieren
    wet_row <- data.frame(matrix(NA, nrow = 1, ncol = length(all_cols)))
    names(wet_row) <- all_cols
    
    # Basisinformationen eintragen
    wet_row$WET_Bezeichnung <- wet_description
    wet_row$WET_Code <- wet_code
    
    # Prüfen ob Modell für diese Art verfügbar ist
    if (oh_function %in% names(species_models)) {
      model_result <- species_models[[oh_function]]
      
      # Calculate thinning heights (start + every 3m until end)
      thinning_heights <- all_heights_list[[i]]
      
      cat(" - ", length(thinning_heights), "interventions\n")
      
      # Calculate ages for each thinning height
      for (j in seq_along(thinning_heights)) {
        height_col <- paste0("h", j)
        age_col <- paste0("t", j)
        
        predicted_age <- predict_age_from_height(thinning_heights[j], model_result)
        
        wet_row[[height_col]] <- thinning_heights[j]
        wet_row[[age_col]] <- round(predicted_age)
      }
      
    } else {
      cat(" - Warning: No model available\n")
      # Mindestens ersten Eingriff mit Starthöhe eintragen
      wet_row$h1 <- df_start_height
      wet_row$t1 <- NA
    }
    
    # Zeile zu Ergebnis hinzufügen
    thinning_schedule <- rbind(thinning_schedule, wet_row)
  }
  
  return(thinning_schedule)
}

# Durchforstungstabelle erstellen
cat("\nCreating thinning schedule based on height-age curves...\n")
thinning_table <- create_thinning_schedule()

# Ergebnisse anzeigen
cat("\nThinning Schedule Table:\n")
print(thinning_table)

# Tabelle als CSV speichern
write.csv(thinning_table, "C:/Users/Jakob/Desktop/Thinning_Schedule_by_Age.csv", row.names = FALSE)
cat("\nThinning schedule saved as: C:/Users/Jakob/Desktop/Thinning_Schedule_by_Age.csv\n")

# Formatierte Tabelle erstellen
library(knitr)
formatted_table <- thinning_table

# Alterswerte auf ganze Zahlen runden
age_cols <- grep("^t[0-9]+$", names(formatted_table))
for (col in age_cols) {
  formatted_table[[col]] <- round(formatted_table[[col]], 0)
}

cat("\nFormatted Thinning Schedule:\n")
print(kable(formatted_table, digits = 0, caption = "Thinning Schedule: Heights (h) and corresponding Ages (t) for each WET"))

# Detaillierte Zusammenfassung erstellen
cat("\nSummary of thinning interventions by WET:\n")
for (i in 1:nrow(thinning_table)) {
  wet_description <- thinning_table$WET_Bezeichnung[i]
  wet_code <- thinning_table$WET_Code[i]
  
  # Find height and age columns
  height_cols <- grep("^h[0-9]+$", names(thinning_table))
  age_cols <- grep("^t[0-9]+$", names(thinning_table))
  
  interventions <- data.frame(
    Height = as.numeric(thinning_table[i, height_cols]),
    Age = as.numeric(thinning_table[i, age_cols])
  )
  
  # Remove rows with NA
  interventions <- interventions[!is.na(interventions$Height) & !is.na(interventions$Age), ]
  
  if (nrow(interventions) > 0) {
    cat("\n", wet_code, " - ", wet_description, ":\n", sep = "")
    for (j in 1:nrow(interventions)) {
      cat("  Thinning", j, ": Height =", interventions$Height[j], "m, Age =", round(interventions$Age[j]), "years\n")
    }
  }
}


#### OBERHÖHE IM ALTER 100 FÜR ALLE BAUMARTEN ####

cat("\n" , "="*80, "\n")
cat("OBERHÖHE IM ALTER 100 JAHREN FÜR ALLE BAUMARTEN\n")
cat("="*80, "\n\n")

# Funktion zur Vorhersage der Oberhöhe im Alter 100
predict_height_at_age100 <- function() {
  
  target_age <- 100
  height_predictions <- data.frame(
    Baumart = character(),
    Modell_Typ = character(), 
    Oberhoehe_Alter_100 = numeric(),
    Modell_Parameter = character(),
    stringsAsFactors = FALSE
  )
  
  # Vollständige Artennamen
  species_names <- list(
    "fasy" = "Fagus sylvatica (Rotbuche)",
    "piab" = "Picea abies (Gemeine Fichte)", 
    "pisy" = "Pinus sylvestris (Waldkiefer)",
    "psme" = "Pseudotsuga menziesii (Douglasie)",
    "qupe" = "Quercus petraea (Traubeneiche)",
    "quro" = "Quercus robur (Stieleiche)",
    "qupe, quro" = "Quercus petraea/robur (Eiche)",
    "abal" = "Abies alba (Weißtanne)",
    "frex" = "Fraxinus excelsior (Gemeine Esche)"
  )
  
  for (sp in names(species_models)) {
    model_result <- species_models[[sp]]
    model <- model_result$model
    model_type <- model_result$type
    params <- coef(model)
    
    # Oberhöhe im Alter 100 berechnen
    if (model_type == "Chapman-Richards") {
      predicted_height <- params["A"] * (1 - exp(-params["k"] * target_age))^params["p"]
      param_text <- paste0("A=", round(params["A"], 2), 
                          ", k=", round(params["k"], 4), 
                          ", p=", round(params["p"], 2))
      
    } else if (model_type == "Gompertz") {
      predicted_height <- params["alpha"] * exp(-params["beta"] * exp(-params["k"] * target_age))
      param_text <- paste0("α=", round(params["alpha"], 2), 
                          ", β=", round(params["beta"], 3), 
                          ", k=", round(params["k"], 4))
      
    } else if (model_type == "Linear") {
      predicted_height <- params[1] + params[2] * target_age
      param_text <- paste0("a=", round(params[1], 2), 
                          ", b=", round(params[2], 4))
      
    } else if (model_type == "Weibull") {
      predicted_height <- params["A"] * (1 - exp(-(params["k"] * target_age)^params["c"]))
      param_text <- paste0("A=", round(params["A"], 2), 
                          ", k=", round(params["k"], 4), 
                          ", c=", round(params["c"], 2))
    }
    
    # Vollständiger Artname
    full_name <- if(sp %in% names(species_names)) species_names[[sp]] else sp
    
    # Zu Ergebnis hinzufügen
    height_predictions <- rbind(height_predictions, data.frame(
      Baumart = full_name,
      Modell_Typ = model_type,
      Oberhoehe_Alter_100 = round(predicted_height, 1),
      Modell_Parameter = param_text,
      stringsAsFactors = FALSE
    ))
  }
  
  # Nach Oberhöhe sortieren (absteigend)
  height_predictions <- height_predictions[order(-height_predictions$Oberhoehe_Alter_100), ]
  
  return(height_predictions)
}

# Vorhersagen berechnen und anzeigen
height_predictions_100 <- predict_height_at_age100()

# Ergebnisse in Tabelle anzeigen
cat("Oberhöhenwerte im Alter 100 basierend auf den angepassten Wachstumsmodellen:\n\n")
print(height_predictions_100, row.names = FALSE)

# Ranking erstellen
cat("\n", "-"*60, "\n")
cat("RANKING DER BAUMARTEN NACH OBERHÖHE IM ALTER 100:\n")
cat("-"*60, "\n")

for (i in 1:nrow(height_predictions_100)) {
  cat(sprintf("%d. %-35s: %5.1f m (%s)\n", 
              i, 
              height_predictions_100$Baumart[i],
              height_predictions_100$Oberhoehe_Alter_100[i],
              height_predictions_100$Modell_Typ[i]))
}

# Ergebnisse als CSV speichern
write.csv(height_predictions_100, 
          "C:/Users/Jakob/Desktop/Oberhoehe_Alter_100_alle_Baumarten.csv", 
          row.names = FALSE)

cat("\n", "-"*60, "\n")
cat("ZUSÄTZLICHE INFORMATIONEN:\n")
cat("-"*60, "\n")

# Statistiken berechnen
max_height <- max(height_predictions_100$Oberhoehe_Alter_100)
min_height <- min(height_predictions_100$Oberhoehe_Alter_100)
mean_height <- mean(height_predictions_100$Oberhoehe_Alter_100)

cat("Höchste Oberhöhe im Alter 100:  ", max_height, "m\n")
cat("Niedrigste Oberhöhe im Alter 100:", min_height, "m\n") 
cat("Durchschnittliche Oberhöhe:     ", round(mean_height, 1), "m\n")
cat("Spannweite:                     ", round(max_height - min_height, 1), "m\n")

# Nach Modelltypen gruppieren
model_types <- table(height_predictions_100$Modell_Typ)
cat("\nAngepasste Modelltypen:\n")
for (model in names(model_types)) {
  cat("  ", model, ": ", model_types[model], " Baumarten\n")
}

cat("\nErgebnisse gespeichert als: C:/Users/Jakob/Desktop/Oberhoehe_Alter_100_alle_Baumarten.csv\n")
cat("\n", "="*80, "\n")

