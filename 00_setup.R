# ============================================================
#  PROJET BIG DATA – PATRIMOINE ARBORÉ DE SAINT-QUENTIN
#  FISA4 – 2026
#  00_setup.R — Packages et chargement des données brutes
# ============================================================
#
#  Ce fichier est sourcé au début de chaque script d'étape.
#  Il charge les packages et lit le CSV brut en mémoire (df_raw).
#
#  Packages nécessaires :
#   install.packages(c("dplyr","ggplot2","tidyr","corrplot",
#                      "leaflet","sf","caret","forcats","scales",
#                      "lubridate","vcd","tibble","RColorBrewer",
#                      "htmlwidgets","pROC","OpenStreetMap"))
# ============================================================

library(dplyr)
library(ggplot2)
library(tidyr)
library(corrplot)
library(leaflet)
library(htmlwidgets)
library(sf)
library(caret)
library(forcats)
library(scales)
library(lubridate)
library(vcd)
library(tibble)
library(RColorBrewer)
library(pROC)           # vraie courbe ROC avec AUC
library(OpenStreetMap)  # fond de carte OSM pour graphiques statiques

# ── Dossiers de sortie ──────────────────────────────────────
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

# ============================================================
#  CHARGEMENT DES DONNÉES BRUTES
# ============================================================
df_raw <- read.csv(
  "data/Patrimoine_Arboré_data.csv",
  fileEncoding     = "UTF-8-BOM",
  stringsAsFactors = FALSE,
  na.strings       = c("", "NA", "RAS", " ")
)

# Nettoyage défensif des noms de colonnes
names(df_raw) <- trimws(names(df_raw))

cat("=== Dimensions brutes ===\n")
cat("Lignes :", nrow(df_raw), "| Colonnes :", ncol(df_raw), "\n\n")
