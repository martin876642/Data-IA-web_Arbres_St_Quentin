# ============================================================
#  run_all.R — Exécute toutes les étapes dans l'ordre
# ============================================================
#  Usage :
#    Rscript R/run_all.R
#  ou depuis RStudio :
#    source("R/run_all.R")
# ============================================================

cat("\n ÉTAPE 1 : Exploration + nettoyage\n")
source("R/01_exploration.R")

cat("\n ÉTAPE 2 : Visualisation graphiques\n")
source("R/02_visualisation_graphiques.R")

cat("\n ÉTAPE 3 : Cartes\n")
source("R/03_cartes.R")

cat("\n ÉTAPE 4 : Corrélations\n")
source("R/04_correlations.R")

cat("\n ÉTAPE 5 : Prédictions\n")
source("R/05_predictions.R")

cat("\n ÉTAPE 6 : Export\n")
source("R/06_export.R")

cat("\n✓ Pipeline complet terminé avec succès.\n")
