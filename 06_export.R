# ============================================================
#  06_export.R — ÉTAPE 6
#  Export du fichier nettoyé pour la partie IA
#
#  Entrée  : outputs/df_nettoye.rds (généré par 01_exploration.R)
#  Sortie  : outputs/figures/*.png
# ============================================================
source("R/00_setup.R")

# Chargement du dataframe nettoyé
if (!file.exists("outputs/df_nettoye.rds")) {
  stop("Exécutez d'abord 01_exploration.R pour générer outputs/df_nettoye.rds")
}
df <- readRDS("outputs/df_nettoye.rds")

# ============================================================
#  ÉTAPE 6 – EXPORT DU FICHIER NETTOYÉ
# ============================================================
cat("─────────────────────────────────────────────────────\n")
cat("ÉTAPE 6 : EXPORT DU FICHIER NETTOYÉ\n")
cat("─────────────────────────────────────────────────────\n")

export_cols <- c(
  "lon","lat",
  "id_arbre","clc_quartier","clc_secteur",
  "haut_tot","haut_tronc","tronc_diam","age_estim",
  "fk_arb_etat","fk_stadedev","fk_port",
  "fk_pied","fk_situation","fk_revetement",
  "feuillage","remarquable",
  "dte_plantation","dte_abattage",
  "a_abattre",
  "nomfrancais"
)

df_export <- df %>%
  select(all_of(export_cols)) %>%
  mutate(
    haut_tot   = as.integer(round(haut_tot)),
    haut_tronc = as.integer(round(haut_tronc)),
    tronc_diam = as.integer(round(tronc_diam)),
    age_estim  = as.integer(round(age_estim)),
    remarquable = ifelse(remarquable == "Oui", 1L, 0L),
    a_abattre   = ifelse(a_abattre   == "Oui", 1L, 0L)
  )

if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")

readr::write_excel_csv(
  df_export,
  file  = "outputs/export_IA.csv",
  na    = "",
  quote = "needed"
)

cat(sprintf("Fichier export_IA.csv : %d lignes × %d colonnes\n",
            nrow(df_export), ncol(df_export)))

# ── Récapitulatif final ─────────────────────────────────────
cat("\n══════════════════════════════════════════════════════\n")
cat("RÉCAPITULATIF FINAL\n")
cat("══════════════════════════════════════════════════════\n")
cat(sprintf("  Données brutes    : %d arbres\n", nrow(df_raw)))
cat(sprintf("  Après nettoyage   : %d arbres\n", nrow(df)))
cat(sprintf("  Fichier IA        : outputs/export_IA.csv\n"))
cat(sprintf("  Figures           : outputs/figures/ (19 fichiers PNG)\n"))
