# ============================================================
#  02_visualisation_graphiques.R — ÉTAPE 2
#  Visualisation des données sur des graphiques
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
#  ÉTAPE 2 – VISUALISATION SUR DES GRAPHIQUES
# ============================================================
cat("─────────────────────────────────────────────────────\n")
cat("ÉTAPE 2 : VISUALISATION DES DONNÉES\n")
cat("─────────────────────────────────────────────────────\n")

save_fig <- function(name, width = 10, height = 6) {
  ggsave(paste0("outputs/figures/", name, ".png"),
         width = width, height = height, dpi = 150)
  cat("  → Sauvegardé :", name, ".png\n")
}

theme_set(theme_minimal(base_size = 13))

# Fig 1 : Stade de développement
df %>%
  filter(!is.na(fk_stadedev)) %>%
  count(fk_stadedev) %>%
  ggplot(aes(x = fct_reorder(fk_stadedev, n), y = n, fill = fk_stadedev)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = n), hjust = -0.2, size = 4) +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Répartition des arbres par stade de développement",
       x = NULL, y = "Nombre d'arbres") +
  expand_limits(y = max(table(df$fk_stadedev), na.rm = TRUE) * 1.15)
save_fig("fig1_stade_dev")

# Fig 2 : Arbres par quartier
df %>%
  filter(!is.na(clc_quartier)) %>%
  count(clc_quartier) %>%
  mutate(clc_quartier = fct_reorder(clc_quartier, n)) %>%
  ggplot(aes(x = clc_quartier, y = n, fill = n)) +
  geom_col() +
  coord_flip() +
  scale_fill_viridis_c(option = "D", direction = -1) +
  labs(title = "Nombre d'arbres par quartier",
       x = NULL, y = "Nombre d'arbres", fill = "Quantité")
save_fig("fig2_arbres_par_quartier", width = 12, height = 8)

# Fig 3 : Violin : hauteur × feuillage
df %>%
  filter(!is.na(feuillage)) %>%
  ggplot(aes(x = feuillage, y = haut_tot, fill = feuillage)) +
  geom_violin(alpha = 0.6, show.legend = FALSE) +
  geom_boxplot(width = 0.1, outlier.shape = NA, show.legend = FALSE) +
  scale_fill_manual(values = c("Feuillu"="#52B788","Conifère"="#1B4332")) +
  labs(title = "Hauteur totale selon le type de feuillage",
       x = NULL, y = "Hauteur totale (m)")
save_fig("fig3_hauteur_feuillage")

# Fig 4 : Distribution hauteur totale
df %>%
  ggplot(aes(x = haut_tot)) +
  geom_histogram(bins = 40, fill = "#2E86AB", color = "white") +
  labs(title = "Distribution de la hauteur totale des arbres",
       x = "Hauteur totale (m)", y = "Nombre d'arbres")
save_fig("fig4_distrib_hauteur")

# Fig 5 : Distribution diamètre tronc
df %>%
  ggplot(aes(x = tronc_diam)) +
  geom_histogram(bins = 40, fill = "#A8DADC", color = "white") +
  labs(title = "Distribution du diamètre du tronc",
       x = "Diamètre (cm)", y = "Nombre d'arbres")
save_fig("fig5_distrib_diametre")

# Fig 6 : Âge estimé par stade (boxplot)
df %>%
  filter(!is.na(fk_stadedev)) %>%
  ggplot(aes(x = fct_reorder(fk_stadedev, age_estim, median),
             y = age_estim, fill = fk_stadedev)) +
  geom_boxplot(outlier.alpha = 0.3, show.legend = FALSE) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Âge estimé selon le stade de développement",
       x = "Stade de développement", y = "Âge estimé (ans)")
save_fig("fig6_age_par_stade")

# Fig 7 : Feuillage
df %>%
  filter(!is.na(feuillage)) %>%
  count(feuillage) %>%
  ggplot(aes(x = feuillage, y = n, fill = feuillage)) +
  geom_col(show.legend = FALSE, width = 0.5) +
  geom_text(aes(label = n), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("Feuillu"="#52B788","Conifère"="#1B4332")) +
  labs(title = "Types de feuillage", x = NULL, y = "Nombre d'arbres")
save_fig("fig7_feuillage")

cat("\n── Figures exportées dans outputs/figures/ ──\n\n")


