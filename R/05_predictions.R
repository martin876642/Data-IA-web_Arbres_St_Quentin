# ============================================================
#  05_predictions.R — ÉTAPE 5
#  Prédiction âge estimé + classification arbres à abattre
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
#  ÉTAPE 5 – PRÉDICTION DE L'ÂGE ESTIMÉ
# ============================================================
cat("─────────────────────────────────────────────────────\n")
cat("ÉTAPE 5 : PRÉDICTION\n")
cat("─────────────────────────────────────────────────────\n")

# ── 5.1 Régression linéaire multiple ────────────────────────
cat("\n=== 5.1 Régression linéaire – age_estim ===\n")

df_reg <- df %>%
  select(age_estim, haut_tot, haut_tronc, tronc_diam,
         fk_stadedev, feuillage, fk_situation) %>%
  drop_na() %>%
  mutate(across(c(fk_stadedev, feuillage, fk_situation), as.factor))

set.seed(42)
idx   <- createDataPartition(df_reg$age_estim, p = 0.8, list = FALSE)
train <- df_reg[ idx, ]
test  <- df_reg[-idx, ]

lm_model <- lm(age_estim ~ ., data = train)
cat("\n── Résumé du modèle linéaire ──\n")
print(summary(lm_model))

pred_lm <- predict(lm_model, newdata = test)
rmse_lm <- sqrt(mean((pred_lm - test$age_estim)^2))
mae_lm  <- mean(abs(pred_lm  - test$age_estim))
r2_lm   <- cor(pred_lm, test$age_estim)^2
cat(sprintf("  RMSE=%.2f | MAE=%.2f | R²=%.4f\n", rmse_lm, mae_lm, r2_lm))

ggplot(data.frame(reel = test$age_estim, pred = pred_lm),
       aes(x = reel, y = pred)) +
  geom_point(alpha = 0.3, color = "#2E86AB") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Régression linéaire – Âge estimé : Réel vs Prédit",
       x = "Valeur réelle (ans)", y = "Valeur prédite (ans)")
save_fig("fig15_reg_lineaire_pred")

coef_df <- as.data.frame(summary(lm_model)$coefficients) %>%
  rownames_to_column("Variable") %>%
  filter(Variable != "(Intercept)") %>%
  mutate(Variable = fct_reorder(Variable, abs(Estimate)))
ggplot(coef_df,
       aes(x = Variable, y = Estimate,
           fill = ifelse(Estimate > 0, "Positif","Négatif"))) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("Positif"="#52B788","Négatif"="#E84855")) +
  labs(title = "Coefficients – Régression linéaire (Age estimé)",
       x = NULL, y = "Coefficient", fill = "Signe")
save_fig("fig16_coefficients_lm", height = 8)

# ── 5.2 Régression logistique – Arbres à abattre ────────────
# Cahier des charges : "On souhaite savoir quels sont les arbres
# à abattre. Faire une étude à l'aide de régression logistique."
cat("\n=== 5.2 Régression logistique – a_abattre ===\n")

df_log <- df %>%
  select(a_abattre, haut_tot, haut_tronc, tronc_diam,
         age_estim, fk_stadedev, feuillage, fk_situation) %>%
  drop_na() %>%
  mutate(across(c(fk_stadedev, feuillage, fk_situation), as.factor))

cat("Distribution variable cible :\n")
print(table(df_log$a_abattre))

idx_l     <- createDataPartition(df_log$a_abattre, p = 0.8, list = FALSE)
train_log <- df_log[ idx_l, ]
test_log  <- df_log[-idx_l, ]
# Équilibrage des classes par upsampling (classe minoritaire)
train_up  <- upSample(select(train_log, -a_abattre),
                      train_log$a_abattre, yname = "a_abattre")

glm_model  <- glm(a_abattre ~ ., data = train_up, family = binomial)
cat("\n── Résumé régression logistique ──\n")
print(summary(glm_model))

pred_prob  <- predict(glm_model, newdata = test_log, type = "response")
pred_class <- factor(ifelse(pred_prob > 0.5,"Oui","Non"),
                     levels = c("Non","Oui"))
cm <- confusionMatrix(pred_class, test_log$a_abattre, positive = "Oui")
cat("\n── Matrice de confusion ──\n")
print(cm)

# ── Vraie courbe ROC avec AUC (package pROC) ────────────────
roc_obj <- roc(response  = test_log$a_abattre,
               predictor = pred_prob,
               levels    = c("Non","Oui"),
               direction = "<")
auc_val <- as.numeric(auc(roc_obj))
cat(sprintf("\n  AUC (Area Under Curve) = %.4f\n", auc_val))

# Construction du data.frame pour ggplot
roc_df <- data.frame(
  fpr   = 1 - roc_obj$specificities,
  tpr   = roc_obj$sensitivities,
  seuil = roc_obj$thresholds
) %>% arrange(fpr, tpr)

# Seuil optimal (maximum de la statistique de Youden = Sens + Spec - 1)
j_stat    <- roc_df$tpr - roc_df$fpr
idx_opt   <- which.max(j_stat)
seuil_opt <- roc_df$seuil[idx_opt]
fpr_opt   <- roc_df$fpr[idx_opt]
tpr_opt   <- roc_df$tpr[idx_opt]

ggplot(roc_df, aes(x = fpr, y = tpr)) +
  geom_abline(linetype = "dashed", color = "grey50") +
  geom_ribbon(aes(ymin = 0, ymax = tpr), fill = "#E84855", alpha = 0.15) +
  geom_path(color = "#E84855", linewidth = 1.3) +
  geom_point(aes(x = fpr_opt, y = tpr_opt),
             color = "#2980B9", size = 4) +
  annotate("label",
           x = fpr_opt + 0.05, y = tpr_opt - 0.08,
           label = sprintf("Seuil optimal = %.2f\n(Sens=%.2f, Spec=%.2f)",
                           seuil_opt, tpr_opt, 1 - fpr_opt),
           fill = "white", color = "#2980B9",
           size = 3.5, fontface = "bold", hjust = 0) +
  annotate("label", x = 0.70, y = 0.10,
           label = sprintf("AUC = %.3f", auc_val),
           fill = "#E84855", color = "white",
           size = 6, fontface = "bold") +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0.01)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0.01)) +
  labs(title    = "Courbe ROC – Régression logistique (Arbres à abattre)",
       subtitle = "Plus la courbe est proche du coin supérieur gauche, meilleur est le modèle",
       x        = "Taux de faux positifs (1 - Spécificité)",
       y        = "Taux de vrais positifs (Sensibilité)") +
  coord_fixed(ratio = 1, xlim = c(0, 1), ylim = c(0, 1)) +
  theme_minimal(base_size = 13)
save_fig("fig17_roc_logistique", width = 9, height = 8)


# ── 5.3 Densité spatiale des arbres (avec fond de carte OSM) ─
cat("\n=== 5.3 Densité spatiale ===\n")

df_dens <- df %>% filter(!is.na(lon), !is.na(lat))

# Limites de la zone (avec marge)
lon_min <- min(df_dens$lon) - 0.01
lon_max <- max(df_dens$lon) + 0.01
lat_min <- min(df_dens$lat) - 0.007
lat_max <- max(df_dens$lat) + 0.007

# Téléchargement des tuiles OSM via package OpenStreetMap
# Même provider que les cartes Leaflet de la partie 3
fond_osm <- tryCatch({
  openmap(
    upperLeft  = c(lat_max, lon_min),
    lowerRight = c(lat_min, lon_max),
    zoom       = 14,
    type       = "osm",
    mergeTiles = TRUE
  )
}, error = function(e) {
  message(sprintf("[INFO] OSM indisponible (%s) — fallback sans fond.", e$message))
  NULL
})

if (!is.null(fond_osm)) {
  # Reprojection en WGS84 (même CRS que lon/lat)
  fond_wgs84 <- openproj(fond_osm)
  
  p_dens <- autoplot(fond_wgs84) +
    stat_density_2d(
      data = df_dens,
      aes(x = lon, y = lat, fill = after_stat(level)),
      geom = "polygon", alpha = 0.50, contour = TRUE, bins = 12
    ) +
    scale_fill_viridis_c(option = "magma", direction = -1,
                         name = "Densité") +
    geom_point(
      data = df_dens %>% filter(remarquable == "Oui"),
      aes(x = lon, y = lat),
      color = "cyan", size = 1.6, alpha = 0.9
    ) +
    labs(title    = "Densité des arbres à Saint-Quentin",
         subtitle = "Fond OpenStreetMap | points cyan = arbres remarquables",
         x = NULL, y = NULL) +
    theme_minimal(base_size = 13)
} else {
  cat("  [ATTENTION] Fond de carte indisponible — rendu sans fond\n")
  p_dens <- ggplot(df_dens, aes(x = lon, y = lat)) +
    stat_density_2d(aes(fill = after_stat(level)),
                    geom = "polygon", alpha = 0.8) +
    scale_fill_viridis_c(option = "magma", direction = -1) +
    geom_point(data = df_dens %>% filter(remarquable == "Oui"),
               aes(x = lon, y = lat),
               color = "cyan", size = 1, alpha = 0.8) +
    labs(title    = "Densité des arbres à Saint-Quentin",
         subtitle = "Points cyan = arbres remarquables",
         x = "Longitude", y = "Latitude", fill = "Densité") +
    coord_equal()
}
ggsave("outputs/figures/fig18_densite_spatiale.png", p_dens,
       width = 10, height = 8, dpi = 150)
cat("  → Sauvegardé : fig18_densite_spatiale.png\n")


# ── 5.4 Zones prioritaires à reboiser (analyse par quartier) ─
# construire un score composite par quartier qui combine
#  - un faible nombre d'arbres (+ priorité)
#  - une forte proportion d'arbres sénescents/vieux (+ priorité)
#  - une faible proportion de jeunes (+ priorité)
# Chaque composante est standardisée (z-score) puis sommée

cat("\n=== 5.4 Identification des zones à reboiser ===\n")

df_quartier_stats <- df %>%
  filter(!is.na(clc_quartier), !is.na(lon), !is.na(lat)) %>%
  group_by(clc_quartier) %>%
  summarise(
    nb_arbres     = n(),
    nb_jeunes     = sum(fk_stadedev %in% c("Jeune","Plantule"), na.rm = TRUE),
    nb_senescents = sum(fk_stadedev %in% c("Senescent","Vieux"), na.rm = TRUE),
    nb_a_abattre  = sum(a_abattre == "Oui", na.rm = TRUE),
    age_moyen     = mean(age_estim, na.rm = TRUE),
    lon_moy       = mean(lon),
    lat_moy       = mean(lat),
    .groups = "drop"
  ) %>%
  mutate(
    pct_jeunes      = nb_jeunes / nb_arbres * 100,
    pct_senescents  = nb_senescents / nb_arbres * 100,
    score_priorite  = as.numeric(scale(-nb_arbres)) +
      as.numeric(scale(pct_senescents)) +
      as.numeric(scale(-pct_jeunes)),
    priorite = cut(
      score_priorite,
      breaks = quantile(score_priorite, c(0, 0.33, 0.66, 1), na.rm = TRUE),
      labels = c("Faible","Moyenne","Haute"),
      include.lowest = TRUE
    )
  ) %>%
  arrange(desc(score_priorite))

cat("\n── Classement des quartiers par priorité de reboisement ──\n")
print(df_quartier_stats %>%
        select(clc_quartier, nb_arbres, pct_jeunes, pct_senescents,
               age_moyen, score_priorite, priorite))

# Graphique 1 : barplot des scores de priorité
ggplot(df_quartier_stats,
       aes(x = fct_reorder(clc_quartier, score_priorite),
           y = score_priorite, fill = priorite)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("Faible"="#27AE60",
                               "Moyenne"="#F39C12",
                               "Haute"="#C0392B")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  labs(title    = "Quartiers prioritaires pour le reboisement",
       subtitle = "Score élevé = peu d'arbres, beaucoup de sénescents, peu de jeunes",
       x = NULL, y = "Score de priorité (standardisé)",
       fill = "Priorité") +
  theme_minimal(base_size = 13)
save_fig("fig19_quartiers_a_reboiser", width = 11, height = 7)

# Graphique 2 : carte des priorités par quartier (avec fond OSM)
if (!is.null(fond_osm)) {
  p_prio <- autoplot(fond_wgs84) +
    geom_point(
      data = df_quartier_stats,
      aes(x = lon_moy, y = lat_moy,
          size = nb_arbres, color = priorite),
      alpha = 0.85
    ) +
    scale_color_manual(values = c("Faible"  = "#27AE60",
                                  "Moyenne" = "#F39C12",
                                  "Haute"   = "#C0392B"),
                       name = "Priorité") +
    scale_size_continuous(range = c(6, 22), name = "Nb arbres") +
    geom_text(
      data = df_quartier_stats,
      aes(x = lon_moy, y = lat_moy, label = clc_quartier),
      size = 2.8, color = "black", fontface = "bold",
      nudge_y = 0.003
    ) +
    labs(title    = "Priorité de reboisement par quartier",
         subtitle = "Rouge = priorité haute | Orange = moyenne | Vert = faible",
         x = NULL, y = NULL) +
    theme_minimal(base_size = 13)
  
  ggsave("outputs/figures/fig20_carte_priorites.png", p_prio,
         width = 12, height = 9, dpi = 150)
  cat("  → Sauvegardé : fig20_carte_priorites.png\n")
}


