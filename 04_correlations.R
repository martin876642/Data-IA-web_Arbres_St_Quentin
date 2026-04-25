# ============================================================
#  04_correlations.R — ÉTAPE 4
#  Étude des corrélations (matrice Spearman, régression, chi²)
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
#  ÉTAPE 4 – ÉTUDE DES CORRÉLATIONS
# ============================================================
cat("─────────────────────────────────────────────────────\n")
cat("ÉTAPE 4 : ÉTUDE DES CORRÉLATIONS\n")
cat("─────────────────────────────────────────────────────\n")

# Variables utilisées tout au long de cette étape
# 4 quantitatives + 3 qualitatives encodées en ordinal
df_cor <- df %>%
  mutate(
    stade_num     = as.integer(factor(fk_stadedev,
                                      levels = c("Jeune","Plantule","Adulte","Vieux",
                                                 "Senescent","Sénescent"),
                                      ordered = TRUE)),
    feuillage_num = as.integer(factor(feuillage,
                                      levels = c("Conifère","Feuillu"))),
    situation_num = as.integer(factor(fk_situation,
                                      levels = c("Alignement","Groupe","Isolé")))
  ) %>%
  select(
    "Hauteur tot."  = haut_tot,
    "Hauteur tronc" = haut_tronc,
    "Diam. tronc"   = tronc_diam,
    "Âge estimé"    = age_estim,
  )


# ── 4.1 Matrice de corrélation (Spearman) ───────────────────
# Demi-matrice supérieure, coefficients en clair, sans étoiles
cor_matrix <- cor(df_cor, method = "spearman",
                  use = "pairwise.complete.obs")

cat("\n── Matrice de corrélation (Spearman) ──\n")
print(round(cor_matrix, 3))

png("outputs/figures/fig9_corrplot.png",
    width = 1000, height = 900, res = 120)
corrplot(
  cor_matrix,
  method       = "color",        # cases colorées
  type         = "upper",        # triangle supérieur uniquement
  diag         = TRUE,           # garder la diagonale
  order        = "original",     # ordre des variables tel quel
  addCoef.col  = "black",        # coefficients en noir
  number.cex   = 0.95,           # taille coefficients
  tl.col       = "black",        # couleur labels
  tl.srt       = 45,             # rotation labels
  tl.cex       = 1.0,            # taille labels
  cl.cex       = 0.9,            # taille légende
  col          = colorRampPalette(c("#C0392B","#FFFFFF","#2980B9"))(200),
  mar          = c(0, 0, 2, 0),
  title        = "Matrice de corrélation (Spearman)"
  # Pas de p.mat : pas d'étoiles ni de cases barrées
  # Pas d'addgrid par défaut : bordures fines automatiques
)
dev.off()
cat("  → Sauvegardé : fig9_corrplot.png\n")


# ── 4.2 Nuage de points : Hauteur totale × Diamètre du tronc ─
# Visualisation directe de la corrélation la plus forte identifiée
# dans la matrice ci-dessus. Ajout de la droite de régression
# linéaire, de son équation (y = ax + b), du coefficient directeur
# et du R² de Pearson sur le graphique.

df_scatter <- df %>%
  filter(!is.na(haut_tot), !is.na(tronc_diam), !is.na(fk_stadedev))

# Calcul de la régression linéaire et des statistiques
fit_lm  <- lm(haut_tot ~ tronc_diam, data = df_scatter)
coef_a  <- coef(fit_lm)[2]                      # coefficient directeur (pente)
coef_b  <- coef(fit_lm)[1]                      # ordonnée à l'origine
r_pear  <- cor(df_scatter$tronc_diam, df_scatter$haut_tot,
               method = "pearson")              # r de Pearson
r2_pear <- r_pear^2                             # R²

# Construction des étiquettes pour l'annotation
signe_b <- ifelse(coef_b >= 0, "+", "-")
eq_lab  <- sprintf("y = %.3f . x %s %.2f", coef_a, signe_b, abs(coef_b))
a_lab   <- sprintf("Coefficient directeur (a) : %.3f", coef_a)
r2_lab  <- sprintf("R² de Pearson : %.3f", r2_pear)
r_lab   <- sprintf("r de Pearson : %.3f", r_pear)

cat("\n── Régression linéaire : haut_tot ~ tronc_diam ──\n")
cat(sprintf("  Equation : %s\n", eq_lab))
cat(sprintf("  Coefficient directeur (a) : %.4f\n", coef_a))
cat(sprintf("  r de Pearson : %.4f\n", r_pear))
cat(sprintf("  R² de Pearson : %.4f\n", r2_pear))

# Graphique : nuage de points + droite de régression + annotations
p_scatter <- ggplot(df_scatter,
                    aes(x = tronc_diam, y = haut_tot)) +
  geom_point(aes(color = fk_stadedev), alpha = 0.4, size = 1.5) +
  geom_smooth(method = "lm", formula = y ~ x,
              color = "#C0392B", fill = "#F1948A",
              linewidth = 1.2, se = TRUE) +
  annotate("label",
           x = min(df_scatter$tronc_diam, na.rm = TRUE),
           y = max(df_scatter$haut_tot, na.rm = TRUE),
           hjust = 0, vjust = 1,
           label = paste(eq_lab, a_lab, r_lab, r2_lab, sep = "\n"),
           fill = "white", color = "black",
           size = 4, fontface = "bold",
           label.size = 0.5) +
  scale_color_brewer(palette = "Set1", na.value = "grey70") +
  labs(title    = "Hauteur totale en fonction du diamètre du tronc",
       subtitle = "Corrélation linéaire – droite de régression + R² de Pearson",
       x        = "Diamètre du tronc (cm)",
       y        = "Hauteur totale (m)",
       color    = "Stade dev.") +
  theme_minimal(base_size = 13)

ggsave("outputs/figures/fig10_scatter_hauteur_diametre.png",
       p_scatter, width = 11, height = 7, dpi = 150)
cat("  → Sauvegardé : fig10_scatter_hauteur_diametre.png\n")


# ── 4.3 Tests du chi² + mosaicplots ─────────────────────────
# Variables qualitatives uniquement. Pour chaque paire :
#  - tableau croisé
#  - test d'indépendance du chi² (avec simulation Monte-Carlo
#    au cas où certaines cellules auraient un effectif < 5)
#  - mosaicplot avec résidus de Pearson colorés
# Fonction : abréviation automatique des labels longs
abrev_label <- function(x, max_len = 10) {
  ifelse(nchar(x) > max_len,
         paste0(substr(x, 1, max_len - 1), "."),
         x)
}

# Fonction : filtre les catégories rares et renvoie le tableau nettoyé
prepare_table <- function(v1, v2, seuil_pct = 1.5) {
  data_clean <- df %>%
    filter(!is.na(.data[[v1]]), trimws(.data[[v1]]) != "",
           !is.na(.data[[v2]]), trimws(.data[[v2]]) != "") %>%
    mutate(across(all_of(c(v1, v2)),
                  ~ droplevels(factor(trimws(.)))))
  
  # Filtrer les modalités représentant moins de `seuil_pct`% des observations
  for (v in c(v1, v2)) {
    pct  <- prop.table(table(data_clean[[v]])) * 100
    keep <- names(pct)[pct >= seuil_pct]
    if (length(keep) >= 2) {
      data_clean <- data_clean[data_clean[[v]] %in% keep, ]
      data_clean[[v]] <- droplevels(factor(data_clean[[v]]))
    }
  }
  
  tbl <- table(data_clean[[v1]], data_clean[[v2]])
  tbl <- tbl[rowSums(tbl) > 0, colSums(tbl) > 0, drop = FALSE]
  
  # Abréviation des labels longs
  rownames(tbl) <- abrev_label(rownames(tbl))
  colnames(tbl) <- abrev_label(colnames(tbl))
  names(dimnames(tbl)) <- c(v1, v2)
  tbl
}

# ─ Fonction principale : chi² + mosaicplot ──────────────────
chi2_mosaic <- function(v1, v2, fichier, titre, seuil_pct = 1.5) {
  tbl <- prepare_table(v1, v2, seuil_pct)
  
  test <- chisq.test(tbl, simulate.p.value = TRUE, B = 2000)
  cat(sprintf("\n── Chi² : %s × %s (modalités >= %.1f%%) ──\n",
              v1, v2, seuil_pct))
  cat(sprintf("  Dimensions : %dx%d | N = %d\n",
              nrow(tbl), ncol(tbl), sum(tbl)))
  cat(sprintf("  χ² = %.2f | p = %.4f | %s\n",
              test$statistic, test$p.value,
              ifelse(test$p.value < 0.05,
                     "DÉPENDANTS (α=5%)", "indépendants")))
  
  if (nrow(tbl) >= 2 && ncol(tbl) >= 2) {
    sous_titre <- sprintf("chi2 = %.1f   |   p = %.4f   |   %s (modalites >= %.1f%%)",
                          test$statistic, test$p.value,
                          ifelse(test$p.value < 0.05,
                                 "variables DEPENDANTES",
                                 "variables independantes"),
                          seuil_pct)
    
    png(paste0("outputs/figures/", fichier),
        width = 1200, height = 850, res = 120)
    par(mar = c(7, 6, 5, 3))
    mosaicplot(
      tbl,
      main      = titre,
      sub       = sous_titre,
      shade     = TRUE,
      las       = 1,          # labels horizontaux (moins chevauchement)
      cex.axis  = 0.9,
      color     = TRUE,
      border    = "white",
      off       = 3,
      dir       = c("v","h")  # force découpe verticale puis horizontale
    )
    dev.off()
    cat("  → Sauvegardé :", fichier, "\n")
  }
  invisible(list(table = tbl, test = test))
}

# ─ Fonction alternative : barplot empilé 100% ───────────────
# Utile quand une modalité est très déséquilibrée (ex : remarquable).
# Affiche les PROPORTIONS plutôt que les effectifs absolus,
# ce qui permet de voir l'écart structurel même avec peu d'effectifs.
chi2_barplot <- function(v1, v2, fichier, titre) {
  tbl <- prepare_table(v1, v2, seuil_pct = 0)  # pas de filtre ici
  
  test <- chisq.test(tbl, simulate.p.value = TRUE, B = 2000)
  cat(sprintf("\n── Chi² : %s × %s ──\n", v1, v2))
  cat(sprintf("  Dimensions : %dx%d | N = %d\n",
              nrow(tbl), ncol(tbl), sum(tbl)))
  cat(sprintf("  χ² = %.2f | p = %.4f | %s\n",
              test$statistic, test$p.value,
              ifelse(test$p.value < 0.05,
                     "DÉPENDANTS (α=5%)", "indépendants")))
  
  # Transformation en data.frame pour ggplot
  df_bar <- as.data.frame(tbl) %>%
    setNames(c("var1","var2","effectif")) %>%
    group_by(var1) %>%
    mutate(pct = effectif / sum(effectif) * 100) %>%
    ungroup()
  
  p <- ggplot(df_bar, aes(x = var1, y = pct, fill = var2)) +
    geom_col(position = "stack", width = 0.65) +
    geom_text(aes(label = ifelse(pct > 3,
                                 sprintf("%.1f%%\n(n=%d)", pct, effectif),
                                 "")),
              position = position_stack(vjust = 0.5),
              size = 3.5, color = "white", fontface = "bold") +
    scale_fill_brewer(palette = "Set2") +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      title = titre,
      subtitle = sprintf("chi2 = %.1f   |   p = %.4f   |   %s",
                         test$statistic, test$p.value,
                         ifelse(test$p.value < 0.05,
                                "variables DEPENDANTES",
                                "variables independantes")),
      x = v1, y = "Proportion",
      fill = v2
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.subtitle = element_text(
        color = ifelse(test$p.value < 0.05, "#C0392B","#27AE60"),
        face = "bold"),
      legend.position = "right"
    )
  
  ggsave(paste0("outputs/figures/", fichier), p,
         width = 10, height = 7, dpi = 150)
  cat("  → Sauvegardé :", fichier, "\n")
  invisible(list(table = tbl, test = test))
}

# fig 11 : mosaicplot classique (bon équilibre d'effectifs)
chi2_mosaic("fk_stadedev",  "feuillage",
            "fig11_mosaic_stade_feuillage.png",
            "Stade de développement × Feuillage",
            seuil_pct = 1.5)

# fig 12 : mosaicplot avec filtre plus strict (Sénescent/Vieux très rares)
chi2_mosaic("fk_situation", "fk_stadedev",
            "fig12_mosaic_situation_stade.png",
            "Situation × Stade de développement",
            seuil_pct = 2.0)

# fig 13 : BARPLOT EMPILÉ 100% (arbres remarquables < 1% — mosaicplot illisible)
chi2_barplot("feuillage", "remarquable",
             "fig13_barplot_remarquable_feuillage.png",
             "Arbres remarquables selon le type de feuillage")

# fig 14 : BARPLOT EMPILÉ 100% (6 états dont 4 très minoritaires)
chi2_barplot("fk_stadedev", "fk_arb_etat",
             "fig14_barplot_etat_stade.png",
             "État de l'arbre selon le stade de développement")

