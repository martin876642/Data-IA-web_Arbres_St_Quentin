# ============================================================
#  01_exploration.R — ÉTAPE 1
#  Exploration des données + nettoyage complet (N1..N12)
#
#  Entrée  : data/Patrimoine_Arboré_data.csv (via 00_setup.R)
#  Sortie  : outputs/df_nettoye.rds (objet df prêt pour les étapes suivantes)
# ============================================================
source("R/00_setup.R")

# ============================================================
#  ÉTAPE 1 – EXPLORATION DES DONNÉES
# ============================================================
cat("─────────────────────────────────────────────────────\n")
cat("ÉTAPE 1 : EXPLORATION DES DONNÉES\n")
cat("─────────────────────────────────────────────────────\n")

# ── 1.1 Aperçu général ─────────────────────────────────────
cat("\n── Structure du jeu de données ──\n")
str(df_raw)

cat("\n── 5 premières lignes ──\n")
print(head(df_raw, 5))

# ── 1.2 Statistiques descriptives (AVANT nettoyage) ────────
cat(names(df_raw), sep = "\n")
num_vars <- c("haut_tot", "haut_tronc", "tronc_diam", "age_estim")

cat("\n── Statistiques descriptives – AVANT nettoyage ──\n")
print(summary(df_raw[, num_vars]))

cat("\n── Écarts-types (avant nettoyage) ──\n")
sapply(df_raw[, num_vars], function(x) round(sd(x, na.rm = TRUE), 2)) |> print()

# ── 1.3 Valeurs manquantes (avant nettoyage) ───────────────
cat("\n── Valeurs manquantes (%) par colonne – avant nettoyage ──\n")
na_pct <- sapply(df_raw, function(x) round(mean(is.na(x)) * 100, 1))
na_pct[na_pct > 0] |> sort(decreasing = TRUE) |> print()

# ── 1.4 Types et valeurs uniques ───────────────────────────
cat("\n── Types de colonnes ──\n")
sapply(df_raw, class) |> print()

cat("\n── Valeurs uniques (variables catégorielles clés) ──\n")
cat_vars <- c("fk_arb_etat","fk_stadedev","fk_port","fk_situation",
              "fk_revetement","feuillage","remarquable","clc_quartier")
lapply(cat_vars, function(v) {
  cat(v, ":", paste(sort(unique(df_raw[[v]])), collapse = " | "), "\n")
})

# ── 1.5 Doublons ───────────────────────────────────────────
nb_dup <- sum(duplicated(df_raw))
cat("\n── Doublons détectés :", nb_dup, "\n")


# ============================================================
#  PHASE DE NETTOYAGE COMPLÈTE
# ============================================================
cat("\n─────────────────────────────────────────────────────\n")
cat("NETTOYAGE DES DONNÉES\n")
cat("─────────────────────────────────────────────────────\n")

df <- df_raw

# ── N1 · Suppression des doublons ──────────────────────────
n_avant <- nrow(df)
df <- df[!duplicated(df), ]
cat(sprintf("[N1] Doublons supprimés          : %d lignes retirées\n",
            n_avant - nrow(df)))

# ── N2 · Suppression des lignes avec < 5 champs renseignés ─
# On exclut les colonnes purement techniques (IDs, timestamps)
# pour ne compter que les champs informatifs sur l'arbre.
cols_techniques <- c("OBJECTID","created_date","created_user",
                     "last_edited_user","last_edited_date",
                     "GlobalID","CreationDate","Creator",
                     "EditDate","Editor","X","Y")
cols_info <- setdiff(names(df), cols_techniques)

n_avant <- nrow(df)
nb_renseignes <- apply(
  df[, cols_info], 1,
  function(r) sum(!is.na(r) & trimws(as.character(r)) != "")
)
df <- df[nb_renseignes >= 5, ]
cat(sprintf("[N2] Lignes < 5 champs info      : %d lignes retirées\n",
            n_avant - nrow(df)))

# ── N3 · Suppression des colonnes inutiles ─────────────────
# Colonnes de métadonnées de gestion (aucune info sur l'arbre),
# colonnes redondantes ou trop peu renseignées pour être exploitées.
cols_a_supprimer <- c(
  "GlobalID",          # identifiant technique interne
  "created_user",      # métadonnée de gestion SIG
  "last_edited_user",  # métadonnée de gestion SIG
  "CreationDate",      # doublon de created_date
  "EditDate",          # doublon de last_edited_date
  "last_edited_date",  # métadonnée de gestion SIG
  "Creator",           # métadonnée de gestion SIG
  "Editor",            # métadonnée de gestion SIG
  "src_geo",           # source géographique, peu variée
  "nomlatin",          # peu renseigné, redondant avec nomfrancais
  "fk_nomtech"         # très peu renseigné
)
# Supprimer uniquement les colonnes effectivement présentes
cols_a_supprimer <- intersect(cols_a_supprimer, names(df))
df <- df %>% select(-all_of(cols_a_supprimer))
cat(sprintf("[N3] Colonnes supprimées          : %d colonnes retirées\n",
            length(cols_a_supprimer)))
cat("      →", paste(cols_a_supprimer, collapse = ", "), "\n")
cat(sprintf("      Colonnes restantes : %d\n", ncol(df)))

# ── N4 · Harmonisation de la casse des variables quali ─────
df <- df %>%
  mutate(
    fk_stadedev  = tools::toTitleCase(tolower(trimws(fk_stadedev))),
    fk_port      = tools::toTitleCase(tolower(trimws(fk_port))),
    fk_arb_etat  = toupper(trimws(fk_arb_etat)),
    fk_situation = tools::toTitleCase(tolower(trimws(fk_situation))),
    feuillage    = trimws(feuillage),
    remarquable  = trimws(remarquable)
  )
cat("[N4] Harmonisation de la casse    : OK\n")

# ── N5 · Correction des fautes orthographiques ─────────────
df <- df %>%
  mutate(
    # "basssin" (3 s) → "bassin" dans clc_secteur
    clc_secteur = gsub("basssin", "bassin", clc_secteur,
                       ignore.case = TRUE),
    # "Griourt" → "Gricourt"
    clc_secteur = gsub("^Griourt$", "Gricourt", clc_secteur),
    # "Avenue Pierre Chocquart" → "Avenue Pierre Choquart"
    clc_secteur = gsub("Chocquart", "Choquart", clc_secteur),
    # Harmonisation casse : "square des marronniers" → forme officielle
    clc_secteur = gsub("^square des marronniers\\s*$",
                       "Square des Marronniers", clc_secteur,
                       ignore.case = TRUE),
    # "rue Hertz" → "Rue Hertz"
    clc_secteur = gsub("^rue Hertz$", "Rue Hertz", clc_secteur),
    # "Rue de la Fere" (sans accent) → "Rue de la Fère"
    clc_secteur = gsub("^Rue de la Fere$", "Rue de la Fère", clc_secteur),
    # Supprimer espaces en fin de chaîne
    clc_secteur = trimws(clc_secteur)
  )
cat("[N5] Fautes orthographiques        : basssin→bassin, Griourt→Gricourt,\n")
cat("                                     Chocquart→Choquart, casse, accents\n")

# ── N6 · Correction / complétion des quartiers ─────────────
# Pour chaque ligne, si le secteur correspond à un des secteurs
# de la liste, on attribue le quartier officiel — même si
# clc_quartier était déjà renseigné (correction d'erreurs).
secteur_to_quartier <- function(secteur) {
  if (is.na(secteur) || trimws(secteur) == "") return(NA_character_)
  mapping <- list(
    list(p = "Richelieu",                     q = "Quartier du Centre-Ville"),
    list(p = "Victor.?Hugo",                  q = "Quartier du Centre-Ville"),
    list(p = "H.tel.de.ville|Hôtel de ville", q = "Quartier du Centre-Ville"),
    list(p = "Isle",                           q = "Quartier du Faubourg d'Isle"),
    list(p = "R.publique",                     q = "Quartier du Vermandois"),
    list(p = "Chauss.e.Romaine|Chaussee romaine",
         q = "Quartier de l'Europe"),
    list(p = "Mend.s.France",                  q = "Quartier de l'Europe"),
    list(p = "Cousteau",                       q = "Quartier Saint-Martin - Oëstres"),
    list(p = "Faidherbe",                      q = "Quartier Remicourt"),
    list(p = "Kennedy",                        q = "Quartier de Neuville")
  )
  for (m in mapping) {
    if (grepl(m$p, secteur, ignore.case = TRUE)) return(m$q)
  }
  return(NA_character_)  # secteur non reconnu → on ne touche pas
}

n_avant_q <- sum(is.na(df$clc_quartier))
df <- df %>%
  rowwise() %>%
  mutate(
    quartier_derive = secteur_to_quartier(clc_secteur),
    clc_quartier    = ifelse(!is.na(quartier_derive),
                             quartier_derive, clc_quartier)
  ) %>%
  select(-quartier_derive) %>%
  ungroup()
n_apres_q <- sum(is.na(df$clc_quartier))
cat(sprintf("[N6] Quartiers corrigés/complétés : %d cellules NA résolues\n",
            n_avant_q - n_apres_q))

# ── N7 · Valeurs aberrantes numériques : 0 → NA ────────────
df <- df %>%
  mutate(
    haut_tot   = ifelse(haut_tot   <= 0, NA, haut_tot),
    haut_tronc = ifelse(haut_tronc <= 0, NA, haut_tronc),
    tronc_diam = ifelse(tronc_diam <= 0, NA, tronc_diam),
    age_estim  = ifelse(age_estim  <= 0, NA, age_estim)
  )
cat("[N7] Valeurs <= 0 (numériques)    : remplacées par NA\n")

# ── N8 · Valeurs aberrantes extrêmes : règle des 3×IQR ─────
outlier_to_na <- function(x) {
  q   <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  ifelse(x > q[2] + 3 * iqr | x < q[1] - 3 * iqr, NA, x)
}
df <- df %>% mutate(across(all_of(num_vars), outlier_to_na))
cat("[N8] Outliers 3×IQR               : remplacés par NA\n")

# ── N9 · Imputation par moyenne de catégorie ───────────────
# Pour chaque variable numérique, les NA sont remplacés par
# la moyenne des arbres du MÊME stade de développement.
# Si le stade est lui-même manquant, on utilise la moyenne globale.
impute_by_group <- function(data, col, group_col) {
  group_means <- data %>%
    filter(!is.na(.data[[group_col]]), !is.na(.data[[col]])) %>%
    group_by(.data[[group_col]]) %>%
    summarise(mean_val = mean(.data[[col]], na.rm = TRUE), .groups = "drop")
  
  global_mean <- mean(data[[col]], na.rm = TRUE)
  
  data %>%
    left_join(group_means, by = group_col) %>%
    mutate(
      !!col := ifelse(
        is.na(.data[[col]]),
        ifelse(is.na(mean_val), global_mean, mean_val),
        .data[[col]]
      )
    ) %>%
    select(-mean_val)
}

n_na_avant <- sapply(df[, num_vars], function(x) sum(is.na(x)))
for (v in num_vars) {
  df <- impute_by_group(df, v, "fk_stadedev")
}
n_na_apres <- sapply(df[, num_vars], function(x) sum(is.na(x)))

cat("[N9] Imputation par moyenne de stade de développement :\n")
for (v in num_vars) {
  imputed <- n_na_avant[v] - n_na_apres[v]
  cat(sprintf("       %-12s : %d valeurs imputées (reste %d NA)\n",
              v, imputed, n_na_apres[v]))
}

# ── N10 · Conversion des dates ───────────────────────────────
df <- df %>%
  mutate(
    dte_plantation = as.Date(substr(dte_plantation, 1, 10), "%Y/%m/%d"),
    dte_abattage   = as.Date(substr(dte_abattage,   1, 10), "%Y/%m/%d")
  )
cat("[N10] Dates converties             : dte_plantation, dte_abattage\n")

# ── N11 · Création de la variable cible binaire ────────────
df <- df %>%
  mutate(
    a_abattre = factor(
      ifelse(fk_arb_etat %in% c("ABATTU","SUPPRIMÉ","REMPLACÉ"),
             "Oui", "Non"),
      levels = c("Non","Oui")
    )
  )
cat("[N11] Variable 'a_abattre' créée  : OK\n")

# ── N12 · Filtrage spatial (aberrations géographiques) ────────
# On filtre directement sur les coordonnées brutes (Y en EPSG:3949) 
# pour optimiser le temps de calcul avant la conversion WGS84.
n_avant_n12 <- nrow(df)

# On conserve les lignes où Y <= 8298640 qui correpondent aux arbres d'une autre commune
df <- df %>%
  filter(is.na(Y) | Y <= 8298640)

cat(sprintf("[N12] Filtrage spatial            : %d lignes retirées (Y > 8298640)\n",
            n_avant_n12 - nrow(df)))

# ── Bilan du nettoyage ──────────────────────────────────────
cat("\n══ BILAN DU NETTOYAGE ═══════════════════════════════\n")
cat(sprintf("  Lignes brutes     : %d\n", nrow(df_raw)))
cat(sprintf("  Lignes nettoyées  : %d\n", nrow(df)))
cat(sprintf("  Lignes retirées   : %d (%.1f%%)\n",
            nrow(df_raw) - nrow(df),
            (nrow(df_raw) - nrow(df)) / nrow(df_raw) * 100))

cat("\n── Statistiques APRÈS nettoyage ──\n")
print(summary(df[, num_vars]))

cat("\n── Valeurs manquantes APRÈS nettoyage ──\n")
na_pct2 <- sapply(df, function(x) round(mean(is.na(x)) * 100, 1))
na_pct2[na_pct2 > 0] |> sort(decreasing = TRUE) |> print()



# ============================================================
#  SAUVEGARDE INTERMÉDIAIRE
#  Sauvegarde du dataframe nettoyé pour les étapes suivantes.
# ============================================================
saveRDS(df, file = "outputs/df_nettoye.rds")
cat("\n── Dataframe nettoyé sauvegardé dans outputs/df_nettoye.rds ──\n")
