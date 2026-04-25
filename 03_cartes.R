# ============================================================
#  03_cartes.R — ÉTAPE 3
#  Visualisation des données sur une carte
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
#  ÉTAPE 3 – VISUALISATION SUR UNE CARTE
# ============================================================
cat("─────────────────────────────────────────────────────\n")
cat("ÉTAPE 3 : VISUALISATION SUR UNE CARTE\n")
cat("─────────────────────────────────────────────────────\n")

# ── Conversion EPSG:3949 (RGF93/CC49) → WGS84 ──────────────
df_sf    <- st_as_sf(df, coords = c("X","Y"), crs = 3949)
df_wgs84 <- st_transform(df_sf, crs = 4326)
coords   <- st_coordinates(df_wgs84)
df$lon   <- coords[,1]
df$lat   <- coords[,2]

cat(sprintf("Coordonnées converties : lon [%.4f → %.4f] | lat [%.4f → %.4f]\n",
            min(df$lon), max(df$lon), min(df$lat), max(df$lat)))

df_map <- df %>% filter(!is.na(lon), !is.na(lat))

# ── Fonction : sauvegarde carte HTML + PNG ──────────────────
save_map <- function(map_obj, nom_base) {
  path_html <- paste0("outputs/figures/", nom_base, ".html")
  path_png  <- paste0("outputs/figures/", nom_base, ".png")
  
  # 1) HTML interactif
  saveWidget(map_obj, path_html, selfcontained = TRUE)
  cat("  → HTML :", nom_base, ".html\n")
  
  # 2) PNG via webshot2 (prioritaire) ou webshot
  if (requireNamespace("webshot2", quietly = TRUE)) {
    webshot2::webshot(path_html, path_png,
                      vwidth = 1200, vheight = 800, delay = 2)
    cat("  → PNG  :", nom_base, ".png  (webshot2)\n")
  } else if (requireNamespace("webshot", quietly = TRUE)) {
    webshot::webshot(path_html, path_png,
                     vwidth = 1200, vheight = 800, delay = 2)
    cat("  → PNG  :", nom_base, ".png  (webshot)\n")
  } else {
    # Fallback statique ggplot si aucun webshot disponible
    warning("webshot/webshot2 absent — fallback PNG statique pour ", nom_base)
    p <- ggplot(df_map, aes(x = lon, y = lat)) +
      geom_point(aes(color = remarquable), size = 0.7, alpha = 0.5) +
      scale_color_manual(values = c("Non"="#2E86AB","Oui"="#E84855"),
                         na.value = "grey60") +
      coord_quickmap() +
      labs(title = nom_base, color = "Remarquable") +
      theme_minimal()
    ggsave(path_png, p, width = 12, height = 8, dpi = 150)
    cat("  → PNG statique (fallback) :", nom_base, ".png\n")
  }
}
# ── Carte 2 : Densité par quartier ──────────────────────────
# Cercles proportionnels + dégradé vert → orange → rouge
df_q <- df %>%
  filter(!is.na(clc_quartier)) %>%
  count(clc_quartier, name = "nb_arbres")

df_q_loc <- df %>%
  filter(!is.na(clc_quartier), !is.na(lon)) %>%
  group_by(clc_quartier) %>%
  summarise(lon = mean(lon), lat = mean(lat), .groups = "drop") %>%
  left_join(df_q, by = "clc_quartier")

pal_dens <- colorNumeric(
  palette = c("#A8E6CF","#56C596","#1A936F","#F4A261","#E63946"),
  domain  = df_q_loc$nb_arbres
)

carte2 <- leaflet(df_q_loc) %>%
  addProviderTiles("OpenStreetMap.Mapnik",
                   options = providerTileOptions(opacity = 0.85)) %>%
  addCircleMarkers(
    lng = ~lon, lat = ~lat,
    radius      = ~rescale(nb_arbres, to = c(15, 50)),
    fillColor   = ~pal_dens(nb_arbres),
    color       = "white",
    weight      = 0.5,
    fillOpacity = 0.80,
    opacity     = 1,
    popup = ~paste0(
      "<b style='font-size:13px'>", clc_quartier, "</b>",
      "<hr style='margin:4px'>",
      "<b>Nombre d'arbres :</b> ", nb_arbres
    ),
    label = ~paste0(clc_quartier, " : ", nb_arbres, " arbres"),
    labelOptions = labelOptions(
      style     = list("font-weight" = "bold", "font-size" = "12px"),
      direction = "top", offset = c(0, -10)
    )
  ) %>%
  addLegend("bottomright",
            pal       = pal_dens,
            values    = ~nb_arbres,
            title     = "Nombre d'arbres<br>par quartier",
            labFormat = labelFormat(suffix = " arbres"),
            opacity   = 0.9) %>%
  setView(lng = mean(df_map$lon), lat = mean(df_map$lat), zoom = 13)

save_map(carte2, "carte2_densite_quartiers")

# ── Carte 3 : Vieillesse – Stade de développement ───────────
# Objectif : visualiser la maturité du patrimoine pour anticiper
# les besoins de renouvellement.

pal_stade_age <- colorFactor(
  palette = c(
    "Jeune"      = "#3498DB",   # bleu   – jeune
    "Plantule"   = "#85C1E9",   # bleu clair – plantule
    "Adulte"     = "#27AE60",   # vert   – adulte
    "Vieux"      = "#E67E22",   # orange – vieux
    "Senescent"  = "#C0392B",   # rouge  – sénescent
    "Sénescent"  = "#C0392B"    # variante accentuée
  ),
  domain   = df_map$fk_stadedev,
  na.color = "#CCCCCC"
)

# Ordre et couleurs pour la légende (du plus jeune au plus vieux)
stade_ordre  <- c("Plantule","Jeune","Adulte","Vieux","Senescent","Sénescent")
stade_colors <- c("#85C1E9","#3498DB","#27AE60","#E67E22","#C0392B","#C0392B")
stades_presents <- stade_ordre[stade_ordre %in% unique(na.omit(df_map$fk_stadedev))]
colors_presents  <- stade_colors[stade_ordre %in% unique(na.omit(df_map$fk_stadedev))]

carte3 <- leaflet(df_map %>% filter(!is.na(fk_stadedev))) %>%
  addProviderTiles("OpenStreetMap.Mapnik",
                   options = providerTileOptions(opacity = 0.85)) %>%
  addCircleMarkers(
    lng         = ~lon, lat = ~lat,
    radius      = 5,
    fillColor   = ~pal_stade_age(fk_stadedev),
    color       = ~pal_stade_age(fk_stadedev),
    weight      = 0.5,
    fillOpacity = 0.75,
    opacity     = 0.9,
    popup = ~paste0(
      "<b style='color:#2c3e50'>", nomfrancais, "</b><br>",
      "<i style='color:#7f8c8d'>", clc_secteur, "</i>",
      "<hr style='margin:4px'>",
      "<b>Stade :</b> <span style='font-size:13px'>", fk_stadedev, "</span><br>",
      "<b>Quartier :</b> ", clc_quartier, "<br>",
      "<b>Age estimé :</b> ", round(age_estim, 0), " ans<br>",
      "<b>Hauteur :</b> ", round(haut_tot, 1), " m"
    )
  ) %>%
  addLegend(
    position = "bottomright",
    colors   = colors_presents,
    labels   = stades_presents,
    title    = "Stade de développement<br><small>Bleu → Vert → Rouge</small>",
    opacity  = 0.9
  ) %>%
  setView(lng = mean(df_map$lon), lat = mean(df_map$lat), zoom = 13)

save_map(carte3, "carte3_vieillesse_stade")


# ── Carte 4 : Diversité – Par essence (nomfrancais) ─────────
# Objectif : analyser la biodiversité urbaine.
# On regroupe les espèces en grandes familles pour la lisibilité.

df_map <- df_map %>%
  mutate(
    famille = case_when(
      grepl("chêne|Chêne",          nomfrancais, ignore.case=TRUE) ~ "Chênes",
      grepl("érable|Érable|erable",  nomfrancais, ignore.case=TRUE) ~ "Érables",
      grepl("tilleul|Tilleul",       nomfrancais, ignore.case=TRUE) ~ "Tilleuls",
      grepl("platane|Platane",       nomfrancais, ignore.case=TRUE) ~ "Platanes",
      grepl("peuplier|Peuplier",     nomfrancais, ignore.case=TRUE) ~ "Peupliers",
      grepl("frêne|Frêne|frene",     nomfrancais, ignore.case=TRUE) ~ "Frênes",
      grepl("marronnier|Marronnier", nomfrancais, ignore.case=TRUE) ~ "Marronniers",
      grepl("cerisier|Cerisier|prunus|Prunus|merisier",
            nomfrancais, ignore.case=TRUE) ~ "Cerisiers/Prunus",
      grepl("pin|Pin|sapin|Sapin|épicéa|epicea|cèdre|cedre",
            nomfrancais, ignore.case=TRUE) ~ "Conifères",
      grepl("bouleau|Bouleau",       nomfrancais, ignore.case=TRUE) ~ "Bouleaux",
      grepl("aulne|Aulne|aune",      nomfrancais, ignore.case=TRUE) ~ "Aulnes",
      grepl("saule|Saule",           nomfrancais, ignore.case=TRUE) ~ "Saules",
      grepl("charme|Charme",         nomfrancais, ignore.case=TRUE) ~ "Charmes",
      is.na(nomfrancais)                                            ~ "Inconnu",
      TRUE                                                          ~ "Autres"
    )
  )

familles_top <- df_map %>%
  filter(famille != "Inconnu") %>%
  count(famille, sort = TRUE) %>%
  head(12) %>%
  pull(famille)

pal_diversite <- colorFactor(
  palette  = c("#8B4513","#E74C3C","#27AE60","#F39C12","#2980B9",
               "#8E44AD","#D35400","#F1C40F","#1ABC9C","#2ECC71",
               "#3498DB","#E67E22","#95A5A6","#7F8C8D"),
  domain   = c(familles_top, "Autres", "Inconnu"),
  na.color = "#CCCCCC"
)

carte4 <- leaflet(df_map %>% filter(!is.na(lon))) %>%
  addProviderTiles("OpenStreetMap.Mapnik",
                   options = providerTileOptions(opacity = 0.85)) %>%
  addCircleMarkers(
    lng         = ~lon, lat = ~lat,
    radius      = 4,
    fillColor   = ~pal_diversite(famille),
    color       = ~pal_diversite(famille),
    weight      = 0.5,
    fillOpacity = 0.70,
    opacity     = 0.85,
    popup = ~paste0(
      "<b style='color:#2c3e50'>", nomfrancais, "</b><br>",
      "<b>Famille :</b> <span style='color:",
      pal_diversite(famille), "'>&#9632; ", famille, "</span><br>",
      "<i style='color:#7f8c8d'>", clc_secteur, "</i>",
      "<hr style='margin:4px'>",
      "<b>Quartier :</b> ", clc_quartier, "<br>",
      "<b>Stade :</b> ", fk_stadedev, "<br>",
      "<b>Hauteur :</b> ", round(haut_tot,1), " m"
    )
  ) %>%
  addLegend("bottomright", pal = pal_diversite,
            values  = ~famille,
            title   = "Famille d'essence",
            opacity = 0.9) %>%
  setView(lng = mean(df_map$lon), lat = mean(df_map$lat), zoom = 13)

save_map(carte4, "carte4_diversite_essences")


# ── Carte 5 : Arbres Remarquables ───────────────────────────
# Objectif : mettre en valeur le patrimoine d'exception.
# Fond sobre, symboles larges et bien visibles.

df_remarquable <- df_map %>% filter(remarquable == "Oui" | remarquable == "1")
df_ordinaire   <- df_map %>% filter(is.na(remarquable) |
                                      !(remarquable %in% c("Oui","1")))

carte5 <- leaflet() %>%
  addProviderTiles("OpenStreetMap.Mapnik",
                   options = providerTileOptions(opacity = 0.85)) %>%
  # Tous les arbres ordinaires en gris discret (contexte)
  addCircleMarkers(
    data        = df_ordinaire,
    lng = ~lon, lat = ~lat,
    radius      = 2,
    fillColor   = "#AAAAAA",
    color       = "#AAAAAA",
    weight      = 0,
    fillOpacity = 0.30,
    opacity     = 0.30
  ) %>%
  # Arbres remarquables : grande étoile visible
  addCircleMarkers(
    data        = df_remarquable,
    lng = ~lon, lat = ~lat,
    radius      = 12,
    fillColor   = "#F1C40F",
    color       = "#E67E22",
    weight      = 3,
    fillOpacity = 0.90,
    opacity     = 1,
    popup = ~paste0(
      "<b style='color:#e67e22;font-size:15px'>",
      "&#9733; ", nomfrancais, "</b><br>",
      "<i style='color:#7f8c8d'>", clc_secteur, "</i>",
      "<hr style='margin:4px'>",
      "<b>Quartier :</b> ", clc_quartier, "<br>",
      "<b>Stade :</b> ",    fk_stadedev,  "<br>",
      "<b>Situation :</b> ", fk_situation, "<br>",
      "<b>Hauteur :</b> ",   round(haut_tot,1),   " m<br>",
      "<b>Diamètre :</b> ",  round(tronc_diam,1), " cm<br>",
      "<b>Age estimé :</b> ", round(age_estim,0),  " ans"
    ),
    label = ~nomfrancais,
    labelOptions = labelOptions(
      style     = list("font-weight"="bold","font-size"="12px",
                       "color"="#e67e22"),
      direction = "top", offset = c(0,-14)
    )
  ) %>%
  addLegend("bottomright",
            colors  = c("#AAAAAA","#F1C40F"),
            labels  = c(
              paste0("Arbres ordinaires (", nrow(df_ordinaire), ")"),
              paste0("Arbres remarquables (", nrow(df_remarquable), ")")
            ),
            title   = "Patrimoine arboré",
            opacity = 0.9) %>%
  setView(lng = mean(df_map$lon), lat = mean(df_map$lat), zoom = 13)

save_map(carte5, "carte5_arbres_remarquables")


# ── Carte 1 : Bilan Carbone – Flux de renouvellement ────────
# Objectif : visualiser la dynamique pertes vs gains.
# Rouge = supprimé/abattu | Jaune = remplacé | Vert = récent EN PLACE

annee_recente <- 2020   # seuil "plantation récente"

df_bilan <- df %>%
  filter(!is.na(lon), !is.na(lat)) %>%
  mutate(
    statut_bilan = case_when(
      fk_arb_etat %in% c("SUPPRIMÉ","ABATTU") ~ "Supprimé / Abattu",
      fk_arb_etat == "REMPLACÉ"               ~ "Remplacé",
      fk_arb_etat == "EN PLACE" &
        !is.na(dte_plantation) &
        as.integer(format(dte_plantation, "%Y")) >= annee_recente
      ~ paste0("En place (>= ", annee_recente,")"),
      fk_arb_etat == "EN PLACE"               ~ "En place (ancienne plantation)",
      TRUE                                     ~ "Autre / inconnu"
    )
  )

# Noms de niveaux construits de façon cohérente
niveau_recen <- paste0("En place (>= ", annee_recente, ")")
niveaux_bilan <- c("Supprimé / Abattu", "Remplacé",
                   niveau_recen,
                   "En place (ancienne plantation)",
                   "Autre / inconnu")
couleurs_bilan <- c("#E74C3C","#F1C40F","#27AE60","#85C1E9","#AAAAAA")

pal_bilan <- colorFactor(
  palette  = couleurs_bilan,
  levels   = niveaux_bilan,
  na.color = "#DDDDDD"
)

# Statistiques pour le titre de la légende
n_supp  <- sum(df_bilan$statut_bilan == "Supprimé / Abattu")
n_remp  <- sum(df_bilan$statut_bilan == "Remplacé")
n_recen <- sum(df_bilan$statut_bilan == niveau_recen)

carte1 <- leaflet(df_bilan) %>%
  addProviderTiles("OpenStreetMap.Mapnik",
                   options = providerTileOptions(opacity = 0.85)) %>%
  # Ordre d'affichage : arbres EN PLACE d'abord (fond), puis pertes et remplacements
  addCircleMarkers(
    data        = df_bilan %>%
      filter(statut_bilan == "En place (ancienne plantation)"),
    lng = ~lon, lat = ~lat,
    radius=3, fillColor="#85C1E9", color="#85C1E9",
    weight=0, fillOpacity=0.40, opacity=0.40
  ) %>%
  addCircleMarkers(
    data        = df_bilan %>%
      filter(statut_bilan == niveau_recen),
    lng = ~lon, lat = ~lat,
    radius=5, fillColor="#27AE60", color="#1E8449",
    weight=1, fillOpacity=0.85, opacity=1,
    popup = ~paste0(
      "<b style='color:#27ae60'>", nomfrancais, "</b><br>",
      "<b>Planté en :</b> ", format(dte_plantation, "%Y"), "<br>",
      "<b>Quartier :</b> ", clc_quartier
    )
  ) %>%
  addCircleMarkers(
    data        = df_bilan %>% filter(statut_bilan == "Remplacé"),
    lng = ~lon, lat = ~lat,
    radius=6, fillColor="#F1C40F", color="#D4AC0D",
    weight=1.5, fillOpacity=0.90, opacity=1,
    popup = ~paste0(
      "<b style='color:#d4ac0d'>", nomfrancais, "</b><br>",
      "<b>Statut :</b> Remplacé<br>",
      "<b>Quartier :</b> ", clc_quartier
    )
  ) %>%
  addCircleMarkers(
    data        = df_bilan %>%
      filter(statut_bilan == "Supprimé / Abattu"),
    lng = ~lon, lat = ~lat,
    radius=5, fillColor="#E74C3C", color="#C0392B",
    weight=1.5, fillOpacity=0.85, opacity=1,
    popup = ~paste0(
      "<b style='color:#c0392b'>", nomfrancais, "</b><br>",
      "<b>Statut :</b> ", fk_arb_etat, "<br>",
      ifelse(!is.na(dte_abattage),
             paste0("<b>Abattu le :</b> ", format(dte_abattage, "%d/%m/%Y"), "<br>"),
             ""),
      "<b>Quartier :</b> ", clc_quartier
    )
  ) %>%
  addLegend("bottomright",
            colors = c("#E74C3C","#F1C40F","#27AE60","#85C1E9"),
            labels = c(
              paste0("Supprimé / Abattu (", n_supp, ")"),
              paste0("Remplacé (", n_remp, ")"),
              paste0("En place >= ", annee_recente, " (", n_recen, ")"),  # nolint
              paste0("En place (avant ", annee_recente, ")")
            ),
            title   = "Flux de renouvellement",
            opacity = 0.9) %>%
  setView(lng = mean(df_map$lon), lat = mean(df_map$lat), zoom = 13)

save_map(carte1, "carte1_bilan_renouvellement")

cat("\n── 1 cartes exportées (HTML + PNG) dans outputs/figures/ ──\n")


# ============================================================
#  SAUVEGARDE du dataframe enrichi (lon, lat)
#  Les étapes 4 et 5 en ont besoin
# ============================================================
saveRDS(df, file = "outputs/df_nettoye.rds")
cat("\n── Dataframe mis à jour (avec lon/lat) dans outputs/df_nettoye.rds ──\n")
