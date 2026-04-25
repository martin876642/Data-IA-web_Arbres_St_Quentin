# Projet Big Data — Patrimoine Arboré de Saint-Quentin

Projet FISA4 — 2026 · Partie Big Data

Analyse et traitement du patrimoine arboré de la ville de Saint-Quentin à partir d'un jeu de données de **11 421 arbres**. Le pipeline R couvre les 6 étapes du cahier des charges : exploration, visualisation graphique, cartographie, corrélations, prédictions (régression linéaire et logistique) et export pour la partie IA.

\---

## Architecture du projet

```
.
├── R/
│   ├── 00\_setup.R                      # Packages + chargement du CSV brut
│   ├── 01\_exploration.R                # ÉTAPE 1 : exploration + nettoyage complet
│   ├── 02\_visualisation\_graphiques.R   # ÉTAPE 2 : histogrammes, boxplots, camemberts
│   ├── 03\_cartes.R                     # ÉTAPE 3 : 6 cartes Leaflet (HTML + PNG)
│   ├── 04\_correlations.R               # ÉTAPE 4 : matrice, scatter, chi² + mosaicplots
│   ├── 05\_predictions.R                # ÉTAPE 5 : régression linéaire + logistique
│   ├── 06\_export.R                     # ÉTAPE 6 : export CSV pour la partie IA
│   └── run\_all.R                       # Lance tout le pipeline dans l'ordre
│
├── data/
│   └── Patrimoine\_Arboré\_data.csv      # Jeu de données brut
│
├── outputs/
│   ├── figures/                        # Graphiques (PNG) et cartes (HTML)
│   ├── df\_nettoye.rds                  # Dataframe intermédiaire (après nettoyage)
│   └── export\_IA.csv                   # Fichier final pour la partie IA
│
├── docs/                               # Rapport, diagramme de Gantt, etc.
├── .gitignore
└── README.md
```

## Installation

### Prérequis

* R >= 4.1

### Installation des packages

```r
install.packages(c(
  "dplyr", "ggplot2", "tidyr", "corrplot",
  "leaflet", "sf", "caret",
  "forcats", "scales", "lubridate", "vcd",
  "htmlwidgets", "tibble", "RColorBrewer",
  "pROC", "OpenStreetMap"
))
```

### Données

Placer le fichier `Patrimoine\_Arboré\_data.csv` dans le dossier `data/`.

## Utilisation

### Lancer tout le pipeline d'un coup

```bash
Rscript R/run\_all.R
```

Ou depuis RStudio :

```r
source("R/run\_all.R")
```

### Exécuter une étape isolée

Chaque script est autonome à condition que les précédents aient été exécutés (car ils écrivent le fichier intermédiaire `outputs/df\_nettoye.rds`).

```r
source("R/01\_exploration.R")   # obligatoire en premier
source("R/03\_cartes.R")         # puis au choix
```

## Pipeline

### ÉTAPE 1 — Exploration + nettoyage (`01\_exploration.R`)

* Statistiques descriptives (moyennes, écarts-types, valeurs manquantes)
* **12 opérations de nettoyage** :

  * N1 : doublons
  * N2 : lignes < 5 champs renseignés
  * N3 : colonnes inutiles (métadonnées SIG, colonnes vides)
  * N4 : harmonisation casse des variables catégorielles
  * N5 : fautes orthographiques (basssin, Griourt, Chocquart…)
  * N6 : correction des quartiers selon le secteur
  * N7 : valeurs numériques ≤ 0 → NA
  * N8 : outliers extrêmes (règle 3×IQR)
  * N9 : imputation par moyenne de stade de développement
  * N10 : conversion des dates
  * N11 : création de la variable cible `a\_abattre`
  * N12 : filtrage spatial (aberrations géographiques)

### ÉTAPE 2 — Graphiques (`02\_visualisation\_graphiques.R`)

7 figures : répartition par stade, quartier, situation, distribution des hauteurs/diamètres, boxplots par stade, feuillage.

### ÉTAPE 3 — Cartes (`03\_cartes.R`)

6 cartes Leaflet interactives (HTML) + captures PNG via webshot2 :

1. Vue globale (stade + remarquables)
2. Densité par quartier
3. Vieillesse (stade de développement)
4. Diversité par famille d'essence
5. Arbres remarquables
6. Bilan carbone (flux de renouvellement)

### ÉTAPE 4 — Corrélations (`04\_correlations.R`)

* Matrice de corrélation Spearman (fig 9)
* Scatter plot hauteur × diamètre avec droite de régression + R² Pearson (fig 10)
* 4 tests du χ² avec mosaicplots et barplots empilés (fig 11–14)

### ÉTAPE 5 — Prédictions (`05\_predictions.R`)

* **5.1** — Régression linéaire multiple (âge estimé) + graphiques coefficients
* **5.2** — Régression logistique (arbres à abattre) + vraie courbe ROC avec AUC
* **5.3** — Densité spatiale avec fond OSM
* **5.4** — Score de priorité par quartier pour le reboisement (bonus)

### ÉTAPE 6 — Export (`06\_export.R`)

Génère `outputs/export\_IA.csv` pour la partie Intelligence Artificielle.

## Auteurs

Trinôme 11 FISA4 — Nantes 2026

## 

