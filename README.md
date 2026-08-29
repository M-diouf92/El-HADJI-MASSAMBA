# Analyse miroir du commerce extérieur — code R

Boîte à outils R pour l'**analyse miroir** : comparaison des importations
déclarées par un pays avec les exportations que ses partenaires déclarent
avoir réalisées vers ce même pays. L'écart entre les deux sources — après
correction méthodologique — constitue un indicateur de risque de
sous-facturation, de fausse déclaration ou de non-déclaration, et permet
de chiffrer la perte potentielle de recettes.

Le code n'utilise que **R base** : aucun package externe à installer.

## Contenu

| Fichier | Rôle |
|---|---|
| `R/analyse_miroir.R` | Bibliothèque de fonctions (appariement, écarts, scoring, pertes, graphiques) |
| `R/exemple_analyse_miroir.R` | Démonstration exécutable de bout en bout |
| `tests/test_analyse_miroir.R` | 28 tests unitaires |

## Prise en main

```bash
Rscript R/exemple_analyse_miroir.R   # démonstration complète
Rscript tests/test_analyse_miroir.R  # tests
```

Sur des données réelles :

```r
source("R/analyse_miroir.R")

imports <- read.csv("donnees/importations_sydonia.csv")  # déclarations nationales
miroir  <- read.csv("donnees/exports_partenaires.csv")   # UN Comtrade, miroir
tarifs  <- read.csv("donnees/tarifs.csv")                # sh6, taux_dd

res <- analyse_miroir(
  imports        = imports,
  miroir         = miroir,
  cles           = c("annee", "partenaire", "sh6"),
  coef_caf_fob   = 1.10,        # ou une table par partenaire
  taux_change    = 1/600,       # FCFA -> USD si les sources diffèrent
  tarifs         = tarifs,
  seuil_relatif  = 0.20,        # 20 % d'écart symétrique
  seuil_valeur   = 10000,       # matérialité minimale
  dossier_sortie = "resultats"
)

imprimer_synthese(res$synthese)
head(res$detail, 20)            # lignes triées par score de risque
```

## Structure des données attendue

**Importations déclarées** (valorisées CAF) :

| annee | partenaire | sh6 | valeur_caf | quantite |
|---|---|---|---|---|
| 2024 | Chine | 870321 | 12 500 000 | 4 200 |

**Miroir partenaires** (leurs exportations vers le pays, valorisées FOB) :

| annee | partenaire | sh6 | valeur_fob | quantite |
|---|---|---|---|---|
| 2024 | Chine | 870321 | 21 000 000 | 4 350 |

Des exemples au bon format sont générés dans `donnees/` par le script de
démonstration.

## Méthode

1. **Harmonisation** — même version du Système harmonisé des deux côtés,
   mêmes libellés de pays (codes ISO3 de préférence), même monnaie
   (`taux_change`), quantités de préférence en kg net.
2. **Correction CAF/FOB** — les importations sont valorisées CAF, les
   exportations partenaires FOB. Sans correction, un écart mécanique de
   6 à 20 % apparaît. `coefficient_caf_fob()` accepte un coefficient
   global ou différencié par partenaire (le fret pèse davantage sur les
   provenances lointaines ou pondéreuses).
3. **Appariement** — jointure **externe complète** : les flux présents
   d'un seul côté sont conservés, car un flux déclaré par le partenaire
   et absent des déclarations nationales est précisément le cas le plus
   significatif.
4. **Indicateurs d'écart** — écart en valeur, ratio miroir `M / X*`,
   écart relatif, **écart symétrique** borné dans `[-2 ; 2]` (comparable
   d'un produit à l'autre, il ne diverge pas quand un flux tend vers
   zéro), log-ratio, écarts de quantité et de **valeur unitaire**.
5. **Qualification** — au-delà d'un seuil d'écart et d'un seuil de
   matérialité : `sous-facturation présumée`, `sur-facturation présumée`,
   `flux non déclaré (présumé)`, `flux sans contrepartie miroir`,
   `cohérent`, `flux non matériel`.
6. **Persistance** — un écart qui se répète chaque année dans le même
   sens est un signal structurel ; un écart isolé relève plus souvent du
   décalage d'enregistrement entre exercices.
7. **Score de risque 0–100** — combinaison de rangs percentiles :
   enjeu financier 40 %, anormalité statistique (score *z* robuste
   médiane/MAD du log-ratio, calculé **à l'intérieur de chaque chapitre
   SH**) 25 %, anomalie de valeur unitaire 20 %, persistance 15 %.
8. **Perte de recettes** — sur la seule base manquante
   `max(0, X* − M)`, cascade TEC CEDEAO / UEMOA :
   `droits = base × (DD + RS + PCS + PC)` puis
   `TVA = (base + droits) × 18 %`. Taux paramétrables ; une table
   tarifaire par SH6 prime sur le taux par défaut.

### Sens de lecture des écarts

| Signe | Lecture |
|---|---|
| `M < X*` | Le pays déclare **moins** que ce que ses partenaires disent lui avoir vendu → présomption de sous-évaluation en douane, de non-déclaration ou de détournement de destination. **Perte de recettes.** |
| `M > X*` | Le pays déclare **plus** → présomption de sur-facturation (transfert de capitaux, fausse origine), ou réexportation mal imputée par le partenaire. |

## Précautions d'interprétation

Un écart miroir est un **indice de risque, pas une preuve d'infraction**.
Plusieurs causes purement statistiques produisent des écarts importants :

- **Pays de provenance ≠ pays d'origine** : le partenaire enregistre la
  destination finale connue de lui, la douane enregistre l'origine
  déclarée ; les plateformes de transbordement (Émirats, Pays-Bas,
  Singapour) créent des écarts massifs sans aucune fraude.
- **Réexportations et entrepôts** enregistrés différemment de part et
  d'autre.
- **Décalages temporels** : une expédition de décembre est exportée sur
  l'exercice N et importée sur N+1.
- **Seuils de déclaration** et traitement de la confidentialité
  statistique différents selon les administrations.
- **Versions du SH différentes** entre les deux sources.
- **Régimes suspensifs** (admission temporaire, transit) inclus ou non.

D'où l'importance des filtres de matérialité, du critère de persistance
pluriannuelle et de la comparaison des **valeurs unitaires**, qui isolent
mieux la sous-évaluation que le seul écart de valeur totale.

## Sorties produites

Dans le dossier `dossier_sortie` :

- `miroir_detail.csv` — toutes les lignes, triées par score de risque
- `miroir_top_alertes.csv` — 200 lignes prioritaires pour le ciblage
- `miroir_par_chapitre.csv`, `_par_partenaire.csv`, `_par_annee.csv`, `_par_produit.csv`
- `miroir_dispersion.png` — nuage log(M) vs log(X*) avec la première bissectrice
- `miroir_top_chapitres.png` — principaux écarts négatifs par chapitre SH
- `miroir_evolution.png` — évolution annuelle des deux séries

## Sources de données usuelles

- **Déclarations nationales** : SYDONIA World (base des déclarations en détail).
- **Miroir partenaires** : UN Comtrade, BACI (CEPII, déjà réconciliée),
  Eurostat Comext pour l'Union européenne.
