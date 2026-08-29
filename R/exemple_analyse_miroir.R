# =====================================================================
# exemple_analyse_miroir.R
# ---------------------------------------------------------------------
# Demonstration de bout en bout de la chaine d'analyse miroir.
#
# Lancement :  Rscript R/exemple_analyse_miroir.R
#
# Le script simule un jeu de donnees (avec des anomalies connues,
# volontairement injectees), execute l'analyse complete, ecrit les
# livrables dans resultats/ et verifie que les anomalies injectees
# ressortent bien en tete du classement de risque.
# =====================================================================

racine <- if (basename(getwd()) == "R") ".." else "."
source(file.path(racine, "R", "analyse_miroir.R"), encoding = "UTF-8")

# ---------------------------------------------------------------------
# 1. Sources de donnees
# ---------------------------------------------------------------------
# En production, remplacer ce bloc par la lecture des deux sources :
#
#   imports <- read.csv("donnees/importations_sydonia.csv")   # SYDONIA World
#   miroir  <- read.csv("donnees/exports_partenaires.csv")    # UN Comtrade
#
# Structure attendue :
#   imports : annee, partenaire, sh6, valeur_caf, quantite
#   miroir  : annee, partenaire, sh6, valeur_fob,  quantite
#
# Points de vigilance sur les donnees reelles :
#   - meme version du Systeme harmonise des deux cotes (HS2017, HS2022...) ;
#   - memes libelles de pays (normaliser via les codes ISO3) ;
#   - meme monnaie (sinon renseigner `taux_change`) ;
#   - cote miroir, retenir le flux "exportations vers le Senegal", pas les
#     reexportations, sauf a vouloir les analyser separement.
# ---------------------------------------------------------------------

message("1/5 - Simulation du jeu de donnees de demonstration...")
sim <- simuler_donnees_miroir(n_produits = 60, annees = 2020:2024, graine = 42)
imports <- sim$imports
miroir <- sim$miroir
tarifs <- sim$tarifs

dir.create(file.path(racine, "donnees"), showWarnings = FALSE)
write.csv(imports, file.path(racine, "donnees", "exemple_importations.csv"),
          row.names = FALSE)
write.csv(miroir, file.path(racine, "donnees", "exemple_exports_partenaires.csv"),
          row.names = FALSE)
write.csv(tarifs, file.path(racine, "donnees", "exemple_tarifs.csv"),
          row.names = FALSE)

cat(sprintf("   importations : %d lignes | miroir : %d lignes\n",
            nrow(imports), nrow(miroir)))

# ---------------------------------------------------------------------
# 2. Coefficients CAF/FOB differencies par partenaire
# ---------------------------------------------------------------------
# Le fret et l'assurance pesent plus lourd sur les provenances lointaines
# ou pondereuses. Un coefficient unique de 1,10 est un point de depart ;
# l'ideal est de le calibrer sur les donnees de fret reellement declarees.
coefs <- data.frame(
  partenaire = c("Chine", "Inde", "Emirats arabes unis", "Turquie", "France", "Espagne"),
  coef_caf_fob = c(1.14, 1.13, 1.11, 1.09, 1.07, 1.07),
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------------
# 3. Analyse complete
# ---------------------------------------------------------------------
message("2/5 - Appariement, qualification des ecarts et scoring...")
res <- analyse_miroir(
  imports = imports,
  miroir = miroir,
  cles = c("annee", "partenaire", "sh6"),
  coef_caf_fob = coefs,
  taux_change = 1,          # 1/600 pour convertir des FCFA en USD
  tarifs = tarifs,
  seuil_relatif = 0.20,     # 20 % d'ecart symetrique
  seuil_valeur = 10000,     # on ignore les flux inferieurs a 10 000 USD
  dossier_sortie = file.path(racine, "resultats"),
  monnaie = "USD"
)

d <- res$detail

# ---------------------------------------------------------------------
# 4. Restitution
# ---------------------------------------------------------------------
message("3/5 - Synthese...")
imprimer_synthese(res$synthese, monnaie = "USD")

cat("--- Repartition des qualifications ---\n")
print(sort(table(d$qualification), decreasing = TRUE))

cat("\n--- Repartition des niveaux de risque ---\n")
print(table(d$niveau_risque, useNA = "ifany"))

cat("\n--- 10 lignes les plus a risque ---\n")
colonnes <- c("annee", "partenaire", "sh6", "valeur_import", "valeur_miroir_caf",
              "ecart_valeur", "ecart_symetrique", "perte_totale",
              "score_risque", "qualification")
top <- head(d[, colonnes], 10)
top$valeur_import <- round(top$valeur_import)
top$valeur_miroir_caf <- round(top$valeur_miroir_caf)
top$ecart_valeur <- round(top$ecart_valeur)
top$ecart_symetrique <- round(top$ecart_symetrique, 2)
top$perte_totale <- round(top$perte_totale)
print(top, row.names = FALSE)

cat("\n--- 10 flux apparies les plus sous-factures (hors flux non declares) ---\n")
sf <- d[d$qualification == "sous-facturation presumee", colonnes]
sf <- head(sf, 10)
sf$valeur_import <- round(sf$valeur_import)
sf$valeur_miroir_caf <- round(sf$valeur_miroir_caf)
sf$ecart_valeur <- round(sf$ecart_valeur)
sf$ecart_symetrique <- round(sf$ecart_symetrique, 2)
sf$perte_totale <- round(sf$perte_totale)
print(sf[, setdiff(colonnes, "qualification")], row.names = FALSE)

cat("\n--- 8 chapitres SH les plus deficitaires ---\n")
ch <- head(res$agregats$par_chapitre, 8)
ch$valeur_import <- round(ch$valeur_import)
ch$valeur_miroir_caf <- round(ch$valeur_miroir_caf)
ch$ecart_valeur <- round(ch$ecart_valeur)
ch$perte_totale <- round(ch$perte_totale)
ch$ratio_miroir <- round(ch$ratio_miroir, 3)
print(ch[, c("sh2", "valeur_import", "valeur_miroir_caf", "ecart_valeur",
             "ratio_miroir", "perte_totale", "nb_lignes")], row.names = FALSE)

cat("\n--- Bilan par partenaire ---\n")
pa <- res$agregats$par_partenaire
pa$ecart_valeur <- round(pa$ecart_valeur)
pa$ratio_miroir <- round(pa$ratio_miroir, 3)
pa$perte_totale <- round(pa$perte_totale)
print(pa[, c("partenaire", "ecart_valeur", "ratio_miroir", "perte_totale", "nb_lignes")],
      row.names = FALSE)

# ---------------------------------------------------------------------
# 5. Controle : les anomalies injectees sont-elles bien detectees ?
# ---------------------------------------------------------------------
message("4/5 - Controle de detection des anomalies injectees...")
injectees <- sim$anomalies_injectees
id_ligne <- paste(d$partenaire, d$sh6, sep = "|")
cibles <- c(injectees$sous_facturation, injectees$sur_facturation)

detecte <- sapply(cibles, function(c) {
  lignes <- d[id_ligne == c & d$statut == "apparie", ]
  if (nrow(lignes) == 0) return(NA_real_)
  mean(lignes$alerte, na.rm = TRUE)
})

cat("\n--- Taux de detection des couples anormaux injectes ---\n")
for (i in seq_along(cibles)) {
  cat(sprintf("  %-45s : %s\n", cibles[i],
              if (is.na(detecte[i])) "non observe"
              else sprintf("%.0f %% des annees en alerte", 100 * detecte[i])))
}

cat(sprintf("\nChapitre 87 (sous-facturation injectee) - ratio miroir observe : %.3f\n",
            with(res$agregats$par_chapitre[res$agregats$par_chapitre$sh2 == "87", ],
                 ratio_miroir)))

message("5/5 - Termine. Livrables ecrits dans resultats/")
cat("\nFichiers produits :\n")
cat(paste0("  - ", list.files(file.path(racine, "resultats")), collapse = "\n"), "\n")
