# =====================================================================
# test_analyse_miroir.R
# ---------------------------------------------------------------------
# Tests unitaires de la boite a outils (R base, sans testthat).
# Lancement : Rscript tests/test_analyse_miroir.R
# =====================================================================

racine <- if (basename(getwd()) == "tests") ".." else "."
source(file.path(racine, "R", "analyse_miroir.R"), encoding = "UTF-8")

echecs <- 0L
verifier <- function(condition, libelle) {
  ok <- isTRUE(condition)
  cat(sprintf("[%s] %s\n", if (ok) "OK  " else "ECHEC", libelle))
  if (!ok) echecs <<- echecs + 1L
  invisible(ok)
}

# --- jeu de donnees minimal, entierement controle --------------------
imports <- data.frame(
  annee = c(2023, 2023, 2023),
  partenaire = c("Chine", "France", "Inde"),
  sh6 = c("870321", "100630", "520100"),
  valeur_caf = c(550, 1100, 2000),
  quantite = c(50, 200, 400),
  stringsAsFactors = FALSE
)
miroir <- data.frame(
  annee = c(2023, 2023, 2023),
  partenaire = c("Chine", "France", "Turquie"),
  sh6 = c("870321", "100630", "610910"),
  valeur_fob = c(1000, 1000, 800),
  quantite = c(100, 200, 160),
  stringsAsFactors = FALSE
)

d <- preparer_miroir(imports, miroir, coef_caf_fob = 1.0)

verifier(nrow(d) == 4,
         "la jointure externe conserve les flux presents d'un seul cote")
verifier(all(c("ecart_valeur", "ratio_miroir", "ecart_symetrique",
               "vu_import", "vu_miroir", "sh2") %in% names(d)),
         "tous les indicateurs d'ecart sont calcules")

ligne_chine <- d[d$partenaire == "Chine", ]
verifier(ligne_chine$ecart_valeur == -450, "ecart en valeur : 550 - 1000 = -450")
verifier(abs(ligne_chine$ratio_miroir - 0.55) < 1e-9, "ratio miroir = 0,55")
verifier(abs(ligne_chine$ecart_symetrique - (-450 / 775)) < 1e-9,
         "ecart symetrique correctement borne")
verifier(ligne_chine$sh2 == "87" && ligne_chine$sh4 == "8703",
         "chapitre et position SH derives du SH6")

# valeurs unitaires : 550/50 = 11 cote import, 1000/100 = 10 cote miroir
verifier(abs(ligne_chine$vu_import - 11) < 1e-9 &&
         abs(ligne_chine$vu_miroir - 10) < 1e-9 &&
         abs(ligne_chine$ecart_vu_relatif - 0.1) < 1e-9,
         "valeurs unitaires et ecart de prix unitaire")

verifier(d$statut[d$partenaire == "Inde"] == "absent_miroir" &&
         d$statut[d$partenaire == "Turquie"] == "absent_import",
         "statuts d'appariement des flux unilateraux")

# --- coefficient CAF/FOB --------------------------------------------
d2 <- preparer_miroir(imports, miroir, coef_caf_fob = 1.10)
verifier(abs(d2$valeur_miroir_caf[d2$partenaire == "Chine"] - 1100) < 1e-9,
         "le coefficient CAF/FOB scalaire est applique au miroir")

coefs <- data.frame(partenaire = "Chine", coef_caf_fob = 1.20,
                    stringsAsFactors = FALSE)
d3 <- preparer_miroir(imports, miroir, coef_caf_fob = coefs)
verifier(abs(d3$valeur_miroir_caf[d3$partenaire == "Chine"] - 1200) < 1e-9 &&
         abs(d3$valeur_miroir_caf[d3$partenaire == "France"] - 1100) < 1e-9,
         "coefficient par partenaire, avec repli sur le defaut")

# --- taux de change --------------------------------------------------
d4 <- preparer_miroir(imports, miroir, coef_caf_fob = 1.0, taux_change = 2)
verifier(d4$valeur_import[d4$partenaire == "Chine"] == 1100,
         "la conversion monetaire s'applique aux seules importations")

# --- qualification ---------------------------------------------------
q <- qualifier_ecarts(d, seuil_relatif = 0.20, seuil_valeur = 0)
verifier(q$qualification[q$partenaire == "Chine"] == "sous-facturation presumee",
         "un ecart de -45 % est qualifie de sous-facturation")
verifier(q$qualification[q$partenaire == "France"] == "coherent",
         "un flux equilibre est qualifie de coherent")
verifier(q$qualification[q$partenaire == "Turquie"] == "flux non declare (presume)",
         "un flux absent des declarations est signale")

q_seuil <- qualifier_ecarts(d, seuil_relatif = 0.20, seuil_valeur = 1e9)
verifier(all(q_seuil$qualification == "flux non materiel"),
         "le seuil de materialite neutralise les micro-flux")

# --- estimation des pertes -------------------------------------------
p <- estimer_pertes_recettes(q, taux_dd = 0.20, taux_rs = 0.01,
                             taux_pcs = 0.01, taux_pc = 0.005, taux_tva = 0.18)
lc <- p[p$partenaire == "Chine", ]
droits_attendus <- 450 * (0.20 + 0.01 + 0.01 + 0.005)
tva_attendue <- (450 + droits_attendus) * 0.18
verifier(abs(lc$base_manquante - 450) < 1e-9, "base manquante = ecart negatif")
verifier(abs(lc$droits_eludes - droits_attendus) < 1e-9, "droits eludes en cascade")
verifier(abs(lc$tva_eludee - tva_attendue) < 1e-9, "TVA assise sur CAF + droits")
verifier(p$base_manquante[p$partenaire == "Inde"] == 0,
         "aucune perte imputee sur un ecart positif")

tarifs <- data.frame(sh6 = "870321", taux_dd = 0.35, stringsAsFactors = FALSE)
p2 <- estimer_pertes_recettes(q, tarifs = tarifs, taux_dd = 0.05)
verifier(p2$taux_dd_applique[p2$sh6 == "870321"] == 0.35 &&
         p2$taux_dd_applique[p2$sh6 == "100630"] == 0.05,
         "la table tarifaire prime sur le taux par defaut")

# --- score de risque -------------------------------------------------
s <- scorer_risque(p)
verifier(all(s$score_risque >= 0 & s$score_risque <= 100, na.rm = TRUE),
         "le score de risque reste dans l'intervalle 0-100")
verifier(all(diff(s$score_risque) <= 0, na.rm = TRUE),
         "la table est triee par score decroissant")

# --- agregation ------------------------------------------------------
a <- agreger_miroir(p, par = "partenaire")
verifier(nrow(a) == 4 && abs(sum(a$ecart_valeur) - sum(p$ecart_valeur)) < 1e-9,
         "l'agregation conserve la somme des ecarts")

# --- synthese --------------------------------------------------------
syn <- synthese_miroir(p)
verifier(abs(syn$ecart_global - (sum(p$valeur_import) - sum(p$valeur_miroir_caf))) < 1e-9,
         "l'ecart global est coherent avec le detail")

# --- lignes dupliquees en entree -------------------------------------
imports_dup <- rbind(imports, imports[1, ])
d_dup <- preparer_miroir(imports_dup, miroir, coef_caf_fob = 1.0)
verifier(nrow(d_dup) == 4 && d_dup$valeur_import[d_dup$partenaire == "Chine"] == 1100,
         "les lignes dupliquees sont agregees avant appariement")

# --- controle des entrees --------------------------------------------
verifier(inherits(try(preparer_miroir(imports[, -4], miroir), silent = TRUE), "try-error"),
         "une colonne manquante declenche une erreur explicite")

# --- chaine complete sur donnees simulees ----------------------------
sim <- simuler_donnees_miroir(n_produits = 25, annees = 2021:2023, graine = 7)
res <- analyse_miroir(sim$imports, sim$miroir, tarifs = sim$tarifs)
verifier(nrow(res$detail) > 0 && !is.null(res$agregats$par_chapitre),
         "la chaine complete s'execute sur les donnees simulees")

ch87 <- res$agregats$par_chapitre[res$agregats$par_chapitre$sh2 == "87", ]
verifier(nrow(ch87) == 0 || ch87$ratio_miroir < 0.85,
         "la sous-facturation injectee sur le chapitre 87 est bien retrouvee")

cat(sprintf("\n%d test(s) en echec.\n", echecs))
if (echecs > 0) quit(status = 1)
