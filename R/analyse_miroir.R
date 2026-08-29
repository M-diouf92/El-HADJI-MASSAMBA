# =====================================================================
# analyse_miroir.R
# ---------------------------------------------------------------------
# Boite a outils R pour l'analyse miroir du commerce exterieur
# (comparaison des importations declarees par le pays declarant avec
#  les exportations declarees par ses partenaires vers ce meme pays).
#
# Objectif : detecter les ecarts statistiques anormaux pouvant reveler
# une sous-facturation, une fausse declaration d'origine, une erreur
# de classement tarifaire ou une non-declaration (contrebande), et
# chiffrer la perte potentielle de recettes.
#
# Dependances : R base uniquement (aucun package externe requis).
# Encodage    : UTF-8
# =====================================================================


# ---------------------------------------------------------------------
# 0. Utilitaires internes
# ---------------------------------------------------------------------

#' Division protegee : renvoie NA si le denominateur est nul/negatif/NA
div_sure <- function(num, den) {
  res <- rep(NA_real_, length(num))
  ok <- !is.na(num) & !is.na(den) & den > 0
  res[ok] <- num[ok] / den[ok]
  res
}

#' Remplace les NA par une valeur (0 par defaut)
na_vers <- function(x, valeur = 0) {
  x[is.na(x)] <- valeur
  x
}

#' Rang percentile (0-1), robuste aux NA
rang_pct <- function(x) {
  res <- rep(NA_real_, length(x))
  ok <- !is.na(x)
  if (sum(ok) <= 1) {
    res[ok] <- 0.5
    return(res)
  }
  res[ok] <- (rank(x[ok], ties.method = "average") - 1) / (sum(ok) - 1)
  res
}

#' Score z robuste (mediane / MAD) : moins sensible aux valeurs extremes
#' que la moyenne / ecart-type, ce qui est indispensable sur des donnees
#' douanieres fortement asymetriques.
z_robuste <- function(x) {
  ok <- !is.na(x) & is.finite(x)
  if (sum(ok) < 3) return(rep(NA_real_, length(x)))
  med <- stats::median(x[ok])
  ec <- stats::mad(x[ok], constant = 1.4826)
  if (is.na(ec) || ec <= 0) ec <- stats::sd(x[ok])
  if (is.na(ec) || ec <= 0) return(rep(0, length(x)))
  (x - med) / ec
}

#' Verifie la presence des colonnes attendues dans un tableau
exiger_colonnes <- function(donnees, colonnes, nom_objet = "le tableau") {
  manquantes <- setdiff(colonnes, names(donnees))
  if (length(manquantes) > 0) {
    stop(sprintf("Colonnes absentes de %s : %s",
                 nom_objet, paste(manquantes, collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}


# ---------------------------------------------------------------------
# 1. Coefficient CAF/FOB
# ---------------------------------------------------------------------
# Les importations sont valorisees CAF (cout, assurance, fret) alors que
# les exportations du partenaire sont valorisees FOB. Comparer les deux
# sans correction cree un ecart mecanique de l'ordre de +6 a +20 %.
# On ramene donc l'export miroir en equivalent CAF.
# ---------------------------------------------------------------------

#' Applique un coefficient CAF/FOB, global ou par partenaire
#'
#' @param donnees data.frame contenant au moins la colonne `partenaire`
#' @param coef soit un scalaire (ex. 1.10), soit un data.frame a deux
#'   colonnes : `partenaire` et `coef_caf_fob`
#' @param defaut coefficient applique aux partenaires absents de la table
#' @return vecteur numerique de coefficients, de la longueur de `donnees`
coefficient_caf_fob <- function(donnees, coef = 1.10, defaut = 1.10) {
  if (is.numeric(coef) && length(coef) == 1L) {
    return(rep(coef, nrow(donnees)))
  }
  if (is.data.frame(coef)) {
    exiger_colonnes(coef, c("partenaire", "coef_caf_fob"), "la table des coefficients CAF/FOB")
    idx <- match(donnees$partenaire, coef$partenaire)
    valeurs <- coef$coef_caf_fob[idx]
    valeurs[is.na(valeurs)] <- defaut
    return(valeurs)
  }
  stop("`coef` doit etre un scalaire numerique ou un data.frame (partenaire, coef_caf_fob).",
       call. = FALSE)
}


# ---------------------------------------------------------------------
# 2. Appariement des deux sources et calcul des ecarts
# ---------------------------------------------------------------------

#' Prepare la table miroir : jointure externe complete + indicateurs d'ecart
#'
#' @param imports declarations nationales (flux entrant), valorisees CAF
#' @param miroir  statistiques des partenaires (leurs exportations vers le
#'   pays declarant), valorisees FOB
#' @param cles vecteur des colonnes de jointure (ex. annee, partenaire, sh6)
#' @param col_valeur_import,col_valeur_miroir noms des colonnes de valeur
#' @param col_quantite_import,col_quantite_miroir noms des colonnes de
#'   quantite (kg net de preference) ; NULL pour ignorer les quantites
#' @param coef_caf_fob scalaire ou table par partenaire (cf. coefficient_caf_fob)
#' @param taux_change facteur multiplicatif applique aux valeurs
#'   d'importation pour les exprimer dans la meme monnaie que le miroir
#'   (ex. 1/600 pour passer de FCFA a USD). Defaut 1 (meme monnaie).
#' @return data.frame apparie enrichi des indicateurs d'ecart
preparer_miroir <- function(imports,
                            miroir,
                            cles = c("annee", "partenaire", "sh6"),
                            col_valeur_import = "valeur_caf",
                            col_valeur_miroir = "valeur_fob",
                            col_quantite_import = "quantite",
                            col_quantite_miroir = "quantite",
                            coef_caf_fob = 1.10,
                            taux_change = 1) {

  exiger_colonnes(imports, c(cles, col_valeur_import), "la table des importations")
  exiger_colonnes(miroir, c(cles, col_valeur_miroir), "la table miroir")

  avec_quantites <- !is.null(col_quantite_import) && !is.null(col_quantite_miroir) &&
    col_quantite_import %in% names(imports) && col_quantite_miroir %in% names(miroir)

  # --- normalisation des deux sources -------------------------------
  imp <- imports[, c(cles, col_valeur_import,
                     if (avec_quantites) col_quantite_import), drop = FALSE]
  names(imp) <- c(cles, "valeur_import", if (avec_quantites) "quantite_import")

  mir <- miroir[, c(cles, col_valeur_miroir,
                    if (avec_quantites) col_quantite_miroir), drop = FALSE]
  names(mir) <- c(cles, "valeur_miroir_fob", if (avec_quantites) "quantite_miroir")

  # agregation defensive : plusieurs lignes peuvent partager la meme cle
  agreger_par_cle <- function(d, cles) {
    mesures <- setdiff(names(d), cles)
    res <- stats::aggregate(d[, mesures, drop = FALSE],
                            by = as.list(d[, cles, drop = FALSE]),
                            FUN = function(v) sum(v, na.rm = TRUE))
    names(res) <- c(cles, mesures)
    res
  }
  imp <- agreger_par_cle(imp, cles)
  mir <- agreger_par_cle(mir, cles)

  # --- jointure externe complete ------------------------------------
  # `all = TRUE` est essentiel : les flux presents d'un seul cote
  # (declares par le partenaire mais absents des declarations nationales,
  # ou l'inverse) sont precisement les cas les plus interessants.
  d <- merge(imp, mir, by = cles, all = TRUE, sort = TRUE)

  d$valeur_import <- na_vers(d$valeur_import) * taux_change
  d$valeur_miroir_fob <- na_vers(d$valeur_miroir_fob)

  # --- passage du FOB partenaire en equivalent CAF -------------------
  d$coef_caf_fob <- coefficient_caf_fob(d, coef_caf_fob)
  d$valeur_miroir_caf <- d$valeur_miroir_fob * d$coef_caf_fob

  # --- statut d'appariement -----------------------------------------
  d$statut <- ifelse(d$valeur_import > 0 & d$valeur_miroir_caf > 0, "apparie",
              ifelse(d$valeur_import <= 0 & d$valeur_miroir_caf > 0, "absent_import",
                     "absent_miroir"))

  # --- indicateurs d'ecart en valeur --------------------------------
  d$ecart_valeur <- d$valeur_import - d$valeur_miroir_caf
  d$ratio_miroir <- div_sure(d$valeur_import, d$valeur_miroir_caf)
  d$ecart_relatif <- div_sure(d$ecart_valeur, d$valeur_miroir_caf)
  # ecart symetrique borne dans [-2 ; 2] : comparable d'un produit a
  # l'autre, il ne diverge pas quand l'un des deux flux tend vers zero.
  denom <- (d$valeur_import + d$valeur_miroir_caf) / 2
  d$ecart_symetrique <- div_sure(d$ecart_valeur, denom)
  d$log_ratio <- ifelse(d$valeur_import > 0 & d$valeur_miroir_caf > 0,
                        log(d$valeur_import / d$valeur_miroir_caf), NA_real_)

  # --- indicateurs d'ecart en quantite et en valeur unitaire --------
  if (avec_quantites) {
    d$quantite_import <- na_vers(d$quantite_import)
    d$quantite_miroir <- na_vers(d$quantite_miroir)
    d$ecart_quantite <- d$quantite_import - d$quantite_miroir
    d$ratio_quantite <- div_sure(d$quantite_import, d$quantite_miroir)
    d$vu_import <- div_sure(d$valeur_import, d$quantite_import)
    d$vu_miroir <- div_sure(d$valeur_miroir_caf, d$quantite_miroir)
    d$ecart_vu_relatif <- div_sure(d$vu_import - d$vu_miroir, d$vu_miroir)
  }

  # --- nomenclature : chapitre (SH2) et position (SH4) --------------
  if ("sh6" %in% names(d)) {
    d$sh6 <- formatC(as.character(d$sh6), width = 6, flag = "0")
    d$sh4 <- substr(d$sh6, 1, 4)
    d$sh2 <- substr(d$sh6, 1, 2)
  }

  d[order(d$ecart_valeur), ]
}


# ---------------------------------------------------------------------
# 3. Qualification des ecarts
# ---------------------------------------------------------------------

#' Qualifie chaque ligne : sous-facturation, sur-facturation, coherent
#'
#' Convention de lecture :
#'  - ecart_valeur < 0 : le pays declare MOINS que ce que ses partenaires
#'    disent lui avoir vendu -> presomption de sous-evaluation en douane,
#'    de non-declaration ou de detournement de destination ;
#'  - ecart_valeur > 0 : le pays declare PLUS -> presomption de
#'    sur-facturation (transfert de capitaux, fausse origine, ou reexport
#'    mal impute par le partenaire).
#'
#' @param d table issue de preparer_miroir()
#' @param seuil_relatif ecart symetrique au-dela duquel la ligne est
#'   signalee (0.20 = 20 %)
#' @param seuil_valeur montant minimal du flux miroir pour retenir la
#'   ligne (filtre le bruit des micro-flux)
#' @param groupe_reference colonne definissant la population de reference
#'   pour le score z (par defaut le chapitre SH2)
qualifier_ecarts <- function(d,
                             seuil_relatif = 0.20,
                             seuil_valeur = 10000,
                             groupe_reference = "sh2") {

  exiger_colonnes(d, c("ecart_valeur", "ecart_symetrique", "valeur_miroir_caf"),
                  "la table miroir preparee")

  # score z du log-ratio, calcule a l'interieur de chaque groupe de
  # reference : un ecart de +30 % n'a pas le meme sens sur un chapitre
  # homogene (cereales) que sur un chapitre heterogene (machines).
  d$z_log_ratio <- NA_real_
  if (!is.null(groupe_reference) && groupe_reference %in% names(d)) {
    for (g in unique(d[[groupe_reference]])) {
      idx <- which(d[[groupe_reference]] == g)
      d$z_log_ratio[idx] <- z_robuste(d$log_ratio[idx])
    }
  } else {
    d$z_log_ratio <- z_robuste(d$log_ratio)
  }

  materiel <- d$valeur_miroir_caf >= seuil_valeur | d$valeur_import >= seuil_valeur

  d$qualification <- ifelse(
    !materiel, "flux non materiel",
    ifelse(d$statut == "absent_import", "flux non declare (presume)",
    ifelse(d$statut == "absent_miroir", "flux sans contrepartie miroir",
    ifelse(d$ecart_symetrique <= -seuil_relatif, "sous-facturation presumee",
    ifelse(d$ecart_symetrique >= seuil_relatif, "sur-facturation presumee",
           "coherent")))))

  d$alerte <- d$qualification %in% c("sous-facturation presumee",
                                     "sur-facturation presumee",
                                     "flux non declare (presume)")
  d
}


# ---------------------------------------------------------------------
# 4. Persistance temporelle
# ---------------------------------------------------------------------

#' Mesure la persistance de l'ecart d'une annee sur l'autre
#'
#' Un ecart qui se repete chaque annee dans le meme sens est un signal
#' structurel ; un ecart isole releve plus souvent de l'erreur
#' statistique (decalage d'enregistrement, transbordement, delai de
#' declaration entre deux exercices).
#'
#' @param d table qualifiee, comportant une colonne `annee`
#' @param cles_produit cles identifiant un couple produit/partenaire
persistance_ecarts <- function(d, cles_produit = c("partenaire", "sh6")) {
  exiger_colonnes(d, c("annee", cles_produit, "ecart_valeur"), "la table miroir")

  id <- do.call(paste, c(d[, cles_produit, drop = FALSE], sep = "|"))
  signe <- sign(d$ecart_valeur)

  n_annees <- tapply(d$annee, id, function(x) length(unique(x)))
  n_negatif <- tapply(signe, id, function(x) sum(x < 0, na.rm = TRUE))
  n_positif <- tapply(signe, id, function(x) sum(x > 0, na.rm = TRUE))

  dominant <- pmax(na_vers(n_negatif), na_vers(n_positif))
  taux <- div_sure(as.numeric(dominant), as.numeric(n_annees))

  d$annees_observees <- as.numeric(n_annees[id])
  d$persistance <- as.numeric(taux[id])
  d
}


# ---------------------------------------------------------------------
# 5. Score de risque composite
# ---------------------------------------------------------------------

#' Calcule un score de risque de 0 a 100 par ligne
#'
#' Le score combine quatre dimensions, exprimees en rangs percentiles
#' pour rester comparables entre elles :
#'   - l'enjeu financier (montant absolu de l'ecart)          : 40 %
#'   - l'anormalite statistique (|z| du log-ratio)            : 25 %
#'   - l'anomalie de valeur unitaire                          : 20 %
#'   - la persistance dans le temps                           : 15 %
#'
#' @param d table qualifiee (et de preference passee par persistance_ecarts)
#' @param poids vecteur nomme des poids ; il est renormalise a 1
scorer_risque <- function(d,
                          poids = c(montant = 0.40, anormalite = 0.25,
                                    prix = 0.20, persistance = 0.15)) {

  exiger_colonnes(d, c("ecart_valeur"), "la table miroir qualifiee")
  poids <- poids / sum(poids)

  p_montant <- rang_pct(abs(d$ecart_valeur))
  p_anormalite <- if ("z_log_ratio" %in% names(d)) rang_pct(abs(d$z_log_ratio)) else NA_real_
  p_prix <- if ("ecart_vu_relatif" %in% names(d)) rang_pct(abs(d$ecart_vu_relatif)) else NA_real_
  p_persistance <- if ("persistance" %in% names(d)) d$persistance else NA_real_

  composantes <- cbind(montant = p_montant, anormalite = p_anormalite,
                       prix = p_prix, persistance = p_persistance)

  # Les composantes indisponibles sont neutralisees et leur poids est
  # redistribue sur les composantes effectivement calculees.
  poids_mat <- matrix(poids[colnames(composantes)], nrow = nrow(composantes),
                      ncol = ncol(composantes), byrow = TRUE)
  dispo <- !is.na(composantes)
  poids_mat[!dispo] <- 0
  composantes[!dispo] <- 0
  somme_poids <- rowSums(poids_mat)
  somme_poids[somme_poids == 0] <- NA_real_

  d$score_risque <- round(100 * rowSums(composantes * poids_mat) / somme_poids, 1)
  d$niveau_risque <- cut(d$score_risque,
                         breaks = c(-Inf, 40, 60, 80, Inf),
                         labels = c("faible", "modere", "eleve", "critique"))
  d[order(-na_vers(d$score_risque, -1)), ]
}


# ---------------------------------------------------------------------
# 6. Estimation de la perte de recettes
# ---------------------------------------------------------------------

#' Estime les droits et taxes eludes sur la base manquante
#'
#' Seuls les ecarts negatifs (assiette declaree inferieure a l'assiette
#' miroir) generent une perte. La cascade appliquee est celle du TEC
#' CEDEAO / UEMOA : les taxes ad valorem s'appliquent a la valeur CAF,
#' la TVA s'applique a la valeur CAF majoree de ces taxes.
#'
#'   base_manquante = max(0, valeur_miroir_caf - valeur_import)
#'   droits         = base * (dd + rs + pcs + pc)
#'   tva            = (base + droits) * taux_tva
#'
#' @param d table miroir (au minimum `ecart_valeur`)
#' @param tarifs data.frame optionnel (sh6, taux_dd) ; a defaut, `taux_dd`
#'   est applique uniformement
#' @param taux_dd droit de douane par defaut (TEC CEDEAO : 0 / 5 / 10 / 35 %)
#' @param taux_rs redevance statistique (1 %)
#' @param taux_pcs prelevement communautaire de solidarite UEMOA (1 %)
#' @param taux_pc prelevement communautaire CEDEAO (0.5 %)
#' @param taux_tva taxe sur la valeur ajoutee (18 %)
estimer_pertes_recettes <- function(d,
                                    tarifs = NULL,
                                    taux_dd = 0.10,
                                    taux_rs = 0.01,
                                    taux_pcs = 0.01,
                                    taux_pc = 0.005,
                                    taux_tva = 0.18) {

  exiger_colonnes(d, "ecart_valeur", "la table miroir")

  dd <- rep(taux_dd, nrow(d))
  if (!is.null(tarifs)) {
    exiger_colonnes(tarifs, c("sh6", "taux_dd"), "la table tarifaire")
    exiger_colonnes(d, "sh6", "la table miroir")
    cle_t <- formatC(as.character(tarifs$sh6), width = 6, flag = "0")
    idx <- match(d$sh6, cle_t)
    dd_t <- tarifs$taux_dd[idx]
    dd[!is.na(dd_t)] <- dd_t[!is.na(dd_t)]
  }
  d$taux_dd_applique <- dd

  d$base_manquante <- pmax(0, -d$ecart_valeur)
  d$droits_eludes <- d$base_manquante * (dd + taux_rs + taux_pcs + taux_pc)
  d$tva_eludee <- (d$base_manquante + d$droits_eludes) * taux_tva
  d$perte_totale <- d$droits_eludes + d$tva_eludee
  d
}


# ---------------------------------------------------------------------
# 7. Agregations et synthese
# ---------------------------------------------------------------------

#' Agrege la table miroir selon une ou plusieurs dimensions
#'
#' @param d table miroir
#' @param par vecteur des colonnes de regroupement (ex. c("annee","sh2"))
#' @param coef_caf_fob coefficient utilise pour recalculer le ratio agrege
agreger_miroir <- function(d, par = c("annee", "sh2")) {
  exiger_colonnes(d, par, "la table miroir")

  mesures <- intersect(c("valeur_import", "valeur_miroir_caf", "ecart_valeur",
                         "base_manquante", "droits_eludes", "tva_eludee",
                         "perte_totale"), names(d))

  res <- stats::aggregate(d[, mesures, drop = FALSE],
                          by = as.list(d[, par, drop = FALSE]),
                          FUN = function(v) sum(v, na.rm = TRUE))
  names(res) <- c(par, mesures)

  res$ratio_miroir <- div_sure(res$valeur_import, res$valeur_miroir_caf)
  res$ecart_relatif <- div_sure(res$ecart_valeur, res$valeur_miroir_caf)
  res$nb_lignes <- as.numeric(stats::aggregate(
    rep(1, nrow(d)), by = as.list(d[, par, drop = FALSE]), FUN = sum)$x)

  res[order(res$ecart_valeur), ]
}

#' Synthese globale de l'analyse miroir
synthese_miroir <- function(d) {
  total_imp <- sum(d$valeur_import, na.rm = TRUE)
  total_mir <- sum(d$valeur_miroir_caf, na.rm = TRUE)
  ecart <- total_imp - total_mir
  list(
    nb_lignes = nrow(d),
    importations_declarees = total_imp,
    miroir_partenaires_caf = total_mir,
    ecart_global = ecart,
    ecart_global_pct = 100 * div_sure(ecart, total_mir),
    taux_couverture = div_sure(total_imp, total_mir),
    nb_alertes = if ("alerte" %in% names(d)) sum(d$alerte, na.rm = TRUE) else NA,
    perte_estimee = if ("perte_totale" %in% names(d)) sum(d$perte_totale, na.rm = TRUE) else NA
  )
}

#' Affiche la synthese de facon lisible
imprimer_synthese <- function(s, monnaie = "USD") {
  fm <- function(x) formatC(x, format = "f", big.mark = " ", digits = 0)
  cat("\n=========== SYNTHESE DE L'ANALYSE MIROIR ===========\n")
  cat(sprintf("Lignes analysees              : %s\n", fm(s$nb_lignes)))
  cat(sprintf("Importations declarees (CAF)  : %s %s\n", fm(s$importations_declarees), monnaie))
  cat(sprintf("Miroir partenaires (eq. CAF)  : %s %s\n", fm(s$miroir_partenaires_caf), monnaie))
  cat(sprintf("Ecart global                  : %s %s (%.1f %%)\n",
              fm(s$ecart_global), monnaie, s$ecart_global_pct))
  cat(sprintf("Taux de couverture M / X*     : %.3f\n", s$taux_couverture))
  if (!is.na(s$nb_alertes)) cat(sprintf("Lignes en alerte              : %s\n", fm(s$nb_alertes)))
  if (!is.na(s$perte_estimee)) cat(sprintf("Perte de recettes estimee     : %s %s\n",
                                           fm(s$perte_estimee), monnaie))
  cat("====================================================\n\n")
  invisible(s)
}


# ---------------------------------------------------------------------
# 8. Graphiques (R base, sans dependance)
# ---------------------------------------------------------------------

#' Nuage de points log(importations) vs log(miroir), avec la premiere
#' bissectrice : tout point sous la droite est un flux sous-declare.
graphique_dispersion <- function(d, fichier = NULL, titre = "Analyse miroir : declarations vs miroir") {
  ok <- d$valeur_import > 0 & d$valeur_miroir_caf > 0
  if (!any(ok)) return(invisible(NULL))
  if (!is.null(fichier)) grDevices::png(fichier, width = 900, height = 800, res = 110)
  couleur <- ifelse(d$ecart_valeur[ok] < 0, "#c0392b", "#2471a3")
  plot(log10(d$valeur_miroir_caf[ok]), log10(d$valeur_import[ok]),
       pch = 20, col = grDevices::adjustcolor(couleur, alpha.f = 0.55),
       xlab = "log10 des exportations partenaires (eq. CAF)",
       ylab = "log10 des importations declarees",
       main = titre)
  graphics::abline(0, 1, lwd = 2, col = "grey30")
  graphics::legend("topleft", bty = "n", pch = 20,
                   col = c("#c0392b", "#2471a3"),
                   legend = c("Sous-declaration (M < X*)", "Sur-declaration (M > X*)"))
  if (!is.null(fichier)) grDevices::dev.off()
  invisible(NULL)
}

#' Barres horizontales des n plus gros ecarts negatifs par dimension
graphique_top_ecarts <- function(agrege, dimension = "sh2", n = 15,
                                 fichier = NULL, monnaie = "USD") {
  a <- agrege[order(agrege$ecart_valeur), ]
  a <- utils::head(a, n)
  if (nrow(a) == 0) return(invisible(NULL))
  if (!is.null(fichier)) grDevices::png(fichier, width = 1000, height = 800, res = 110)
  op <- graphics::par(mar = c(5, 8, 4, 2))
  on.exit({graphics::par(op); if (!is.null(fichier)) grDevices::dev.off()}, add = TRUE)
  graphics::barplot(rev(a$ecart_valeur / 1e6), horiz = TRUE, las = 1,
                    names.arg = rev(as.character(a[[dimension]])),
                    col = "#c0392b", border = NA,
                    xlab = sprintf("Ecart M - X* (millions %s)", monnaie),
                    main = sprintf("Principaux ecarts negatifs par %s", dimension))
  graphics::abline(v = 0, col = "grey30")
  invisible(NULL)
}

#' Evolution annuelle des deux series et de l'ecart
graphique_evolution <- function(d, fichier = NULL, monnaie = "USD") {
  if (!"annee" %in% names(d)) return(invisible(NULL))
  a <- agreger_miroir(d, par = "annee")
  a <- a[order(a$annee), ]
  if (!is.null(fichier)) grDevices::png(fichier, width = 1000, height = 700, res = 110)
  ymax <- max(c(a$valeur_import, a$valeur_miroir_caf), na.rm = TRUE) / 1e6
  plot(a$annee, a$valeur_import / 1e6, type = "b", pch = 19, lwd = 2,
       col = "#2471a3", ylim = c(0, ymax * 1.1),
       xlab = "Annee", ylab = sprintf("Millions %s", monnaie),
       main = "Importations declarees et miroir partenaires")
  graphics::lines(a$annee, a$valeur_miroir_caf / 1e6, type = "b", pch = 17,
                  lwd = 2, col = "#c0392b")
  graphics::legend("bottomleft", bty = "n", lwd = 2, pch = c(19, 17),
                   col = c("#2471a3", "#c0392b"),
                   legend = c("Importations declarees (CAF)", "Miroir partenaires (eq. CAF)"))
  if (!is.null(fichier)) grDevices::dev.off()
  invisible(NULL)
}


# ---------------------------------------------------------------------
# 9. Chaine complete
# ---------------------------------------------------------------------

#' Execute l'analyse miroir de bout en bout et ecrit les livrables
#'
#' @param imports,miroir tables sources
#' @param dossier_sortie repertoire ou ecrire les CSV et les graphiques
#' @param ... arguments transmis a preparer_miroir()
#' @return liste : table detaillee, agregats, synthese
analyse_miroir <- function(imports,
                           miroir,
                           cles = c("annee", "partenaire", "sh6"),
                           coef_caf_fob = 1.10,
                           taux_change = 1,
                           tarifs = NULL,
                           seuil_relatif = 0.20,
                           seuil_valeur = 10000,
                           dossier_sortie = NULL,
                           monnaie = "USD",
                           ...) {

  d <- preparer_miroir(imports, miroir, cles = cles,
                       coef_caf_fob = coef_caf_fob,
                       taux_change = taux_change, ...)
  d <- qualifier_ecarts(d, seuil_relatif = seuil_relatif, seuil_valeur = seuil_valeur)
  if ("annee" %in% cles) d <- persistance_ecarts(d)
  d <- estimer_pertes_recettes(d, tarifs = tarifs)
  d <- scorer_risque(d)

  agregats <- list(
    par_chapitre = if ("sh2" %in% names(d)) agreger_miroir(d, c("sh2")) else NULL,
    par_partenaire = if ("partenaire" %in% names(d)) agreger_miroir(d, c("partenaire")) else NULL,
    par_annee = if ("annee" %in% names(d)) agreger_miroir(d, c("annee")) else NULL,
    par_produit = if ("sh6" %in% names(d)) agreger_miroir(d, c("sh6")) else NULL
  )

  s <- synthese_miroir(d)

  if (!is.null(dossier_sortie)) {
    dir.create(dossier_sortie, showWarnings = FALSE, recursive = TRUE)
    ecrire <- function(x, nom) {
      if (!is.null(x)) utils::write.csv(x, file.path(dossier_sortie, nom),
                                        row.names = FALSE, fileEncoding = "UTF-8")
    }
    ecrire(d, "miroir_detail.csv")
    ecrire(utils::head(d[d$alerte %in% TRUE, ], 200), "miroir_top_alertes.csv")
    ecrire(agregats$par_chapitre, "miroir_par_chapitre.csv")
    ecrire(agregats$par_partenaire, "miroir_par_partenaire.csv")
    ecrire(agregats$par_annee, "miroir_par_annee.csv")
    ecrire(agregats$par_produit, "miroir_par_produit.csv")

    graphique_dispersion(d, file.path(dossier_sortie, "miroir_dispersion.png"))
    if (!is.null(agregats$par_chapitre)) {
      graphique_top_ecarts(agregats$par_chapitre, "sh2",
                           fichier = file.path(dossier_sortie, "miroir_top_chapitres.png"),
                           monnaie = monnaie)
    }
    graphique_evolution(d, file.path(dossier_sortie, "miroir_evolution.png"), monnaie = monnaie)
  }

  list(detail = d, agregats = agregats, synthese = s)
}


# ---------------------------------------------------------------------
# 10. Generateur de donnees de demonstration
# ---------------------------------------------------------------------

#' Simule un jeu de donnees miroir avec des anomalies injectees
#'
#' Utile pour tester la chaine sans donnees reelles : le generateur
#' introduit volontairement de la sous-facturation sur certains couples
#' produit/partenaire, de la sur-facturation sur d'autres, ainsi que des
#' flux totalement absents des declarations nationales.
simuler_donnees_miroir <- function(n_produits = 60,
                                   partenaires = c("Chine", "France", "Inde", "Turquie",
                                                   "Emirats arabes unis", "Espagne"),
                                   annees = 2020:2024,
                                   graine = 42) {
  set.seed(graine)

  chapitres <- c("10", "15", "27", "39", "52", "61", "72", "84", "85", "87")
  sh6 <- unique(paste0(sample(chapitres, n_produits, replace = TRUE),
                       formatC(sample(1000:9999, n_produits, replace = TRUE), width = 4, flag = "0")))
  n_produits <- length(sh6)

  grille <- expand.grid(annee = annees, partenaire = partenaires, sh6 = sh6,
                        stringsAsFactors = FALSE)
  n <- nrow(grille)

  # flux "vrai" du partenaire, log-normal (distribution realiste)
  valeur_fob <- round(exp(stats::rnorm(n, mean = 11.5, sd = 2)), 0)
  quantite <- round(valeur_fob / exp(stats::rnorm(n, mean = 0.7, sd = 0.8)), 0)

  miroir <- data.frame(grille, valeur_fob = valeur_fob, quantite = quantite,
                       stringsAsFactors = FALSE)

  # construction des importations declarees a partir du miroir
  coef <- 1.10
  bruit <- exp(stats::rnorm(n, mean = 0, sd = 0.18))   # bruit statistique normal
  facteur <- rep(1, n)

  # anomalie 1 : sous-facturation structurelle sur 3 couples
  couples_sous <- paste(sample(partenaires, 3), sample(sh6, 3), sep = "|")
  id <- paste(grille$partenaire, grille$sh6, sep = "|")
  facteur[id %in% couples_sous] <- 0.45

  # anomalie 2 : sur-facturation sur 2 couples
  couples_sur <- paste(sample(partenaires, 2), sample(sh6, 2), sep = "|")
  facteur[id %in% couples_sur] <- 1.9

  # anomalie 3 : sous-facturation ciblee sur un chapitre a forte taxation
  facteur[substr(grille$sh6, 1, 2) == "87"] <- 0.65

  valeur_caf <- round(miroir$valeur_fob * coef * bruit * facteur, 0)
  quantite_imp <- round(miroir$quantite * exp(stats::rnorm(n, 0, 0.10)), 0)

  imports <- data.frame(grille, valeur_caf = valeur_caf, quantite = quantite_imp,
                        stringsAsFactors = FALSE)

  # anomalie 4 : 4 % des flux totalement absents des declarations nationales
  absents <- sample(seq_len(n), size = floor(0.04 * n))
  imports <- imports[-absents, ]

  # bruit realiste : 2 % de flux declares sans contrepartie miroir
  sans_miroir <- sample(seq_len(nrow(miroir)), size = floor(0.02 * nrow(miroir)))
  miroir <- miroir[-sans_miroir, ]

  # table tarifaire coherente avec le TEC CEDEAO (5 bandes)
  tarifs <- data.frame(
    sh6 = sh6,
    taux_dd = sample(c(0, 0.05, 0.10, 0.20, 0.35), length(sh6), replace = TRUE,
                     prob = c(0.05, 0.25, 0.35, 0.25, 0.10)),
    stringsAsFactors = FALSE
  )

  list(imports = imports, miroir = miroir, tarifs = tarifs,
       anomalies_injectees = list(sous_facturation = couples_sous,
                                  sur_facturation = couples_sur,
                                  chapitre_cible = "87"))
}
