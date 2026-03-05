# ============================================================
# Table 2: Predictors of podium-to-print publication (logistic regression)
# Outputs:
#   outputs/table2_aans_pub_predictors.csv
#   outputs/table2_aans_pub_predictors.html
# ============================================================

options(stringsAsFactors = FALSE)

DATA_PATH  <- Sys.getenv("AANS_DATA_PATH", unset = file.path("data", "AANScleanedv3 JMR 2.25.csv"))
OUT_DIR    <- Sys.getenv("AANS_OUT_DIR",   unset = "outputs")
OUT_PREFIX <- file.path(OUT_DIR, "table2_aans_pub_predictors")

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
stopifnot(file.exists(DATA_PATH))

df0 <- read.csv(DATA_PATH, check.names = FALSE)

required_cols <- c(
  "Year",
  "Subspecialty",
  "Medstudentfirstauthor1Yes0No",
  "Seniorauthorcountry",
  "Numberofauthorsintheabstract",
  "FirstauthorgenderabstractMF",
  "SeniorauthorgenderofabstractMF",
  "Award",
  "Published"
)
missing_cols <- setdiff(required_cols, names(df0))
if (length(missing_cols) > 0) {
  stop(paste0("Missing columns:\n- ", paste(missing_cols, collapse = "\n- ")))
}

as01 <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  if (is.logical(x)) return(ifelse(is.na(x), NA_integer_, ifelse(x, 1L, 0L)))
  if (is.character(x)) {
    xt <- trimws(x)
    if (xt %in% c("", "NA", "NaN")) return(NA_integer_)
    xn <- suppressWarnings(as.numeric(xt))
    return(ifelse(is.na(xn), NA_integer_, ifelse(xn == 1, 1L, 0L)))
  }
  xn <- suppressWarnings(as.numeric(x))
  ifelse(is.na(xn), NA_integer_, ifelse(xn == 1, 1L, 0L))
}

clean_year <- function(y) {
  yn <- suppressWarnings(as.numeric(y))
  if (all(na.omit(yn) %in% c(0, 1))) {
    ifelse(yn == 0, "2010", ifelse(yn == 1, "2019", NA_character_))
  } else if (all(na.omit(yn) %in% c(2010, 2019))) {
    as.character(yn)
  } else {
    as.character(y)
  }
}

fmt_p3 <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) return("<0.001")
  sprintf("%.3f", p)
}

fmt_or_ci <- function(or, lcl, ucl, digits = 2) {
  if (!is.finite(or) || !is.finite(lcl) || !is.finite(ucl)) return("NA")
  sprintf(paste0("%.", digits, "f (%.", digits, "f–%.", digits, "f)"), or, lcl, ucl)
}

safe_glm <- function(formula, data) {
  tryCatch(glm(formula, family = binomial(), data = data),
           error = function(e) e)
}

get_term_stats <- function(fit, term) {
  if (inherits(fit, "error")) return(list(orci = "NA", p = "NA"))
  if (!inherits(fit, "glm"))  return(list(orci = "NA", p = "NA"))

  co <- summary(fit)$coefficients
  if (is.null(co) || !(term %in% rownames(co))) return(list(orci = "NA", p = "NA"))

  beta <- co[term, "Estimate"]
  se   <- co[term, "Std. Error"]
  p    <- co[term, "Pr(>|z|)"]

  if (!is.finite(beta) || !is.finite(se) || se <= 0) return(list(orci = "NA", p = fmt_p3(p)))

  or  <- exp(beta)
  lcl <- exp(beta - 1.96 * se)
  ucl <- exp(beta + 1.96 * se)
  list(orci = fmt_or_ci(or, lcl, ucl, digits = 2), p = fmt_p3(p))
}

lr_pvalue <- function(fit_reduced, fit_full) {
  if (inherits(fit_reduced, "error") || inherits(fit_full, "error")) return(NA_real_)
  if (!inherits(fit_reduced, "glm") || !inherits(fit_full, "glm")) return(NA_real_)
  a <- tryCatch(anova(fit_reduced, fit_full, test = "Chisq"), error = function(e) NULL)
  if (is.null(a) || nrow(a) < 2) return(NA_real_)
  as.numeric(a$`Pr(>Chi)`[nrow(a)])
}

auc_rank <- function(y, p) {
  ok <- is.finite(p) & !is.na(y)
  y <- y[ok]; p <- p[ok]
  if (length(unique(y)) != 2) return(NA_real_)
  n1 <- sum(y == 1); n0 <- sum(y == 0)
  if (n1 == 0 || n0 == 0) return(NA_real_)
  r <- rank(p, ties.method = "average")
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

df <- df0
df$year <- factor(clean_year(df$Year), levels = c("2010", "2019"))

sub_map <- c(
  "1" = "Functional/Peripheral Nerve",
  "2" = "Pediatrics",
  "3" = "Spine",
  "4" = "Vascular/Stroke",
  "5" = "General/Other",
  "6" = "Trauma/Critical Care",
  "7" = "Tumor"
)
sub_raw <- trimws(as.character(df$Subspecialty))
sub_num <- suppressWarnings(as.numeric(sub_raw))
sub_lab <- sub_raw
if (all(na.omit(sub_num) %in% suppressWarnings(as.numeric(names(sub_map))))) {
  sub_lab <- unname(sub_map[as.character(sub_num)])
}

sub_code_map <- c(
  "General/Other" = "general_other",
  "Functional/Peripheral Nerve" = "functional_peripheral_nerve",
  "Pediatrics" = "pediatrics",
  "Spine" = "spine",
  "Trauma/Critical Care" = "trauma_critical_care",
  "Tumor" = "tumor",
  "Vascular/Stroke" = "vascular_stroke"
)

df$subspec <- unname(sub_code_map[sub_lab])
df$subspec <- factor(
  df$subspec,
  levels = c("general_other",
             "functional_peripheral_nerve",
             "pediatrics",
             "spine",
             "trauma_critical_care",
             "tumor",
             "vascular_stroke")
)

df$med_student <- factor(ifelse(as01(df$Medstudentfirstauthor1Yes0No) == 1, "yes", "no"),
                         levels = c("no", "yes"))

df$senior_country <- factor(ifelse(as01(df$Seniorauthorcountry) == 1, "international", "usa"),
                            levels = c("usa", "international"))

df$n_authors <- suppressWarnings(as.numeric(df$Numberofauthorsintheabstract))

df$first_gender <- factor(ifelse(as01(df$FirstauthorgenderabstractMF) == 1, "female", "male"),
                          levels = c("male", "female"))
df$senior_gender <- factor(ifelse(as01(df$SeniorauthorgenderofabstractMF) == 1, "female", "male"),
                           levels = c("male", "female"))

df$award <- factor(ifelse(as01(df$Award) == 1, "yes", "no"),
                   levels = c("no", "yes"))

df$published <- as01(df$Published)

df <- df[!is.na(df$year) & df$year %in% c("2010","2019") & !is.na(df$published), , drop = FALSE]

vars_needed <- c("published","year","subspec","med_student","senior_country","n_authors","first_gender","senior_gender","award")
mdata <- df[, vars_needed, drop = FALSE]
mdata <- mdata[complete.cases(mdata), , drop = FALSE]

N_model <- nrow(mdata)
events  <- sum(mdata$published == 1)

fit_null <- safe_glm(published ~ 1, data = mdata)

fit_uni_year          <- safe_glm(published ~ year, data = mdata)
fit_uni_subspec       <- safe_glm(published ~ subspec, data = mdata)
fit_uni_med_student   <- safe_glm(published ~ med_student, data = mdata)
fit_uni_senior_ctry   <- safe_glm(published ~ senior_country, data = mdata)
fit_uni_n_authors     <- safe_glm(published ~ n_authors, data = mdata)
fit_uni_first_gender  <- safe_glm(published ~ first_gender, data = mdata)
fit_uni_senior_gender <- safe_glm(published ~ senior_gender, data = mdata)
fit_uni_award         <- safe_glm(published ~ award, data = mdata)

fit_adj <- safe_glm(
  published ~ year + subspec + med_student + senior_country + n_authors + first_gender + senior_gender + award,
  data = mdata
)

p_subspec_uni <- lr_pvalue(fit_null, fit_uni_subspec)

fit_adj_reduced_no_subspec <- safe_glm(
  published ~ year + med_student + senior_country + n_authors + first_gender + senior_gender + award,
  data = mdata
)
p_subspec_adj <- lr_pvalue(fit_adj_reduced_no_subspec, fit_adj)

if (inherits(fit_adj, "glm") && inherits(fit_null, "glm")) {
  p_hat <- predict(fit_adj, type = "response")
  auc <- auc_rank(mdata$published, p_hat)
  mcfadden_r2 <- 1 - (as.numeric(logLik(fit_adj)) / as.numeric(logLik(fit_null)))
} else {
  auc <- NA_real_
  mcfadden_r2 <- NA_real_
}

label_map <- c(
  "year2019" = "Year: 2019 vs 2010",
  "SUBSPEC_GLOBAL" = "Subspecialty (global LR test; reference: General/Other)",
  "subspecfunctional_peripheral_nerve" = "  Functional/Peripheral Nerve vs General/Other",
  "subspecpediatrics" = "  Pediatrics vs General/Other",
  "subspecspine" = "  Spine vs General/Other",
  "subspectrauma_critical_care" = "  Trauma/Critical Care vs General/Other",
  "subspectumor" = "  Tumor vs General/Other",
  "subspecvascular_stroke" = "  Vascular/Stroke vs General/Other",
  "med_studentyes" = "Medical student first author: Yes vs No",
  "senior_countryinternational" = "International senior author: International vs USA",
  "n_authors" = "Number of authors in abstract (per +1 author)",
  "first_genderfemale" = "Female first author (abstract): Female vs Male",
  "senior_genderfemale" = "Female senior author (abstract): Female vs Male",
  "awardyes" = "Award-winning abstract: Yes vs No"
)

present_sub_levels <- levels(droplevels(mdata$subspec))
sub_level_terms <- paste0("subspec", present_sub_levels[present_sub_levels != "general_other"])
sub_level_terms <- intersect(
  sub_level_terms,
  c("subspecfunctional_peripheral_nerve","subspecpediatrics","subspecspine",
    "subspectrauma_critical_care","subspectumor","subspecvascular_stroke")
)

row_order <- c(
  "year2019",
  "SUBSPEC_GLOBAL",
  sub_level_terms,
  "med_studentyes",
  "senior_countryinternational",
  "n_authors",
  "first_genderfemale",
  "senior_genderfemale",
  "awardyes"
)

table2 <- data.frame(
  Predictor = character(),
  `Unadjusted OR (95% CI)` = character(),
  `P-value` = character(),
  `Adjusted OR (95% CI)` = character(),
  `P-value (adj)` = character(),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

add_row <- function(pred, u_orci, u_p, a_orci, a_p) {
  table2 <<- rbind(
    table2,
    data.frame(
      Predictor = pred,
      `Unadjusted OR (95% CI)` = u_orci,
      `P-value` = u_p,
      `Adjusted OR (95% CI)` = a_orci,
      `P-value (adj)` = a_p,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  )
}

uni_model_for_term <- function(term) {
  if (grepl("^year", term)) return(fit_uni_year)
  if (grepl("^subspec", term)) return(fit_uni_subspec)
  if (grepl("^med_student", term)) return(fit_uni_med_student)
  if (grepl("^senior_country", term)) return(fit_uni_senior_ctry)
  if (term == "n_authors") return(fit_uni_n_authors)
  if (grepl("^first_gender", term)) return(fit_uni_first_gender)
  if (grepl("^senior_gender", term)) return(fit_uni_senior_gender)
  if (grepl("^award", term)) return(fit_uni_award)
  fit_uni_year
}

for (term in row_order) {
  if (term == "SUBSPEC_GLOBAL") {
    add_row(label_map[[term]], "-", fmt_p3(p_subspec_uni), "-", fmt_p3(p_subspec_adj))
    next
  }
  uni_fit <- uni_model_for_term(term)
  uni_vals <- get_term_stats(uni_fit, term)
  adj_vals <- get_term_stats(fit_adj, term)
  add_row(label_map[[term]], uni_vals$orci, uni_vals$p, adj_vals$orci, adj_vals$p)
}

for (j in seq_len(ncol(table2))) table2[[j]][is.na(table2[[j]]) | table2[[j]] == ""] <- "NA"

write.csv(table2, paste0(OUT_PREFIX, ".csv"), row.names = FALSE)

html_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

is_global <- table2$Predictor == label_map[["SUBSPEC_GLOBAL"]]
is_indent <- grepl("^\\s\\s", table2$Predictor)

css <- "
body { font-family: 'Times New Roman'; font-size: 10pt; }
table { border-collapse: collapse; width: 100%; }
caption { caption-side: top; text-align: left; font-weight: bold; margin-bottom: 6pt; }
th, td { padding: 4pt 6pt; vertical-align: top; }
th { text-align: left; border-bottom: 0.5pt solid #000; }
table { border-top: 1pt solid #000; border-bottom: 1pt solid #000; }
td.c { text-align: center; white-space: nowrap; }
td.r { text-align: right; white-space: nowrap; }
tr.hdr td { font-weight: bold; padding-top: 6pt; }
td.indent { padding-left: 14pt; }
.notes { margin-top: 8pt; font-size: 9pt; }
"

make_row <- function(i) {
  tr_class <- if (isTRUE(is_global[i])) " class='hdr'" else ""
  pred_class <- if (isTRUE(is_indent[i])) "indent" else ""
  pred <- html_escape(sub("^\\s\\s", "", table2[i, 1, drop = TRUE]))
  u_or <- html_escape(table2[i, 2, drop = TRUE])
  u_p  <- html_escape(table2[i, 3, drop = TRUE])
  a_or <- html_escape(table2[i, 4, drop = TRUE])
  a_p  <- html_escape(table2[i, 5, drop = TRUE])

  sprintf(
    "<tr%s><td class='%s'>%s</td><td class='c'>%s</td><td class='r'>%s</td><td class='c'>%s</td><td class='r'>%s</td></tr>",
    tr_class, pred_class, pred, u_or, u_p, a_or, a_p
  )
}

rows_html <- vapply(seq_len(nrow(table2)), make_row, character(1), USE.NAMES = FALSE)

caption <- sprintf(
  "Table 2. Predictors of podium-to-print publication (logistic regression; complete-case N=%d, published n=%d).",
  N_model, events
)

notes <- sprintf(
  paste0(
    "Notes: ORs with 95%% CIs shown. Univariable and adjusted p-values are Wald tests from the respective logistic models. ",
    "Subspecialty (global) p-values are likelihood-ratio tests (models with vs without subspecialty). ",
    "Model AUC=%.3f; McFadden R2=%.3f. ",
    "Gender variables reflect inferred/perceived gender (proxy), not self-identified gender."
  ),
  auc, mcfadden_r2
)

html <- c(
  "<html><head><meta charset='utf-8'/>",
  "<style>", css, "</style></head><body>",
  "<table>",
  "<caption>", html_escape(caption), "</caption>",
  "<tr><th>Predictor</th><th>Unadjusted OR (95% CI)</th><th>P-value</th><th>Adjusted OR (95% CI)</th><th>P-value</th></tr>",
  rows_html,
  "</table>",
  "<div class='notes'>", html_escape(notes), "</div>",
  "</body></html>"
)

writeLines(html, paste0(OUT_PREFIX, ".html"))
message("Wrote: ", OUT_PREFIX, ".csv / .html")
