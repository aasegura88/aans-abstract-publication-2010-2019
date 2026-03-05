# ============================================================
# Table 1: Characteristics of AANS podium abstracts (2010 vs 2019)
# Outputs:
#   outputs/table1_aans_clean.csv
#   outputs/table1_aans_clean.html
#   outputs/table1_aans_clean.docx (optional if officer+flextable installed)
# ============================================================

options(stringsAsFactors = FALSE)
set.seed(1)  # for simulated chi-square p-values

DATA_PATH  <- Sys.getenv("AANS_DATA_PATH", unset = file.path("data", "AANScleanedv3 JMR 2.25.csv"))
OUT_DIR    <- Sys.getenv("AANS_OUT_DIR",   unset = "outputs")
OUT_PREFIX <- file.path(OUT_DIR, "table1_aans_clean")

FONT_NAME <- Sys.getenv("AANS_FONT_NAME", unset = "Times New Roman")
FONT_SIZE <- suppressWarnings(as.integer(Sys.getenv("AANS_FONT_SIZE", unset = "10")))
if (!is.finite(FONT_SIZE)) FONT_SIZE <- 10L

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
  "Genderconcordantfirstandseniorofabstract1Yes0No",
  "Award",
  "Published",
  "Timetopublicationmonthstimefromabstractapril2019"
)
missing_cols <- setdiff(required_cols, names(df0))
if (length(missing_cols) > 0) {
  stop(paste0("Missing columns:\n- ", paste(missing_cols, collapse = "\n- ")))
}

# ---------- recode helpers ----------
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

# ---------- formatting helpers ----------
NA_MARK <- "—"

fmt_p <- function(p) {
  if (is.na(p)) return(NA_MARK)
  if (p < 0.001) return("<0.001")
  sprintf("%.3f", p)
}
fmt_es <- function(x) {
  if (is.na(x)) return(NA_MARK)
  sprintf("%.3f", x)
}
fmt_n_pct <- function(n, denom) {
  if (is.na(n) || is.na(denom) || denom == 0) return(NA_MARK)
  sprintf("%d (%.1f%%)", n, 100 * n / denom)
}
fmt_median_iqr <- function(x, digits = 1) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_MARK)
  q <- stats::quantile(x, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, names = FALSE)
  sprintf(paste0("%.", digits, "f (%.", digits, "f–%.", digits, "f)"), q[2], q[1], q[3])
}

# ---------- stats helpers ----------
pval_2x2 <- function(x, g) {
  ok <- !is.na(x) & !is.na(g)
  x <- x[ok]; g <- g[ok]
  tab <- table(x, g)
  if (nrow(tab) != 2 || ncol(tab) != 2) return(NA_real_)
  exp <- suppressWarnings(stats::chisq.test(tab, correct = FALSE)$expected)
  if (any(exp < 5)) return(stats::fisher.test(tab)$p.value)
  stats::chisq.test(tab, correct = FALSE)$p.value
}

pval_cat_global <- function(x, g) {
  ok <- !is.na(x) & !is.na(g)
  x <- x[ok]; g <- g[ok]
  tab <- table(x, g)
  if (length(tab) == 0) return(NA_real_)
  exp <- suppressWarnings(stats::chisq.test(tab, correct = FALSE)$expected)
  if (any(exp < 5)) return(stats::chisq.test(tab, simulate.p.value = TRUE, B = 20000)$p.value)
  stats::chisq.test(tab, correct = FALSE)$p.value
}

cramers_v <- function(x, g) {
  ok <- !is.na(x) & !is.na(g)
  x <- x[ok]; g <- g[ok]
  tab <- table(x, g)
  if (sum(tab) == 0) return(NA_real_)
  chi <- suppressWarnings(stats::chisq.test(tab, correct = FALSE))
  r <- nrow(tab); c <- ncol(tab)
  k <- min(r - 1, c - 1)
  if (k <= 0) return(NA_real_)
  as.numeric(sqrt(chi$statistic / (sum(tab) * k)))
}

asd_binary <- function(x0, x1) {
  x0 <- x0[!is.na(x0)]; x1 <- x1[!is.na(x1)]
  if (length(x0) == 0 || length(x1) == 0) return(NA_real_)
  p0 <- mean(x0 == 1); p1 <- mean(x1 == 1)
  p  <- (sum(x0 == 1) + sum(x1 == 1)) / (length(x0) + length(x1))
  denom <- sqrt(p * (1 - p))
  if (!is.finite(denom) || denom == 0) return(0)
  abs((p1 - p0) / denom)
}

asd_cont <- function(x0, x1) {
  x0 <- x0[is.finite(x0)]; x1 <- x1[is.finite(x1)]
  if (length(x0) == 0 || length(x1) == 0) return(NA_real_)
  m0 <- mean(x0); m1 <- mean(x1)
  s0 <- stats::sd(x0); s1 <- stats::sd(x1)
  sp <- sqrt(((length(x0) - 1) * s0^2 + (length(x1) - 1) * s1^2) / (length(x0) + length(x1) - 2))
  if (!is.finite(sp) || sp == 0) return(0)
  abs((m1 - m0) / sp)
}

pval_wilcox <- function(x, g) {
  ok <- is.finite(x) & !is.na(g)
  x <- x[ok]; g <- g[ok]
  if (length(unique(g)) != 2) return(NA_real_)
  stats::wilcox.test(x ~ g, exact = FALSE)$p.value
}

# ---------- recode columns ----------
df <- df0
df$Year_lab <- clean_year(df$Year)

sub_map <- c(
  "1" = "Functional/Peripheral Nerve",
  "2" = "Pediatrics",
  "3" = "Spine",
  "4" = "Vascular/Stroke",
  "5" = "General/Other",
  "6" = "Trauma/Critical Care",
  "7" = "Tumor"
)
df$Subspecialty_lab <- trimws(as.character(df$Subspecialty))
sub_num <- suppressWarnings(as.numeric(df$Subspecialty_lab))
if (all(na.omit(sub_num) %in% suppressWarnings(as.numeric(names(sub_map))))) {
  df$Subspecialty_lab <- unname(sub_map[as.character(sub_num)])
}

df$med_student_first    <- as01(df$Medstudentfirstauthor1Yes0No)
df$senior_international <- as01(df$Seniorauthorcountry)
df$n_authors            <- suppressWarnings(as.numeric(df$Numberofauthorsintheabstract))

df$first_female      <- as01(df$FirstauthorgenderabstractMF)
df$senior_female     <- as01(df$SeniorauthorgenderofabstractMF)
df$gender_concordant <- as01(df$Genderconcordantfirstandseniorofabstract1Yes0No)

df$award     <- as01(df$Award)
df$published <- as01(df$Published)

df$time_to_pub_mo <- suppressWarnings(as.numeric(df$Timetopublicationmonthstimefromabstractapril2019))
df$time_to_pub_mo[df$published != 1] <- NA_real_

df <- df[df$Year_lab %in% c("2010", "2019"), , drop = FALSE]
g2010 <- df[df$Year_lab == "2010", , drop = FALSE]
g2019 <- df[df$Year_lab == "2019", , drop = FALSE]

n_all  <- nrow(df)
n_2010 <- nrow(g2010)
n_2019 <- nrow(g2019)

col_overall <- sprintf("Overall (n=%d)", n_all)
col_2010    <- sprintf("2010 (n=%d)", n_2010)
col_2019    <- sprintf("2019 (n=%d)", n_2019)

den_sub_all  <- sum(!is.na(df$Subspecialty_lab))
den_sub_2010 <- sum(!is.na(g2010$Subspecialty_lab))
den_sub_2019 <- sum(!is.na(g2019$Subspecialty_lab))

rows <- list()
add_row <- function(ch, o, y0, y1, es, pv) {
  rows[[length(rows) + 1]] <<- data.frame(
    Characteristic = ch,
    o = o, y0 = y0, y1 = y1, es = es, pv = pv,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

add_row("Number of abstracts", sprintf("%d", n_all), sprintf("%d", n_2010), sprintf("%d", n_2019), NA_MARK, NA_MARK)

v_sub <- cramers_v(df$Subspecialty_lab, df$Year_lab)
p_sub <- pval_cat_global(df$Subspecialty_lab, df$Year_lab)
add_row("Subspecialty, n (%)", NA_MARK, NA_MARK, NA_MARK, paste0("V=", fmt_es(v_sub)), fmt_p(p_sub))

sub_order <- c(
  "Functional/Peripheral Nerve",
  "General/Other",
  "Pediatrics",
  "Spine",
  "Trauma/Critical Care",
  "Tumor",
  "Vascular/Stroke"
)
sub_levels <- sub_order[sub_order %in% unique(df$Subspecialty_lab)]
extra_levels <- setdiff(unique(df$Subspecialty_lab), sub_levels)
sub_levels <- c(sub_levels, sort(extra_levels))

for (lvl in sub_levels) {
  add_row(
    paste0("  ", lvl),
    fmt_n_pct(sum(df$Subspecialty_lab == lvl,  na.rm = TRUE), den_sub_all),
    fmt_n_pct(sum(g2010$Subspecialty_lab == lvl, na.rm = TRUE), den_sub_2010),
    fmt_n_pct(sum(g2019$Subspecialty_lab == lvl, na.rm = TRUE), den_sub_2019),
    NA_MARK,
    NA_MARK
  )
}

add_binary <- function(var_all, var_2010, var_2019, label) {
  den_all  <- sum(!is.na(var_all))
  den_2010 <- sum(!is.na(var_2010))
  den_2019 <- sum(!is.na(var_2019))
  n_yes_all  <- sum(var_all  == 1, na.rm = TRUE)
  n_yes_2010 <- sum(var_2010 == 1, na.rm = TRUE)
  n_yes_2019 <- sum(var_2019 == 1, na.rm = TRUE)
  add_row(
    paste0(label, ", n (%)"),
    fmt_n_pct(n_yes_all,  den_all),
    fmt_n_pct(n_yes_2010, den_2010),
    fmt_n_pct(n_yes_2019, den_2019),
    fmt_es(asd_binary(var_2010, var_2019)),
    fmt_p(pval_2x2(var_all, df$Year_lab))
  )
}

add_binary(df$med_student_first,    g2010$med_student_first,    g2019$med_student_first,    "Medical student first author")
add_binary(df$senior_international, g2010$senior_international, g2019$senior_international, "International senior author")

add_row(
  "Number of authors in abstract, median (IQR)",
  fmt_median_iqr(df$n_authors,  digits = 1),
  fmt_median_iqr(g2010$n_authors, digits = 1),
  fmt_median_iqr(g2019$n_authors, digits = 1),
  fmt_es(asd_cont(g2010$n_authors, g2019$n_authors)),
  fmt_p(pval_wilcox(df$n_authors, df$Year_lab))
)

add_binary(df$first_female,      g2010$first_female,      g2019$first_female,      "Female first author (abstract; inferred/perceived)")
add_binary(df$senior_female,     g2010$senior_female,     g2019$senior_female,     "Female senior author (abstract; inferred/perceived)")
add_binary(df$gender_concordant, g2010$gender_concordant, g2019$gender_concordant, "Gender concordance (first vs senior; abstract)")
add_binary(df$award,             g2010$award,             g2019$award,             "Award-winning abstract")
add_binary(df$published,         g2010$published,         g2019$published,         "Published (podium-to-print)")

add_row(
  "Time to publication (months), median (IQR) [published only]",
  fmt_median_iqr(df$time_to_pub_mo, digits = 1),
  fmt_median_iqr(g2010$time_to_pub_mo, digits = 1),
  fmt_median_iqr(g2019$time_to_pub_mo, digits = 1),
  fmt_es(asd_cont(g2010$time_to_pub_mo, g2019$time_to_pub_mo)),
  fmt_p(pval_wilcox(df$time_to_pub_mo, df$Year_lab))
)

tab <- do.call(rbind, rows)
for (j in seq_len(ncol(tab))) tab[[j]][is.na(tab[[j]]) | tab[[j]] == ""] <- NA_MARK
names(tab) <- c("Characteristic", col_overall, col_2010, col_2019, "Effect size", "P-value")

write.csv(tab, paste0(OUT_PREFIX, ".csv"), row.names = FALSE)

# HTML export (Word-openable)
html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

is_indent <- grepl("^  ", tab$Characteristic)
is_block  <- tab$Characteristic == "Subspecialty, n (%)"

tab_html <- tab
tab_html$Characteristic <- sub("^  ", "", tab_html$Characteristic)

css <- sprintf("
body { font-family: '%s'; font-size: %dpt; }
table { border-collapse: collapse; width: 100%%; }
caption { caption-side: top; text-align: left; font-weight: bold; margin-bottom: 6pt; }
th, td { padding: 4pt 6pt; vertical-align: top; }
th { text-align: left; border-bottom: 0.5pt solid #000; }
table { border-top: 1pt solid #000; border-bottom: 1pt solid #000; }
td.num { text-align: center; }
td.es, td.pv { text-align: right; white-space: nowrap; }
tr.block td { font-weight: bold; padding-top: 6pt; }
tr.sep td { border-bottom: 0.5pt solid #000; }
td.indent { padding-left: 14pt; }
.notes { margin-top: 8pt; font-size: 9pt; }
", FONT_NAME, FONT_SIZE)

make_row <- function(i) {
  cls <- ""
  if (is_block[i]) cls <- " class='block'"
  last_sub <- if (any(is_indent)) max(which(is_indent)) else -1
  sep_cls <- if (i == last_sub) " class='sep'" else ""

  tr_open <- "<tr>"
  if (nzchar(cls) && nzchar(sep_cls)) tr_open <- "<tr class='block sep'>"
  else if (nzchar(cls)) tr_open <- paste0("<tr", cls, ">")
  else if (nzchar(sep_cls)) tr_open <- paste0("<tr", sep_cls, ">")

  ch_td_class <- if (is_indent[i]) "indent" else ""
  sprintf(
    "%s<td class='%s'>%s</td><td class='num'>%s</td><td class='num'>%s</td><td class='num'>%s</td><td class='es'>%s</td><td class='pv'>%s</td></tr>",
    tr_open,
    ch_td_class,
    html_escape(tab_html$Characteristic[i]),
    html_escape(tab_html[[col_overall]][i]),
    html_escape(tab_html[[col_2010]][i]),
    html_escape(tab_html[[col_2019]][i]),
    html_escape(tab_html[["Effect size"]][i]),
    html_escape(tab_html[["P-value"]][i])
  )
}

html <- c(
  "<html><head><meta charset='utf-8'/>",
  "<style>", css, "</style></head><body>",
  "<table>",
  "<caption>Table 1. Characteristics of AANS podium presentation abstracts (2010 vs 2019)</caption>",
  sprintf(
    "<tr><th>Characteristic</th><th>%s</th><th>%s</th><th>%s</th><th>Effect size</th><th>P-value</th></tr>",
    html_escape(col_overall), html_escape(col_2010), html_escape(col_2019)
  ),
  vapply(seq_len(nrow(tab)), make_row, character(1)),
  "</table>",
  "<div class='notes'>Notes: — indicates not applicable. Effect size uses ASD for binary/continuous variables and Cramer's V (shown as V=...) for subspecialty overall. Gender variables reflect inferred/perceived gender (proxy), not self-identified gender.</div>",
  "</body></html>"
)
writeLines(html, paste0(OUT_PREFIX, ".html"))

# Optional DOCX (only if officer+flextable are installed)
have_docx <- requireNamespace("officer", quietly = TRUE) && requireNamespace("flextable", quietly = TRUE)
if (have_docx) {
  library(officer)
  library(flextable)

  ft <- flextable(tab)
  ft <- font(ft, fontname = FONT_NAME, part = "all")
  ft <- fontsize(ft, size = FONT_SIZE, part = "all")
  ft <- bold(ft, part = "header")

  ft <- align(ft, j = 1, align = "left", part = "all")
  ft <- align(ft, j = 2:4, align = "center", part = "all")
  ft <- align(ft, j = 5:6, align = "right", part = "all")

  sub_i <- which(grepl("^  ", tab$Characteristic))
  if (length(sub_i) > 0) ft <- padding(ft, i = sub_i, j = 1, padding.left = 12, part = "body")

  block_i <- which(tab$Characteristic == "Subspecialty, n (%)")
  if (length(block_i) == 1) ft <- bold(ft, i = block_i, part = "body")

  ft <- border_remove(ft)
  b_top <- fp_border(color = "black", width = 1)
  b_mid <- fp_border(color = "black", width = 0.5)
  ft <- hline_top(ft, part = "all", border = b_top)
  ft <- hline_bottom(ft, part = "header", border = b_mid)
  ft <- hline_bottom(ft, part = "body", border = b_top)
  if (length(sub_i) > 0) ft <- hline(ft, i = max(sub_i), part = "body", border = b_mid)

  ft <- autofit(ft)
  ft <- add_footer_lines(
    ft,
    values = c(
      "Notes: — indicates not applicable.",
      "Effect size: ASD for binary/continuous variables; Cramer's V for subspecialty overall.",
      "Gender variables reflect inferred/perceived gender (proxy), not self-identified gender."
    )
  )
  save_as_docx(`Table 1` = ft, path = paste0(OUT_PREFIX, ".docx"))
}
message("Wrote: ", OUT_PREFIX, ".csv / .html", if (have_docx) " / .docx" else "")
