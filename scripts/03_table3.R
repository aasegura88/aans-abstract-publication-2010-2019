# ============================================================
# Table 3: Publication characteristics among published abstracts (2010 vs 2019)
# - Raw + Holm-adjusted p-values across 7 prespecified comparisons
# Outputs:
#   outputs/table3_publication_characteristics_revised.csv
#   outputs/table3_publication_characteristics_revised.html
# ============================================================

options(stringsAsFactors = FALSE)
set.seed(1)  # for simulated chi-square p-values

DATA_PATH  <- Sys.getenv("AANS_DATA_PATH", unset = file.path("data", "AANScleanedv3 JMR 2.25.csv"))
OUT_DIR    <- Sys.getenv("AANS_OUT_DIR",   unset = "outputs")
OUT_PREFIX <- file.path(OUT_DIR, "table3_publication_characteristics_revised")

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
stopifnot(file.exists(DATA_PATH))

df0 <- read.csv(DATA_PATH, check.names = FALSE)

required_cols <- c(
  "Year","Published",
  "Numberofauthorsintheabstract","Numberofauthorsinthefinalpublication",
  "Impactfactor",
  "Firstauthorgenderofpublication","Seniorauthorgenderofpublication",
  "Timetopublicationmonthstimefromabstractapril2019"
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

year_label <- function(y) {
  yn <- suppressWarnings(as.numeric(if (is.factor(y)) as.character(y) else y))
  ifelse(yn %in% c(0, 2010), "2010",
         ifelse(yn %in% c(1, 2019), "2019", NA_character_))
}

fmt_p3 <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) return("<0.001")
  sprintf("%.3f", p)
}

fmt_n_pct <- function(n, denom) {
  if (is.na(n) || is.na(denom) || denom == 0) return("NA")
  sprintf("%d (%.1f%%)", n, 100 * n / denom)
}

fmt_median_iqr <- function(x, digits = 1) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return("NA")
  q <- stats::quantile(x, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, names = FALSE)
  sprintf(paste0("%.", digits, "f (%.", digits, "f–%.", digits, "f)"), q[2], q[1], q[3])
}

pval_wilcox <- function(x, g) {
  ok <- is.finite(x) & !is.na(g)
  x <- x[ok]; g <- g[ok]
  if (length(unique(g)) != 2) return(NA_real_)
  stats::wilcox.test(x ~ g, exact = FALSE)$p.value
}

pval_binary <- function(x01, g) {
  ok <- !is.na(x01) & !is.na(g)
  x01 <- x01[ok]; g <- g[ok]
  tab <- table(g, x01)
  if (nrow(tab) != 2 || ncol(tab) != 2) return(NA_real_)
  stats::fisher.test(tab)$p.value
}

pval_cat <- function(cat, g) {
  ok <- !is.na(cat) & !is.na(g)
  cat <- cat[ok]; g <- g[ok]
  tab <- table(g, cat)
  if (nrow(tab) < 2 || ncol(tab) < 2) return(NA_real_)
  exp <- suppressWarnings(stats::chisq.test(tab, correct = FALSE)$expected)
  if (any(exp < 5)) return(stats::chisq.test(tab, simulate.p.value = TRUE, B = 20000)$p.value)
  stats::chisq.test(tab, correct = FALSE)$p.value
}

df0$published01 <- as01(df0$Published)
df0$year_lab <- year_label(df0$Year)

pub <- df0[df0$published01 == 1 & df0$year_lab %in% c("2010","2019"), , drop = FALSE]

n_all  <- nrow(pub)
n_2010 <- sum(pub$year_lab == "2010")
n_2019 <- sum(pub$year_lab == "2019")

pub$time_to_pub <- suppressWarnings(as.numeric(pub$Timetopublicationmonthstimefromabstractapril2019))
pub$IF_raw      <- suppressWarnings(as.numeric(pub$Impactfactor))
pub$n_abs       <- suppressWarnings(as.numeric(pub$Numberofauthorsintheabstract))
pub$n_pub       <- suppressWarnings(as.numeric(pub$Numberofauthorsinthefinalpublication))
pub$delta_authors <- pub$n_pub - pub$n_abs

pub$IF <- ifelse(is.finite(pub$IF_raw) & pub$IF_raw > 0, pub$IF_raw, NA_real_)

pub$first_pub_female <- ifelse(toupper(trimws(as.character(pub$Firstauthorgenderofpublication))) == "F", 1L,
                               ifelse(toupper(trimws(as.character(pub$Firstauthorgenderofpublication))) == "M", 0L, NA_integer_))

pub$senior_pub_female <- ifelse(toupper(trimws(as.character(pub$Seniorauthorgenderofpublication))) == "F", 1L,
                                ifelse(toupper(trimws(as.character(pub$Seniorauthorgenderofpublication))) == "M", 0L, NA_integer_))

pub$delta_cat <- NA_character_
pub$delta_cat[is.finite(pub$delta_authors) & pub$delta_authors > 0] <- "Increased"
pub$delta_cat[is.finite(pub$delta_authors) & pub$delta_authors == 0] <- "Unchanged"
pub$delta_cat[is.finite(pub$delta_authors) & pub$delta_authors < 0] <- "Decreased"

g2010 <- pub[pub$year_lab == "2010", , drop = FALSE]
g2019 <- pub[pub$year_lab == "2019", , drop = FALSE]

p_time   <- pval_wilcox(pub$time_to_pub, pub$year_lab)
p_if     <- pval_wilcox(pub$IF, pub$year_lab)
p_npub   <- pval_wilcox(pub$n_pub, pub$year_lab)
p_delta  <- pval_wilcox(pub$delta_authors, pub$year_lab)
p_dcat   <- pval_cat(pub$delta_cat, pub$year_lab)
p_ffirst <- pval_binary(pub$first_pub_female, pub$year_lab)
p_fsen   <- pval_binary(pub$senior_pub_female, pub$year_lab)

p_raw_vec <- c(
  time_to_pub = p_time,
  impact_factor = p_if,
  n_authors_final = p_npub,
  delta_authors = p_delta,
  delta_category = p_dcat,
  female_first = p_ffirst,
  female_senior = p_fsen
)

p_holm_vec <- rep(NA_real_, length(p_raw_vec))
okp <- is.finite(p_raw_vec)
p_holm_vec[okp] <- p.adjust(p_raw_vec[okp], method = "holm")
names(p_holm_vec) <- names(p_raw_vec)

col_overall <- sprintf("Overall published (n=%d)", n_all)
col_2010    <- sprintf("2010 published (n=%d)", n_2010)
col_2019    <- sprintf("2019 published (n=%d)", n_2019)

rows <- list()
add_row <- function(ch, o, y0, y1, p_raw, p_holm) {
  rows[[length(rows) + 1]] <<- data.frame(
    Characteristic = ch,
    o = o, y0 = y0, y1 = y1,
    p_raw = p_raw, p_holm = p_holm,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

add_row(
  "Time to publication (months), median (IQR)",
  fmt_median_iqr(pub$time_to_pub, 1),
  fmt_median_iqr(g2010$time_to_pub, 1),
  fmt_median_iqr(g2019$time_to_pub, 1),
  fmt_p3(p_raw_vec["time_to_pub"]),
  fmt_p3(p_holm_vec["time_to_pub"])
)

add_row(
  "Journal impact factor, median (IQR)",
  fmt_median_iqr(pub$IF, 1),
  fmt_median_iqr(g2010$IF, 1),
  fmt_median_iqr(g2019$IF, 1),
  fmt_p3(p_raw_vec["impact_factor"]),
  fmt_p3(p_holm_vec["impact_factor"])
)

add_row(
  "Number of authors in final publication, median (IQR)",
  fmt_median_iqr(pub$n_pub, 1),
  fmt_median_iqr(g2010$n_pub, 1),
  fmt_median_iqr(g2019$n_pub, 1),
  fmt_p3(p_raw_vec["n_authors_final"]),
  fmt_p3(p_holm_vec["n_authors_final"])
)

add_row(
  "Change in author count (publication - abstract), median (IQR)",
  fmt_median_iqr(pub$delta_authors, 1),
  fmt_median_iqr(g2010$delta_authors, 1),
  fmt_median_iqr(g2019$delta_authors, 1),
  fmt_p3(p_raw_vec["delta_authors"]),
  fmt_p3(p_holm_vec["delta_authors"])
)

add_row(
  "Change in author count category, n (%)",
  "-", "-", "-",
  fmt_p3(p_raw_vec["delta_category"]),
  fmt_p3(p_holm_vec["delta_category"])
)

delta_levels <- c("Decreased","Unchanged","Increased")
for (lvl in delta_levels) {
  add_row(
    paste0("  ", lvl),
    fmt_n_pct(sum(pub$delta_cat == lvl, na.rm = TRUE), sum(!is.na(pub$delta_cat))),
    fmt_n_pct(sum(g2010$delta_cat == lvl, na.rm = TRUE), sum(!is.na(g2010$delta_cat))),
    fmt_n_pct(sum(g2019$delta_cat == lvl, na.rm = TRUE), sum(!is.na(g2019$delta_cat))),
    "-", "-"
  )
}

add_row(
  "Female first author (publication), n (%)",
  fmt_n_pct(sum(pub$first_pub_female == 1, na.rm = TRUE), sum(!is.na(pub$first_pub_female))),
  fmt_n_pct(sum(g2010$first_pub_female == 1, na.rm = TRUE), sum(!is.na(g2010$first_pub_female))),
  fmt_n_pct(sum(g2019$first_pub_female == 1, na.rm = TRUE), sum(!is.na(g2019$first_pub_female))),
  fmt_p3(p_raw_vec["female_first"]),
  fmt_p3(p_holm_vec["female_first"])
)

add_row(
  "Female senior author (publication), n (%)",
  fmt_n_pct(sum(pub$senior_pub_female == 1, na.rm = TRUE), sum(!is.na(pub$senior_pub_female))),
  fmt_n_pct(sum(g2010$senior_pub_female == 1, na.rm = TRUE), sum(!is.na(g2010$senior_pub_female))),
  fmt_n_pct(sum(g2019$senior_pub_female == 1, na.rm = TRUE), sum(!is.na(g2019$senior_pub_female))),
  fmt_p3(p_raw_vec["female_senior"]),
  fmt_p3(p_holm_vec["female_senior"])
)

table3 <- do.call(rbind, rows)
for (j in seq_len(ncol(table3))) table3[[j]][is.na(table3[[j]]) | table3[[j]] == ""] <- "NA"
names(table3) <- c("Characteristic", col_overall, col_2010, col_2019, "P-value (raw)", "P-value (Holm)")

write.csv(table3, paste0(OUT_PREFIX, ".csv"), row.names = FALSE)

html_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

is_indent <- grepl("^\\s\\s", table3$Characteristic)
is_block  <- table3$Characteristic == "Change in author count category, n (%)"

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
  tr_class <- if (isTRUE(is_block[i])) " class='hdr'" else ""
  pred_class <- if (isTRUE(is_indent[i])) "indent" else ""

  ch <- html_escape(sub("^\\s\\s", "", table3[i, 1, drop = TRUE]))
  o  <- html_escape(table3[i, 2, drop = TRUE])
  y0 <- html_escape(table3[i, 3, drop = TRUE])
  y1 <- html_escape(table3[i, 4, drop = TRUE])
  pr <- html_escape(table3[i, 5, drop = TRUE])
  ph <- html_escape(table3[i, 6, drop = TRUE])

  sprintf(
    "<tr%s><td class='%s'>%s</td><td class='c'>%s</td><td class='c'>%s</td><td class='c'>%s</td><td class='r'>%s</td><td class='r'>%s</td></tr>",
    tr_class, pred_class, ch, o, y0, y1, pr, ph
  )
}

rows_html <- vapply(seq_len(nrow(table3)), make_row, character(1), USE.NAMES = FALSE)

caption <- sprintf(
  "Table 3. Characteristics of full-text publications arising from AANS podium abstracts (published only; 2010 n=%d, 2019 n=%d).",
  n_2010, n_2019
)

notes <- paste(
  "Notes: Values are n (%) or median (IQR).",
  "Raw p-values compare 2010 vs 2019 among published manuscripts (Fisher exact for binary; Wilcoxon rank-sum for continuous; chi-square with simulation for multi-level categorical).",
  "Holm-adjusted p-values control family-wise error across the 7 primary comparisons shown in the table (excluding indented subcategory rows).",
  "Impact factor values <= 0 were treated as missing.",
  "Gender variables reflect inferred/perceived gender and should be interpreted as a proxy rather than self-identified gender.",
  "Denominators may vary by row due to missingness in specific variables.",
  sep = " "
)

html <- c(
  "<html><head><meta charset='utf-8'/>",
  "<style>", css, "</style></head><body>",
  "<table>",
  "<caption>", html_escape(caption), "</caption>",
  "<tr>",
  "<th>Characteristic</th>",
  sprintf("<th>%s</th>", html_escape(col_overall)),
  sprintf("<th>%s</th>", html_escape(col_2010)),
  sprintf("<th>%s</th>", html_escape(col_2019)),
  "<th>P-value (raw)</th>",
  "<th>P-value (Holm)</th>",
  "</tr>",
  rows_html,
  "</table>",
  "<div class='notes'>", html_escape(notes), "</div>",
  "</body></html>"
)

writeLines(html, paste0(OUT_PREFIX, ".html"))
message("Wrote: ", OUT_PREFIX, ".csv / .html")
