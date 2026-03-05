# ============================================================
# Figure 2: Student first authorship ↑ + Publication within 36 months ↓
# - Panel A: student first author share (exact 95% binomial CI)
# - Panel B: publication within 36 months by student stratum (exact 95% binomial CI)
# Outputs:
#   outputs/figure2_clean_main.png
#   outputs/figure2_clean_main.pdf
# ============================================================

options(stringsAsFactors = FALSE)

DATA_PATH  <- Sys.getenv("AANS_DATA_PATH", unset = file.path("data", "AANScleanedv3 JMR 2.25.csv"))
OUT_DIR    <- Sys.getenv("AANS_OUT_DIR",   unset = "outputs")
OUT_PREFIX <- file.path(OUT_DIR, "figure2_clean_main")

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
stopifnot(file.exists(DATA_PATH))

# User knobs
DPI <- 300
WIDTH_IN <- 8.0
HEIGHT_IN <- 6.8
SHOW_PVAL_CAPTION <- TRUE
PUB_WINDOW_MONTHS <- 36

# How to handle Published==1 but missing time-to-publication:
#   "drop" = exclude from within-window endpoint (matches your KM approach)
#   "assume_over" = treat as NOT within window (keeps denominators)
MISSING_TIME_PUBLISHED_POLICY <- "drop"

req_pkgs <- c("dplyr", "ggplot2", "scales", "patchwork")
missing <- req_pkgs[!vapply(req_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Missing packages: ", paste(missing, collapse = ", "),
       "\nInstall in R with: install.packages(c(", paste(sprintf('"%s"', missing), collapse = ", "), "))")
}
invisible(lapply(req_pkgs, library, character.only = TRUE))

df0 <- read.csv(DATA_PATH, check.names = FALSE)

required_cols <- c("Year", "Medstudentfirstauthor1Yes0No", "Published")
missing_cols <- setdiff(required_cols, names(df0))
if (length(missing_cols) > 0) stop("Missing columns: ", paste(missing_cols, collapse = ", "))

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

fmt_p <- function(p) {
  if (is.na(p)) return("-")
  if (p < 0.001) return("<0.001")
  sprintf("%.3f", p)
}

exact_ci <- function(x, n, conf.level = 0.95) {
  if (is.na(x) || is.na(n) || n <= 0) return(c(NA_real_, NA_real_))
  bt <- stats::binom.test(x = x, n = n, conf.level = conf.level)
  c(bt$conf.int[1], bt$conf.int[2])
}

to_numeric_loose <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) {
    x <- gsub(",", "", x)
    x <- gsub("[^0-9.]+", "", x)
  }
  suppressWarnings(as.numeric(x))
}

# Auto-detect time-to-publication column
TIME_TO_PUB_COL <- NULL
nm <- names(df0)
nm_low <- tolower(nm)
hits <- which(
  grepl("time.*publication", nm_low) |
    grepl("publication.*time", nm_low) |
    grepl("time_to_publication", nm_low) |
    grepl("months.*publication", nm_low) |
    grepl("time.*to.*pub", nm_low) |
    grepl("podium.*print.*time", nm_low)
)
if (length(hits) > 0) TIME_TO_PUB_COL <- nm[hits[1]]

use_window_final <- !is.null(TIME_TO_PUB_COL)

df <- df0 %>%
  mutate(
    Year_lab = clean_year(Year),
    Year_num = suppressWarnings(as.integer(clean_year(Year))),
    student01 = as01(Medstudentfirstauthor1Yes0No),
    published01 = as01(Published),
    time_to_pub_mo = if (use_window_final) to_numeric_loose(.data[[TIME_TO_PUB_COL]]) else NA_real_
  ) %>%
  filter(Year_lab %in% c("2010", "2019")) %>%
  filter(!is.na(student01)) %>%
  mutate(Student_lab = factor(ifelse(student01 == 1, "Medical student", "Non-student"),
                              levels = c("Non-student", "Medical student")))

# Panel A uses all rows with student01 present
dfA <- df %>% filter(!is.na(Year_lab))

# Panel B endpoint: publication within 36 months (if time column exists), else overall publication
if (use_window_final) {
  dfB <- df %>%
    filter(!is.na(published01)) %>%
    mutate(
      pub_within_window = case_when(
        published01 == 0 ~ 0L,
        published01 == 1 & is.finite(time_to_pub_mo) ~ ifelse(time_to_pub_mo <= PUB_WINDOW_MONTHS, 1L, 0L),
        published01 == 1 & !is.finite(time_to_pub_mo) & MISSING_TIME_PUBLISHED_POLICY == "assume_over" ~ 0L,
        TRUE ~ NA_integer_
      )
    )

  if (MISSING_TIME_PUBLISHED_POLICY == "drop") {
    dfB <- dfB %>% filter(!(published01 == 1 & !is.finite(time_to_pub_mo)))
  } else {
    dfB <- dfB %>% filter(!is.na(pub_within_window))
  }

  outcome_title <- paste0("B. Publication within ", PUB_WINDOW_MONTHS, " months declined in both strata")
  outcome_ylabel <- paste0("Published within ", PUB_WINDOW_MONTHS, " months")
} else {
  dfB <- df %>%
    filter(!is.na(published01)) %>%
    mutate(pub_within_window = published01)

  outcome_title <- "B. Publication declined in both strata"
  outcome_ylabel <- "Published"
}

# Summaries: Panel A
sum_student <- dfA %>%
  group_by(Year_num) %>%
  summarise(n = n(),
            student_n = sum(student01 == 1),
            prop = student_n / n,
            .groups = "drop")

ciA <- mapply(exact_ci, x = sum_student$student_n, n = sum_student$n)
sum_student$ci_low  <- as.numeric(ciA[1, ])
sum_student$ci_high <- as.numeric(ciA[2, ])
sum_student$label <- sprintf("%d/%d\n(%.1f%%)", sum_student$student_n, sum_student$n, 100 * sum_student$prop)

# Summaries: Panel B
sum_pub <- dfB %>%
  group_by(Year_num, Student_lab) %>%
  summarise(n = n(),
            pub_n = sum(pub_within_window == 1),
            prop = pub_n / n,
            .groups = "drop")

ciB <- mapply(exact_ci, x = sum_pub$pub_n, n = sum_pub$n)
sum_pub$ci_low  <- as.numeric(ciB[1, ])
sum_pub$ci_high <- as.numeric(ciB[2, ])
sum_pub$label <- sprintf("%d/%d\n(%.1f%%)", sum_pub$pub_n, sum_pub$n, 100 * sum_pub$prop)

# Tests for caption
tab_student_share <- table(dfA$student01, dfA$Year_lab)
p_student_share <- stats::fisher.test(tab_student_share)$p.value

tab_pub_overall <- table(dfB$pub_within_window, dfB$Year_lab)
p_pub_overall <- stats::fisher.test(tab_pub_overall)$p.value

df_lr <- dfB %>%
  mutate(year01 = ifelse(Year_lab == "2019", 1, 0),
         student_bin = ifelse(Student_lab == "Medical student", 1, 0),
         outcome = pub_within_window)

m0 <- glm(outcome ~ year01 + student_bin, data = df_lr, family = binomial())
m1 <- glm(outcome ~ year01 * student_bin, data = df_lr, family = binomial())
p_interaction <- anova(m0, m1, test = "LRT")$`Pr(>Chi)`[2]

note_txt <- ""
if (use_window_final && MISSING_TIME_PUBLISHED_POLICY == "drop") {
  n_missing_time_published <- sum(df$published01 == 1 & !is.finite(df$time_to_pub_mo), na.rm = TRUE)
  if (n_missing_time_published > 0) {
    note_txt <- paste0(" Note: ", n_missing_time_published,
                       " published abstract(s) had missing time-to-publication and were excluded from the within-window endpoint.")
  }
}

caption_txt <- paste0(
  "P-values: student-share p=", fmt_p(p_student_share),
  "; publication p=", fmt_p(p_pub_overall),
  "; interaction p=", fmt_p(p_interaction), " (LRT). ",
  "Error bars are exact 95% binomial CIs.",
  note_txt
)

base_theme <- theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title.y = element_text(margin = margin(r = 10)),
    plot.margin = margin(10, 18, 10, 18)
  )

yA_max <- min(1.0, max(sum_student$ci_high, na.rm = TRUE) + 0.10)

pA <- ggplot(sum_student, aes(x = Year_num, y = prop)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.0, linewidth = 0.8) +
  geom_point(size = 3, shape = 17) +
  geom_text(data = subset(sum_student, Year_num == 2010),
            aes(label = label),
            nudge_x = 0.8, hjust = 0, vjust = -0.6, size = 3.2) +
  geom_text(data = subset(sum_student, Year_num == 2019),
            aes(label = label),
            nudge_x = -0.8, hjust = 1, vjust = -0.6, size = 3.2) +
  scale_x_continuous(breaks = c(2010, 2019), labels = c("2010", "2019"),
                     expand = expansion(mult = c(0.20, 0.20))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, yA_max),
                     expand = expansion(mult = c(0.00, 0.02))) +
  coord_cartesian(clip = "off") +
  labs(title = "A. Medical-student first authorship increased",
       y = "Medical-student first author",
       x = "Year") +
  base_theme

yB_max <- min(1.0, max(sum_pub$ci_high, na.rm = TRUE) + 0.10)

pB <- ggplot(sum_pub, aes(x = Year_num, y = prop)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.0, linewidth = 0.8) +
  geom_point(size = 3) +
  geom_line(aes(group = 1), linewidth = 0.6) +
  geom_text(data = subset(sum_pub, Year_num == 2010),
            aes(label = label),
            nudge_x = 0.8, hjust = 0, vjust = -0.6, size = 3.0) +
  geom_text(data = subset(sum_pub, Year_num == 2019),
            aes(label = label),
            nudge_x = -0.8, hjust = 1, vjust = -0.6, size = 3.0) +
  facet_wrap(~ Student_lab, nrow = 1) +
  scale_x_continuous(breaks = c(2010, 2019), labels = c("2010", "2019"),
                     expand = expansion(mult = c(0.20, 0.20))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, yB_max),
                     expand = expansion(mult = c(0.00, 0.02))) +
  coord_cartesian(clip = "off") +
  labs(title = outcome_title, y = outcome_ylabel, x = "Year") +
  base_theme +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        panel.spacing = grid::unit(2.2, "lines"))

main_title <- "Podium-to-print publication declined\ndespite increased medical-student first authorship"

fig <- pA / pB +
  patchwork::plot_layout(heights = c(1, 1.2)) +
  patchwork::plot_annotation(
    title = main_title,
    caption = if (SHOW_PVAL_CAPTION) caption_txt else NULL,
    theme = theme(
      plot.title = element_text(face = "bold", size = 18),
      plot.caption = element_text(size = 10, hjust = 0),
      plot.margin = margin(12, 18, 12, 18)
    )
  )

ggsave(paste0(OUT_PREFIX, ".png"), fig, width = WIDTH_IN, height = HEIGHT_IN, dpi = DPI)
ggsave(paste0(OUT_PREFIX, ".pdf"), fig, width = WIDTH_IN, height = HEIGHT_IN)

message("Wrote: ", OUT_PREFIX, ".png / .pdf")
