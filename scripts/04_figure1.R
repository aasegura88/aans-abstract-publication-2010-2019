# ============================================================
# Figure 1: Kaplan–Meier time to podium-to-print publication (2010 vs 2019)
# - Administrative censoring at 36 months
# - Plots cumulative probability published: 1 - S(t)
# Outputs:
#   outputs/figure1_km_time_to_publication_professional.pdf
#   outputs/figure1_km_time_to_publication_professional.png
#   outputs/figure1_km_timepoint_estimates.csv
#   outputs/figure1_km_sessionInfo.txt
# ============================================================

FOLLOW_UP_MONTHS <- 36
TIME_BREAK_BY    <- 6
SHOW_RISK_TABLE  <- TRUE

FIG_W_IN <- 8.5
FIG_H_IN <- if (SHOW_RISK_TABLE) 7 else 3.8
PNG_DPI  <- 600

BASE_SIZE <- 11
TITLE_SIZE <- 16
SUBTITLE_SIZE <- 11

DATA_PATH <- Sys.getenv("AANS_DATA_PATH", unset = file.path("data", "AANScleanedv3 JMR 2.25.csv"))
OUT_DIR   <- Sys.getenv("AANS_OUT_DIR",   unset = "outputs")

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
stopifnot(file.exists(DATA_PATH))

req_pkgs <- c("survival", "ggplot2", "scales", "patchwork")
missing <- req_pkgs[!vapply(req_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Missing packages: ", paste(missing, collapse = ", "),
       "\nInstall in R with: install.packages(c(", paste(sprintf('"%s"', missing), collapse = ", "), "))")
}
invisible(lapply(req_pkgs, library, character.only = TRUE))
options(stringsAsFactors = FALSE)

df0 <- read.csv(DATA_PATH, check.names = FALSE)

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

fmt_p <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) return("<0.001")
  sprintf("%.3f", p)
}

wrap_text <- function(x, width = 95) paste(strwrap(x, width = width), collapse = "\n")

survfit_to_df <- function(fit, t_end) {
  strata_names <- names(fit$strata)
  counts <- as.integer(fit$strata)

  ends <- cumsum(counts)
  starts <- c(1, head(ends, -1) + 1)

  out <- vector("list", length(strata_names))
  for (i in seq_along(strata_names)) {
    idx <- starts[i]:ends[i]
    ti <- fit$time[idx]
    si <- fit$surv[idx]

    df_i <- data.frame(time = c(0, ti), surv = c(1, si), strata = strata_names[i])

    last_t <- tail(df_i$time, 1)
    last_s <- tail(df_i$surv, 1)
    if (is.finite(t_end) && last_t < t_end) {
      df_i <- rbind(df_i, data.frame(time = t_end, surv = last_s, strata = strata_names[i]))
    }
    out[[i]] <- df_i
  }
  do.call(rbind, out)
}

req <- c("Year", "Published", "Timetopublicationmonthstimefromabstractapril2019")
stopifnot(all(req %in% names(df0)))

year <- year_label(df0$Year)
published01 <- as01(df0$Published)
t_pub <- suppressWarnings(as.numeric(df0$Timetopublicationmonthstimefromabstractapril2019))

keep <- !is.na(year) & year %in% c("2010", "2019")
dat <- data.frame(
  year = factor(year[keep], levels = c("2010", "2019")),
  published01 = published01[keep],
  t_pub = t_pub[keep]
)

bad <- which(dat$published01 == 1 & !is.finite(dat$t_pub))
if (length(bad)) {
  warning(sprintf("Dropping %d row(s): Published==1 but missing/invalid time-to-publication.", length(bad)))
  dat <- dat[-bad, , drop = FALSE]
}

t_end <- FOLLOW_UP_MONTHS
dat$time <- ifelse(dat$published01 == 1 & is.finite(dat$t_pub),
                   pmin(dat$t_pub, t_end),
                   t_end)
dat$event <- as.integer(dat$published01 == 1 & is.finite(dat$t_pub) & dat$t_pub <= t_end)

surv_obj <- survival::Surv(time = dat$time, event = dat$event)
fit <- survival::survfit(surv_obj ~ year, data = dat)

lr <- survival::survdiff(surv_obj ~ year, data = dat)
p_lr <- 1 - pchisq(lr$chisq, df = length(lr$n) - 1)

n_total <- table(dat$year)
n_events <- tapply(dat$event, dat$year, sum)

subtitle_raw <- paste0(
  "Log-rank p=", fmt_p(p_lr),
  " | 2010: ", n_events[["2010"]], "/", n_total[["2010"]], " published within window; ",
  "2019: ", n_events[["2019"]], "/", n_total[["2019"]], " | ",
  "Censored at ", t_end, " months"
)
subtitle_txt <- wrap_text(subtitle_raw, width = 92)

df_curve <- survfit_to_df(fit, t_end)
df_curve$year <- factor(sub("^year=", "", df_curve$strata), levels = c("2010", "2019"))
df_curve$cum_published <- 1 - df_curve$surv

times_risk <- seq(0, t_end, by = TIME_BREAK_BY)
sm <- summary(fit, times = times_risk, extend = TRUE)
df_risk <- data.frame(
  year = factor(sub("^year=", "", sm$strata), levels = c("2010", "2019")),
  time = sm$time,
  n_risk = sm$n.risk
)
df_risk$hjust <- ifelse(df_risk$time == 0, 0, 0.5)

x_pad_left  <- 0.9
x_pad_right <- 0.9

x_scale <- scale_x_continuous(
  breaks = times_risk,
  limits = c(0, t_end),
  expand = expansion(add = c(x_pad_left, x_pad_right))
)

# Use linetype only (print- and colorblind-friendly)
p_curve <- ggplot(df_curve, aes(x = time, y = cum_published, linetype = year)) +
  geom_step(direction = "hv", linewidth = 0.9, color = "black") +
  x_scale +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1),
    limits = c(0, 1),
    breaks = scales::pretty_breaks(5)
  ) +
  scale_linetype_manual(values = c("2010" = "solid", "2019" = "dashed")) +
  labs(
    title = "Time to podium-to-print publication",
    # subtitle = subtitle_txt,
    x = NULL,
    y = "Cumulative probability published",
    linetype = NULL
  ) +
  theme_classic(base_size = BASE_SIZE) +
  theme(
    text = element_text(family = "sans"),
    plot.title = element_text(face = "bold", size = TITLE_SIZE, hjust = 0.5),
    # plot.subtitle = element_text(size = SUBTITLE_SIZE, hjust = 0.5, lineheight = 1.05),
    legend.position = "top",
    legend.text = element_text(size = 12),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin = margin(t = 12, r = 28, b = 6, l = 18)
  )

p_risk <- ggplot(df_risk, aes(x = time, y = year)) +
  geom_text(aes(label = n_risk, hjust = hjust), size = 4) +
  x_scale +
  labs(title = "Number at risk", x = "Months from AANS podium presentation", y = NULL) +
  theme_classic(base_size = BASE_SIZE) +
  theme(
    text = element_text(family = "sans"),
    plot.title = element_text(face = "bold", size = 12, hjust = 0),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.margin = margin(t = 0, r = 28, b = 10, l = 18)
  )

if (SHOW_RISK_TABLE) {
  fig <- p_curve / p_risk + patchwork::plot_layout(heights = c(3.2, 1.1))
} else {
  fig <- p_curve + theme(axis.text.x = element_text(), axis.ticks.x = element_line()) +
    labs(x = "Months from AANS podium presentation")
}

pdf_file <- file.path(OUT_DIR, "figure1_km_time_to_publication_professional.pdf")
png_file <- file.path(OUT_DIR, "figure1_km_time_to_publication_professional.png")

if (capabilities("cairo")) {
  ggsave(pdf_file, fig, width = FIG_W_IN, height = FIG_H_IN, units = "in", device = cairo_pdf)
} else {
  ggsave(pdf_file, fig, width = FIG_W_IN, height = FIG_H_IN, units = "in")
}
ggsave(png_file, fig, width = FIG_W_IN, height = FIG_H_IN, units = "in", dpi = PNG_DPI)

times_out <- c(6, 12, 24, 36)
times_out <- times_out[times_out <= t_end]
sm2 <- summary(fit, times = times_out, extend = TRUE)
tp <- data.frame(
  year = sub("^year=", "", sm2$strata),
  months = sm2$time,
  surv_not_published = sm2$surv,
  cum_published = 1 - sm2$surv,
  cum_published_lcl = 1 - sm2$upper,
  cum_published_ucl = 1 - sm2$lower,
  stringsAsFactors = FALSE
)
write.csv(tp, file.path(OUT_DIR, "figure1_km_timepoint_estimates.csv"), row.names = FALSE)
writeLines(capture.output(sessionInfo()), file.path(OUT_DIR, "figure1_km_sessionInfo.txt"))

message("Wrote: ", pdf_file, " / ", png_file)
