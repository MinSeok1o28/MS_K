# Gate analysis with quantile-based grouping

# Period2 Group variable creation
mod2 <- mod2 %>%
  mutate(
    tau_dr_rf_cf = tau_dr_rf_cf,
    tau_local_cf = tau_local_cf,
    age_group = cut(age,
                    breaks = c(0,30,40,50,60,Inf),
                    labels = c("<30","30-39","40-49","50-59","60+"),
                    right = FALSE),
    det_group = ntile(det_count, 3),
    wgt_group = ntile(wgt_min, 3),
    insuran_group = ntile(insuran, 3),
    race_group = ntile(race, 3),
    severity_group = ntile(severity, 3),
    loc_group = case_when(
      loc_CLINIC   == 1 ~ "CLINIC",
      loc_HOSPITAL == 1 ~ "HOSPITAL",
      loc_OFFICE   == 1 ~ "OFFICE",
      TRUE              ~ NA_character_
    )
  )

# GATE function
test_calc_gate <- function(df, tau_col, group_var) {
  df %>%
    filter(!is.na(.data[[tau_col]]), !is.na(.data[[group_var]])) %>%
    group_by(Group = .data[[group_var]]) %>%
    summarise(
      N = n(),
      GATE = mean(.data[[tau_col]], na.rm = TRUE),
      SE = sd(.data[[tau_col]], na.rm = TRUE) / sqrt(N),
      CI_lo = GATE - 1.96 * SE,
      CI_hi = GATE + 1.96 * SE,
      .groups = "drop"
    )
}

# Group list
period2_groups <- list(
  age = "age_group",
  severity = "severity_group",
  det = "det_group",
  wgt = "wgt_group",
  loc = "loc_group",
  insuran = "insuran_group",
  race = "race_group"
)

models <- c(dr = "tau_dr_rf_cf", cf = "tau_local_cf")

# Plotting for Period2
library(ggplot2)

dir_name <- "gate_plots_period2"
if (!dir.exists(dir_name)) dir.create(dir_name, recursive = TRUE)

for (name in names(period2_groups)) {
  gv <- period2_groups[[name]]
  df_list <- lapply(names(models), function(m) {
    test_calc_gate(mod2, tau_col = models[m], group_var = gv) %>%
      mutate(model = ifelse(m == "dr", "DR+RF", "Local-CF"))
  })
  df_plot <- bind_rows(df_list)

  p <- ggplot(df_plot, aes(x = factor(Group), y = GATE, fill = model)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    geom_errorbar(aes(ymin = CI_lo, ymax = CI_hi),
                  position = position_dodge(width = 0.8),
                  width = 0.2, size = 0.8) +
    labs(
      x = name,
      y = "GATE (95% CI)",
      title = paste0("Period2: GATE by ", name),
      fill = "Model"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  if (name %in% c("det", "wgt", "insuran", "race", "severity")) {
    p <- p + scale_x_discrete(labels = function(x) paste0("Q", x))
  }

  print(p)

  filename <- file.path(dir_name, paste0("gate2_", name, ".png"))
  ggsave(filename,
         plot = p,
         width = 6, height = 4,
         dpi = 300)
}


# Period3 ------------------------------------------------------------

mod3 <- mod3 %>%
  mutate(
    tau_dr_rf_cf = tau_dr_rf_cf,
    tau_local_cf = tau_local_cf,
    age_group = cut(age,
                    breaks = c(0,30,40,50,60,Inf),
                    labels = c("<30","30-39","40-49","50-59","60+"),
                    right = FALSE),
    det_group = ntile(det_count, 4),
    wgt_group = ntile(wgt_min, 4),
    insuran_group = ntile(insuran, 4),
    race_group = ntile(race, 4),
    severity_group = ntile(severity, 4),
    loc_group = case_when(
      loc_CLINIC   == 1 ~ "CLINIC",
      loc_HOSPITAL == 1 ~ "HOSPITAL",
      loc_OFFICE   == 1 ~ "OFFICE",
      TRUE              ~ NA_character_
    )
  )

period3_groups <- list(
  age = "age_group",
  severity = "severity_group",
  det = "det_group",
  wgt = "wgt_group",
  loc = "loc_group",
  insuran = "insuran_group",
  race = "race_group"
)


# Plotting for Period3

dir_name <- "gate_plots_period3"
if (!dir.exists(dir_name)) dir.create(dir_name, recursive = TRUE)

for (name in names(period3_groups)) {
  gv <- period3_groups[[name]]
  df_list <- lapply(names(models), function(m) {
    test_calc_gate(mod3, tau_col = models[m], group_var = gv) %>%
      mutate(model = ifelse(m == "dr", "DR+RF", "Local-CF"))
  })
  df_plot <- bind_rows(df_list)

  p <- ggplot(df_plot, aes(x = factor(Group), y = GATE, fill = model)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    geom_errorbar(aes(ymin = CI_lo, ymax = CI_hi),
                  position = position_dodge(width = 0.8),
                  width = 0.2, size = 0.8) +
    labs(
      x = name,
      y = "GATE (95% CI)",
      title = paste0("Period3: GATE by ", name),
      fill = "Model"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  if (name %in% c("det", "wgt", "insuran", "race", "severity")) {
    p <- p + scale_x_discrete(labels = function(x) paste0("Q", x))
  }

  print(p)

  filename <- file.path(dir_name, paste0("gate3_", name, ".png"))
  ggsave(filename,
         plot = p,
         width = 6, height = 4,
         dpi = 300)
}
