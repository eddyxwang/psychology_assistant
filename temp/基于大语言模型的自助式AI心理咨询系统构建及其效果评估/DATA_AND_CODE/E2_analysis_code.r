library(bruceR)

set.wd()

options(digits = 6, emmeans.digits = 6)

output_file <- "E2_results.txt"
sink(output_file)

load("E2_data.RData")

data <- data %>%
  rename(
    ID = id,
    Wave = wave,
    Group = group,
    Subgroup = subgroup,
    Age = age,
    Gender = gender,
    Stress = dass_s,
    Anxiety = dass_a,
    Depression = dass_d,
    Loneliness = ssl
  ) %>%
  mutate(
    Wave = case_when(
      Wave == 1 ~ "T1",
      Wave == 2 ~ "T2",
      Wave == 3 ~ "T3",
      TRUE ~ as.character(Wave)
    ),
    Gender = case_when(
      Gender == "男" ~ "Male",
      Gender == "女" ~ "Female",
      TRUE ~ as.character(Gender)
    )
  )

factor_vars <- c("ID", "Wave", "Group", "Subgroup", "Gender")
for (var in factor_vars) {
  data[[var]] <- as.factor(data[[var]])
}

completion_rate <- data %>%
  group_by(Wave) %>%
  summarise(
    Participants = n(),
    Percent = round(n()/nrow(data[data$Wave == "T1",])*100, 1)
  )
print(completion_rate, digits = 6)

completers <- data %>%
  group_by(ID) %>%
  summarise(Waves_Completed = n_distinct(Wave)) %>%
  mutate(Completer = Waves_Completed == 3)

data <- data %>%
  left_join(completers, by = "ID")

baseline_data <- data %>% filter(Wave == "T1")

age_test <- t.test(Age ~ Completer, data = baseline_data)
cat("年龄差异 t-test:", "t =", round(age_test$statistic, 2), 
    "df =", round(age_test$parameter, 2), 
    "p =", round(age_test$p.value, 3), 
    "95% CI [", round(age_test$conf.int[1], 2), ", ", round(age_test$conf.int[2], 2), "]\n")

gender_test <- chisq.test(table(baseline_data$Completer, baseline_data$Gender))
cat("性别差异 chi-squared test:", "X2 =", round(gender_test$statistic, 2), 
    "df =", gender_test$parameter, 
    "p =", round(gender_test$p.value, 3), "\n")

health_vars <- c("Depression", "Anxiety", "Stress", "Loneliness")
for (var in health_vars) {
  formula <- as.formula(paste(var, "~ Completer"))
  test_result <- t.test(formula, data = baseline_data)
  cat(var, "差异 t-test:", "t =", round(test_result$statistic, 2), 
      "df =", round(test_result$parameter, 2), 
      "p =", round(test_result$p.value, 3), 
      "95% CI [", round(test_result$conf.int[1], 2), ", ", round(test_result$conf.int[2], 2), "]\n")
}

des_vars <- c("Age","Gender","Depression", "Anxiety", "Stress", "Loneliness")
for (wave in c("T1", "T2", "T3")) {
  wave_data <- data %>% filter(Wave == wave)
  Describe(wave_data[, des_vars], digits = 2)
  Corr(wave_data[, des_vars], digits = 2)
}

dependent_vars <- c("Depression", "Anxiety", "Stress", "Loneliness")
models <- list()
model_summaries <- list()
interaction_tests <- list()

for (dv in dependent_vars) {
  formula <- as.formula(paste(dv, "~ Subgroup * Wave + Age + Gender + (1 | ID)"))
  model <- lmer(formula, data = data)
  models[[dv]] <- model
  
  formula_no_int <- as.formula(paste(dv, "~ Subgroup + Wave + Age + Gender + (1 | ID)"))
  model_no_int <- lmer(formula_no_int, data = data)
  models[[paste0(dv, "_no_int")]] <- model_no_int
  
  anova_test <- anova(model_no_int, model)
  interaction_tests[[dv]] <- anova_test
  print(anova_test, digits = 6)
  model_summary(model, std = F, digits = 2, zero = T, check = F)
  
  if (anova_test$`Pr(>Chisq)`[2] < 0.1) {
    emm <- emmeans(model, ~ Subgroup | Wave)
    print(summary(emm, infer = TRUE, digits = 6), digits = 6)
    for (group in levels(data$Subgroup)) {
      group_emm <- emmeans(model, ~ Wave | Subgroup, at = list(Subgroup = group))
      contrasts <- contrast(group_emm, method = "pairwise")
      contrasts_summary <- summary(contrasts, infer = TRUE)
      print(contrasts_summary, digits = 6)
    }
    for (wave in c("T1", "T2", "T3")) {
      wave_emm <- emmeans(model, ~ Subgroup | Wave, at = list(Wave = wave))
      contrasts <- contrast(wave_emm, method = "pairwise")
      contrasts_summary <- summary(contrasts, infer = TRUE)
      print(contrasts_summary, digits = 6)
    }
  } else {
    group_emm <- emmeans(model_no_int, ~ Subgroup)
    group_pairs <- pairs(group_emm)
    group_pairs_summary <- summary(group_pairs, infer = TRUE)
    print(group_pairs_summary, digits = 6)
    
    time_emm <- emmeans(model_no_int, ~ Wave)
    time_pairs <- pairs(time_emm)
    time_pairs_summary <- summary(time_pairs, infer = TRUE)
    print(time_pairs_summary, digits = 6)
  }
}

completers_data <- data %>% filter(Completer == TRUE)

for (dv in dependent_vars) {
  formula <- as.formula(paste(dv, "~ Subgroup * Wave + Age + Gender + (1 | ID)"))
  full_model <- models[[dv]]
  completers_model <- lmer(formula, data = completers_data)
  full_summary <- summary(full_model)
  completers_summary <- summary(completers_model)
  full_coefs <- full_summary$coefficients
  completers_coefs <- completers_summary$coefficients
  common_params <- intersect(rownames(full_coefs), rownames(completers_coefs))
  coef_comparison <- data.frame(
    Parameter = common_params,
    Full_Sample_Est = full_coefs[common_params, "Estimate"],
    Full_Sample_p = full_coefs[common_params, "Pr(>|t|)"],
    Completers_Est = completers_coefs[common_params, "Estimate"],
    Completers_p = completers_coefs[common_params, "Pr(>|t|)"]
  )
  significant_opposite_signs <- (coef_comparison$Full_Sample_p < 0.05 | coef_comparison$Completers_p < 0.05) & 
    (sign(coef_comparison$Full_Sample_Est) != sign(coef_comparison$Completers_Est))
  if (sum(significant_opposite_signs) > 0) {
    sig_opposite_params <- coef_comparison[significant_opposite_signs, ]
    for (i in 1:nrow(sig_opposite_params)) {
      param <- sig_opposite_params$Parameter[i]
      full_est <- sig_opposite_params$Full_Sample_Est[i]
      full_p <- sig_opposite_params$Full_Sample_p[i]
      comp_est <- sig_opposite_params$Completers_Est[i]
      comp_p <- sig_opposite_params$Completers_p[i]
      full_sig <- if(full_p < 0.05) "显著" else "不显著"
      comp_sig <- if(comp_p < 0.05) "显著" else "不显著"
      cat(sprintf("参数 '%s':\n", param))
      cat(sprintf("  全样本: β = %.4f, p = %.4f (%s)\n", full_est, full_p, full_sig))
      cat(sprintf("  完成者: β = %.4f, p = %.4f (%s)\n", comp_est, comp_p, comp_sig))
    }
  } else {
    cat("\n稳健性检验结果: 未发现系数显著且符号相反的参数，结果稳健。\n")
  }
}

all_interaction_models <- list()
for (dv in dependent_vars) {
  all_interaction_models[[dv]] <- models[[dv]]
}

model_summary(
  all_interaction_models, 
  std = TRUE,
  digits = 2,
  file = "all_models_standardized.doc",
  zero = T, 
  modify.head = dependent_vars
)

model_summary(
  all_interaction_models, 
  std = FALSE,
  digits = 2,
  file = "all_models_unstandardized.doc",
  zero = T, 
  modify.head = dependent_vars
)

while (sink.number() > 0) sink()


# 可视化====
# Load necessary libraries
library(ggplot2)
library(gridExtra)
library(grid)
library(tidyverse)

# Load font handling packages
dependent_vars <- c("Depression", "Anxiety", "Stress", "Loneliness")

if (!requireNamespace("showtext", quietly = TRUE)) install.packages("showtext")
library(showtext)
library(sysfonts)

# Attempt to set Chinese locale support for different systems
locale_options <- c(
  "zh_CN.UTF-8",                    # Linux/Mac
  "Chinese (Simplified)_China.936", # Windows
  "chs",                            # Windows abbreviated
  "C"                               # Default
)

for (loc in locale_options) {
  tryCatch({
    Sys.setlocale("LC_CTYPE", loc)
    break
  }, error = function(e) {
    # Silently fail and try the next
  })
}

# Enable showtext support
showtext_auto()

# Add fonts (font filenames may differ by system; adjust if necessary)
font_add("times", "times.ttf")       # Times New Roman
font_add("simsum", "simsun.ttc")     # SimSun (Chinese)

# Define font sizes and line widths
font_size_pt <- 8                    # Six-point font (8 pt)
line_width_pt <- 0.2                 # 0.5 pt line width

# Define a simple theme with specified fonts and sizes, retaining panel border
simple_theme <- theme_bw() +
  theme(
    text = element_text(family = "times", size = font_size_pt),
    plot.title = element_text(family = "simsum", size = font_size_pt, hjust = 0.5),
    axis.title = element_text(family = "times", size = font_size_pt),
    axis.text = element_text(family = "times", size = font_size_pt),
    legend.position = "none",  # Hide legends in individual plots
    panel.grid.major = element_line(color = "gray90", linewidth = line_width_pt/2),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray50", fill = NA, linewidth = line_width_pt),
    plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")
  )

# Assume `dependent_vars` and `models` are predefined objects in your session.
# Here, each dependent variable's data are processed using emmeans and stored in a list.
plot_data_list <- list()
for (dv in dependent_vars) {
  # Calculate estimated marginal means (replace with your actual model and variable names)
  emm_data <- as.data.frame(emmeans(models[[dv]], ~ Subgroup | Wave))
  emm_data$Variable <- dv  # Add an identifier for the variable
  plot_data_list[[dv]] <- emm_data
}

# Combine data from all dependent variables.
all_plot_data <- bind_rows(plot_data_list)

# Define subplot titles (including prefixes for journal formatting)
var_labels <- c(
  "Depression" = "A. 抑郁",
  "Anxiety"    = "B. 焦虑",
  "Stress"     = "C. 压力",
  "Loneliness" = "D. 孤独感"
)

# 提取唯一实验组
unique_groups <- unique(all_plot_data$Subgroup)

# 创建组名映射 - 将CFMR组改为XX组格式
# 四个组是按C, F, M, R顺序
group_name_mapping <- setNames(
  c("C组  ", "F组  ", "M组  ", "R组  ")[1:length(unique_groups)], 
  unique_groups
)

# Define single grayscale color for all groups
colors <- rep("#666666", length(unique_groups))  # Uniform medium gray for all groups

# Define clear, filled shapes for better distinction
shapes <- c(
  16,  # Filled circle (C组)
  17,  # Filled triangle up (F组)
  15,  # Filled square (M组)
  18   # Filled diamond (R组)
)[1:length(unique_groups)]

# Name the vectors using original group names (as they appear in data)
names(colors) <- unique_groups
names(shapes) <- unique_groups

# Define line types - 所有组使用实线
line_types <- rep("solid", length(unique_groups))
names(line_types) <- unique_groups

# Assume Wave is a factor with ordered levels (e.g., "T1", "T2", "T3")
wave_levels <- levels(all_plot_data$Wave)  # Or unique(all_plot_data$Wave) if not factored yet
if (!is.factor(all_plot_data$Wave)) {
  all_plot_data$Wave <- factor(all_plot_data$Wave, levels = c("T1", "T2", "T3"))  # Assume standard levels
  wave_levels <- c("T1", "T2", "T3")
}

# Generate individual plots for each dependent variable.
plot_list <- list()
for (dv in dependent_vars) {
  dv_data <- all_plot_data %>% filter(Variable == dv)
  
  # Assign numeric x for continuous plotting and midpoint calculation
  dv_data$x_num <- as.numeric(dv_data$Wave)
  
  # Determine y-axis limits and breaks (with a slight expansion)
  y_min <- min(dv_data$emmean - dv_data$SE) * 0.9
  y_max <- max(dv_data$emmean + dv_data$SE) * 1.1
  y_breaks <- seq(ceiling(y_min), floor(y_max), by = 1)
  
  # Calculate interpolated midpoints for additional shapes (between T1-T2 and T2-T3)
  mid_data <- dv_data %>%
    group_by(Subgroup) %>%
    arrange(x_num) %>%
    mutate(
      next_x = lead(x_num),
      next_y = lead(emmean),
      mid_x = (x_num + next_x) / 2,
      mid_y = (emmean + next_y) / 2
    ) %>%
    filter(!is.na(next_x)) %>%
    ungroup() %>%
    mutate(Wave = NA)  # No Wave for mids to avoid labeling issues
  
  # Plot with optimizations: smaller points, mid-shapes, single grayscale color
  p <- ggplot(dv_data, aes(x = x_num, y = emmean, 
                           color = Subgroup, group = Subgroup,
                           shape = Subgroup, linetype = Subgroup)) +
    geom_hline(yintercept = y_breaks, color = "gray95", linewidth = line_width_pt/2) +
    geom_line(linewidth = 0.25) +  # Thinner line (from 0.3 to 0.25) for finer appearance
    geom_point(size = 1.5) +  # Keep reduced size to minimize overlap
    geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), 
                  width = 0.1, linewidth = 0.4) +
    geom_point(data = mid_data, aes(x = mid_x, y = mid_y, 
                                    shape = Subgroup, color = Subgroup), 
               size = 1.2, inherit.aes = FALSE) +  # Smaller mid-shapes
    scale_shape_manual(values = shapes) +
    scale_color_manual(values = colors) +
    scale_linetype_manual(values = line_types) +
    scale_y_continuous(
      limits = c(y_min, y_max),
      breaks = y_breaks,
      expand = c(0, 0)
    ) +
    scale_x_continuous(
      breaks = 1:length(wave_levels),
      labels = wave_levels,
      expand = c(0.05, 0.05)  # Slight expansion to prevent clipping
    ) +
    labs(
      title = var_labels[dv],
      x = "测量时间点",
      y = "得分"
    ) +
    simple_theme
  
  plot_list[[dv]] <- p
}

# Create a dedicated legend plot with the new group names and updated shapes/sizes
legend_data <- data.frame(
  x = rep(1:2, each = length(unique_groups)),
  y = rep(1, 2 * length(unique_groups)),
  group = rep(unique_groups, 2)
)

legend_plot <- ggplot(legend_data, aes(x = x, y = y, group = group)) +
  geom_line(aes(linetype = group, color = group), linewidth = 0.25) +  # Match thinner line
  geom_point(aes(shape = group, color = group), size = 1.5) +  # Match reduced point size
  scale_shape_manual(values = shapes, name = "组别:", 
                     labels = group_name_mapping) +
  scale_color_manual(values = colors, name = "组别:", 
                     labels = group_name_mapping) +
  scale_linetype_manual(values = line_types, name = "组别:", 
                        labels = group_name_mapping) +
  guides(
    color = guide_legend(override.aes = list(
      linetype = line_types,
      shape = shapes
    ), title = "组别:")
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(family = "simsum", size = font_size_pt, face = "bold"),
    legend.text = element_text(family = "times", size = font_size_pt),
    legend.key.width = unit(1.5, "cm"),
    legend.key.height = unit(0.6, "cm"),
    legend.background = element_rect(fill = "white", color = NA),
    legend.spacing.x = unit(0.3, "cm")  # Keep spacing for readability
  )

# Extract the legend grob.
tmp <- ggplot_gtable(ggplot_build(legend_plot))
leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
legend_grob <- tmp$grobs[[leg]]

# Arrange subplots and legend into a combined grid.
plots_grid <- arrangeGrob(
  plot_list[[dependent_vars[1]]], 
  plot_list[[dependent_vars[2]]],
  plot_list[[dependent_vars[3]]], 
  plot_list[[dependent_vars[4]]],
  ncol = 2
)

combined_plots <- grid.arrange(
  plots_grid,
  legend_grob,
  nrow = 2,
  heights = c(10, 1)
)

# Set image dimensions as required (full-column width between 100-140mm, here 140mm)
width_mm <- 140
height_mm <- 110  # Adjusted based on aspect ratio
width_in <- width_mm / 25.4
height_in <- height_mm / 25.4

# Save the final combined plot in SVG and PNG formats at 600 dpi.
# ggsave("Figure_4.svg", combined_plots,
#        width = width_in, height = height_in, dpi = 600)
ggsave("Figure_4.png", combined_plots, 
       width = width_in, height = height_in, dpi = 600)

# Display the combined plot
print(combined_plots)