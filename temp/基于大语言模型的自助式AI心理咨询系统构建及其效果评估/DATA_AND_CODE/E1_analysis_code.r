library(car)       # Levene's test
library(bruceR)     # 数据处理

# 读取数据
scb_data <- read.csv("E1_scb.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
scp_data <- read.csv("E1_scp.csv", encoding = "UTF-8", stringsAsFactors = FALSE)

# 确保模型列为因子
scb_data$模型 <- as.factor(scb_data$模型)

# 定义评估维度
dimensions <- c("规范性", "专业度", "情感理解与共情能力", "一致性与连贯性", "总分")

# 创建输出文件
output_file <- "E1_results.txt"
sink(output_file, append = FALSE)
cat("====== E1实验方差齐性检验及稳健统计分析结果 ======\n")

# 函数：进行方差齐性检验 (仅Levene's test)
test_homogeneity <- function(data, dv) {
  leveneTest(data[[dv]], data$模型)
}

# 函数：Games-Howell事后检验
games_howell_manual <- function(data, dv_name) {
  models <- levels(data$模型)
  results <- data.frame(comparison = character(), mean_diff = numeric(), se = numeric(),
                        t_stat = numeric(), df = numeric(), p_value = numeric(),
                        ci_lower = numeric(), ci_upper = numeric(), stringsAsFactors = FALSE)
  
  for (i in 1:(length(models)-1)) {
    for (j in (i+1):length(models)) {
      group1 <- data[[dv_name]][data$模型 == models[i]]
      group2 <- data[[dv_name]][data$模型 == models[j]]
      
      n1 <- length(group1); n2 <- length(group2)
      m1 <- mean(group1); m2 <- mean(group2)
      s1 <- sd(group1); s2 <- sd(group2)
      
      se <- sqrt((s1^2/n1) + (s2^2/n2))
      t_stat <- (m1 - m2) / se
      df <- (s1^2/n1 + s2^2/n2)^2 / ((s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1))
      p_val <- 2 * (1 - pt(abs(t_stat), df))
      t_crit <- qt(0.975, df)
      ci_lower <- (m1 - m2) - t_crit * se
      ci_upper <- (m1 - m2) + t_crit * se
      
      results <- rbind(results, data.frame(comparison = paste(models[i], "vs", models[j]),
                                           mean_diff = m1 - m2, se = se, t_stat = t_stat,
                                           df = df, p_value = p_val,
                                           ci_lower = ci_lower, ci_upper = ci_upper))
    }
  }
  results
}

# 主分析循环
for (dim in dimensions) {
  cat(paste("======分析维度:", dim), "======\n")
  
  # 1. 描述性统计
  desc_stats <- scb_data %>%
    group_by(模型) %>%
    summarise(n = n(), mean = mean(.data[[dim]], na.rm = TRUE), sd = sd(.data[[dim]], na.rm = TRUE),
              min = min(.data[[dim]], na.rm = TRUE), max = max(.data[[dim]], na.rm = TRUE),
              .groups = 'drop')
  
  cat("\n1. 描述性统计:\n")
  print(desc_stats)
  
  # 2. 方差齐性检验 (仅Levene's)
  cat("\n2. 方差齐性检验:\n")
  levene_result <- test_homogeneity(scb_data, dim)
  levene_p <- levene_result$`Pr(>F)`[1]
  cat(sprintf("Levene's Test: F(%d, %d) = %.3f, p = %.3f\n", 
              levene_result$Df[1], levene_result$Df[2], levene_result$`F value`[1], levene_p))
  
  status <- ifelse(levene_p < 0.05, "方差不齐 (p < 0.05)", "方差齐性 (p ≥ 0.05)")
  cat(paste("结论:", status, "\n"))
  method <- ifelse(levene_p < 0.05, "Welch ANOVA", "Traditional ANOVA")
  
  # 3. 主效应检验
  cat("\n3. 主效应检验 (", method, "):\n")
  
  if (method == "Welch ANOVA") {
    welch_result <- oneway.test(scb_data[[dim]] ~ scb_data$模型, var.equal = FALSE)
    cat(sprintf("Welch ANOVA: F(%.2f, %.2f) = %.3f, p = %.3f\n",
                welch_result$parameter[1], welch_result$parameter[2],
                welch_result$statistic, welch_result$p.value))
    
    cat("\n4. Games-Howell事后检验:\n")
    print(games_howell_manual(scb_data, dim), digits = 3)
  } else {
    aov_result <- aov(scb_data[[dim]] ~ scb_data$模型)
    aov_summary <- summary(aov_result)
    cat(sprintf("Traditional ANOVA: F(%d, %d) = %.3f, p = %.3f\n",
                aov_summary[[1]]$Df[1], aov_summary[[1]]$Df[2],
                aov_summary[[1]]$`F value`[1], aov_summary[[1]]$`Pr(>F)`[1]))
    
    cat("\n4. Tukey HSD事后检验:\n")
    print(TukeyHSD(aov_result))
  }
}

# 总结报告
cat("\n", rep("=", 60), collapse = "", "\n")
cat("总结报告\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

cat("\n各维度方差齐性检验结果汇总:\n")
for (dim in dimensions) {
  levene_result <- test_homogeneity(scb_data, dim)
  levene_p <- levene_result$`Pr(>F)`[1]
  status <- ifelse(levene_p < 0.05, "方差不齐", "方差齐性")
  recommended <- ifelse(levene_p < 0.05, "Welch ANOVA + Games-Howell", "传统ANOVA + Tukey HSD")
  cat(sprintf("%s: %s (p = %.3f) -> 推荐: %s\n", dim, status, levene_p, recommended))
}

cat("\n分析说明:\n")
cat("1. 使用Levene's test检验方差齐性 (对非正态性稳健)\n")
cat("2. p < 0.05: 采用Welch ANOVA和Games-Howell事后检验\n")
cat("3. p ≥ 0.05: 采用传统ANOVA和Tukey HSD事后检验\n")
cat("4. Games-Howell不假设方差齐性和样本等大，更稳健\n")
cat("5. 置信区间提供效应量估计\n")

# 配对样本t检验
cat("\n", rep("=", 60), collapse = "", "\n")
cat("===== GPT-4o 优化前(SCB)与优化后(SCP)的配对样本T检验 =====\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

scb_gpt <- scb_data %>%
  filter(模型 == "GPT-4o") %>%
  arrange(对话id)

scp_sorted <- scp_data %>%
  arrange(对话id)

if (!all(scb_gpt$对话id == scp_sorted$对话id)) {
  stop("对话id不匹配，无法进行配对t检验")
}

cat("\n配对样本T检验结果:\n")
for (dim in dimensions) {
  t_test <- t.test(scp_sorted[[dim]], scb_gpt[[dim]], paired = TRUE)
  cat(sprintf("%s: t(%d) = %.3f, p = %.3f\n", 
              dim, t_test$parameter, t_test$statistic, t_test$p.value))
}

sink()  # 关闭sink

message("分析完成，结果保存至 ", output_file)