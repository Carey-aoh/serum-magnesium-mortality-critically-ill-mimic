rm(list = ls())
gc()
setwd("E:\\100-科研\\400_镁\\006重启版")
getwd()
library(readr)



# Step 1: 安装和加载必要的包
# 加载必要的包
packages <- c("MatchIt", "tableone", "cobalt", "survey", "ggplot2", "survival", "survminer", "dplyr", "WeightIt")
invisible(lapply(packages, library, character.only = TRUE))

# Step 2: 读入数据（替换为你的数据路径）
data <- read.csv("E:\\100-科研\\400_镁\\006重启版\\1.7_2.1_0524.csv")

colnames(data)[1] <- "id"  # 确保 ID 是 subject_id
str(data)
names(data)
summary(data)


# 1. 确保分类变量是因子
factor_vars <- c(
  "diabetes", "cerebrovascular_disease", "congestive_heart_failure", "renal_disease", 
  "liver_disease", "chronic_pulmonary_disease", "peripheral_vascular_disease", 
  "myocardial_infarct", "ventilation", "rrt", "calcium_supplementation", 
  "magnesium_sulfate_supplementation","malignant_cancer"
)

data[factor_vars] <- lapply(data[factor_vars], as.factor)

# 2. 设置Psm所需变量和Cox回归分析的变量
psm_vars <- c("Age", "Weight", "Charlson", "sofa", "renal_disease", "WBC",
              "ventilation")

cox_vars <- c(
  "Age", "Weight", "WBC",  "Hemoglobin", "Glucose","Chloride",
  "BUN", "Charlson", "sofa", 
  "diabetes", "cerebrovascular_disease","congestive_heart_failure","renal_disease","malignant_cancer",
  "liver_disease",  "peripheral_vascular_disease","chronic_pulmonary_disease",
  "myocardial_infarct", "ventilation", "rrt","magnesium_sulfate_supplementation"
)


# 3. 创建PSM公式
formula_psm <- as.formula(
  paste("magnesium_sulfate_supplementation ~", paste(psm_vars, collapse = " + "))
)

# 4. 执行倾向评分匹配（PSM）
psm <- matchit(formula_psm,
               data = data,
               method = "nearest",
               ratio = 1,
               caliper = 0.05)


# 5. 获取匹配后的数据
matched_data <- match.data(psm)




# 计算稳定的逆概率加权（Stabilized IPTW）
iptw_stabilized <- weightit(formula_psm, 
                            data = data, 
                            method = "ps", 
                            estimand = "ATE", 
                            stabilized = TRUE)


# 提取协变量平衡信息
bal_iptw_stabilized <- bal.tab(iptw_stabilized, un = TRUE)

# 查看平衡信息
print(bal_iptw_stabilized)


# ========== 提取三个阶段的协变量平衡信息 ==========
bal_unmatched <- bal.tab(formula_psm, data = data, estimand = "ATE")
bal_psm <- bal.tab(psm, un = TRUE)
#bal_iptw <- bal.tab(iptw, un = TRUE)
bal_iptw_stabilized <- bal.tab(iptw_stabilized, un = TRUE)

matched_data <- match.data(psm)


# 加载必要的包
library(survival)
library(dplyr)
library(broom)


# 初始化结果存储列表
results <- list()

### 1. 计算各组基本情况 --------------------------------------------------------
group1 <- matched_data$magnesium_sulfate_supplementation == 1
group0 <- matched_data$magnesium_sulfate_supplementation == 0

### 2. 主要结局：28天死亡率 --------------------------------------------------
# 描述性统计
death28_1 <- sum(matched_data$death_ICU28days[group1], na.rm = TRUE)
death28_0 <- sum(matched_data$death_ICU28days[group0], na.rm = TRUE)
perc28_1 <- round(death28_1/sum(group1)*100, 1)
perc28_0 <- round(death28_0/sum(group0)*100, 1)

# Cox回归分析
# 单变量
uni_cox_28 <- coxph(Surv(icu_intime_to_dod, death_ICU28days) ~ magnesium_sulfate_supplementation, 
                    data = matched_data)
uni_hr_28 <- exp(coef(uni_cox_28))
uni_ci_28 <- exp(confint(uni_cox_28))
uni_p_28 <- summary(uni_cox_28)$coefficients[1,5]

# 多变量
formula_multi_28 <- as.formula(
  paste("Surv(icu_intime_to_dod, death_ICU28days) ~", 
        paste(cox_vars, collapse = "+"))
)
multi_cox_28 <- coxph(formula_multi_28, data = matched_data)
multi_ci_28 <- exp(confint(multi_cox_28)["magnesium_sulfate_supplementation1", ])
multi_hr_28 <- exp(coef(multi_cox_28)["magnesium_sulfate_supplementation1"])
multi_p_28 <- summary(multi_cox_28)$coefficients["magnesium_sulfate_supplementation1", "Pr(>|z|)"]


# 存储结果
results[["28d_mortality"]] <- data.frame(
  outcome = "28天全因死亡率",
  MgSO4 = paste0(death28_1, " (", perc28_1, "%)"),
  No_MgSO4 = paste0(death28_0, " (", perc28_0, "%)"),
  univariate = sprintf("%.2f (%.2f-%.2f)", uni_hr_28, uni_ci_28[1], uni_ci_28[2]),
  univariate_p = format.pval(uni_p_28, eps = 0.001),
  multivariate = sprintf("%.2f (%.2f-%.2f)", multi_hr_28, multi_ci_28[1], multi_ci_28[2]),
  multivariate_p = format.pval(multi_p_28, eps = 0.001)
)

### 3. 主要结局：90天死亡率 --------------------------------------------------
# 描述性统计
death90_1 <- sum(matched_data$death_ICU90days[group1], na.rm = TRUE)
death90_0 <- sum(matched_data$death_ICU90days[group0], na.rm = TRUE)
perc90_1 <- round(death90_1/sum(group1)*100, 1)
perc90_0 <- round(death90_0/sum(group0)*100, 1)

# Cox回归分析
# 单变量
uni_cox_90 <- coxph(Surv(icu_intime90, death_ICU90days) ~ magnesium_sulfate_supplementation, 
                    data = matched_data)
uni_hr_90 <- exp(coef(uni_cox_90))
uni_ci_90 <- exp(confint(uni_cox_90))
uni_p_90 <- summary(uni_cox_90)$coefficients[1,5]

# 多变量
formula_multi_90<- as.formula(
  paste("Surv(icu_intime90, death_ICU90days) ~", 
        paste(cox_vars, collapse = "+"))
)
multi_cox_90 <- coxph(formula_multi_90, data = matched_data)
multi_ci_90 <- exp(confint(multi_cox_90)["magnesium_sulfate_supplementation1", ])
multi_hr_90 <- exp(coef(multi_cox_90)["magnesium_sulfate_supplementation1"])
multi_p_90 <- summary(multi_cox_90)$coefficients["magnesium_sulfate_supplementation1", "Pr(>|z|)"]


# 存储结果
results[["90d_mortality"]] <- data.frame(
  outcome = "90天全因死亡率",
  MgSO4 = paste0(death90_1, " (", perc90_1, "%)"),
  No_MgSO4 = paste0(death90_0, " (", perc90_0, "%)"),
  univariate = sprintf("%.2f (%.2f-%.2f)", uni_hr_90, uni_ci_90[1], uni_ci_90[2]),
  univariate_p = format.pval(uni_p_90, eps = 0.001),
  multivariate = sprintf("%.2f (%.2f-%.2f)", multi_hr_90, multi_ci_90[1], multi_ci_90[2]),
  multivariate_p = format.pval(multi_p_90, eps = 0.001)
)













### 3. ICU死亡率 --------------------------------------------------------------
# 描述性统计
icu_death_1 <- sum(matched_data$death_icu[group1], na.rm = TRUE)
icu_death_0 <- sum(matched_data$death_icu[group0], na.rm = TRUE)
perc_icu_1 <- round(icu_death_1/sum(group1)*100, 1)
perc_icu_0 <- round(icu_death_0/sum(group0)*100, 1)

# 逻辑回归分析
# 单变量
uni_glm_icu <- glm(death_icu ~ magnesium_sulfate_supplementation, 
                   family = binomial, data = matched_data)
uni_or_icu <- exp(coef(uni_glm_icu)[2])
uni_ci_icu <- exp(confint(uni_glm_icu)[2,])
uni_p_icu <- summary(uni_glm_icu)$coefficients[2,4]

# 多变量
formula_multi_icu <- as.formula(
  paste("death_icu ~", paste(cox_vars, collapse = "+"))
)
multi_glm_icu <- glm(formula_multi_icu, family = binomial, data = matched_data)
multi_or_icu <- exp(coef(multi_glm_icu)["magnesium_sulfate_supplementation1"])
multi_ci_icu <- exp(confint(multi_glm_icu)["magnesium_sulfate_supplementation1",])
multi_p_icu <- summary(multi_glm_icu)$coefficients["magnesium_sulfate_supplementation1",4]

# 存储结果
results[["icu_mortality"]] <- data.frame(
  outcome = "ICU死亡率",
  MgSO4 = paste0(icu_death_1, " (", perc_icu_1, "%)"),
  No_MgSO4 = paste0(icu_death_0, " (", perc_icu_0, "%)"),
  univariate = sprintf("%.2f (%.2f-%.2f)", uni_or_icu, uni_ci_icu[1], uni_ci_icu[2]),
  univariate_p = format.pval(uni_p_icu, eps = 0.001),
  multivariate = sprintf("%.2f (%.2f-%.2f)", multi_or_icu, multi_ci_icu[1], multi_ci_icu[2]),
  multivariate_p = format.pval(multi_p_icu, eps = 0.001)
)





### 4. 住院死亡率 -------------------------------------------------------------
# （代码结构与ICU死亡率类似，此处省略，需补充）
# 描述性统计
hosp_death_1 <- sum(matched_data$death_hosp[group1], na.rm = TRUE)
hosp_death_0 <- sum(matched_data$death_hosp[group0], na.rm = TRUE)
perc_hosp_1 <- round(hosp_death_1/sum(group1)*100, 1)
perc_hosp_0 <- round(hosp_death_0/sum(group0)*100, 1)

# 逻辑回归分析
# 单变量
uni_glm_hosp <- glm(death_hosp ~ magnesium_sulfate_supplementation, 
                    family = binomial, data = matched_data)
uni_or_hosp <- exp(coef(uni_glm_hosp)[2])
uni_ci_hosp <- exp(confint(uni_glm_hosp)[2,])
uni_p_hosp <- summary(uni_glm_hosp)$coefficients[2,4]

# 多变量
formula_multi_hosp <- as.formula(
  paste("death_hosp ~", paste(cox_vars, collapse = "+"))
)
multi_glm_hosp<- glm(formula_multi_hosp, family = binomial, data = matched_data)
multi_or_hosp <- exp(coef(multi_glm_hosp)["magnesium_sulfate_supplementation1"])
multi_ci_hosp <- exp(confint(multi_glm_hosp)["magnesium_sulfate_supplementation1",])
multi_p_hosp <- summary(multi_glm_hosp)$coefficients["magnesium_sulfate_supplementation1",4]

# 存储结果
results[["hosp_mortality"]] <- data.frame(
  outcome = "hosp死亡率",
  MgSO4 = paste0(hosp_death_1, " (", perc_hosp_1, "%)"),
  No_MgSO4 = paste0(hosp_death_0, " (", perc_hosp_0, "%)"),
  univariate = sprintf("%.2f (%.2f-%.2f)", uni_or_hosp, uni_ci_hosp[1], uni_ci_hosp[2]),
  univariate_p = format.pval(uni_p_icu, eps = 0.001),
  multivariate = sprintf("%.2f (%.2f-%.2f)", multi_or_hosp, multi_ci_hosp[1], multi_ci_hosp[2]),
  multivariate_p = format.pval(multi_p_hosp, eps = 0.001)
)




### 6. ICU住院时间 -----------------------------------------------------------
### 6. ICU住院时间 -----------------------------------------------------------
# 描述性统计
los_1 <- median(matched_data$los_icu[group1], na.rm = TRUE)
iqr_1 <- quantile(matched_data$los_icu[group1], c(0.25, 0.75), na.rm = TRUE)
los_0 <- median(matched_data$los_icu[group0], na.rm = TRUE)
iqr_0 <- quantile(matched_data$los_icu[group0], c(0.25, 0.75), na.rm = TRUE)

# Mann-Whitney U检验（中位数差异）
#mw_test <- wilcox.test(los_icu ~ magnesium_sulfate_supplementation, 
#data = matched_data, conf.int = TRUE, conf.level = 0.95)
mw_test <- wilcox.test(matched_data$los_icu[matched_data$magnesium_sulfate_supplementation == 1],
                       matched_data$los_icu[matched_data$magnesium_sulfate_supplementation == 0],
                       conf.int = TRUE, conf.level = 0.95)

# 存储结果（中位数差）
# 统一字段名以兼容 bind_rows 拼接
results[["icu_los"]] <- data.frame(
  outcome = "ICU住院时间（天）",
  MgSO4 = sprintf("%.1f (%.1f-%.1f)", los_1, iqr_1[1], iqr_1[2]),
  No_MgSO4 = sprintf("%.1f (%.1f-%.1f)", los_0, iqr_0[1], iqr_0[2]),
  univariate = sprintf("MD %.2f (%.2f - %.2f)", mw_test$estimate, 
                       mw_test$conf.int[1], mw_test$conf.int[2]),
  univariate_p = format.pval(mw_test$p.value, eps = 0.001),
  multivariate = NA,
  multivariate_p = NA
)

levels(matched_data$magnesium_sulfate_supplementation)
# 正确应显示 [1] "No_MgSO4" "MgSO4"


### 7. 整合结果 --------------------------------------------------------------
final_table <- bind_rows(results) %>%
  select(outcome, MgSO4, No_MgSO4, 
         univariate, univariate_p, 
         multivariate, multivariate_p)

# 显示结果
knitr::kable(final_table, align = c("l","c","c","c","c","c","c"),
             col.names = c("结局", "硫酸镁组", "非硫酸镁组",
                           "单变量分析 HR/OR/MD (95% CI)", "P值",
                           "多变量分析 HR/OR/MD (95% CI)", "P值"))

