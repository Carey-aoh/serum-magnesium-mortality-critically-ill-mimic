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
psm_data <- match.data(psm)

# 6. 合并Cox所需的变量（使用"id"而非"subject_id"）
psm_data <- merge(psm_data, data[, c("id", cox_vars, "icu_intime_to_dod", "death_ICU28days")], 
                  by = "id", all.x = TRUE, suffixes = c("", "_data"))

table(psm_data$magnesium_sulfate_supplementation)
# ICU28天死亡 vs 补镁状态
table(psm_data$death_ICU28days, psm_data$magnesium_sulfate_supplementation)
# 死亡率（按组百分比）
prop.table(table(psm_data$death_ICU28days, psm_data$magnesium_sulfate_supplementation), margin = 2)


# 生成交叉表显示人数
mortality_table <- table(
  Magnesium = psm_data$magnesium_sulfate_supplementation,
  Death = psm_data$death_ICU28days
)

# 计算按列百分比（按治疗组计算死亡率）
mortality_rate <- prop.table(mortality_table, margin = 1) * 100

# 组合人数和百分比显示结果
final_table <- cbind(
  "0_No_Magnesium" = paste0(mortality_table[1, ], " (", round(mortality_rate[1, ], 1), "%)"),
  "1_Magnesium" = paste0(mortality_table[2, ], " (", round(mortality_rate[2, ], 1), "%)")
)

rownames(final_table) <- c("Survival", "Death")
final_table

# 加载必要的包
library(survival)
library(survminer)
library(ggplot2)

# 检查数据结构和变量名称
names(psm_data)

# 检查生存时间和结局变量是否存在缺失值
sum(is.na(psm_data$icu_intime_to_dod))
sum(is.na(psm_data$death_ICU28days))

# 处理缺失值（根据实际情况选择合适的方法）
psm_data <- na.omit(psm_data)  # 如果缺失较少可以直接删除

# 创建生存对象
surv_obj <- Surv(time = psm_data$icu_intime_to_dod,
                 event = psm_data$death_ICU28days)


# Cox比例风险回归分析
# 确保分类变量为因子
psm_data$magnesium_sulfate_supplementation <- as.factor(psm_data$magnesium_sulfate_supplementation)

# 构建Cox模型公式（调整协变量）
cox_formula <- as.formula(
  paste("surv_obj ~ magnesium_sulfate_supplementation +",
        paste(cox_vars[!cox_vars %in% "magnesium_sulfate_supplementation"], 
              collapse = " + "))
)

# 运行Cox回归
cox_model <- coxph(cox_formula, data = psm_data)

# 查看结果
summary(cox_model)

# 比例风险假设检验
cox_zph <- cox.zph(cox_model)
print(cox_zph)

# 可视化比例风险检验结果
ggcoxzph(cox_zph)


library(survival)

# 单变量 Cox 回归
univ_cox_results <- lapply(cox_vars, function(var) {
  formula <- as.formula(paste("Surv(icu_intime_to_dod, death_ICU28days) ~", var))
  cox_model <- coxph(formula, data = psm_data)
  
  hr <- exp(coef(cox_model))                          # HR
  conf <- exp(confint(cox_model))                     # 置信区间
  p <- summary(cox_model)$coefficients[,"Pr(>|z|)"]   # P值
  
  # 合并成一行
  data.frame(
    Variable = var,
    HR = hr,
    Lower95 = conf[1],
    Upper95 = conf[2],
    P_value = p
  )
})

# 合并所有变量的结果
univ_cox_results_df <- do.call(rbind, univ_cox_results)

# 查看结果
print(univ_cox_results_df)

write.csv(univ_cox_results_df, "univ_cox_results2.csv", row.names = FALSE)


# 单变量 Cox 回归（添加系数版本）
univ_cox_results <- lapply(cox_vars, function(var) {
  formula <- as.formula(paste("Surv(icu_intime_to_dod, death_ICU28days) ~", var))
  cox_model <- coxph(formula, data = psm_data)
  
  beta <- coef(cox_model)                            # 系数 β
  hr <- exp(beta)                                    # HR = exp(β)
  conf <- exp(confint(cox_model))                    # 置信区间
  p <- summary(cox_model)$coefficients[,"Pr(>|z|)"]  # P值
  
  # 合并成一行（添加 Beta 列）
  data.frame(
    Variable = var,
    Beta = beta,
    HR = hr,
    Lower95 = conf[1],
    Upper95 = conf[2],
    P_value = p
  )
})

# 合并所有变量的结果
univ_cox_results_df <- do.call(rbind, univ_cox_results)

# 查看结果
print(univ_cox_results_df)

# 导出 CSV（包含系数）
write.csv(univ_cox_results_df, "univ_cox_results_with_coefficients2.csv", row.names = FALSE)




# 构建公式
multi_formula <- as.formula(paste("Surv(icu_intime_to_dod, death_ICU28days) ~", paste(cox_vars, collapse = " + ")))

# 拟合多变量 Cox 模型
multi_cox_model <- coxph(multi_formula, data = psm_data)

# 提取结果
summary(multi_cox_model)

multi_summary <- summary(multi_cox_model)



multi_cox_results_df <- data.frame(
  Variable = rownames(multi_summary$coefficients),
  HR = round(multi_summary$conf.int[, "exp(coef)"], 3),       # 直接用 exp(coef)
  Lower95 = round(multi_summary$conf.int[, "lower .95"], 3),  # 已经是 exp(coef) 的 CI
  Upper95 = round(multi_summary$conf.int[, "upper .95"], 3),
  P_value = signif(multi_summary$coefficients[, "Pr(>|z|)"], 3)
)


print(multi_cox_results_df)
write.csv(multi_cox_results_df, "multi_cox_results2.csv", row.names = FALSE)
