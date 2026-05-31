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

# 1. 指定 table1 所有变量（去掉因变量）
table1_vars <- cox_vars[cox_vars != "magnesium_sulfate_supplementation"]

# 2. 非正态变量
nonnormal_vars <- c("Age", "Weight", "WBC",  "Hemoglobin", "Glucose", 
                     "Chloride", "BUN",
                    "Charlson", "sofa")

# 3. 创建 Table 1 对象（不要加 nonnormal 参数）
table1 <- CreateTableOne(vars = table1_vars, 
                         strata = "magnesium_sulfate_supplementation", 
                         data = psm_data, 
                         factorVars = factor_vars)

# 4. 打印输出时添加 nonnormal
print(table1, showAllLevels = TRUE, smd = TRUE, nonnormal = nonnormal_vars)


# 构建 Table 1
table2_vars <- cox_vars[cox_vars != "magnesium_sulfate_supplementation"]  # 去掉因变量
# 2. 非正态变量
nonnormal_vars <- c("Age", "Weight", "WBC", "RBC", "Hemoglobin", "Glucose", 
                    "Calcium", "Chloride", "BUN", "Creatinine", 
                    "Charlson", "sofa", "Apache_ii")

# 3. 创建 Table 1 对象（不要加 nonnormal 参数）
table2 <- CreateTableOne(vars = table2_vars, 
                         strata = "magnesium_sulfate_supplementation", 
                         data =data, 
                         factorVars = factor_vars)

# 输出 Table 1，带标准化差异（SMD）
print(table2, showAllLevels = TRUE, smd = TRUE,nonnormal = nonnormal_vars)

# 导出匹配后的 Table1（PSM 后的数据）
printed_table1 <- print(table1, 
                        showAllLevels = TRUE, 
                        smd = TRUE,
                        nonnormal = nonnormal_vars,
                        printToggle = FALSE) # 禁止直接打印到控制台

# 将结果保存为 CSV
write.csv(printed_table1, file = "table1_psm.csv")

# 导出未匹配的 Table2（原始数据）
printed_table2 <- print(table2, 
                        showAllLevels = TRUE, 
                        smd = TRUE,
                        nonnormal = nonnormal_vars,
                        printToggle = FALSE)

write.csv(printed_table2, file = "table2_unmatched.csv")
