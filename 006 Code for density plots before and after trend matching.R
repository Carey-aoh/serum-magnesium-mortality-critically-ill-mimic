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
  "magnesium_sulfate_supplementation"
)

data[factor_vars] <- lapply(data[factor_vars], as.factor)

# 2. 设置Psm所需变量和Cox回归分析的变量
psm_vars <- c("Age", "Weight", "Charlson", "sofa", "renal_disease", 
              "WBC", "ventilation")

cox_vars <- c(
  "Age", "Weight", "WBC", "RBC", "Hemoglobin", "Glucose", "Calcium", "Chloride", 
  "BUN", "Creatinine", "Charlson", "sofa", "Apache_ii",
  "diabetes", "cerebrovascular_disease", "congestive_heart_failure", "renal_disease",
  "liver_disease", "chronic_pulmonary_disease", "peripheral_vascular_disease",
  "myocardial_infarct", "ventilation", "rrt", "calcium_supplementation", 
  "magnesium_sulfate_supplementation"
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





# 1. 指定 table1 所有变量（去掉因变量）
table1_vars <- cox_vars[cox_vars != "magnesium_sulfate_supplementation"]

# 2. 非正态变量
nonnormal_vars <- c("Age", "Weight", "WBC", "RBC", "Hemoglobin", "Glucose", 
                    "Calcium", "Chloride", "BUN", "Creatinine", 
                    "Charlson", "sofa", "Apache_ii")

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



# 提取原始数据和匹配后数据的倾向评分
# 原始数据的倾向评分
data$propensity_score <- psm$distance

# 匹配后数据的倾向评分（直接从 psm_data 中获取）
psm_data$propensity_score <- psm_data$distance  # 直接使用已存在的 distance 列

# 标记数据来源
data$matched <- "Before Matching"
psm_data$matched <- "After Matching"

# 合并数据（仅保留所需列）
combined_data <- rbind(
  data[, c("id", "magnesium_sulfate_supplementation", "propensity_score", "matched")],
  psm_data[, c("id", "magnesium_sulfate_supplementation", "propensity_score", "matched")]
)

library(ggplot2)

# 拆分数据
before_data <- combined_data[combined_data$matched == "Before Matching", ]
after_data <- combined_data[combined_data$matched == "After Matching", ]

# 匹配前图
plot_before <- ggplot(before_data, aes(x = propensity_score, fill = factor(magnesium_sulfate_supplementation))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(
    name = "Treatment Group",
    labels = c("No Magnesium", "Magnesium Supplemented"),
    values = c("#F8766D", "#00BFC4")
  ) +
  labs(
    title = "Before Matching",
    x = "Propensity Score",
    y = "Density"
  ) +
  theme_minimal()

# 匹配后图
plot_after <- ggplot(after_data, aes(x = propensity_score, fill = factor(magnesium_sulfate_supplementation))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(
    name = "Treatment Group",
    labels = c("No Magnesium", "Magnesium Supplemented"),
    values = c("#F8766D", "#00BFC4")
  ) +
  labs(
    title = "After Matching",
    x = "Propensity Score",
    y = "Density"
  ) +
  theme_minimal()

# 显示图形
print(plot_before)
print(plot_after)


# 设置输出路径和参数（示例为PNG格式）
# 设置输出路径和参数（示例为PNG格式）
png(filename = "E:/100-科研/400_镁/006重启版/密度前.png",  # 替换为你的路径
    width = 8,       # 宽度(英寸)
    height = 6,      # 高度(英寸)
    units = "in",    # 尺寸单位（英寸）
    res = 1200,      # 分辨率(dpi)
    type = "cairo")  # 抗锯齿引擎（使线条更平滑）

# 绘制图形
plot_before <- ggplot(before_data, aes(x = propensity_score, fill = factor(magnesium_sulfate_supplementation))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(
    name = "Treatment Group",
    labels = c("No Magnesium", "Magnesium Supplemented"),
    values = c("#F8766D", "#00BFC4")
  ) +
  labs(
    title = "Before Matching",
    x = "Propensity Score",
    y = "Density"
  ) +
  theme_minimal()

# 渲染图形（可选）
print(plot_before)

# 必须关闭图形设备才能完成保存
dev.off()


#密度后
# 设置输出路径和参数（示例为PNG格式）
png(filename = "E:/100-科研/400_镁/006重启版/密度后.png",  # 替换为你的路径
    width = 8,       # 宽度(英寸)
    height = 6,      # 高度(英寸)
    units = "in",    # 尺寸单位（英寸）
    res = 1200,      # 分辨率(dpi)
    type = "cairo")  # 抗锯齿引擎（使线条更平滑）

# 匹配后图
plot_after <- ggplot(after_data, aes(x = propensity_score, fill = factor(magnesium_sulfate_supplementation))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(
    name = "Treatment Group",
    labels = c("No Magnesium", "Magnesium Supplemented"),
    values = c("#F8766D", "#00BFC4")
  ) +
  labs(
    title = "After Matching",
    x = "Propensity Score",
    y = "Density"
  ) +
  theme_minimal()

# 渲染图形（可选）
print(plot_after)

# 必须关闭图形设备才能完成保存
dev.off()
