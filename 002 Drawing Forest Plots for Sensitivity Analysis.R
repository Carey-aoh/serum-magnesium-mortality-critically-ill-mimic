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

matched_data <- match.data(psm)




# 计算稳定的逆概率加权（Stabilized IPTW）
iptw_gbm <- weightit(formula_psm, 
                     data = data, 
                     method = "gbm", 
                     estimand = "ATE", 
                     stabilized = TRUE,
                     criterion = "smd.mean")  # 或者选择 "smd.max", "covariate.balance" 等


# 提取权重
weights_iptw <- get.w(iptw_gbm)
# 计算权重的 99 百分位数
p99_weight <- quantile(weights_iptw, 0.99)
# 截断权重：超过 99 百分位数的权重设置为该值
weights_iptw_truncated <- pmin(weights_iptw, p99_weight)
# 将截断后的权重重新赋值给 iptw_gbm
iptw_gbm_truncated <- iptw_gbm
iptw_gbm_truncated$weights <- weights_iptw_truncated

# 查看截断后的权重分布
summary(weights_iptw_truncated)
# 使用新的截断后的 IPTW 权重方法，重新提取平衡信息
bal_iptw_gbm_truncated <- bal.tab(iptw_gbm_truncated, un = TRUE)
# 查看新的平衡信息
print(bal_iptw_gbm_truncated)


# ========== 提取三个阶段的协变量平衡信息 ==========
bal_unmatched <- bal.tab(formula_psm, data = data, estimand = "ATE")
bal_psm <- bal.tab(psm, un = TRUE)



#生存分析
# 使用Surv对象定义生存时间和事件
library(survival)
data$surv_object <- Surv(data$icu_intime_to_dod, data$death_ICU28days)
#原始数据单变量
cox_uni_original <- coxph(surv_object ~ magnesium_sulfate_supplementation, data = data)
summary(cox_uni_original)

#原始数据多变量
cox_formula <- as.formula(
  paste("surv_object ~", paste(cox_vars, collapse = " + "))
)
cox_multi_original <- coxph(cox_formula, data = data)
summary(cox_multi_original)

#psm数据单变量
# 使用cluster(id)处理匹配对
matched_data <- match.data(psm)
cox_uni_psm <- coxph(surv_object ~ magnesium_sulfate_supplementation + cluster(id), data = matched_data)
summary(cox_uni_psm)

#PSM后数据多变量
cox_multi_psm <- coxph(cox_formula, data = matched_data)
summary(cox_multi_psm)

#iptw单变量分析
# 使用 gbm 方法计算出的 IPTW 权重
data$iptw_weights_gbm <- weights_iptw_truncated

# 创建 survey 设计对象
design_iptw_gbm <- svydesign(ids = ~1, weights = ~iptw_weights_gbm, data = data)


cox_uni_iptw <- svycoxph(surv_object ~ magnesium_sulfate_supplementation, design = design_iptw_gbm)
summary(cox_uni_iptw)

cox_multi_iptw <- svycoxph(cox_formula, design = design_iptw_gbm)
summary(cox_multi_iptw)


library(forestplot)

# 构建结果数据框
results <- data.frame(
  Model = c(
    "Univariate (Original)", "Multivariate (Original)",
    "Univariate (PSM)", "Multivariate (PSM)",
    "Univariate (IPTW)", "Multivariate (IPTW)"
  ),
  HR = c(
    1.2, 0.7935,
    0.8457, 0.8445,
    0.8141, 0.7326
  ),
  Lower = c(
    1.084, 0.7133,
    0.7456 , 0.7427,
    0.7305, 0.6490
  ),
  Upper = c(
    1.327, 0.8826,
    0.9592, 0.9603,
    0.9073, 0.8270
  ),
  p_value = c(
    "<0.001", "<0.001",
    "0.009 ", "0.009",
    "<0.001", "<0.001"
  )
)

# 构建文本标签
label_text <- cbind(
  c("Model", results$Model),
  c("HR (95% CI)", sprintf("%.2f (%.2f–%.2f)", results$HR, results$Lower, results$Upper)),
  c("p-value", results$p_value)
)

# 绘制森林图
png("E:/100-科研/400_镁/006重启版/forestplot.png", 
    width = 10, height = 6, units = "in", res = 1200)
forestplot(
  labeltext = label_text,
  mean = c(NA, results$HR),
  lower = c(NA, results$Lower),
  upper = c(NA, results$Upper),
  xlog = TRUE,
  graph.pos = 2,
  boxsize = 0.2,  # 方块大小
  line.margin = unit(8, "mm"),
  is.summary = c(TRUE, rep(FALSE, 6)),
  col = fpColors(box = "#1c61b6", line = "#1c61b6", summary = "#003366"),
  ci.vertices = TRUE,  # 显示置信区间尖角
  ci.vertices.height = 0.1,
  zero = 1,  # 添加参考线
  lwd.zero = 2,  # 参考线加粗
  lwd.ci = 2,  # CI线加粗
  txt_gp = fpTxtGp(
    label = gpar(fontsize = 11),
    ticks = gpar(fontsize = 14),
    xlab = gpar(fontsize = 12, fontface = "bold")
  ),
  title = "Association of Magnesium Supplementation with 28-Day ICU Mortality"
)

# 关闭图像设备
dev.off()

