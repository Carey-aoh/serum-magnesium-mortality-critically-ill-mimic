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

# 重新生成协变量平衡图
p<-love.plot(
  formula_psm,
  data = data,
  weights = list(PSM = get.w(psm), IPTW = get.w(iptw_gbm_truncated)),
  treat = "magnesium_sulfate_supplementation",
  var.order = "unadjusted",
  abs = TRUE,
  colors = c("#66c2a5", "#fc8d62", "#8da0cb"),
  shapes = c(19, 17, 15),
  line = TRUE,
  title = "Covariate Balance Across Adjustment Methods (with truncated weights)",
  sample.names = c("Unadjusted", "PSM", "IPTW (Truncated)"),
  position = "bottom",
  var.names = c(
    "Age" = "Age",
    "Weight" = "Weight ",
    "Charlson" = "Charlson ",
    "sofa" = "SOFA ",
    "renal_disease" = "Renal Disease",
    "myocardial_infarct" = "Myocardial Infarction",
    "ventilation" = "Ventilation"
  )
)
p


# 添加x=0.1的竖线
p1<-p + ggplot2::geom_vline(xintercept = 0.1, linetype = "dashed", color = "red")

ggsave(
  filename = "E:/100-科研/400_镁/006重启版/loveplot.png",  # 添加文件名和 .png 后缀
  plot = p1,
  device = "png",
  dpi = 1200,
  width = 10,
  height = 6
)
