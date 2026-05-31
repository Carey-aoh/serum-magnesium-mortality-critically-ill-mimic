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

# 绘制Kaplan-Meier生存曲线
fit_km <- survfit(surv_obj ~ magnesium_sulfate_supplementation, data = psm_data)

ggsurvplot(
  fit_km,
  data = psm_data,
  pval = TRUE,          # 显示log-rank检验p值
  conf.int = TRUE,      # 显示置信区间
  risk.table = TRUE,    # 显示风险表
  legend.labs = c("No Supplement", "Supplement"),  # 设置图例标签
  title = "Kaplan-Meier Survival Curve",
  xlab = "Time (days)",
  ylab = "Survival Probability",
  break.time.by = 7,    # 按7天间隔分割x轴
  palette = c("#E7B800", "#2E9FDF")  # 自定义颜色
)

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

library(survminer)

# 拟合生存曲线
fit <- survfit(Surv(icu_intime_to_dod, death_ICU28days) ~ magnesium_sulfate_supplementation, data = psm_data)

#0512更改图形


#多变量cox生存曲线
multi_formula <- as.formula(paste("Surv(icu_intime_to_dod, death_ICU28days) ~", paste(cox_vars, collapse = " + ")))
# 拟合多变量 Cox 模型
multi_cox_model <- coxph(multi_formula, data = psm_data)
summary(multi_cox_model)
multi_summary <- summary(multi_cox_model)


multi_summary <- summary(multi_cox_model)

# 设定你关心的变量名，比如暴露变量叫 magnesium_supplement
target_var <- "magnesium_sulfate_supplementation1"



install.packages("ggsurvfit")
library(ggsurvfit)
# 1. 提取 HR 和 p 值
hr <- round(multi_summary$conf.int[target_var, "exp(coef)"], 2)
conf_lower <- round(multi_summary$conf.int[target_var, "lower .95"], 2)
conf_upper <- round(multi_summary$conf.int[target_var, "upper .95"], 2)
p_val <- signif(multi_summary$coefficients[target_var, "Pr(>|z|)"], 3)

# 2. 生存拟合
#fit <- survfit(Surv(icu_intime_to_dod, death_ICU28days) ~ magnesium_sulfate_supplementation, data = psm_data)

# 3. 生存曲线图

p<-survfit2(Surv(icu_intime_to_dod, death_ICU28days) ~ magnesium_sulfate_supplementation, data = psm_data) %>%
  ggsurvfit(linewidth = 0.8) +
  
  # 风险表
  add_risktable(
    risktable_height = 0.2,
    risktable_stats = c("{n.risk}"),
    stats_label = list(n.risk = "No. at risk"),
    size = 5,
    theme = list(
      theme_risktable_default(axis.text.y.size = 14, plot.title.size = 14),
      theme(plot.title = element_text(face = "plain"))
    )
  ) +
  
  # 风险表组名添加符号（圆点）
  add_risktable_strata_symbol(symbol = "\U25CF", size = 20) +
  
  # 生存曲线的删失点标注
  add_censor_mark(size = 4, shape = 73) +
  
  # 添加 log-rank 检验的 p 值
  add_pvalue(
    location = "annotation", x = 24, y = 0.82, hjust = 1,
    size = 6, caption = "log-rank {p.value}"
  ) +
  
  # 添加 HR 注释
  annotate(
    "text", x = 24, y = 0.85, hjust = 1, size = 6,
    label = paste0("HR ", hr, " (95% CI ", conf_lower, "-", conf_upper, ")")
  ) +
  
  # 添加组名注释（可选，图例已经能代表）
  #annotate("text", x = 11, y = 0.90, label = "No Mg supplement", size = 4.5, hjust = 0) +
  #annotate("text", x = 18, y = 0.96, label = "Mg supplement", size = 4.5, hjust = 1) +
  
  # 标签和坐标轴
  labs(
    title = "",
    x = "Days Since ICU Admission",
    y = "Survival Probability",
    color = "Mg supplementation"  # 添加图例标题
  ) +
  scale_x_continuous(breaks = seq(0, 28, 4), expand = c(0.03, 0)) +
  scale_y_continuous(breaks = seq(0.6, 1, 0.1),limits = c(0.6, 1)) +
  
  # 颜色控制（加上 labels 显示指定图例标签）
  scale_color_manual(
    values = c('#F7931D','#0072B2'),
    labels = c("No Mg supplement", "Mg supplement")
  ) +
  scale_fill_manual(
    values = c('#F7931D','#0072B2'),
    labels = c("No Mg supplement", "Mg supplement")
  ) +
  
  # 主题美化
  theme_classic() +
  theme(
    axis.text = element_text(size = 14, color = "black"),
    axis.title.y = element_text(size = 14, color = "black", vjust = 1),
    axis.title.x = element_text(size = 14, color = "black"),
    panel.grid.major.y = element_line(color = "#DCDDDF"),
    legend.position = "top",  # 或 "right"，"bottom"，根据需求调整
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14)
  )


ggsave("survival_plot2.png", plot = p, width = 10, height = 8, dpi = 1200)



p <- survfit2(Surv(icu_intime_to_dod, death_ICU28days) ~ magnesium_sulfate_supplementation, data = psm_data) %>%
  ggsurvfit(linewidth = 0.8) +
  
  # 风险表
  add_risktable(
    risktable_height = 0.2,
    risktable_stats = c("{n.risk}"),
    stats_label = list(n.risk = "No. at risk"),
    size = 5,
    theme = list(
      theme_risktable_default(
        axis.text.y.size = 14, 
        plot.title.size = 14,
      ),
      theme(plot.title = element_text(face = "plain", family = "TT Times New Roman"))
    )
  ) +
  
  # 风险表组名添加符号
  add_risktable_strata_symbol(symbol = "\U25CF", size = 20) +
  
  # 删失点标注
  add_censor_mark(size = 4, shape = 73) +
  
  # 添加 log-rank 检验的 p 值
  add_pvalue(
    location = "annotation", x = 24, y = 0.82, hjust = 1,
    size = 6, caption = "log-rank {p.value}"
  ) +
  
  # 添加 HR 注释（无逗号）
  annotate(
    "text", x = 24, y = 0.85, hjust = 1, size = 6, family = "TT Times New Roman",
    label = paste0("HR ", hr, " (95% CI ", conf_lower, "-", conf_upper, ")")
  ) +
  
  # 标签与坐标轴
  labs(
    title = "",
    x = "Days Since ICU Admission",
    y = "Survival Probability",
    color = "Mg supplementation"
  ) +
  scale_x_continuous(breaks = seq(0, 28, 4), expand = c(0.03, 0)) +
  scale_y_continuous(breaks = seq(0.6, 1, 0.1), limits = c(0.6, 1)) +
  
  # 自定义颜色
  scale_color_manual(
    values = c('#F7931D','#0072B2'),
    labels = c("No Mg supplement", "Mg supplement")
  ) +
  scale_fill_manual(
    values = c('#F7931D','#0072B2'),
    labels = c("No Mg supplement", "Mg supplement")
  ) +
  
  # 主图主题美化 + 字体统一
  theme_classic() +
  theme(
    text = element_text(family = "TT Times New Roman"),  # 统一字体
    axis.text = element_text(size = 14, color = "black"),
    axis.title.y = element_text(size = 14, color = "black", vjust = 1),
    axis.title.x = element_text(size = 14, color = "black"),
    panel.grid.major.y = element_line(color = "#DCDDDF"),
    legend.position = "top",
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14)
  )

# 保存图片
ggsave("survival_plot2.png", plot = p, width = 10, height = 8, dpi = 1200)


#
# 先注册字体别名
windowsFonts(`TT Times New Roman` = windowsFont("Times New Roman"))

# 生存分析拟合
fit <- survfit2(Surv(icu_intime_to_dod, death_ICU28days) ~ magnesium_sulfate_supplementation, data = psm_data)

# 假设你已经计算好了 HR 和置信区间
# 例如：
hr <- "0.84"
conf_lower <- "0.74"
conf_upper <- "0.96"

# 绘图
p <- fit %>%
  ggsurvfit(linewidth = 0.8) +
  
  # 风险表
  add_risktable(
    risktable_height = 0.2,
    risktable_stats = c("{n.risk}"),
    stats_label = list(n.risk = "No. at risk"),
    size = 5,
    theme = list(
      theme_risktable_default(
        axis.text.y.size = 14,
        plot.title.size = 14
      ),
      theme(plot.title = element_text(face = "plain", family = "TT Times New Roman"))
    )
  ) +
  
  # 风险表组名前加圆点符号
  add_risktable_strata_symbol(symbol = "\u25CF", size = 20) +
  
  # 删失点
  add_censor_mark(size = 4, shape = 73) +
  
  # log-rank p 值
  add_pvalue(
    location = "annotation", x = 24, y = 0.82, hjust = 1,
    size = 6, caption = "log-rank {p.value}"
  ) +
  
  # HR 注释
  annotate(
    "text", x = 24, y = 0.85, hjust = 1, size = 6, family = "TT Times New Roman",
    label = paste0("HR ", hr, " (95% CI ", conf_lower, "-", conf_upper, ")")
  ) +
  
  # 坐标轴与标签
  labs(
    title = "",
    x = "Days Since ICU Admission",
    y = "Survival Probability",
    color = "Mg supplementation"
  ) +
  scale_x_continuous(breaks = seq(0, 28, 4), expand = c(0.03, 0)) +
  scale_y_continuous(breaks = seq(0.6, 1, 0.1), limits = c(0.6, 1)) +
  
  # 自定义颜色
  scale_color_manual(
    values = c('#F7931D','#0072B2'),
    labels = c("No Mg supplement", "Mg supplement")
  ) +
  scale_fill_manual(
    values = c('#F7931D','#0072B2'),
    labels = c("No Mg supplement", "Mg supplement")
  ) +
  
  # 主题美化 + 字体统一
  theme_classic() +
  theme(
    text = element_text(family = "TT Times New Roman"),
    axis.text = element_text(size = 14, color = "black"),
    axis.title.y = element_text(size = 14, color = "black", vjust = 1),
    axis.title.x = element_text(size = 14, color = "black"),
    panel.grid.major.y = element_line(color = "#DCDDDF"),
    legend.position = "top",
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14)
  )

# 打印图形
print(p)






