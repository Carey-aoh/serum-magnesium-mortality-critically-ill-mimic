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




#亚组分析
library(survival)
library(dplyr)
library(broom)

class(df)

# 创建子组变量
matched_data <- matched_data %>%
  mutate(
    age_group = ifelse(Age < 65, "<65", "≥65"),
    charlson_group = ifelse(Charlson < 6, "<6", "≥6"),
    sofa_group = case_when(
      sofa <= 6 ~ "Low (0-6)",
      sofa <= 11 ~ "Medium (7-11)",
      sofa >= 12 ~ "High (≥12)"
    )
  )

# 要分析的子组变量
subgroups <- c("age_group", "charlson_group", "sofa_group", 
               "renal_disease",  "ventilation")

# 存储结果
results <- list()

for (var in subgroups) {
  for (level in unique(na.omit(matched_data[[var]]))) {
    sub_data <- matched_data[matched_data[[var]] == level, ]
    
    # 死亡率统计
    events_treated <- sum(sub_data$death_ICU28days == 1 & sub_data$magnesium_sulfate_supplementation == 1)
    total_treated <- sum(sub_data$magnesium_sulfate_supplementation == 1)
    
    events_control <- sum(sub_data$death_ICU28days == 1 & sub_data$magnesium_sulfate_supplementation == 0)
    total_control <- sum(sub_data$magnesium_sulfate_supplementation == 0)
    
    # Cox 模型
    cox_model <- coxph(Surv(icu_intime_to_dod, death_ICU28days) ~ magnesium_sulfate_supplementation, data = sub_data)
    hr <- tidy(cox_model, exponentiate = TRUE, conf.int = TRUE)
    
    results[[length(results)+1]] <- data.frame(
      Subgroup = var,
      Level = level,
      Death_Treated = paste0(events_treated, "/", total_treated, " (", round(100 * events_treated / total_treated, 1), "%)"),
      Death_Control = paste0(events_control, "/", total_control, " (", round(100 * events_control / total_control, 1), "%)"),
      HR = round(hr$estimate, 2),
      CI = paste0("(", round(hr$conf.low, 2), "-", round(hr$conf.high, 2), ")"),
      HR_CI = paste0(round(hr$estimate, 2), " ", "(", round(hr$conf.low, 2), "-", round(hr$conf.high, 2), ")"),
      P_value = signif(hr$p.value, 3)
    )
  }
}

# 合并并查看结果
subgroup_table <- do.call(rbind, results)
print(subgroup_table)

# 可选：保存为CSV
write.csv(subgroup_table, "subgroup_analysis_table.csv", row.names = FALSE)

library(ggplot2)
library(dplyr)

# 原始数据
subgroup_table <- data.frame(
  Subgroup = c("age_group", "age_group", "charlson_group", "charlson_group", 
               "sofa_group", "sofa_group", "sofa_group", "renal_disease", 
               "renal_disease", 
               "ventilation", "ventilation"),
  Level = c("<65", "≥65", "<6", "≥6", "Low (0–6)", "High (≥12)", "Medium (7–11)", 
            "No", "Yes", "No", "Yes"),
  HR = c(0.78, 0.85, 0.93, 0.79, 0.99 , 0.48, 0.65, 0.86, 0.79 , 0.96 ,0.57 ),
  CI_lower = c(0.61, 0.74, 0.75, 0.67, 0.84, 0.33, 0.51, 0.74, 0.61,  0.82, 0.45),
  CI_upper = c(1.02, 0.99, 1.16, 0.92, 1.16, 0.69, 0.82, 0.99, 1.03,  1.11, 0.72),
  P_value = c("0.066", "0.031", "0.524", "0.002", "0.867", "<0.001", "<0.001", "0.038",
              "0.084",  "0.567","<0.001")
)

# 添加标签，确保 SubLabel 唯一
subgroup_table <- subgroup_table %>%
  mutate(
    Significance = ifelse(P_value < 0.05, "Significant", "Not significant"),
    p_label = paste0("p = ", formatC(P_value, format = "e", digits = 2)),
    SubLabel = paste0("   ", Subgroup, ": ", Level)
  )

# 构造 y 轴标签顺序
ordered_labels <- c(
  "age_group", "   age_group: <65", "   age_group: ≥65",
  "charlson_group", "   charlson_group: <6", "   charlson_group: ≥6",
  "sofa_group", "   sofa_group: Low (0–6)", "   sofa_group: High (≥12)", "   sofa_group: Medium (7–11)",
  "renal_disease", "   renal_disease: No", "   renal_disease: Yes",
  "ventilation", "   ventilation: No", "   ventilation: Yes"
)

# 主标签行
main_labels <- data.frame(
  Label = c("age_group", "charlson_group", "sofa_group", "renal_disease",  "ventilation"),
  HR = NA, CI_lower = NA, CI_upper = NA, P_value = NA, Significance = NA, p_label = NA
)

# 子组数据
plot_data <- subgroup_table %>%
  mutate(Label = SubLabel) %>%
  select(Label, HR, CI_lower, CI_upper, P_value, Significance, p_label)

# 合并并排序
final_data <- bind_rows(main_labels, plot_data) %>%
  mutate(Label = factor(Label, levels = rev(ordered_labels)))

# 绘图



ggplot(final_data, aes(x = HR, y = Label)) +
  geom_point(aes(color = Significance), size = 3, na.rm = TRUE) +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper, color = Significance), height = 0.2, na.rm = TRUE) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black", linewidth = 0.8) +
  geom_text(aes(label = p_label), x = 1.55, hjust = 0, size = 4, na.rm = TRUE) +  # 增大p值标签字体
  scale_color_manual(values = c("Significant" = "#d62728", "Not significant" = "grey50")) +
  coord_cartesian(xlim = c(0.3, 1.6)) +
  xlab("Hazard Ratio (HR)") + ylab("") +
  ggtitle("Subgroup Analysis: Magnesium Supplementation and 28-Day ICU Mortality") +
  theme_minimal(base_size = 18) +
  theme(
    axis.text.y = element_text(size = 18, hjust = 0),  # 增大y轴标签字体
    axis.text.x = element_text(size = 14),  # 增大x轴标签字体
    plot.title = element_text(size = 18, face = "bold"),  # 增大标题字体
    legend.text = element_text(size = 18),  # 增大图例文字
    legend.position = "top",
    legend.title = element_blank(),
    panel.grid = element_blank(),  # 去掉背景网格线
    axis.title.x = element_text(size = 14),  # x轴标题字体大小
    axis.title.x.top = element_text(size = 14),  # 设置x轴标题的位置
    axis.text.x.top = element_text(size = 14),  # 设置x轴数字的位置
    axis.ticks.x.top = element_line(size = 1),  # 设置x轴的刻度线
    axis.line.x.top = element_line(size = 1)  # 设置x轴线条
  ) +
  scale_x_continuous(position = "top")  # 将x轴移到顶部




windowsFonts(`TT Times New Roman` = windowsFont("Times New Roman"))

theme_minimal(base_family = "TT Times New Roman", base_size = 18)

ggplot(final_data, aes(x = HR, y = Label)) +
  geom_point(aes(color = Significance), size = 3, na.rm = TRUE) +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper, color = Significance), height = 0.2, na.rm = TRUE) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black", linewidth = 0.8) +
  geom_text(aes(label = p_label), x = 1.55, hjust = 0, size = 4, family = "TT Times New Roman", na.rm = TRUE) +
  scale_color_manual(values = c("Significant" = "#d62728", "Not significant" = "grey50")) +
  coord_cartesian(xlim = c(0.3, 1.6)) +
  xlab("Hazard Ratio (HR)") + ylab("") +
  ggtitle("Subgroup Analysis: Magnesium Supplementation and 28-Day ICU Mortality") +
  theme_minimal(base_family = "TT Times New Roman", base_size = 18) +
  theme(
    axis.text.y = element_text(size = 18, hjust = 0, family = "TT Times New Roman"),
    axis.text.x = element_text(size = 14, family = "TT Times New Roman"),
    plot.title = element_text(size = 18, face = "bold", family = "TT Times New Roman"),
    legend.text = element_text(size = 18, family = "TT Times New Roman"),
    legend.position = "top",
    legend.title = element_blank(),
    panel.grid = element_blank(),
    axis.title.x = element_text(size = 14, family = "TT Times New Roman"),
    axis.title.x.top = element_text(size = 14, family = "TT Times New Roman"),
    axis.text.x.top = element_text(size = 14, family = "TT Times New Roman"),
    axis.ticks.x.top = element_line(size = 1),
    axis.line.x.top = element_line(size = 1)
  ) +
  scale_x_continuous(position = "top")






































# 调整图形的尺寸
options(repr.plot.width = 8, repr.plot.height = 6)  # 可以调整为你需要的宽度和高度
ggplot(final_data, aes(x = HR, y = Label)) +
  geom_point(aes(color = Significance), size = 3, na.rm = TRUE) +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper, color = Significance), height = 0.2, na.rm = TRUE) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black", linewidth = 0.8) +
  geom_text(aes(label = p_label), x = 1.55, hjust = 0, size = 4, na.rm = TRUE) +  # 增大p值标签字体
  scale_color_manual(values = c("Significant" = "#d62728", "Not significant" = "grey50")) +
  coord_cartesian(xlim = c(0.3, 1.6)) +
  xlab("Hazard Ratio (HR)") + ylab("") +
  ggtitle("Subgroup Analysis: Magnesium Supplementation and 28-Day ICU Mortality") +
  theme_minimal(base_size = 18) +
  theme(
    axis.text.y = element_text(size = 18, hjust = 0),  # 增大y轴标签字体
    axis.text.x = element_text(size = 18),  # 增大x轴标签字体
    plot.title = element_text(size = 18, face = "bold"),  # 增大标题字体
    legend.text = element_text(size = 18),  # 增大图例文字
    legend.position = "top",
    legend.title = element_blank(),
    panel.grid = element_blank(),  # 去掉背景网格线
    axis.title.x = element_text(size = 18),  # x轴标题字体大小
    axis.title.x.top = element_text(size = 18),  # 设置x轴标题的位置
    axis.text.x.top = element_text(size = 18),  # 设置x轴数字的位置
    axis.ticks.x.top = element_line(size = 1),  # 设置x轴的刻度线
    axis.line.x.top = element_line(size = 1)  # 设置x轴线条
  ) +
  scale_x_continuous(position = "top")  # 将x轴移到顶部









































library(ggplot2)
library(dplyr)
# 原始数据
subgroup_table <- data.frame(
  Subgroup = c("age_group", "age_group", "charlson_group", "charlson_group", 
               "sofa_group", "sofa_group", "sofa_group", "rrt", 
               "rrt", "myocardial_infarct", "myocardial_infarct", 
               "ventilation", "ventilation"),
  Level = c("<65", "≥65", "<6", "≥6", "Low (0–6)", "High (≥12)", "Medium (7–11)", 
            "No", "Yes", "No", "Yes", "No", "Yes"),
  HR = c(0.92, 0.81, 0.94, 0.8, 1.03 , 0.45, 0.63, 0.81, 1.28 , 0.88 ,0.76 ,  0.95 , 0.62 ),
  CI_lower = c(0.72, 0.7, 0.75, 0.68, 0.87, 0.31, 0.49, 0.71, 0.87,  0.76, 0.55,0.82, 0.49),
  CI_upper = c(1.19, 0.94, 1.16, 0.93, 1.21, 0.65, 0.8, 0.93, 1.88,  1.01,1.05, 1.11, 0.78),
  P_value = c("0.535", "0.006", "0.552", "0.005", "0.724", "<0.001", "<0.001", "0.003",
              "0.212",  "0.06","0.092", "0.554", "<0.001")
)

# 添加标签，确保 SubLabel 唯一
subgroup_table <- subgroup_table %>%
  mutate(
    Significance = ifelse(P_value < 0.05, "Significant", "Not significant"),
    p_label = paste0("p = ", formatC(P_value, format = "e", digits = 2)),
    SubLabel = paste0("   ", Subgroup, ": ", Level)
  )

# 构造 y 轴标签顺序
ordered_labels <- c(
  "age_group", "   age_group: <65", "   age_group: ≥65",
  "charlson_group", "   charlson_group: <6", "   charlson_group: ≥6",
  "sofa_group", "   sofa_group: Low (0–6)", "   sofa_group: High (≥12)", "   sofa_group: Medium (7–11)",
  "rrt", "   rrt: No", "   rrt: Yes",
  "myocardial_infarct", "   myocardial_infarct: No", "   myocardial_infarct: Yes",
  "ventilation", "   ventilation: No", "   ventilation: Yes"
)

# 主标签行
main_labels <- data.frame(
  Label = c("age_group", "charlson_group", "sofa_group", "rrt", "myocardial_infarct", "ventilation"),
  HR = NA, CI_lower = NA, CI_upper = NA, P_value = NA, Significance = NA, p_label = NA
)

# 子组数据
plot_data <- subgroup_table %>%
  mutate(Label = SubLabel) %>%
  select(Label, HR, CI_lower, CI_upper, P_value, Significance, p_label)

# 合并并排序
final_data <- bind_rows(main_labels, plot_data) %>%
  mutate(Label = factor(Label, levels = rev(ordered_labels)))

# 绘图
ggplot(final_data, aes(x = HR, y = Label)) +
  geom_point(aes(color = Significance), size = 4, shape = 16, na.rm = TRUE) +  # 增加点的大小，使用填充圆形
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper, color = Significance), height = 0.3, na.rm = TRUE) +  # 增加误差条宽度
  geom_vline(xintercept = 1, linetype = "dashed", color = "black", size = 1.2) +  # 更粗的虚线
  geom_text(aes(label = p_label), x = 1.55, hjust = 0, size = 4, fontface = "italic", na.rm = TRUE) +  # p 值标签样式
  scale_color_manual(values = c("Significant" = "#E63946", "Not significant" = "gray70")) +  # 优化颜色
  coord_cartesian(xlim = c(0.3, 1.6)) +  # 设置 x 轴范围
  xlab("Hazard Ratio (HR)") + ylab("") + 
  ggtitle("Subgroup Analysis: Magnesium Supplementation and 28-Day ICU Mortality") +
  theme_minimal(base_size = 16) +  # 更大的字体
  theme(
    axis.text.y = element_text(size = 18, hjust = 0, color = "black"),  # Y轴标签加大并修改颜色
    plot.title = element_text(size = 18, face = "bold", color = "#1D3557"),  # 改进标题样式
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 18),
    plot.margin = margin(10, 10, 10, 10),  # 调整图形边距
    panel.grid.major = element_line(color = "gray90"),  # 更淡的网格线
    panel.grid.minor = element_blank()  # 不显示次要网格线
  )



