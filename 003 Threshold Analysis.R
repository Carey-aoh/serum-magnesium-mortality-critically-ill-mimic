# 清空环境
rm(list = ls())
gc()


# 加载包
library(survival)
library(rms)
library(segmented)
library(ggplot2)
library(dplyr)
library(forestplot)
# 设置数据目录并读取数据
setwd("E:/100-科研/400_镁/006重启版")
data<- read.csv("E:\\100-科研\\400_镁\\002csv\\admission生存时间.csv")

data <- data %>%
  mutate(
    gender = factor(gender),
    renal_disease = factor(renal_disease),
    liver_disease = factor(liver_disease),
    ventilationrt = factor(ventilation),
    rrt = factor(rrt),
    magnesium_sulfate_supplementation = factor(magnesium_sulfate_supplementation),
  )

dd <- datadist(data)
options(datadist = "dd")

#模型1
model1 <- cph(Surv(icu_intime_to_dod, death_ICU28days) ~ rcs(Magnesium, 4), data = data)
summary(model1)
pred1 <- Predict(model1, Magnesium, fun = exp, ref.zero = TRUE)

# 先保存图形对象到一个变量
p <- ggplot(pred1, aes(x = Magnesium, y = yhat)) +
  geom_line(color = "#0072B2", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "#56B4E9") +
  labs(
    title = "",
    y = "Hazard Ratio (HR)",
    x = "Magnesium (mg/dL)"
  ) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )
p
p1 <- p + coord_cartesian(ylim = c(0.8, 2.5))
p1
# 或导出为 PNG 格式（更通用）
ggsave(
  filename = "E:/100-科研/400_镁/006重启版/model1.png",  # 添加文件名和 .png 后缀
  plot = p1,
  device = "png",
  dpi = 1200,
  width = 8,
  height = 6
)



#模型2
model2 <- cph(Surv(icu_intime_to_dod, death_ICU28days) ~ rcs(Magnesium, 4) + Age + Weight +gender, data = data)
pred2 <- Predict(model2, Magnesium, fun = exp, ref.zero = T)

p2<-ggplot(pred2, aes(x = Magnesium, y = yhat)) +
  geom_line(color = "#0072B2", size = 1) +          # 修改线条颜色和粗细
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "#56B4E9") +
  labs(
    title = "",
    y = "Hazard Ratio (HR)",
    x = "Magnesium (mg/dL)",
    subtitle = NULL,  # 去除subtitle中的调整变量注释
    caption = NULL    # 去除可能的caption
  ) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  theme_bw() +                                      # 使用简洁主题
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14), # 标题居中加粗
    panel.grid.minor = element_blank()              # 去除次要网格线
  )

p21 <- p2 + coord_cartesian(ylim = c(0.8, 2.5))
p21
# 或导出为 PNG 格式（更通用）
ggsave(
  filename = "E:/100-科研/400_镁/006重启版/model2.png",  # 添加文件名和 .png 后缀
  plot = p21,
  device = "png",
  dpi = 1200,
  width = 8,
  height = 6
)



library(survival)

# 第一步：保留生存时间 > 0 的数据
data_filtered <- subset(data, icu_intime_to_dod > 0)

# 重新设置数据分布（重要！过滤数据后必须重置）
dd <- datadist(data_filtered)
options(datadist = "dd")


# 第二步：定义函数，计算每个 cutpoint 的 log-likelihood
get_loglik <- function(cut) {
  data_cut <- data_filtered
  data_cut$Magnesium_low  <- ifelse(data_cut$Magnesium <= cut, data_cut$Magnesium, cut)
  data_cut$Magnesium_high <- ifelse(data_cut$Magnesium > cut, data_cut$Magnesium - cut, 0)
  
  model <- coxph(Surv(icu_intime_to_dod, death_ICU28days) ~ Magnesium_low + Magnesium_high +
                   Age + Weight + WBC + RBC + Platelets + Hemoglobin + Glucose +
                   Potassium + Sodium + Calcium + Chloride + BUN + number_of_doses +
                   rrt
                 , data = data_cut)
  
  return(logLik(model)[1])  # 返回 log-likelihood
}

# 第三步：生成候选 cutpoints（5% ~ 95% 分位）
quantiles <- quantile(data_filtered$Magnesium, probs = seq(0.05, 0.95, by = 0.01))
cut_grid <- as.numeric(quantiles)

# 第四步：计算每个 cutpoint 的 log-likelihood
loglik_vals <- sapply(cut_grid, get_loglik)

# 第五步：找出最大 log-likelihood 的 cutpoint
best_cut <- cut_grid[which.max(loglik_vals)]
print(paste("最佳拐点为：", round(best_cut, 3)))



# 第六步：可视化 log-likelihood 曲线并添加最佳拐点标签

# 设置输出路径和参数（示例为PNG格式）
png(filename = "E:/100-科研/400_镁/006重启版/loglik_curve.png",  # 替换为你的路径
    width = 8,       # 宽度(英寸)
    height = 6,      # 高度(英寸)
    units = "in", 
    res = 1200,      # 分辨率(dpi)
    type = "cairo")  # 抗锯齿引擎（使线条更平滑）



# 修改后的代码
plot(cut_grid, loglik_vals, type = "l", lwd = 2,
     col = "#0072B2",  # 设置线条颜色
     xlab = "Magnesium (mg/dL)", ylab = "Log-likelihood",
     main = "")
abline(v = best_cut, lty = 2, col = "red")
text(
  x = best_cut,
  y = grconvertY(0.95, "npc"),
  labels = paste("best cut =", round(best_cut, 3)),
  col = "red",
  pos = 4
)


# 必须关闭图形设备才能完成保存
dev.off()

png(filename = "E:/100-科研/400_镁/006重启版/loglik_curve6.png",
    width = 8, height = 6, units = "in", res = 1200, type = "cairo")

# 更合适的绘图参数设置
par(mar = c(2, 2.7, 2, 1.5),     # 加大左边距，避免标签被覆盖
    cex.axis = 0.7,              # 坐标轴数字字体大小
    cex.lab = 0.9,               # 标签字体大小
    las = 0,                     # y轴标签水平显示
    mgp = c(1.0, 0.2, 0),        # 第1项: 标签与轴线的距离（更远），2: 刻度线距轴线的距离
    tck = -0.010)               # 控制刻度线长度（负值为向内）

# 绘图
plot(cut_grid, loglik_vals, type = "l", lwd = 2,
     col = "#0072B2",
     xlab = "Magnesium (mg/dL)", ylab = "Log-likelihood",
     main = "")

abline(v = best_cut, lty = 2, col = "red")

usr <- par("usr")  # 获取坐标轴范围
text(
  x = best_cut,
  y = usr[4] - 0.05 * (usr[4] - usr[3]),  # 控制文字靠上
  labels = paste("best cut =", round(best_cut, 3)),
  col = "red",
  pos = 4
)

dev.off()








# 加载所需包
library(rms)
library(ggplot2)

# 设置数据
dd <- datadist(data_filtered)
options(datadist = 'dd')

# 构建 RCS 模型（以模型3为例）
rcs_model3 <- cph(Surv(icu_intime_to_dod, death_ICU28days) ~rcs(Magnesium, 4) +Age + Weight +gender+ WBC + RBC 
                  + Platelets + Hemoglobin + Glucose +
                    Potassium + Sodium + Calcium + Chloride + BUN + number_of_doses +
                    rrt,data = data_filtered, x = TRUE, y = TRUE, surv = TRUE)

# 绘制 RCS 曲线 + 标出拐点（假设 best_cut 为之前计算出的阈值）
best_cut <- 2.1 # 请替换为你之前找到的最佳 cutpoint

# 使用 rms 提供的绘图数据
plot_data <- Predict(rcs_model3, Magnesium, fun=exp, ref.zero=TRUE)



p3 <- ggplot(plot_data, aes(x = Magnesium, y = yhat)) +
  geom_line(color = "#0072B2", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "#56B4E9") +
  geom_vline(xintercept = best_cut, linetype = "dashed", color = "#FF3030", size = 0.7, alpha = 0.8) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  annotate("text",
           x = best_cut,
           y = max(plot_data$yhat, na.rm = TRUE) - 0.32,  # 可根据数据适当调整 y 值位置
           label = paste("Inflection Point:\n", round(best_cut, 3), "mg/dL"),
           hjust = -0.1, vjust = 1.2,
           color = "#FF3030", size = 3.8) +         # 字体颜色与竖线一致
  labs(
    title = "",
    y = "Hazard Ratio (HR)",
    x = "Magnesium (mg/dL)",
    caption = NULL
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )
p3
ggsave(
  filename = "E:/100-科研/400_镁/006重启版/model3.png",  # 添加文件名和 .png 后缀
  plot = p3,
  device = "png",
  dpi = 1200,
  width = 8,
  height = 6
)


