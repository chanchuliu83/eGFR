library(haven)
library(dplyr)
#打开数据集及插补矩阵
datach <- read_dta("D:/items/lip/charls.dta")

#两个纵向序列，2011年的2015年的
# 提取包含2011年至2020年的ID
ids_with_2011 <- datach %>%
  filter(iwy == 2011) %>%
  distinct(ID) %>%  # 去重
  pull(ID)  # 提取ID向量

# 创建datach1：包含2011年至2020年的数据
datach1 <- datach %>%
  filter(ID %in% ids_with_2011)

# 如果在2011年必要的基线数据存在缺失，那么删除该ID所对应的样本
missing_ids <- datach1 %>%
  group_by(ID) %>%
  summarise(
    has_na = any(
      is.na(across(c(gender, age, bl_cysc,kidneye))) & 
        (iwy == 2011)
    )
  )%>%
  filter(has_na) %>%
  pull(ID)

datach1_cleaned <- datach1[!datach1$ID %in% missing_ids, ]

# 如果2011年存在髋关节骨折，则删除所对应的样本
ids_to_remove <- datach1_cleaned %>%
  filter(iwy == 2011, hip == 1) %>%  
  distinct(ID) %>%                   
  pull(ID) 
datach1_cleaned2 <- datach1_cleaned[!datach1_cleaned$ID %in% ids_to_remove, ]

# 删除在2013、2015、2018、2020年份中hip变量存在缺失值的样本
missing_ids_hip <- datach1_cleaned2 %>%
  filter(iwy %in% c(2013, 2015, 2018, 2020) & is.na(hip)) %>%
  distinct(ID) %>%
  pull(ID)
datach1_cleaned3 <- datach1_cleaned2 %>%
  filter(!ID %in% missing_ids_hip)

#排除肿瘤患者
ids_to_remove_cancre <- datach1_cleaned3 %>%
  filter(cancre == 1) %>%    # 筛选出 cancre=1 的记录
  distinct(ID) %>%           # 去重，确保每个 ID 只出现一次
  pull(ID)                   # 提取 ID 为向量
datach1_cleaned4 <- datach1_cleaned3[!datach1_cleaned3$ID %in% ids_to_remove_cancre, ]

#排除肾病患者
ids_to_remove_kidneye <- datach1_cleaned4 %>%
  filter(kidneye == 1) %>%    # 筛选出 kidneye=1 的记录
  distinct(ID) %>%           # 去重，确保每个 ID 只出现一次
  pull(ID)                   # 提取 ID 为向量
datach1_cleaned5 <- datach1_cleaned4[!datach1_cleaned4$ID %in% ids_to_remove_kidneye, ]

#生成生存数据
#创建新数据集data，保留下列变量
data1 <- c("ID", "iwy", "iwm", "gender", "rural", 
           "hibpe", "diabe", 'stroke', 'hearte', "lunge", 
           "psyche", "arthre", "dyslipe", "livere", "digeste", "asthmae", "memrye", 
           "drinkev", "smokev", "bmi", "bl_cysc", "hip", "age", "edu" )
data1<- datach1_cleaned5[, data1]

# 计算每个观测的时间（以月为单位）
data1 <- data1 %>%
  group_by(ID) %>%
  mutate(
    time = (iwy - min(iwy)) * 12 + iwm - min(iwm[iwy == min(iwy)])
  ) %>%
  ungroup()

# 找到每个ID的阳性事件发生时间
# 如果hip存在缺失值，则使用hip为非缺失值时所对应的最大时间
# 如果hip始终为0，则使用最大观察时间替换
result1 <- data1 %>%
  group_by(ID) %>%
  summarise(
    time_to_event = ifelse(
      any(!is.na(hip) & hip == 1),
      min(time[!is.na(hip) & hip == 1]),
      ifelse(
        any(!is.na(hip) & hip == 0),
        max(time[!is.na(hip)], na.rm = TRUE),
        max(time, na.rm = TRUE)
      )
    ),
    status = ifelse(
      any(!is.na(hip) & hip == 1),
      1,
      0
    )
  ) %>%
  ungroup()

# 根据 iwy = 2011 筛选样本并创建data_1
data11 <- data1[data1$iwy == 2011, ]
#数据集进行拼接（仅保留同时存在于两个数据集的 ID）
data12<- data11 %>%
  inner_join(result1, by = "ID")

#计算变量eGFRcysc
data12 <- data12 %>%
  mutate(
    a = case_when(
      bl_cysc <= 0.8 ~ -0.499,
      bl_cysc > 0.8 ~ -1.328
    ),
    eGFR = 133 * ((bl_cysc / 0.8)^a) * (0.996^age) * (ifelse(gender == 0, 0.932, 1))
  ) %>%
  select(-a)  # 删除中间变量a

#2015年数据清洗
# 创建datach2：包含其他年份的数据（排除datach1中的ID）
datach2 <- datach %>%
  filter(!ID %in% ids_with_2011)

#删除2011和2015年的数据
datach2 <- datach2 %>%
  filter(!(iwy %in% c(2011, 2012, 2013, 2014)))

#如果不存在2015年的数据，则删除该ID
datach2 <- datach2 %>%
  group_by(ID) %>%
  filter(any(iwy == 2015)) %>%
  ungroup()

## 如果在2015年必要的基线数据存在缺失，那么删除该ID所对应的样本
missing2_ids <- datach2 %>%
  group_by(ID) %>%
  summarise(
    has_na = any(
      is.na(across(c(gender, age, bl_cysc, kidneye))) & 
        (iwy == 2015)
    )
  )%>%
  filter(has_na) %>%
  pull(ID)
datach2_cleaned <- datach2[!datach2$ID %in% missing2_ids, ]

# 如果2015年存在髋关节骨折，则删除所对应的样本
ids_to_remove2 <- datach2_cleaned %>%
  filter(iwy == 2015, hip == 1) %>%  # 筛选2011年且hip=1的记录
  distinct(ID) %>%                   # 去重，确保每个ID只出现一次
  pull(ID)                           # 提取ID为向量
datach2_cleaned2 <- datach2_cleaned[!datach2_cleaned$ID %in% ids_to_remove2, ]

# 删除在2018、2020年份中hip变量存在缺失值的样本
missing_ids2_hip <- datach2_cleaned2 %>%
  filter(iwy %in% c(2018, 2020) & is.na(hip)) %>%
  distinct(ID) %>%
  pull(ID)
datach2_cleaned3 <- datach2_cleaned2 %>%
  filter(!ID %in% missing_ids2_hip)

#排除肿瘤患者
ids_to_remove_cancre2 <- datach2_cleaned3 %>%
  filter(cancre == 1) %>%    # 筛选出 cancre=1 的记录
  distinct(ID) %>%           # 去重，确保每个 ID 只出现一次
  pull(ID)                   # 提取 ID 为向量
datach2_cleaned4 <- datach2_cleaned3[!datach2_cleaned3$ID %in% ids_to_remove_cancre2, ]
count <- datach2_cleaned4 %>% distinct(ID) %>%nrow()
count#5193

#排除肾病患者
ids_to_remove_kidneye2 <- datach2_cleaned4 %>%
  filter(kidneye == 1) %>%    # 筛选出 kidneye=1 的记录
  distinct(ID) %>%           # 去重，确保每个 ID 只出现一次
  pull(ID)                   # 提取 ID 为向量
datach2_cleaned5 <- datach2_cleaned4[!datach2_cleaned4$ID %in% ids_to_remove_kidneye2, ]

#生成生存数据
#创建新数据集data2（包含了中风和心脏病），保留下列变量
data2 <- c("ID", "iwy", "iwm", "gender", "rural", 
           "hibpe", "diabe", 'stroke', 'hearte', "lunge", 
           "psyche", "arthre", "dyslipe", "livere", "digeste", "asthmae", "memrye", 
           "drinkev", "smokev", "bmi", "bl_cysc", "hip", "age", "edu")
data2<- datach2_cleaned5[, data2]

# 计算每个观测的时间（以月为单位）
data2 <- data2 %>%
  group_by(ID) %>%
  mutate(
    time = (iwy - min(iwy)) * 12 + iwm - min(iwm[iwy == min(iwy)])
  ) %>%
  ungroup()

# 找到每个ID的阳性事件发生时间
# 如果hip存在缺失值，则使用hip为非缺失值时所对应的最大时间
# 如果hip始终为0，则使用最大观察时间替换
result2 <- data2 %>%
  group_by(ID) %>%
  summarise(
    time_to_event = ifelse(
      any(!is.na(hip) & hip == 1),
      min(time[!is.na(hip) & hip == 1]),
      ifelse(
        any(!is.na(hip) & hip == 0),
        max(time[!is.na(hip)], na.rm = TRUE),
        max(time, na.rm = TRUE)
      )
    ),
    status = ifelse(
      any(!is.na(hip) & hip == 1),
      1,
      0
    )
  ) %>%
  ungroup()

# 根据 iwy = 2015 筛选样本并创建data_1
data21 <- data2[data2$iwy == 2015, ]
#数据集进行拼接
# 内连接（仅保留同时存在于两个数据集的 ID）
data22<- data21 %>%
  inner_join(result2, by = "ID")

#计算变量eGFRcysc
data22 <- data22 %>%
  mutate(
    a = case_when(
      bl_cysc <= 0.8 ~ -0.499,
      bl_cysc > 0.8 ~ -1.328
    ),
    eGFR = 133 * ((bl_cysc / 0.8)^a) * (0.996^age) * (ifelse(gender == 0, 0.932, 1))
  ) %>%
  select(-a)  # 删除中间变量a

# 合并两个数据集
data3 <- bind_rows(data12, data22)
data3 <- data3%>%select(-hip)#删除hip

#BMI转换为有序分类变量
data3 <- data3 %>%
  mutate(
    bmi = case_when(
      bmi < 18.5 ~ 1,
      bmi >= 18.5 & bmi < 25 ~ 2,
      bmi >= 25 & bmi < 30 ~ 3,
      bmi >= 30 ~ 4))

#进行下一步数据缺失值分析和插补，移除标签(数据插补时不能带有标签)
library(labelled)
library(VIM)
library(mice)
data3 <- remove_labels(data3)
md.pattern(data3, rotate.names = TRUE)
# 可视化缺失值分布，bmi缺失最多，为11%
aggr_plot <- aggr(data3, col = c("navyblue", "red"), 
                  numbers = TRUE, sortVars = TRUE, 
                  labels = names(data3), cex.axis = 0.7, 
                  gap = 3, ylab = c("Missing Data Pattern", "Frequency"))
#按照常用的选择多重插补rf法
data3imp <- mice(
data3,
method = "rf",
seed = 1234,
print = FALSE,
m = 5,)
data3imp$predictorMatrix
data3imp$predictorMatrix[, c("ID", 'iwy', 'iwm', 'bl_cysc',
'time', "time_to_event", "status", 'eGFR')] <- 0
data3imp <- mice(
  data3,
  method = "rf",
  seed = 1234,
  print = FALSE,
  m = 5,
  predictorMatrix = data3imp$predictorMatrix
)
data3imp#查看矩阵信息
plot(data3imp)#查看均值与方差收敛与否
densityplot(data3imp, layout = c(3, 1))#查看密度曲线重合度（红色插补，蓝色观测）

#提取五次的插补结果
data3imp1<- mice::complete(data3imp, action = 1)
data3imp2<- mice::complete(data3imp, action = 2)
data3imp3<- mice::complete(data3imp, action = 3)
data3imp4<- mice::complete(data3imp, action = 4)
data3imp5<- mice::complete(data3imp, action = 5)

#计算数据内部一致性，挑选合适的插补集
library(ltm)
detach("package:dplyr", unload = TRUE)
library(dplyr)
imp1 <- data3imp1 %>% select(bmi, hibpe, diabe, stroke, hearte,  drinkev, lunge, psyche, arthre, dyslipe, livere, digeste, asthmae, memrye, edu, smokev)
imp2 <- data3imp2 %>% select(bmi, hibpe, diabe, stroke, hearte,  drinkev, lunge, psyche, arthre, dyslipe, livere, digeste, asthmae, memrye, edu, smokev)
imp3 <- data3imp3 %>% select(bmi, hibpe, diabe, stroke, hearte,  drinkev, lunge, psyche, arthre, dyslipe, livere, digeste, asthmae, memrye, edu, smokev)
imp4 <- data3imp4 %>% select(bmi, hibpe, diabe, stroke, hearte,  drinkev, lunge, psyche, arthre, dyslipe, livere, digeste, asthmae, memrye, edu, smokev)
imp5 <- data3imp5 %>% select(bmi, hibpe, diabe, stroke, hearte,  drinkev, lunge, psyche, arthre, dyslipe, livere, digeste, asthmae, memrye, edu, smokev)
cronbach.alpha(imp1) #0.289
cronbach.alpha(imp2) 
cronbach.alpha(imp3) 
cronbach.alpha(imp4) #0.29
cronbach.alpha(imp5) 

#创建age2变量
data3imp4 <- data3imp4 %>%
  mutate(
    age2 = case_when(
      age <= 50 ~ '1',
      age > 50 & age <= 60 ~ '2',
      age > 60 & age <= 70 ~ '3',
      age >= 70 ~ '4'))

#基线分析
categorical_vars <- c("gender", "rural", "smokev", "drinkev", 
                      "hibpe", "diabe", "stroke", "hearte", "lunge", 'psyche',
                      "arthre", "dyslipe", "livere", "digeste", "asthmae", "memrye",
                      'age2','edu','bmi')

for (var in categorical_vars) {
  table_result <- table(data3imp4[[var]], data3imp4$status)
  cat(paste("变量", var, "与 status 的四格表：\n"))
  print(table_result)
  cat("\n")
}
#进行卡方检验
for (var in categorical_vars) {
  contingency_table <- table(data3imp4[[var]], data3imp4$status)
  chi_test <- chisq.test(contingency_table)
  cat("chi：", var, "和 status\n")
  print(chi_test)
  cat("\n")
}

# 对有序分类变量和连续协变量进行曼惠特尼检验
data3imp4$bmi <- as.numeric(data3imp4$bmi)
data3imp4$edu <- as.numeric(data3imp4$edu)
data3imp4$age2 <- as.numeric(data3imp4$age2)
continuous_vars <- c("edu", "age", "age2", "bmi", "bl_cysc", "eGFR", "time_to_event")

for (var in continuous_vars) {
  wilcox_test <- wilcox.test(data3imp4[[var]] ~ data3imp4$status, exact = FALSE)
  cat("MU：", var, "和 status\n")
  print(wilcox_test)
  cat("\n")
}
median_IQR <- data3imp4 %>%
  group_by(status) %>%
  summarise(
    median_eGFR = median(eGFR, na.rm = TRUE),
    IQR_eGFR = IQR(eGFR, na.rm = TRUE)
  )
median_IQR

#MGM
library(mgm)
datamgm <- data3imp4 %>%
  select(bmi, edu, gender, age2,   
         arthre, livere, digeste, hibpe, dyslipe, stroke,
         smokev, drinkev, status, eGFR) %>%
  mutate(
    gender = as.numeric(gender),
    bmi = as.numeric(bmi),
    age2 = as.numeric(age2),
    edu = as.numeric(edu),
    arthre = as.numeric(arthre),
    hibpe = as.numeric(hibpe),
    stroke = as.numeric(stroke),
    dyslipe = as.numeric(dyslipe),
    livere = as.numeric(livere),
    digeste = as.numeric(digeste),
    drinkev = as.numeric(drinkev),
    smokev = as.numeric(smokev),
    status = as.numeric(status)
  )

# 定义变量类型：'g' 表示连续高斯变量，'c' 表示二分类变量
types <- c('g', 'g', 'c', 'g',
           'c','c','c','c', 'c', 'c',
           'c','c','c','g')

# 定义分类变量的类别数
levels <- c(1, 1, 2, 1,
            2, 2, 2, 2, 2, 2, 
            2, 2, 2, 1)

# 构建混合图模型
fitmgm <- mgm(
  data = as.matrix(datamgm),
  type = types,
  level = levels,
  lambdaSel = 'EBIC',  
  lambdaGam = 0.25 )

# 提取边权重矩阵,因为符合是单独储存的，所以需要提取符号矩阵
edge_weights <- fitmgm$pairwise$wadj
edge_signs <- fitmgm$pairwise$signs
edge_signs_df <- as.data.frame(edge_signs)
edge_weights_df <- as.data.frame(edge_weights)
View(edge_weights_df)
View(edge_signs_df)

#对eGFR进行分组
data3imp4 <- data3imp4 %>%
  mutate(
    eGFR2 = case_when(
      eGFR < 60 ~ 3,
      eGFR >= 60 & eGFR < 90 ~ 2,
      eGFR >= 90 ~ 1
    )
  )
data3imp4$eGFR2 <- as.character(data3imp4$eGFR2, ordered = TRUE)

#k-m
library(survival)
fit_km <- survfit(Surv(time_to_event, status) ~ eGFR2, data = data3imp4)
log_rank_test <- survdiff(Surv(time_to_event, status) ~ eGFR2, data = data3imp4)
print(log_rank_test)

#无调整模型：
data3imp4$age2 <- as.numeric(data3imp4$age2)
data3imp4$bmi <- as.numeric(data3imp4$bmi)
data3imp4$edu <- as.numeric(data3imp4$edu)
fit0 <- coxph(Surv(time_to_event, status) ~ eGFR2, data = data3imp4)
summary(fit0)
#调整模型1：
fit1 <- coxph(Surv(time_to_event, status) ~ eGFR2 + age2 +gender, data = data3imp4)
summary(fit1)
#调整模型2：
fit2 <- coxph(Surv(time_to_event, status) ~ eGFR2 + age2 + gender + bmi + drinkev + smokev, data = data3imp4)
summary(fit2)
#调整模型3
fit3 <- coxph(Surv(time_to_event, status) ~ eGFR2 + age2 + gender + bmi + edu + drinkev + smokev
              + hibpe + stroke + arthre  + dyslipe + livere + digeste, data = data3imp4)
summary(fit3)

#非线性关系检验，只能用coxph才能进行检验
library(rms)
dd <- datadist(data3imp4) 
options(datadist='dd') 

#无调整模型的RCS
fit_rcs0 <- cph(Surv(time_to_event, status) ~ rcs(eGFR,4),x=TRUE, y=TRUE,data=data3imp4)
anova(fit_rcs0)

#调整模型3的RCS
fit_rcs3 <- cph(Surv(time_to_event, status) ~ rcs(eGFR,4) + age2 + gender + bmi + edu + drinkev + smokev
                + hibpe + stroke + arthre  + dyslipe + livere + digeste, x=TRUE, y=TRUE,data=data3imp4)

anova(fit_rcs3)

#敏感性分析
#把年龄进行分层
fit_sc <- coxph(
  Surv(time_to_event, status) ~ strata(age2) + gender + bmi + edu + drinkev + smokev
  + hibpe + stroke + arthre  + dyslipe + livere + digeste + eGFR2, data = data3imp4)
summary(fit_sc)
#使用时间加速失效模型：
fit_aft <- survreg(
  Surv(time_to_event, status) ~ age2 + gender + bmi + edu + drinkev + smokev
  + hibpe + stroke + arthre  + dyslipe + livere + digeste + eGFR2, dist = 'weibull',
  data = data3imp4,
)
summary(fit_aft)
#对weibull回归进行进行比例风险转换
library(SurvRegCensCov)
ConvertWeibull(fit_aft,conf.level = 0.95)


#中介分析
library(mediation)
set.seed(1234)
#check 95%CIupper value
trace(mediation:::print.summary.mediate, 
      at = 11,
      tracer = quote({
        printCoefmat <- function(x, digits) {
          p <- x[, 4] #p-values seem to be stored rounded
          x[, 1:3] <- sprintf("%.6f", x[, 1:3])
          x[, 4] <- sprintf("%.3f", p)
          print(x, quote = FALSE, right = TRUE)
        } 
      }),
      print = FALSE)
#建立中介模型
data3imp4 <- data3imp4 %>%
  mutate(
    age3 = case_when(
      age < 60 ~ 0,
      age >= 60 ~ 1))
data3imp4 <- data3imp4 %>%
  mutate(
    eGFR4 = case_when(
      eGFR < 60 ~ 0,
      eGFR >= 60 ~ 1
    )
  )

med_fit_age <- lm(eGFR4 ~  age3, data = data3imp4)
out_fit_age <- lm(status ~ age3 + eGFR4, data = data3imp4)
med_fit_hibpe <- lm(eGFR4 ~  hibpe, data = data3imp4)
out_fit_hibpe_ <- lm(status ~ hibpe + eGFR4, data = data3imp4)
med_fit_stroke_ <- lm(eGFR4 ~  stroke, data = data3imp4)
out_fit_stroke <- lm(status ~ stroke + eGFR4, data = data3imp4)
med_fit_dyslipe <- lm(eGFR4 ~  dyslipe, data = data3imp4)
out_fit_dyslipe <- lm(status ~ dyslipe + eGFR4, data = data3imp4)
med_fit2 <- lm(eGFR4 ~  age3 + gender + bmi + edu + drinkev + smokev
               + hibpe + stroke + arthre  + dyslipe + livere + digeste, data = data3imp4)
out_fit2 <- lm(status ~ age3 + gender + bmi + edu + drinkev + smokev
               + hibpe + stroke + arthre  + dyslipe + livere + digeste + eGFR4, data = data3imp4)

med_out_age <- mediate(model.m = med_fit_age, model.y = out_fit_age, treat = "age3", mediator = "eGFR4", boot = TRUE, sims = 1000, boot.ci.type = "perc")
mediation:::print.summary.mediate(summary(med_out_age))
med_out_age2 <- mediate(model.m = med_fit2, model.y = out_fit2, treat = "age3", mediator = "eGFR4", boot = TRUE, sims = 1000, boot.ci.type = "perc")
mediation:::print.summary.mediate(summary(med_out_age2))

med_out_hibpe <- mediate(model.m = med_fit_hibpe, model.y = out_fit_hibpe_, treat = "hibpe", mediator = "eGFR4", boot = TRUE, sims = 1000, boot.ci.type = "perc")
mediation:::print.summary.mediate(summary(med_out_hibpe))
med_out_hibpe2 <- mediate(model.m = med_fit2, model.y = out_fit2, treat = "hibpe", mediator = "eGFR4", boot = TRUE, sims = 1000, boot.ci.type = "perc")
mediation:::print.summary.mediate(summary(med_out_hibpe2))

med_out_stroke <- mediate(model.m = med_fit_stroke_, model.y = out_fit_stroke, treat = "stroke", mediator = "eGFR4", boot = TRUE, sims = 1000, boot.ci.type = "perc")
mediation:::print.summary.mediate(summary(med_out_stroke))
med_out_stroke2 <- mediate(model.m = med_fit2, model.y = out_fit2, treat = "stroke", mediator = "eGFR4", boot = TRUE, sims = 1000, boot.ci.type = "perc")
mediation:::print.summary.mediate(summary(med_out_stroke2))

med_out_dyslipe <- mediate(model.m = med_fit_dyslipe, model.y = out_fit_dyslipe, treat = "dyslipe", mediator = "eGFR4", boot = TRUE, sims = 1000, boot.ci.type = "perc")
mediation:::print.summary.mediate(summary(med_out_dyslipe))
med_out_dyslipe2 <- mediate(model.m = med_fit2, model.y = out_fit2, treat = "dyslipe", mediator = "eGFR4", boot = TRUE, sims = 1000, boot.ci.type = "perc")
mediation:::print.summary.mediate(summary(med_out_dyslipe2))



