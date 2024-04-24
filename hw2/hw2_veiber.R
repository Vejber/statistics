# Загрузка необходимых библиотек
library(tidyverse)
library(ggplot2)
library(readr)
library(corrplot)
library(tidyr)
library(GGally)
library(plotly)
library(stats)
library(reshape2)

#Просмотр данных
data <- read_delim("~/Downloads/addicts.csv", delim = ";")
head(data)
summary(data)

# Функция для выполнения тестов хи-квадрат и критерия Фишера и печати результатов
perform_tests <- function(data, indep_var) {
  # Создаем таблицу сопряженности для зависимой переменной и независимой переменной
  contingency_table <- table(data[[indep_var]], data$end)
  
  # Критерий хи-квадрат
  chi_test <- chisq.test(contingency_table)
  
  # Точный критерий Фишера, используется, если какие-либо ожидаемые частоты < 5
  if(any(chi_test$expected < 5)) {
    fisher_test <- fisher.test(contingency_table)
    fisher_p_value <- fisher_test$p.value
  } else {
    fisher_p_value <- NA
  }
  
  # Вычисляем условные вероятности
  prop_table <- prop.table(contingency_table, margin = 1)
  
  # Печатаем результаты
  cat("\nКритерий хи-квадрат для", indep_var, ":\n")
  print(chi_test)
  
  if(!is.na(fisher_p_value)) {
    cat("\nТочный критерий Фишера для", indep_var, ":\n")
    print(fisher_p_value)
  }
  
  cat("\nУсловные вероятности для", indep_var, ":\n")
  print(prop_table)
  
  # Значимости отличия
  cat("\nP-значения:\n")
  cat("Хи-квадрат: ", chi_test$p.value, "\n")
  if(!is.na(fisher_p_value)) {
    cat("Фишера: ", fisher_p_value, "\n")
  }
}

# Выполнение тестов для каждой переменной
perform_tests(data, "curwor")
perform_tests(data, "st")
perform_tests(data, "se")

#Вычислить  коэффициенты неопределенности для зависимой переменной end и для каждой из независимых  категориальных переменных curwor, se, st.

# Загрузка необходимых библиотек
library(vcd)  # Для коэффициента V Крамера

# Функция для расчета коэффициента неопределенности между двумя переменными
calc_uncertainty_coefficient <- function(data, var1, var2) {
  # Создание таблицы сопряженности
  contingency_table <- table(data[[var1]], data[[var2]])
  
  # Вычисление теста хи-квадрат
  chi_sq_test <- chisq.test(contingency_table)
  
  # Вычисление V Крамера
  cramers_v <- sqrt(chi_sq_test$statistic / (nrow(data) * (min(dim(contingency_table)) - 1)))
  
  return(cramers_v)
}

# Вычисление коэффициента неопределенности для каждой независимой переменной
cramers_v_curwor <- calc_uncertainty_coefficient(data, "end", "curwor")
cramers_v_se <- calc_uncertainty_coefficient(data, "end", "se")
cramers_v_st <- calc_uncertainty_coefficient(data, "end", "st")

# Вывод результатов
cat("Коэффициент V Крамера для end и curwor:", cramers_v_curwor, "\n")
cat("Коэффициент V Крамера для end и se:", cramers_v_se, "\n")
cat("Коэффициент V Крамера для end и st:", cramers_v_st, "\n")

# Функция для расчета коэффициентов неопределенности для списка переменных
calc_uncertainty_coefficients <- function(data, dependent_var, independent_vars) {
  sapply(independent_vars, function(independent_var) {
    contingency_table <- table(data[[dependent_var]], data[[independent_var]])
    chi_sq_test <- chisq.test(contingency_table)
    cramers_v <- sqrt(chi_sq_test$statistic / (nrow(data) * (min(dim(contingency_table)) - 1)))
    cramers_v
  })
}

# Список независимых переменных
independent_vars <- c("curwor", "se", "st")

# Вычисление коэффициентов Крамера
cramers_v_values <- calc_uncertainty_coefficients(data, "end", independent_vars)

# Названия для вывода результатов
names(cramers_v_values) <- independent_vars

# Вывод результатов
print(cramers_v_values)

# Проверка гипотезы о равенстве дисперсий двух выборок
data$se <- as.factor(data$se)

# Получение уникальных значений для признака se, исключая NA
unique_se_values <- unique(data$se[!is.na(data$se)])

# Список для хранения данных по группам и дисперсий
grouped_se <- list()
grouped_variance_se <- numeric(length(unique_se_values))

# Преобразование asi4_dr к числовому типу
# Замена запятых на точки в строках
data$asi4_dr <- gsub(",", ".", data$asi4_dr)

# Преобразование в числовой тип
data$asi4_dr <- as.numeric(data$asi4_dr)
head(data)
# Удаление NA из списка уникальных значений se
unique_se_values <- na.omit(unique(data$se))

# Цикл по всем уникальным значениям, исключая NA
for (i in seq_along(unique_se_values)) {
  value <- unique_se_values[i]
  
  # Выборка данных по текущему значению se, исключая строки с NA в asi4_dr
  group_data <- data[data$se == value & !is.na(data$asi4_dr), "asi4_dr", drop = FALSE]
  
  # Вычисление дисперсии для текущей группы, учитывая NA значения
  grouped_variance_se[i] <- var(group_data$asi4_dr, na.rm = TRUE)
  
  # Сохранение данных по группе в список
  grouped_se[[as.character(value)]] <- group_data
  
  # Вывод дисперсии для текущей группы
  cat(sprintf("Дисперсия для группы %s = %.5f\n", value, grouped_variance_se[i]))
}

# Проверка равенства дисперсий и средних для первых двух доступных групп
if (length(grouped_se) >= 2) {
  # Критерий Фишера для проверки равенства дисперсий
  f_test_result <- var.test(grouped_se[[1]]$asi4_dr, grouped_se[[2]]$asi4_dr)
  cat(sprintf("P-значение критерия Фишера для групп %s и %s: %.5f\n",
              names(grouped_se)[1], names(grouped_se)[2], f_test_result$p.value))
  
  # Критерий Стьюдента для проверки равенства средних
  t_test_result <- t.test(grouped_se[[1]]$asi4_dr, grouped_se[[2]]$asi4_dr, 
                          var.equal = f_test_result$p.value > 0.05)
  cat(sprintf("P-значение критерия Стьюдента для групп %s и %s: %.5f\n",
              names(grouped_se)[1], names(grouped_se)[2], t_test_result$p.value))
}

# Загрузка необходимых библиотек
library(car)   # для теста Ливиня
library(stats) # для ANOVA и теста Бартлетта
library(multcomp) # для множественных сравнений

# Переменная ответа asi4_dr и группирующая переменная educat находятся в датафрейме data
# Преобразование educat в фактор
data$educat <- as.factor(data$educat)

# Проверка гипотезы о равенстве дисперсий
# Тест Ливиня
levene_test <- leveneTest(data$asi4_dr, data$educat, center = median)
print(levene_test)

# Тест Бартлетта
bartlett_test <- bartlett.test(data$asi4_dr, data$educat)
print(bartlett_test)

# Однофакторный дисперсионный анализ
anova_result <- aov(asi4_dr ~ educat, data = data)
summary(anova_result)

# Множественные сравнения
# Используем поправки: Тьюки, Бонферрони, Холма
tukey_test <- TukeyHSD(anova_result)
print(tukey_test)

# Множественные сравнения с поправкой Бонферрони
bonferroni_test <- glht(anova_result, linfct = mcp(educat = "Tukey"))
summary(bonferroni_test, test = adjusted("bonferroni"))

# Множественные сравнения с поправкой Холма
holm_test <- glht(anova_result, linfct = mcp(educat = "Tukey"))
summary(holm_test, test = adjusted("holm"))

# Выполнение критерия Вилкоксона для сравнения двух выборок
# Фильтрация данных для двух выбранных групп по переменной educat
group1_data <- data[data$educat == 1, "asi4_dr", drop = FALSE]
group2_data <- data[data$educat == 2, "asi4_dr", drop = FALSE]
vector_gr_1 <- c(group1_data)
vector_gr_2 <- c(group2_data)

wilcox_test <- wilcox.test(vector_gr_1$asi4_dr, vector_gr_2$asi4_dr, alternative = "two.sided")

# Вывод результатов теста
print(wilcox_test)

#Индекс депрессии в разные моменты времени. Проверка однородности зависимых выборок
#Проверка однородности изменений во времени по критерию стьюдента для зависимых выборок и по ранговому критерию вилкоксона
data <- read_delim("~/Desktop/dataNF.csv", delim = ";")
head(data)

library(ez)

# Преобразование данных
data$SEX.1 <- as.factor(data$SEX.1)
data$PRCOD.1 <- as.factor(data$PRCOD.1)
# Замена NA средними значениями по каждому столбцу
features <- c("BDI.1", "BDI.4", "BDI.5", "BDI.7")
data <- data %>%
  mutate(across(all_of(features), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Просмотр измененных данных
head(data)

# Устанавливаем новые имена для колонок
names(data) <- gsub("BDI\\.", "BDI_", names(data))
names(data) <- gsub("PRCOD\\.", "PRCOD_", names(data))
names(data) <- gsub("SEX\\.", "SEX_", names(data))

# Просмотр измененных данных
head(data)

# Находим медиану для переменной BDI_1
med1 <- median(data$BDI_1, na.rm = TRUE)

# Преобразование переменных BDI.1 и BDI.4 в дихотомические по медиане med1
data$BDI_1.binary <- ifelse(data$BDI_1 > med1, 1, 0)
data$BDI_4.binary <- ifelse(data$BDI_4 > med1, 1, 0)

# Вывод первых строк обновленного датафрейма для проверки
head(data$BDI_1.binary)
head(data$BDI_4.binary)

# Создание гистограммы для BDI.1.binary
ggplot(data, aes(x = factor(BDI_1.binary))) +
  geom_bar(fill = "blue", alpha = 0.7) +
  labs(title = "Гистограмма BDI_1 по медиане", x = "BDI_1 (Больше медианы = 1)", y = "Количество") +
  theme_minimal()

# Создание гистограммы для BDI_4.binary
ggplot(data, aes(x = factor(BDI_4.binary))) +
  geom_bar(fill = "red", alpha = 0.7) +
  labs(title = "Гистограмма BDI.4 по медиане", x = "BDI_4 (Больше медианы = 1)", y = "Количество") +
  theme_minimal()

# Для точек наблюдения BDI_1, BDI_7 проверить значимость изменений в динамике по критерию Мак-Немара и Кохрена
# Преобразование переменных BDI_1 и BDI_7 в дихотомические по медиане med1
data$BDI_1.binary <- ifelse(data$BDI_1 > med1, 1, 0)
data$BDI_7.binary <- ifelse(data$BDI_7 > med1, 1, 0)
# Выполнение теста Мак-Немара
mcnemar_test <- mcnemar.test(data$BDI_1.binary, data$BDI_7.binary)
print(mcnemar_test)
# тест Кохрена
library(DescTools)
cochran_test <- CochranQTest(cbind(data$BDI_1.binary, data$BDI_7.binary))
print(cochran_test)

t_test_results <- t.test(data$BDI_1, data$BDI_4, paired = TRUE)
print(t_test_results)
wilcox_test_results <- wilcox.test(data$BDI_1, data$BDI_4, paired = TRUE)
print(wilcox_test_results)

# Преобразование данных в длинный формат
long_data <- data %>%
  pivot_longer(
    cols = c(BDI_1, BDI_4, BDI_5, BDI_7),
    names_to = "Time",
    values_to = "BDI"
  )

# Преобразование времени в категориальный тип
long_data$Time <- as.factor(long_data$Time)

# Просмотр результатов
print(long_data)

# Преобразование идентификатора субъекта и времени в факторы
long_data$PRCOD_1 <- as.factor(long_data$PRCOD_1)
long_data$Time <- as.factor(long_data$Time)
long_data$SEX_1 <- as.factor(long_data$SEX_1)

# Выполнение ANOVA с повторными измерениями
anova_results <- ezANOVA(
  data = long_data,
  dv = .(BDI),             # Зависимая переменная
  wid = .(PRCOD_1),        # Уникальный идентификатор субъекта
  within = .(Time, SEX_1), # Внутригрупповые факторы
  type = 3,
  detailed = TRUE         # Возвращает более подробные результаты
)

# Вывод результатов
print(anova_results)

a_result <- ezANOVA(data = long_data,
                         dv = .(BDI), 
                         wid = .(SEX_1), 
                         within = .(Time, PRCOD_1),
                         type = 3,
                         detailed = TRUE)
print(a_result)

# Линейный график для переменной по времени и полу
ggplot(long_data, aes(x = Time, y = BDI, color = SEX_1, group = SEX_1)) +
  geom_line() +
  geom_point() +
  labs(title = "BDI by Time and Sex", x = "Time", y = "BDI", color = "Sex") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red"))

# Линейный график для переменной по времени и PRCOD_1
ggplot(long_data, aes(x = Time, y = BDI, color = PRCOD_1, group = PRCOD_1)) +
  geom_line() +
  geom_point() +
  labs(title = "BDI by Time and PRCOD_1", x = "Time", y = "BDI", color = "PRCOD_1") +
  theme_minimal() +
  scale_color_manual(values = c("green", "orange", "blue", "red"))

#########################
library(nlme)   # Load necessary library for mixed-effects models

# Create a unique identifier assuming each row is a unique observation per time point
data$ID <- seq_len(nrow(data))

# Convert factors as necessary
data$PRCOD_1 <- as.factor(data$PRCOD_1)
data$SEX_1 <- as.factor(data$SEX_1)
#data$Time <- factor(c("BDI_1", "BDI_4", "BDI_7", "BDI_5"))
#head(data)

# Reshape data from wide to long format
long_data <- reshape(data, varying = list(c("BDI_1", "BDI_4", "BDI_7", "BDI_5")),
                     times = c(1, 4, 7, 5),
                     v.names = "BDI",
                     timevar = "Time",
                     idvar = c("ID", "PRCOD_1", "SEX_1"),
                     direction = "long")

# Fit the repeated measures ANOVA model
model <- lme(BDI ~ Time * SEX_1, random = ~ 1 | ID/Time, data = long_data, method = "REML")

# Summarize the results
summary(model)
Anova(model, type="III")

# Fit the repeated measures ANOVA model
model <- lme(BDI ~ Time * PRCOD_1, random = ~ 1 | ID/Time, data = long_data, method = "REML")

# Summarize the results
summary(model)
Anova(model, type="III")
