#Моделирование нелинейной модели
# Загрузка необходимых библиотек
library(nlstools)
library(ggplot2)
library(tidyverse)
library(readr)
library(corrplot)
library(tidyr)
library(GGally)
library(plotly)
library(reshape2)

# Установка начальных параметров
a <- 0.14
b <- 1
epsilon <- 0.33

# Генерация данных
set.seed(123)  # для воспроизводимости
x <- seq(0, 10, length.out = 100)
y_true <- exp(a * x + b)
y <- y_true + rnorm(length(x), mean = 0, sd = epsilon)

# Моделирование данных
nl_model <- nls(y ~ exp(a * x + b),
             start = list(a = 0.1, b = 0.9),
             algorithm = "port",
             control = nls.control(maxiter = 100))

# Вывод результатов моделирования
print(summary(nl_model))

# Визуализация результатов
ggplot(data = data.frame(x, y, y_fitted = predict(nl_model)), aes(x = x)) +
  geom_point(aes(y = y), color = 'blue') +
  geom_line(aes(y = y_fitted), color = 'red') +
  ggtitle("Нелинейная модель: y = exp(a*x + b)") +
  theme_minimal()

#Построение линейной модели и сравнение с нелинейной
# Моделирование линейной модели
lm_model <- lm(y ~ x)
# Вывод результатов моделирования
print(summary(lm_model))

# Визуализация результатов
data_frame <- data.frame(x, y, y_fitted_nl = predict(nl_model), y_fitted_lm = predict(lm_model))
ggplot(data = data_frame, aes(x = x)) +
  geom_point(aes(y = y), color = 'blue', alpha = 0.5) +
  geom_line(aes(y = y_fitted_nl), color = 'red') +
  geom_line(aes(y = y_fitted_lm), color = 'green', linetype = "dashed") +
  ggtitle("Сравнение моделей: Нелинейная (Красная) vs Линейная (Зелёная)") +
  theme_minimal()

#Работа с линейной моделью
# Загрузка необходимых библиотек
library(stats)

# lm_model уже определена как lm_model <- lm(y ~ x)

# Выполнение дисперсионного анализа
anova_result <- anova(lm_model)
print(anova_result)

# Вывод сводки модели, включая значимость коэффициентов
summary_result <- summary(lm_model)
print(summary_result)

# Проверка значимости прогноза (общей модели)
print("Проверка значимости прогноза (F-статистика):")
print(summary_result$fstatistic)

# Проверка значимости коэффициентов
print("Проверка значимости коэффициентов регрессии:")
print(coef(summary_result))

# Для сравнения можно вручную вычислить F-статистику и p-value
rss <- sum(residuals(lm_model)^2) # Сумма квадратов остатков
tss <- sum((y - mean(y))^2) # Общая сумма квадратов
df_res <- lm_model$df.residual # Число степеней свободы остатков
df_total <- length(y) - 1 # Общее число степеней свободы
f_statistic <- ((tss - rss) / (df_total - df_res)) / (rss / df_res) # F-статистика
p_value <- pf(f_statistic, df_total - df_res, df_res, lower.tail = FALSE) # P-value для F-статистики

cat(sprintf("Вручную вычисленная F-статистика: %f, P-value: %f\n", f_statistic, p_value))

# Загрузка данных
data <- read_delim("~/Downloads/addicts.csv", delim = ";")
head(data)

# Подготовка данных
my_data <- data[ , c("rubsex", "tlfba2", "asi3_alc", "sstati", "tlfbh2")]

# Преобразование из <chr> с запятой в <dbl>
# Преобразование всех колонок в data
for (i in names(my_data)) {
  if (is.character(my_data[[i]])) {
    my_data[[i]] <- gsub(" ", "", my_data[[i]])  # Удаляем пробелы
    my_data[[i]] <- gsub(",", ".", my_data[[i]])  # Заменяем запятые на точки
    my_data[[i]] <- as.numeric(my_data[[i]])     # Преобразование в числовой тип
  }
}

# Вычисление корреляционной матрицы
cor_matrix <- cor(my_data, use = "complete.obs")  # Использование только полных наблюдений
print("Корреляционная матрица:")
print(cor_matrix)

# Создание графика
ggplot(data, aes_string(x = my_data$tlfba2, y = my_data$asi3_alc)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", col = "blue") +  # Линейная модель
  labs(title = paste("Двумерная диаграмма для tlfba2 и asi3_alc"),
       x = my_data$tlfba2, y = my_data$asi3_alc) +
  theme_minimal()

# Построение модели множественной регрессии
my_model <- lm(tlfbh2 ~ rubsex + tlfba2 + asi3_alc + sstati, data = my_data)

# Вывод результатов модели
summary(my_model)
print(my_model)

library(ppcor)

# Функция расчета частного коэффициента корреляции и проверки его значимости
calculate_partial_correlation <- function(data, x, y, controls) {
  # Подготовка данных, удаление NA
  data <- data[, c(x, y, controls)]
  data <- na.omit(data)
  
  # Моделирование зависимой переменной
  formula_y <- reformulate(controls, response = y)
  model_y <- lm(formula_y, data = data)
  residuals_y <- residuals(model_y)
  
  # Моделирование независимой переменной
  formula_x <- reformulate(controls, response = x)
  model_x <- lm(formula_x, data = data)
  residuals_x <- residuals(model_x)
  
  # Расчет корреляции между остатками
  cor_test <- cor.test(residuals_x, residuals_y)
  return(list(correlation = cor_test$estimate, p_value = cor_test$p.value))
}

# Переменные для анализа
features <- c("rubsex", "tlfba2", "asi3_alc", "sstati") # Пример переменных
target <- "tlfbh2" # Зависимая переменная

# Расчет частных корреляций
partial_corrs <- list()
for (var in features) {
  controls <- setdiff(features, var)
  result <- calculate_partial_correlation(my_data, var, target, controls)
  partial_corrs[[var]] <- result
}

# Вывод результатов
for (var in names(partial_corrs)) {
  coef <- partial_corrs[[var]]$correlation
  p_value <- partial_corrs[[var]]$p_value
  cat(sprintf("Признак: %s,\t коэффициент корреляции: %.4f,\t p-value (значимость): %.4f\n", var, coef, p_value))
}
