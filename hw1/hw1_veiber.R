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

#Моделирование равномерного распределения U(1, 3)
set.seed(123)  # для воспроизводимости
n <- 150
true_min <- 1
true_max <- 3
sample <- runif(n, min = true_min, max = true_max)

# Метод моментов
mean_sample <- mean(sample)
var_sample <- var(sample)
estimated_min <- 2 * mean_sample - sqrt(12 * var_sample)
estimated_max <- 2 * mean_sample + sqrt(12 * var_sample)

# Метод максимального правдоподобия
mle_min <- min(sample)
mle_max <- max(sample)

# Вывод результатов
cat("Метод моментов:\n")
cat(sprintf("Оценка min: %f\n", estimated_min))
cat(sprintf("Оценка max: %f\n\n", estimated_max))

cat("Метод максимального правдоподобия:\n")
cat(sprintf("Оценка min: %f\n", mle_min))
cat(sprintf("Оценка max: %f\n", mle_max))

# Построение гистограммы с плотностью распределения и проверка согласия
data <- sample

# Построение гистограммы
hist(data, breaks = 10, probability = TRUE, col = "skyblue",
     main = "Гистограмма и плотность равномерного распределения",
     xlab = "Значения", ylab = "Плотность")

# Добавление теоретической плотности равномерного распределения
curve(dunif(x, min = 1, max = 3), add = TRUE, col = "red", lwd = 2)

# Для критерия Хи-квадрат нам необходимо разбить данные на бины
# Выберем разумное количество бинов, например 10
bins = seq(true_min, true_max, length.out = 11)  # создаем бины

# Создаем теоретические вероятности для равномерного распределения
# Каждый бин имеет равную вероятность, так как распределение равномерное
expected_counts = rep(n / length(bins), length(bins) - 1)

# Получаем частоты в бинах для нашей выборки
observed_counts = hist(data, breaks = bins, plot = FALSE)$counts

# Применяем критерий хи-квадрат Пирсона
chi_squared_test = chisq.test(x = observed_counts, p = expected_counts, rescale.p = TRUE)

chi_squared_test

# Вычисление основных статистик
mean_val = mean(data)  # Среднее
variance_val = var(data)  # Дисперсия
sd_val = sd(data)  # Стандартное отклонение
sem_val = sd_val / sqrt(n)  # Ошибка среднего (стандартная ошибка среднего)

# Квартили
quantiles_val = quantile(data, probs = c(0.25, 0.5, 0.75))

# Асимметрия и эксцесс
# Подключаем библиотеку moments для доступа к функциям skewness и kurtosis
library(moments)

# Вычисляем асимметрию
skewness_val <- skewness(data)

# Вычисляем эксцесс, уменьшаем на 3 для получения эксцесса относительно нормального распределения
kurtosis_val <- kurtosis(data) - 3

cat(sprintf("Среднее: %f\n", mean_val))
cat(sprintf("Дисперсия: %f\n", variance_val))
cat(sprintf("Стандартное отклонение: %f\n", sd_val))
cat(sprintf("Ошибка среднего: %f\n", sem_val))
cat(sprintf("Квартили: %f\n", quantiles_val))
cat(sprintf("Асимметрия: %f\n", skewness_val))
cat(sprintf("Эксцесс: %f\n", kurtosis_val))
