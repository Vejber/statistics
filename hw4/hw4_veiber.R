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

library(base)

# Пропускаем NA значения
my_data <- na.omit(my_data)

# Выполнение PCA
pca_result <- prcomp(my_data, scale. = TRUE)  # Масштабирование данных для стандартизации

# Получение факторных нагрузок
loadings <- pca_result$rotation

# Создание DataFrame для факторных нагрузок
loadings_df <- as.data.frame(loadings)
names(loadings_df) <- paste0("PC", 1:ncol(loadings_df))

# Печать результатов
print(loadings_df)

# Просмотр долей объясненной дисперсии
print(summary(pca_result))

# Извлечение результатов PCA для первых двух главных компонент
scores <- as.data.frame(pca_result$x[, 1:2])

# Создание двумерной диаграммы первых двух компонент
ggplot(scores, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = my_data$tlfbh2), alpha = 0.7) +  # Цвет точек по зависимой переменной, если нужно
  labs(x = "Первый главный компонент (PC1)", y = "Второй главный компонент (PC2)",
       title = "Двумерная диаграмма первых двух факторов") +
  theme_minimal() +
  scale_color_gradient(low = "blue", high = "red")  # Изменение градиента цвета, если это необходимо


# Подгружаем необходимые библиотеки
library(MASS)
library(caret)  # Для разделения данных и оценки точности

# Загрузка данных
data <- read_delim("~/Downloads/addicts.csv", delim = ";")
head(data)

# Подготовка данных
my_data <- data[ , c("rubsex", "tlfba2", "asi3_alc", "sstati", "tlfbh2", "end")]
head(my_data)

# Преобразование из <chr> с запятой в <dbl>
# Преобразование всех колонок в data
for (i in names(my_data)) {
  if (is.character(my_data[[i]])) {
    my_data[[i]] <- gsub(" ", "", my_data[[i]])  # Удаляем пробелы
    my_data[[i]] <- gsub(",", ".", my_data[[i]])  # Заменяем запятые на точки
    my_data[[i]] <- as.numeric(my_data[[i]])     # Преобразование в числовой тип
  }
}


# Пропускаем NA значения
my_data <- na.omit(my_data)

target <- my_data$end
head(target)

# Разделение данных на тренировочные и тестовые
set.seed(81)  # Для воспроизводимости разделения
split <- createDataPartition(my_data$end, p = 0.7, list = FALSE)
train_data <- my_data[split, ]
test_data <- my_data[-split, ]

# Обучение дискриминантной модели
lda_model <- lda(end ~ ., data = train_data)

# Предсказание на тестовой выборке
predictions <- predict(lda_model, newdata = test_data)
y_pred <- predictions$class

# Точность
accuracy <- sum(test_data$end == y_pred) / nrow(test_data)
print(sprintf('Точность дискриминантной модели: %.3f', accuracy))

# Создание confusion matrix
conf_matrix <- confusionMatrix(as.factor(y_pred), as.factor(test_data$end))

# Получение таблицы сопряженности
conf_table <- as.table(conf_matrix$table)

# Построение тепловой карты матрицы классификации
ggplot(data = as.data.frame(conf_table), aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), vjust = 1.5, color = "black") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Матрица классификации", x = "Предсказанное", y = "Действительное") +
  theme_minimal()

print(conf_matrix)
##################################
# Вычисление дискриминантных значений
lda_values <- predict(lda_model, my_data)$x

# Расчет эвристического граничного значения
heuristic_threshold <- mean(lda_values[,1])  # Предполагаем, что речь идет о первой дискриминантной функции

# Расчет Байесовского граничного значения
priors <- lda_model$prior
means <- colMeans(lda_values)
scaling_coef <- lda_model$scaling[, 1]  # Предполагаем, что речь идет о первой дискриминантной функции
intercept <- -sum(means * scaling_coef)
bayesian_threshold <- (log(priors[2] / priors[1]) - intercept) / scaling_coef

print(paste("Эвристическое граничное значение:", heuristic_threshold))
print(paste("Байесовское граничное значение:", bayesian_threshold))


my_data$lda_values <- lda_values
features <- c("rubsex", "tlfba2", "asi3_alc", "sstati", "tlfbh2")
my_data_long <- my_data %>%
  pivot_longer(cols = features, names_to = "feature", values_to = "value")

# Создание ggplot графика с фасетами для каждой переменной из 'features'
p <- ggplot(my_data_long, aes(x = lda_values, y = value, color = factor(end))) +
  geom_point(alpha = 0.7) +
  facet_wrap(~ feature, scales = "free_y") +
  geom_vline(xintercept = heuristic_threshold, color = "green", linetype = "dashed", size = 1) +
  geom_vline(xintercept = bayesian_threshold, color = "blue", linetype = "solid", size = 1) +
  labs(color = "Классы") +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Значения дискриминантной функции", y = "Значение признака")

# Вывод графика
print(p)

library(rpart)
# Модель с информационным выигрышем (Используем критерий "entropy")
tree_gain <- rpart(factor(train_data$end) ~ ., data = train_data, method = "class", parms = list(split = "information"))

# Стандартная модель (Используем критерий "gini")
tree_standard <- rpart(factor(train_data$end) ~ ., data = train_data, method = "class", parms = list(split = "gini"))

# Предсказания для модели на основе информационного выигрыша
y_pred_gain <- predict(tree_gain, test_data, type = "class")

# Предсказания для стандартной модели
y_pred_standard <- predict(tree_standard, test_data, type = "class")

# Расчет точности для модели с информационным выигрышем
accuracy_gain <- confusionMatrix(y_pred_gain, factor(test_data$end))$overall['Accuracy']

# Расчет точности для стандартной модели
accuracy_standard <- confusionMatrix(y_pred_standard, factor(test_data$end))$overall['Accuracy']

# Вывод результатов
cat(sprintf("Точность модели с информационным выигрышем: %.3f\n", accuracy_gain))
cat(sprintf("Точность стандартной модели: %.3f\n", accuracy_standard))

library(gridExtra)

plot_confusion_matrix <- function(y_true, y_pred, title) {
  # Преобразуем y_pred и y_true в факторы для работы confusionMatrix
  conf_matrix <- confusionMatrix(as.factor(y_pred), as.factor(y_true))
  print(conf_matrix)
  
  # Преобразуем таблицу матрицы ошибок в dataframe для ggplot
  conf_matrix_df <- as.data.frame(conf_matrix$table)
  names(conf_matrix_df) <- c("Reference", "Prediction", "Freq")
  
  # Построение матрицы ошибок с помощью ggplot
  ggplot(conf_matrix_df, aes(x = Reference, y = Prediction, fill = Freq)) +
    geom_tile(colour = "white") +  # Здесь теперь корректно используется x и y
    geom_text(aes(label = Freq), vjust = 1.5, color = "black") +
    scale_fill_gradient(low = "white", high = "steelblue") +
    labs(title = title, x = "Предсказанное", y = "Действительное") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5))
}

# Создание графиков для каждой модели
p1 <- plot_confusion_matrix(test_data$end, y_pred_gain, "Модель дерево с информационным выигрышем")
p2 <- plot_confusion_matrix(test_data$end, y_pred_standard, "Модель дерево стандартное")


# Объединение графиков в один ряд
grid.arrange(p1, p2, ncol = 2)

library(rpart.plot)
# Визуализация дерева
rpart.plot(tree_gain, main="Модель дерево с информационным выигрышем", box.palette="RdBu", shadow.col="gray", border.col="black")
# Визуализация дерева
rpart.plot(tree_standard, main="Модель дерево стандартное", box.palette="RdBu", shadow.col="gray", border.col="black")
