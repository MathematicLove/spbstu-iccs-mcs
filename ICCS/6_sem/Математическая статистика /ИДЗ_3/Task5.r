# Загрузка необходимых пакетов
if (!require(moments)) install.packages("moments")
library(moments)

# Данные из Task1.r (распределение Пуассона)
x1 <- c(0,0,2,1,0,0,0,0,0,1,0,1,0,0,0,2,0,2,1,0,1,1,0,1,1,3,0,0,0,0,0,1,0,0,0,0,4,1,5,2,0,0,2,0,0,1,1,0,0,1)
n1 <- length(x1)

# Функция для расчета вероятностей Пуассона
prob.pois <- function(lambda) {
    p <- dpois(0:5, lambda)
    # Объединяем последние интервалы, если нужно
    p[4] <- sum(p[4:6])
    return(p[1:4])
}

# Функция для расчета статистики хи-квадрат для Пуассона
csq.stat.pois <- function(lambda) {
    p <- prob.pois(lambda)
    obs <- c(sum(x1 == 0), sum(x1 == 1), sum(x1 == 2), sum(x1 >= 3))
    exp <- p * n1
    
    # Вывод промежуточных значений для отладки
    if(lambda == 0.6) {  # Проверяем для исходного значения
        cat("\nПроверка для λ = 0.6 (исходное значение):\n")
        cat("Наблюдаемые частоты:", obs, "\n")
        cat("Ожидаемые вероятности:", round(p, 4), "\n")
        cat("Ожидаемые частоты:", round(exp, 4), "\n")
        cat("Вклады в хи-квадрат:", round((obs - exp)^2 / exp, 4), "\n")
    }
    
    return(sum((obs - exp)^2 / exp))
}

# Минимизация для Пуассона с разными начальными точками
result.pois1 <- optimize(csq.stat.pois, interval = c(0.1, 5))
result.pois2 <- optimize(csq.stat.pois, interval = c(0.5, 1.5))  # Более узкий интервал
result.pois3 <- optimize(csq.stat.pois, interval = c(0.4, 0.8))  # Еще более узкий интервал

# Выбираем лучший результат
results.pois <- list(result.pois1, result.pois2, result.pois3)
best.pois <- results.pois[[which.min(sapply(results.pois, function(x) x$objective))]]

lambda_hat_pois <- best.pois$minimum
chi_square_pois <- best.pois$objective

# Проверяем значение в точке минимума
cat("\nПроверка для λ =", round(lambda_hat_pois, 4), "(оценка):\n")
p <- prob.pois(lambda_hat_pois)
obs <- c(sum(x1 == 0), sum(x1 == 1), sum(x1 == 2), sum(x1 >= 3))
exp <- p * n1
cat("Наблюдаемые частоты:", obs, "\n")
cat("Ожидаемые вероятности:", round(p, 4), "\n")
cat("Ожидаемые частоты:", round(exp, 4), "\n")
cat("Вклады в хи-квадрат:", round((obs - exp)^2 / exp, 4), "\n")

# Расчет p-value для Пуассона
p_value_pois <- pchisq(chi_square_pois, df = 2, lower.tail = FALSE)

# Данные из Task2.r (экспоненциальное распределение)
x2 <- c(10.34, 2.18, 8.80, 2.28, 1.95, 0.85, 3.73, 10.26, 5.01, 0.70,
        2.38, 0.25, 0.45, 0.31, 1.73, 2.67, 1.00, 1.59, 14.28, 2.14,
        1.85, 0.67, 2.70, 2.07, 5.31, 6.37, 3.24, 3.27, 1.31, 2.75,
        6.06, 1.05, 0.86, 2.43, 0.03, 3.70, 0.11, 1.06, 6.28, 0.55,
        9.07, 6.52, 0.94, 2.61, 0.89, 1.67, 0.24, 1.68, 3.34, 1.38)
n2 <- length(x2)
h <- 1.20
bins2 <- seq(0, ceiling(max(x2)/h)*h, by = h)

# Функция для расчета вероятностей экспоненциального распределения
prob.exp <- function(lambda) {
    diff(pexp(bins2, rate = lambda))
}

# Функция для расчета статистики хи-квадрат для экспоненциального распределения
csq.stat.exp <- function(lambda) {
    p <- prob.exp(lambda)
    obs <- hist(x2, breaks = bins2, plot = FALSE)$counts
    exp <- p * n2
    
    # Объединяем интервалы с малым количеством наблюдений
    while(any(exp < 5)) {
        i <- which(exp < 5)[1]
        if(i == 1) {
            exp[2] <- exp[2] + exp[1]
            obs[2] <- obs[2] + obs[1]
            exp <- exp[-1]
            obs <- obs[-1]
        } else {
            exp[i-1] <- exp[i-1] + exp[i]
            obs[i-1] <- obs[i-1] + obs[i]
            exp <- exp[-i]
            obs <- obs[-i]
        }
    }
    
    # Вывод промежуточных значений для отладки
    if(abs(lambda - 0.2) < 0.001) {  # Проверяем для исходного значения
        cat("\nПроверка для λ = 0.2 (исходное значение):\n")
        cat("Наблюдаемые частоты:", obs, "\n")
        cat("Ожидаемые вероятности:", round(p, 4), "\n")
        cat("Ожидаемые частоты:", round(exp, 4), "\n")
        cat("Вклады в хи-квадрат:", round((obs - exp)^2 / exp, 4), "\n")
    }
    
    return(sum((obs - exp)^2 / exp))
}

# Минимизация для экспоненциального распределения с разными начальными точками
result.exp1 <- optimize(csq.stat.exp, interval = c(0.01, 1))
result.exp2 <- optimize(csq.stat.exp, interval = c(0.1, 0.4))  # Более узкий интервал
result.exp3 <- optimize(csq.stat.exp, interval = c(0.15, 0.25))  # Еще более узкий интервал

# Выбираем лучший результат
results.exp <- list(result.exp1, result.exp2, result.exp3)
best.exp <- results.exp[[which.min(sapply(results.exp, function(x) x$objective))]]

lambda_hat_exp <- best.exp$minimum
chi_square_exp <- best.exp$objective

# Проверяем значение в точке минимума
cat("\nПроверка для λ =", round(lambda_hat_exp, 4), "(оценка):\n")
p <- prob.exp(lambda_hat_exp)
obs <- hist(x2, breaks = bins2, plot = FALSE)$counts
exp <- p * n2
# Объединяем интервалы с малым количеством наблюдений
while(any(exp < 5)) {
    i <- which(exp < 5)[1]
    if(i == 1) {
        exp[2] <- exp[2] + exp[1]
        obs[2] <- obs[2] + obs[1]
        exp <- exp[-1]
        obs <- obs[-1]
    } else {
        exp[i-1] <- exp[i-1] + exp[i]
        obs[i-1] <- obs[i-1] + obs[i]
        exp <- exp[-i]
        obs <- obs[-i]
    }
}
cat("Наблюдаемые частоты:", obs, "\n")
cat("Ожидаемые вероятности:", round(p, 4), "\n")
cat("Ожидаемые частоты:", round(exp, 4), "\n")
cat("Вклады в хи-квадрат:", round((obs - exp)^2 / exp, 4), "\n")

# Расчет p-value для экспоненциального распределения
p_value_exp <- pchisq(chi_square_exp, df = length(obs) - 2, lower.tail = FALSE)

# Вывод результатов
cat("\nРезультаты для распределения Пуассона (Task1):\n")
cat("Оценка λ:", lambda_hat_pois, "\n")
cat("Статистика хи-квадрат:", chi_square_pois, "\n")
cat("p-value:", p_value_pois, "\n")

cat("\nРезультаты для экспоненциального распределения (Task2):\n")
cat("Оценка λ:", lambda_hat_exp, "\n")
cat("Статистика хи-квадрат:", chi_square_exp, "\n")
cat("p-value:", p_value_exp, "\n") 