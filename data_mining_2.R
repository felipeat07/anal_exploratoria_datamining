# Carregar a biblioteca DAL
source("https://raw.githubusercontent.com/cefet-rj-dal/daltoolbox/main/jupyter.R")
load_library("daltoolbox")

# Carregar os dados CSV
dados <- read.csv("students_mental_health_survey.csv", na.strings = "")

# Recode a variável 'Depression_Score'
dados <- dados %>%
  mutate(Depression_Score = recode(Depression_Score, 
                                   "0" = "Baixissimo",
                                   "1" = "Muito Baixo", 
                                   "2" = "Baixo", 
                                   "3" = "Médio", 
                                   "4" = "Alto", 
                                   "5" = "Muito Alto"))

dados$Depression_Score <- as.factor(dados$Depression_Score)

# Preencher dados faltantes
dados$CGPA[is.na(dados$CGPA)] <- mean(dados$CGPA, na.rm = TRUE) # CGPA preenchido com a média
dados$Substance_Use[is.na(dados$Substance_Use)] <- names(which.max(table(dados$Substance_Use))) # Substance_Use preenchido com a moda

# Mapeamento dos valores de qualidade
dados <- dados %>%
  mutate(Sleep_Quality = recode(Sleep_Quality, "Good" = "3", "Average" = "2", "Poor" = "1"),
         Diet_Quality = recode(Diet_Quality, "Good" = "3", "Average" = "2", "Poor" = "1"),
         Physical_Activity = recode(Physical_Activity, "High" = "3", "Moderate" = "2", "Low" = "1"),
         Extracurricular_Involvement = recode(Extracurricular_Involvement, "High" = "3", "Moderate" = "2", "Low" = "1"),
         Social_Support = recode(Social_Support, "High" = "3", "Moderate" = "2", "Low" = "1"),
         Substance_Use = recode(Substance_Use, "Frequently" = "3", "Occasionally" = "2", "Never" = "1"),
         Counseling_Service_Use = recode(Counseling_Service_Use, "Frequently" = "3", "Occasionally" = "2", "Never" = "1"),
         Family_History  = recode(Family_History, "No" = "0", "Yes" = "1"),
         Chronic_Illness   = recode(Chronic_Illness, "No" = "0", "Yes" = "1"))

# Extraindo os níveis da variável dependente
slevels <- levels(dados$Depression_Score)

# Preparar o conjunto de dados para amostragem aleatória
set.seed(1)
sr <- sample_random()
sr <- train_test(sr, dados)

# Dividir os dados em conjuntos de treino e teste
dados_train <- sr$train
dados_test <- sr$test

# Treinamento do modelo de árvore de decisão
model <- cla_dtree("Depression_Score", slevels)
model <- fit(model, dados_train)

# Previsão no conjunto de treino
train_prediction <- predict(model, dados_train)

# Avaliação do modelo no conjunto de treino
dados_train_predictand <- adjust_class_label(dados_train[,"Depression_Score"])
train_eval <- evaluate(model, dados_train_predictand, train_prediction)

# Imprimir a avaliação no conjunto de treino de forma bonita
library(knitr)
library(kableExtra)

cat("\nAvaliação no conjunto de treino:\n")
train_eval_metrics <- data.frame(Métrica = names(train_eval$metrics), Valor = train_eval$metrics)
print(kable(train_eval_metrics, caption = "Métricas de Avaliação - Conjunto de Treino") %>%
        kable_styling(full_width = F))

# Previsão no conjunto de teste
test_prediction <- predict(model, dados_test)

# Avaliação do modelo no conjunto de teste
dados_test_predictand <- adjust_class_label(dados_test[,"Depression_Score"])
test_eval <- evaluate(model, dados_test_predictand, test_prediction)

# Imprimir a avaliação no conjunto de teste de forma bonita
cat("\nAvaliação no conjunto de teste:\n")
test_eval_metrics <- data.frame(Métrica = names(test_eval$metrics), Valor = test_eval$metrics)
print(kable(test_eval_metrics, caption = "Métricas de Avaliação - Conjunto de Teste") %>%
        kable_styling(full_width = F))

# Matriz de Confusão
library(caret)
library(gt)

# Certifique-se de que as previsões e os rótulos são vetores
test_prediction_vector <- as.factor(test_prediction) # Certificando-se de que as previsões são um fator
dados_test_predictand_vector <- as.factor(dados_test_predictand) # Certificando-se de que os rótulos verdadeiros são um fator

# Ajustar níveis se necessário
dados_test_predictand_vector <- factor(dados_test_predictand_vector, levels = levels(test_prediction_vector))

# Criar a matriz de confusão para o conjunto de teste
confusion_matrix <- confusionMatrix(test_prediction_vector, dados_test_predictand_vector)

# Imprimir a matriz de confusão de forma bonita
cat("\nMatriz de Confusão:\n")
confusion_table <- as.data.frame(confusion_matrix$table)

confusion_table %>%
  gt() %>%
  tab_header(title = "Matriz de Confusão") %>%
  cols_label(
    Prediction = "Previsão",
    Reference = "Referência",
    0 = "Classe 0",
    1 = "Classe 1"
  ) %>%
  fmt_number(
    columns = everything(),
    decimals = 0
  ) %>%
  tab_options(
    table.font.size = 14,
    table.width = 600,
    column_labels.background.color = "#f0f0f0",
    table.border.color = "black"
  )

# Imprimir as métricas da avaliação
cat("\nMétricas Gerais do Modelo:\n")
print(confusion_matrix$overall)
