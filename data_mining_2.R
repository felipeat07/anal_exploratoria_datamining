# Carregar os pacotes
library(rpart)
library(rpart.plot)
library(dplyr)
library(ggplot2)
library(caret)  # Para obter estatísticas do modelo

# Carregar o arquivo CSV (substitua o caminho do arquivo pelo correto se necessário)
dados <- read.csv("students_mental_health_survey.csv", na.strings = "")

# Converter variáveis categóricas em fatores
dados <- dados %>%
  mutate_if(is.character, as.factor)

# Remover observações com valores ausentes
dados <- na.omit(dados)

# Visualizar a distribuição original das classes (antes do balanceamento)
ggplot(dados, aes(x = as.factor(Depression_Score))) + 
  geom_bar(fill = "steelblue") + 
  labs(title = "Distribuição Original das Classes", x = "Depression Score", y = "Contagem") +
  theme_minimal()

# Balanceamento por undersampling (sem a variável Diet_Quality)
set.seed(123)  # Definir semente para reprodutibilidade
min_class_size <- min(table(dados$Depression_Score))  # Tamanho da menor classe

dados_balanceado <- dados %>%
  select(-Diet_Quality) %>%  # Remover a variável Diet_Quality
  group_by(Depression_Score) %>%
  sample_n(min_class_size) %>%
  ungroup()

# Visualizar a distribuição das classes após o balanceamento (com contagem)
ggplot(dados_balanceado, aes(x = as.factor(Depression_Score))) + 
  geom_bar(fill = "steelblue") + 
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +  # Mostrar contagem em cima das barras
  labs(title = "Distribuição das Classes Após Balanceamento (Undersampling)", x = "Depression Score", y = "Contagem") +
  theme_minimal()

# Dividir os dados em 70% para treino e 30% para teste
set.seed(123)
indice_treino <- sample(1:nrow(dados_balanceado), 0.7 * nrow(dados_balanceado))
dados_treino <- dados_balanceado[indice_treino, ]
dados_teste <- dados_balanceado[-indice_treino, ]

# Treinar o modelo de árvore de decisão para prever 'Depression_Score' (sem Diet_Quality) no conjunto de treinamento
modelo_arvore_treino <- rpart(Depression_Score ~ ., data = dados_treino, method = "class")

# Visualizar a árvore de decisão treinada com o conjunto de TREINAMENTO
rpart.plot(modelo_arvore_treino, type = 2, extra = 104, cex = 0.8, main = "Árvore de Decisão - Conjunto de Treinamento")

# Fazer previsões no conjunto de teste com o modelo treinado no conjunto de TREINAMENTO
previsoes <- predict(modelo_arvore_treino, newdata = dados_teste, type = "class")

# Gerar a matriz de confusão com base nas previsões do conjunto de teste
conf_matrix <- confusionMatrix(as.factor(previsoes), as.factor(dados_teste$Depression_Score))

# Exibir a matriz de confusão e estatísticas do modelo
print("Matriz de Confusão:")
print(conf_matrix$table)

# Acurácia e outras estatísticas
print(paste("Acurácia: ", round(conf_matrix$overall['Accuracy'] * 100, 2), "%"))
print(conf_matrix$byClass)  # Estatísticas por classe

