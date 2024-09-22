library(daltoolbox)
load_library("daltoolbox")
load_library("ggplot2")
load_library("RColorBrewer")
load_library("dplyr")

# Carregar o arquivo CSV
dados <- read.csv("students_mental_health_survey.csv")

# Visualizar as primeiras linhas e estrutura dos dados
head(dados)
str(dados)

# Visualizar dados faltantes
missing_data <- colSums(is.na(dados))
print(missing_data)

# Visualização de distribuições das variáveis numéricas
numeric_vars <- dados %>% select_if(is.numeric)
print(numeric_vars)

#setttando cor e font
colors <- brewer.pal(4, 'Set1')
font <- theme(text = element_text(size=16))


#plotando box_plot da variaveis numericas
grf <- plot_boxplot(numeric_vars, colors="white") + font
plot(grf)


# Filtrar apenas as colunas "Course" e "CGPA"
dados_course_cgpa <- dados %>%
  select(Course, CGPA)

# Grafico de barra do CGPA por curso
grf <- plot_bar(dados_course_cgpa, label_x = "Course", label_y = "CGPA", colors=colors[1]) + font
plot(grf)


# Calcular média do Depression_Score por Course, Sleep_Quality e Physical_Activity
depression_analysis <- dados %>%
  group_by(Course, Sleep_Quality, Physical_Activity) %>%
  summarise(Média_Depression_Score = mean(Depression_Score, na.rm = TRUE), .groups = 'drop')
print(depression_analysis)


#Filtrar colunas curso e media de depressao
dados_depr_corse <- depression_analysis %>%
  select(Course, Média_Depression_Score)


grf <- plot_bar(dados_depr_corse, label_x = "Course", label_y = "Média_Depression_Score", colors=colors[1]) + font
plot(grf)




# Fala pessoal, estou com dificuldades de criar esse grafico abaixo de dispersao, quando ploto ele aprece muitos
# poucos pontos no grafico, queria usar ele para tentar achar algum padrao de aumento de depression_score a medida que diminua
# a atividade fisica e qualidade de sono, sabe? Tambem nao sei se eh o grafico adequado para isso!!


# Usar as variáveis numéricas
grf <- plot_scatter(dados |> select(x = Stress_Level, value = Sleep_Quality, variable = Depression_Score), 
                    colors=colors[1:3]) + font
plot(grf)





