## Pacotes ####
install.packages("ggplot2")
install.packages("hrbrthemes")
install.packages("dplyr")
install.packages("corrplot")
install.packages("ggthemes")
install.packages("gridExtra")
install.packages("MASS")
install.packages("ResourceSelection")
install.packages("epiDisplay")
install.packages("caret")
install.packages("effects")
install.packages("finalfit")

## Bibliotecas ####
library("ggplot2")
library("hrbrthemes")
library("ggthemes")
library("dplyr")
library("corrplot")
library("GGally")
library("gridExtra")
library("MASS")
library("ResourceSelection")
library("epiDisplay")
library("caret")
library("effects")
library("car")
library("finalfit")
## Leitura e estruturação do data frame ####

# Caminho da base de dados
caminho = "c:\\"

# Importa a base para o R
base <- read.csv2(caminho, encoding = "UTF-8", stringsAsFactors = TRUE)

# Retira a string Home_ dos nomes das colunas do df
colnames(base) <- sub("Home_", "", colnames(base))

# Ajusta a escala da variável Cmp_percent_Passes
base <- base %>% 
  mutate(Cmp_percent_Passes = (if_else(Cmp_percent_Passes < 100, Cmp_percent_Passes * 10, Cmp_percent_Passes))/10)

# Renomeia a coluna Away
names(base)[names(base) == "Away"] <- "Home_Away"

## Análise exploratória ####
# Verificando se a base tem valores ausentes ####
base = base[,c("Win", "Home_Away", "PKatt", "Sh", "SoT", "CrdR", "Int", "Cmp_Passes")]
na_count <- colSums(is.na(base))
ggplot(data = data.frame(coluna = names(na_count), count = na_count), aes(x = coluna, y = count)) +
  geom_bar(stat = "identity", fill = "#3ea3af", color = "black") +
  labs(title = "Contagem de NAs por Coluna", x = "Variáveis", y = "Contagem de NAs") + ylim(-0.5,0.5) + theme_minimal()

# Visão geral dos dados ####
summary(base) # Estatísticas da base
round(sqrt(diag(var(base))),2) # Desvio padrão da base
freq.home_away <- base %>% 
  group_by(Home_Away) %>%
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n) * 100) %>%
  ungroup()

correlacao_graph <- ggpairs(base)

# Correlação entre as variáveis explicativas ####
M <- cor(base[-8])
corrplot(M, method = "circle")

# Desbalanceamento entre as variáveis numéricas ####
t_dados = stack(base[,c('PKatt','Sh','SoT','CrdR','Int','Cmp_Passes')])
names(t_dados) = c('Valores','Variáveis')

ggplot(t_dados,aes(x=Variáveis,y=Valores, color = Variáveis)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Comparação das escalas das variáveis presentes no conjunto de dados") +
  xlab("Variáveis") +
  ylab("Valores")

base[,7] = (base[,7]-mean(base$Cmp_Passes))/sqrt(var(base$Cmp_Passes))
base[,3] = (base[,3]-mean(base$Sh))/sqrt(var(base$Sh))
base[,6] = (base[,6]-mean(base$Int))/sqrt(var(base$Int))
base[,4] = (base[,4]-mean(base$SoT))/sqrt(var(base$SoT))

t_dados = stack(base[,c('PKatt','Sh','SoT','CrdR','Int','Cmp_Passes')])
names(t_dados) = c('Valores','Variáveis')

ggplot(t_dados,aes(x=Variáveis,y=Valores, color = Variáveis)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Comparação das escalas das variáveis presentes no conjunto de dados") +
  xlab("Variáveis") +
  ylab("Valores")

## Funcoes ####
# Função pra criar frequência ####
func_freq <- function(data, var){
  data %>% 
    group_by({{var}}, Win) %>%
    summarise(Frequencia = n()) %>%
    group_by({{var}}) %>%
    mutate(Percentual = Frequencia / sum(Frequencia) * 100)
}

# Função para gráfico de proporção ####
func_plot_proporcao <- function(freq, var, titulo, nome_var){
  ggplot({{freq}}, aes(x = factor({{var}}), y = freq, fill = factor(Win))) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("#ff1d44", "#3ea3af"), 
                      labels = c("Derrota", "Vitória")) +
    labs(title = titulo,
         x = nome_var,
         y = "Percentual",
         fill = "Resultado") +
    theme_minimal()
}

## Visualização gráfica das variáveis ####
# Mandante vs Visitante ####
freq.Mandante <- base %>% func_freq(., Home_Away)
plot.Mandante <- ggplot(freq.Mandante, aes(x = factor(Win), y = Percentual, fill = factor(Home_Away))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#ff1d44", "#3ea3af"), 
                    labels = c("Visitante", "Mandante")) +
  labs(title = "Proporção de vtórias e derrotas por Local do Jogo",
       x = "Resultado",
       y = "Percentual de partidas",
       fill = "Local do jogo") +
  scale_x_discrete(labels = c("Derrota", "Vitória")) +
  theme_minimal()

# Chutes ####
#freq.sh <- base_n_padronizada %>% func_freq(., Sh)
#plot.Sh <- ggplot(data = freq.sh, aes(x = factor(Sh), y = freq)) +
#  geom_point(size = 5, aes(colour = factor(Win))) +
#  ggtitle(label = 'Relação entre chutes e resultado da partida') +
#  theme(plot.title = element_text(hjust = 0.5)) + 
#  theme_stata() +
#  xlab('Nº de chutes') +
#  ylab('Percentual de resultados') + 
#  labs(colour = "Resultado") + 
#  scale_color_manual(values = c("0" = "#ff1d44", "1" = "#3ea3af"),
#                     labels = c("0" = "Derrota", "1" = "Vitória"))

freq.sh <- base %>%
  group_by(Sh, Win) %>%
  summarise(Frequencia = n()) %>%
  group_by(Sh) %>%
  mutate(Percentual = Frequencia / sum(Frequencia) * 100)

plot.Sh <- ggplot(data = freq.sh, aes(x = factor(Sh), y = Percentual)) +
  geom_point(size = 5, aes(colour = factor(Win))) +
  ggtitle(label = 'Relação entre chutes e resultado da partida') +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme_stata() +
  xlab('Nº de chutes') +
  ylab('Percentual de resultados') + 
  labs(colour = "Resultado") + 
  scale_color_manual(values = c("0" = "#ff1d44", "1" = "#3ea3af"),
                     labels = c("0" = "Derrota", "1" = "Vitória"))

# Chutes no gol ####
#ordem_chutes <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10+")
#freq.SoT <- base_n_padronizada %>% 
#  mutate(SoT_agrupado = if_else(SoT > 9, "10+", as.factor(SoT))) %>%
#  func_freq(., SoT_agrupado) %>%
#  mutate(SoT_agrupado = factor(SoT_agrupado, 
#                               levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10+"),
#                               ordered = TRUE))

#plot.SoT <- ggplot(data = freq.SoT, aes(x = SoT_agrupado, y = freq)) +
#  geom_point(size = 5, aes(colour = factor(Win))) +
#  ggtitle(label = 'Relação entre chutes no gol e resultado da partida') +
#  theme(plot.title = element_text(hjust = 0.5)) + 
#  theme_stata() +
#  xlab('Nº de chutes no gol') +
#  ylab('Percentual de resultados') + 
#  labs(colour = "Resultado") + 
#  scale_color_manual(values = c("0" = "#ff1d44", "1" = "#3ea3af"),
#                    labels = c("0" = "Derrota", "1" = "Vitória"))

freq.SoT <- base_n_padronizada %>%
  group_by(SoT, Win) %>%
  summarise(Frequencia = n()) %>%
  group_by(SoT) %>%
  mutate(Percentual = Frequencia / sum(Frequencia) * 100)

plot.SoT <- ggplot(data = freq.SoT, aes(x = factor(SoT), y = Percentual)) +
  geom_point(size = 5, aes(colour = factor(Win))) +
  ggtitle(label = 'Relação entre chutes no gol e resultado da partida') +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme_stata() +
  xlab('Nº de chutes') +
  ylab('Percentual de resultados') + 
  labs(colour = "Resultado") + 
  scale_color_manual(values = c("0" = "#ff1d44", "1" = "#3ea3af"),
                     labels = c("0" = "Derrota", "1" = "Vitória"))

plot.sh_sot <- grid.arrange(
  arrangeGrob(plot.Sh, plot.SoT, ncol = 2),
  ncol = 1
)
ggsave("plot_chutes.png", plot.sh_sot, width = 12, height = 6, units = "in", dpi = 300)
  
# Cartão vermelho ####
freq.Red_Card <-base_n_padronizada %>% func_freq(., CrdR)
plot.RedCard <- ggplot(data = freq.Red_Card, aes(x = factor(CrdR), y = Percentual)) +
  geom_point(size = 5, aes(colour = factor(Win))) +
  ggtitle(label = 'Relação entre cartões vermelhos e resultado da partida') +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme_stata() +
  xlab('Nº de cartões vermelhos') +
  ylab('Percentual de resultados') + 
  labs(colour = "Resultado") + 
  scale_color_manual(values = c("0" = "#ff1d44", "1" = "#3ea3af"),
                     labels = c("0" = "Derrota", "1" = "Vitória"))

plot.RedCard_bar <- ggplot(freq.Red_Card, aes(x = factor(Win), y = n, fill = factor(CrdR))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Resultado da Partida", 
       y = "Total de partidas", 
       fill = "Cartões vermelhos") +
  scale_fill_manual(values = c("0" = "#9fd9b3", "1" = "#3ea3af", "2" = "#ff1d44"),
                    labels = c("0", "1", "2")) +  # Aqui estão os rótulos
  scale_x_discrete(labels = c("Derrota", "Vitória")) +
  theme_minimal()
ggsave("plot_carao_vermelho.png", plot.RedCard_bar, width = 12, height = 6, units = "in", dpi = 300)


# Passes completos ####
freq.Passes <- base_n_padronizada %>% 
  mutate(Cmp_Passes = cut(Cmp_Passes, breaks = c(0, 150, 300, 450, 600, Inf), labels = c("0-150", "151-300", "301-450", "451-600", "600+"))) %>%
  func_freq(., Cmp_Passes)
plot.Passes <- ggplot(data = freq.Passes, aes(x = factor(Cmp_Passes), y = Percentual)) +
  geom_point(size = 5, aes(colour = factor(Win))) +
  ggtitle(label = 'Relação entre passes completos e resultado da partida') +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme_stata() +
  xlab('Nº de passes completos') +
  ylab('Percentual de resultados') + 
  labs(colour = "Resultado") + 
  scale_color_manual(values = c("0" = "#ff1d44", "1" = "#3ea3af"),
                     labels = c("0" = "Derrota", "1" = "Vitória"))

# Pênalti ####
freq.Penalti <- base_n_padronizada %>%
  group_by(PKatt, Win) %>%
  summarise(Frequencia = n()) %>%
  group_by(PKatt) %>%
  mutate(Percentual = Frequencia / sum(Frequencia) * 100)

plot.Penaltis <- ggplot(freq.Penalti, aes(x = factor(PKatt), y = Percentual, fill = factor(Win))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Quantidade de pênaltis na partida", 
       y = "Percentual entre vitórias e derrotas", 
       fill = "Resultado da partida") +
  scale_fill_manual(values = c("0" = "#ff1d44", "1" = "#3ea3af"),
                    labels = c("Derrota", "Vitória")) +  # Aqui estão os rótulos
  scale_x_discrete(labels = c("0", "1", "2")) +
  theme_minimal()
ggsave("plot_penaltis.png", plot.Penaltis, width = 12, height = 6, units = "in", dpi = 300)

# Interceptacoes ####
unique(base_n_padronizada$Int)
freq.Interceptacoes <- base_n_padronizada %>% 
  mutate(Int = cut(Int, breaks = c(-Inf, 5, 10, 15, 20, Inf), labels = c("0-5", "6-10", "11-15", "16-20", "21+"))) %>%
  func_freq(., Int)
plot.Interceptacoes <- ggplot(data = freq.Interceptacoes, aes(x = factor(Int), y = Percentual)) +
  geom_point(size = 5, aes(colour = factor(Win))) +
  ggtitle(label = 'Relação entre interceptações e resultado da partida') +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme_stata() +
  xlab('Nº de interceptações') +
  ylab('Percentual de resultados') + 
  labs(colour = "Resultado") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_color_manual(values = c("0" = "#ff1d44", "1" = "#3ea3af"),
                     labels = c("0" = "Derrota", "1" = "Vitória"))

plot.Int_Avancos <- grid.arrange(
  arrangeGrob(plot.Interceptacoes, plot.Avancos, ncol = 2),
  ncol = 1
)
ggsave("plot_Int_Avancos.png", plot.Int_Avancos, width = 12, height = 6, units = "in", dpi = 300)

## 20/11/2023 ##
## Modelagem ####
# Montando o modelo MANUALMENTE ####
modelo.logit_1 <- glm(Win~PKatt+Sh+SoT+CrdR+Int+Cmp_Passes+Home_Away, 
                family = binomial(link = 'logit'),
                data = base)

summary(modelo.logit_1)
resumo_1 = summary(modelo.logit_1)
# Acessar a log-likelihood
log_likelihood = resumo_1$deviance
# Acessar o número de parâmetros
k = length(coef(modelo.logit_1))
# Acessar o número de observações
n = nobs(modelo.logit_1)
# Calcular o BIC
bic_1 = -2 * log_likelihood + k * log(n)
bic_1

modelo.logit_2 <- glm(Win~PKatt+Sh+SoT+CrdR+Cmp_Passes+Home_Away, 
                      family = binomial(link = 'logit'),
                      data = base)
summary(modelo.logit_2)
resumo_2 = summary(modelo.logit_2)
round(resumo_3$coefficients,2)
# Acessar a log-likelihood
log_likelihood = resumo_2$deviance
# Acessar o número de parâmetros
k = length(coef(modelo.logit_2))
# Acessar o número de observações
n = nobs(modelo.logit_2)
# Calcular o BIC
bic_2 = -2 * log_likelihood + k * log(n)
bic_2


modelo.logit_3 <- glm(Win~PKatt+Sh+SoT+Cmp_Passes+Home_Away, 
                      family = binomial(link = 'logit'),
                      data = base)

summary(modelo.logit_3)
resumo_3 = summary(modelo.logit_3)
# Acessar a log-likelihood
log_likelihood = resumo_3$deviance
# Acessar o número de parâmetros
k = length(coef(modelo.logit_3))
# Acessar o número de observações
n = nobs(modelo.logit_3)
# Calcular o BIC
bic_3 = -2 * log_likelihood + k * log(n)
bic_3

# Teste de diferença de deviance ####
teste_deviance <- anova(modelo.logit_1, modelo.logit_2, test = "Chisq")
print(teste_deviance)

teste_deviance2 <- anova(modelo.logit_2, modelo.logit_3, test = "Chisq")
print(teste_deviance2)
# Montando o modelo via STEPWISE ####
modelo_inicial <- glm(Win ~ ., family = binomial, data = base)
modelo_stepwise <- stepAIC(modelo_inicial, direction = "backward")
summary(modelo_stepwise)
# Escolha do modelo final ####
summary(mod_final) = modelo_stepwise
# Identificação de outliers/pontos influentes ####
influence.measures(mod_final)
max(cooks.distance(mod_final))
max(dffits(mod_final))
max(dfbetas(mod_final))
max(hatvalues(mod_final))
2*6/nrow(base)
sort(hatvalues(mod_final))

# Bondade de ajuste ####
hoslem.test(mod_final$fitted.values, mod_final$residuals, g = 10)
lroc(mod_final)
lroc(mod_final)$auc

# Previsão e matriz de confusão ####
# Faça previsões no conjunto de dados de treinamento
previsoes <- predict(mod_final, newdata = base, type = "response")

# Converta as probabilidades em classes (0 ou 1) usando um ponto de corte
ponto_corte <- 0.52  # Ponto de corte para classificação
classes_previstas <- ifelse(previsoes > ponto_corte, 1, 0)
# Crie a matriz de confusão
matriz_confusao <- confusionMatrix(factor(classes_previstas), factor(base$Win))

# Exiba a matriz de confusão
print(matriz_confusao)

#### 3 ####
#tabela observado vs estimado
#sensibilidade x especificidade 
#curva roic

# BONUS ####
plot(allEffects(mod_final)) #INTERESSANTE, mas tinhamos que fazer com os valores normais e não padronizados 
marginalModelPlots(mod_final) #INTERESSANTE


### Razão de chances ####
coeficientes = mod_final$coefficients
exp(coeficientes)

# A chance de um time mandante ganhar em relação a um time visitante é 124% 
# maior do que sendo um time visitante (exp{beta1})
exp(coeficientes[2]) /(1+exp(coeficientes[2]))

# A cada chute no gol há um aumento de 82% de chances de vitória de um time
exp(coeficientes[5])

# A cada cartão vermelho que um time recebe a chance de vitória é reduzida em 47%
exp(coeficientes[6])

# A cada pênalti que um time recebe a chance de vitória aumenta em 123%
exp(coeficientes[3])

modelo_resumido = summary(mod_final)
desvio_padrao = sqrt(modelo_resumido$coefficients[,2])

df_razao_chance = data.frame(exp(coeficientes)-1.96*desvio_padrao, exp(coeficientes)+1.96*desvio_padrao, exp(coeficientes))
colnames(df_razao_chance) <- c("IC_menor", "IC_maior", "or")

df_razao_chance$variavel <- rownames(df_razao_chance)
df_razao_chance <- df_razao_chance[-1,]

ggplot(df_razao_chance, aes(x = or, y = variavel)) +
  geom_errorbarh(aes(xmin = IC_menor, xmax = IC_maior), height = 0.2, size = 1) +
  geom_point(size = 2, color = "#3ea3af") +
  labs(title = "Intervalo de Confiança para Razões de Chances", y = "Variáveis explicativas") +
  xlab("Razão de Chances") +
  theme_minimal()

IC = data.frame(round(df_razao_chance[1], 2), round(df_razao_chance[2], 2))