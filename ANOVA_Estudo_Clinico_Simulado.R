
# Projeto: ANOVA em Estudo Clínico Simulado
# Autor: Jean Mendes
# Objetivo: Comparar 3 tratamentos fictícios usando ANOVA

# Pacotes necessários
install.packages(c("ggplot2", "dplyr", "car"))
library(ggplot2)
library(dplyr)
library(car)

# Simulando dados
set.seed(123)
n <- 30  # número de pacientes por grupo

grupo <- rep(c("Tratamento_A", "Tratamento_B", "Tratamento_C"), each = n)
efeito_tratamento <- c(10, 15, 20)  # média de resposta por grupo
resposta <- c(rnorm(n, mean = efeito_tratamento[1], sd = 5),
              rnorm(n, mean = efeito_tratamento[2], sd = 5),
              rnorm(n, mean = efeito_tratamento[3], sd = 5))

dados <- data.frame(Grupo = factor(grupo), Resposta = resposta)

# Análise exploratória
ggplot(dados, aes(x = Grupo, y = Resposta, fill = Grupo)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Resposta por Grupo de Tratamento", y = "Resposta", x = "Grupo")

# Verificando pressupostos da ANOVA
modelo <- aov(Resposta ~ Grupo, data = dados)
par(mfrow = c(2, 2))
plot(modelo)

# Teste de homocedasticidade
leveneTest(Resposta ~ Grupo, data = dados)

# ANOVA
resumo_anova <- summary(modelo)
print(resumo_anova)

# Pós-teste (Tukey HSD)
tukey <- TukeyHSD(modelo)
print(tukey)
