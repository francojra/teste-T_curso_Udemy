
# Teste T - Dados ToothGrowth -------------------------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data do script: 06/05/22 -----------------------------------------------------------------------------------------------------------------
# Referência: Curso Udemy ------------------------------------------------------------------------------------------------------------------

# Pacote necessário ------------------------------------------------------------------------------------------------------------------------

library(tidyverse)

# Dados ------------------------------------------------------------------------------------------------------------------------------------

?ToothGrowth

dente <- ToothGrowth
head(dente)
str(dente)

# Análise da normalidade dos dados ---------------------------------------------------------------------------------------------------------

qplot(x = len, data = dente, binwidth = 3) # Histograma

### Teste Shapiro-Wilk de normalidade
### H0 = Distribuição normal e p > 0.05

shapiro.test(dente$len)

# Gráfico da diferença entre grupos --------------------------------------------------------------------------------------------------------

ggplot(dente, aes(x = supp, y = len)) +
  geom_boxplot() +
  geom_jitter() +
  labs(title = "Crescimento Dentário Porcos da Índia",
       x = "Tipo de suplemento",
       y = "Crescimento dentário")

# Cálculo das médias dos grupos ------------------------------------------------------------------------------------------------------------

mean(dente$len) # Média geral

group_by(dente, supp) %>%
  summarise(mean(len))
