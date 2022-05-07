
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

# Teste T para uma amostra -----------------------------------------------------------------------------------------------------------------

### Teste para determinar se a média é significativamente diferente de um valor
### Verdadeiro valor = 18 (H0)
### p < 0.05 (H1)

t.test(dente$len, mu = 18) # Resultado não significativamente diferente

### Para saber se é maior ou menor que determinado valor

t.test(dente$len, alternative = "greater", mu = 3) # Resultado significativo

# Teste T para dois grupos independentes ---------------------------------------------------------------------------------------------------

### Testa a diferença entre as médias de dois grupos independentes
### H0: não há diferença entre as médias dos grupos

OJ <- dente$len[dente$supp == "OJ"]
VC <- dente$len[dente$supp == "VC"]

t.test(OJ, VC, paired = FALSE, var.equal = FALSE, conf.level = 0.95)

t.test(OJ, VC, paired = FALSE, alternative = "greater")

### paired = FALSE: medidas coletadas separadamente
### Se os valores são coletados separadamente e em condições diferentes, as amostras
### são independentes, ou seja, dois grupos diferentes

