####################### Regressão Logística Multinomial #######################

# Passo 1: Carregar os pacotes que serão usados

if(!require(pacman)) install.packages("pacman")
library(pacman)

pacman::p_load(dplyr, car, psych, nnet, AER, lmtest,
               gtsummary, reshape2, ggplot2, DescTools)


# Passo 2: Carregar o banco de dados


dados <- readxl::read_xlsx("data/dados_perfil_2006_2017.xlsx")

View(dados)                                 # Visualização dos dados em janela separada
glimpse(dados)                              # Visualização de um resumo dos dados



# Passo 2.1: Transformação do banco de dados
# Modifiicar nome das colunas, e dos valores das células


## Renomeando as colunas
colnames(dados)[colnames(dados) %in%
                  c("P1.sexo", "P2.idade", "P3.cor", "P9.RFM",
                    "P26.Areaint","P27.bachlic")] <- c("sexo", "idade", "cor", "rfm",
                                                       "areaint","bachlic")

## Removendo valores indesejados
# Remover os valores "Todas" e "Nenhuma"
dados <- dados[!(dados$areaint %in% c(5, 6)),]

# Remover os valores 7 e 9 da coluna rfm
dados <- dados[!(dados$rfm %in% c(7, 9)),]

# Remover "Outras respostas", "Outras/NR"
dados <- dados[!(dados$cor %in% c(6, 9)),]

## Renomeando as variáveis

dados$sexo <- ifelse(dados$sexo == 1, "Masculino", "Feminino")

dados$cor <- factor(dados$cor,
                    levels = c(1, 2, 3, 4, 5),
                    labels = c("Branca", "Preta", "Parda", "Amarela",
                               "Indígena"))

dados$areaint <- factor(dados$areaint,
                     levels = c(1, 2, 3, 4),
                     labels = c("Antropologia", "Ciência Política", "Sociologia",
                                "Ainda não sabe"))


## Excluir a coluna bachlic usando dplyr
dados <- dados %>% dplyr::select(-bachlic)

## Formatando os dados como integer e factor

dados$sexo <- as.factor(dados$sexo)
dados$cor <- as.factor(dados$cor)
dados$areaint <- as.factor(dados$areaint)

dados$rfm <- as.integer(round(dados$rfm))

### Garantindo que o banco de dados não tenha valores N'As

dados <- dados[complete.cases(dados[, c("sexo", "cor", "areaint", "rfm")]), ]


# Passo 3: Análise das frequências das categorias da VD

summary(dados)

table(dados$sexo, dados$cor, dados$rfm)


# Passo 4: Checagem das categorias de referência

levels(dados$sexo) # "Feminino" = categoria de referência
# dados$sexo <- relevel(dados$sexo, ref = "Masculino")

levels(dados$cor) # "Branca" = categoria de referência

levels(dados$areaint)  # Antropologia = categoria de referência


# Passo 5: Checagem dos pressupostos

## 1. Variável dependente nominal (categorias mutuamente exclusivas)
## 2. Independência das observações (sem medidas repetidas)

## 3. Ausência de multicolinearidade
psych::pairs.panels(dados[, c(3, 5, 6, 7)])

m <- lm(as.numeric(areaint) ~ sexo + cor + rfm, data = dados)
car::vif(m)


## 4. Independência de alternativas irrelevantes (teste Hausman-McFadden)
# Críticas válidas aos testes que checam esse pressuposto: https://statisticalhorizons.com/iia
install.packages("mlogit")
library(mlogit)

modiia <- mlogit::mlogit(areaint ~ 1 | sexo + cor + rfm,
                         data = dados, shape = "wide",
                         reflevel = "Antropologia")

modiia2 <- mlogit::mlogit(areaint ~ 1 | sexo + cor + rfm,
                         data = dados, shape = "wide",
                         reflevel = "Antropologia",
                         alt.subset = c("Antropologia", "Ciência Política", "Ainda não sabe"))

modiia3 <- mlogit::mlogit(areaint ~ 1 | sexo + cor + rfm,
                          data = dados, shape = "wide",
                          reflevel = "Antropologia",
                          alt.subset = c("Antropologia", "Ciência Política", "Sociologia"))

modiia4 <- mlogit::mlogit(areaint ~ 1 | sexo + cor + rfm,
                          data = dados, shape = "wide",
                          reflevel = "Antropologia",
                          alt.subset = c("Antropologia", "Sociologia", "Ainda não sabe"))



mlogit::hmftest(modiia, modiia2)
mlogit::hmftest(modiia, modiia3)
mlogit::hmftest(modiia, modiia4)



# Passo 6. Construção do modelo e interpretação dos resultados


## Construção do modelo e do modelo nulo (usando o pacote nnet):

mod <- multinom(areaint ~ sexo + cor + rfm, data = dados, model = TRUE)

mod0 <- multinom(areaint ~ 1, data = dados, model = TRUE)



# Ajuste do modelo

anova(mod, mod0)

DescTools::PseudoR2(mod, which = "Nagelkerke")


# Overall effects

car::Anova(mod, type = "II", test = "Wald")


# Efeitos específicos

summary(mod)


## Obtenção dos valores de p - por Wald (pacote lmtest)
lmtest::coeftest(mod)



## Obtenção das razões de chance com IC 95% (usando log-likelihood)
exp(coef(mod))
exp(confint(mod))


## Tabela completa (pacote gtsummary)
gtsummary::tbl_regression(mod, exponentiate = FALSE)
gtsummary::tbl_regression(mod, exponentiate = TRUE)



# Passo 7 (OPCIONAL): Criação e análise de um segundo modelo

mod2 <- multinom(areaint ~ sexo + rfm, data = dados, model = TRUE)


## Ajuste do modelo

anova(mod0, mod2)

DescTools::PseudoR2(mod2, which = "Nagelkerke")


## Overall effects
Anova(mod2, type="II", test="Wald")


## Efeitos específicos
summary(mod2)


## Obtenção dos valores de p - por Wald (pacote lmtest)
lmtest::coeftest(mod2)


## Obtenção das razões de chance com IC 95% (usando log-likelihood)
exp(coef(mod2))
exp(confint(mod2))


# Comparação entre os modelos

## AIC e BIC
AIC(mod, mod2)
BIC(mod, mod2)


## Qui-quadrado
anova(mod2, mod, test="Chisq")



# Passo 8. Tabelas de classificação e gráficos

## Tabela de classificação
### Segundo modelo (sem confiança na política)

tab <- table(Observado = dados$areaint, Previsto = predict(mod2))

prop.table(tab)

acuracia <- sum(diag(tab)) / sum(tab)

# caret::confusionMatrix(predict(mod2), dados$areaint)



# Visualização gráfica
## https://rpubs.com/heruwiryanto/multinom-reg

dados_prev <- cbind(dados[, c(3, 5, 6, 7)], predict(mod2, type = "probs",
                                                    se = TRUE))

dados_prev <- reshape2::melt(dados_prev,
                             id.vars = c("sexo", "cor", "rfm","areaint"),
                             value.name = "Probabilidade",
                             variable.name = "AreaInteresse")


ggplot(dados_prev, aes(x = cor, y = Probabilidade, color = sexo)) +
  geom_smooth(method = "loess", size = 0.5) +
  labs(x = "Renda familiar, em salários mínimos", y = "Probabilidade") +
  scale_y_continuous(labels = scales::percent_format(decimal.mark = ",")) +
  scale_x_discrete(labels = scales::label_wrap(1))+
  facet_grid(AreaInteresse ~ .) +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(fill = NA)))




##########
library(ggplot2)
library(reshape2)
library(scales)

# Assumindo que `predict(mod2, type = "probs", se = TRUE)` retorna um data frame ou matriz
predicoes <- predict(mod2, type = "probs", se = TRUE)

# Combine os dados e as previsões
dados_prev <- cbind(dados[, c(3, 5, 6, 7)], predicoes)

# Transforme para formato longo com melt()
dados_prev <- melt(dados_prev,
                   id.vars = c("sexo", "cor", "rfm", "areaint"),
                   variable.name = "AreaInteresse",
                   value.name = "Probabilidade")

# Crie o gráfico
ggplot(dados_prev, aes(x = rfm, y = Probabilidade, color = sexo)) +
  geom_smooth(method = "loess", size = 0.5) +
  labs(x = "Renda familiar, em salários mínimos", y = "Probabilidade") +
  scale_y_continuous(labels = scales::percent_format(decimal.mark = ",")) +
  scale_x_continuous() +  # Ajuste conforme necessário
  facet_grid(AreaInteresse ~ .) +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(fill = NA)))


dados_prev_filtered <- dados_prev[dados_prev$AreaInteresse != "Ainda não sabe", ]

# Criar o gráfico
ggplot(dados_prev_filtered, aes(x = rfm, y = Probabilidade, color = sexo)) +
  geom_point(alpha = 0.6, size = 1.5) +  # Adicionar pontos com transparência
  geom_smooth(method = "loess", size = 0.8, linetype = "solid", se = TRUE) +  # Linha de suavização com intervalo de confiança
  labs(
    title = "Probabilidade de Área de Interesse por Renda Familiar",
    x = "Renda Familiar, em Salários Mínimos",
    y = "Probabilidade",
    color = "Sexo"
  ) +
  scale_y_continuous(labels = scales::percent_format(decimal.mark = ",")) +
  scale_x_continuous(labels = scales::number_format(decimal.mark = ",", accuracy = 1)) +  # Ajustar os rótulos do eixo x
  facet_grid(AreaInteresse ~ ., scales = "free_y") +  # Facetas com escalas independentes no eixo y
  theme_bw() +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    strip.text = element_text(size = 12),
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_legend(override.aes = list(fill = NA)))

############
library(ggplot2)
library(scales)

# Filtrar os dados para remover a categoria "Ainda não sabe" de AreaInteresse
dados_prev_filtered <- dados_prev[dados_prev$AreaInteresse != "Ainda não sabe", ]

# Criar o gráfico
ggplot(dados_prev_filtered, aes(x = rfm, y = Probabilidade, color = sexo)) +
  geom_point(alpha = 0.6, size = 1.5) +  # Adicionar pontos com transparência
  geom_smooth(method = "loess", size = 0.8, linetype = "solid", se = TRUE) +  # Linha de suavização com intervalo de confiança
  labs(
    title = "Probabilidade de Área de Interesse por Renda Familiar",
    x = "Renda Familiar, em Salários Mínimos",
    y = "Probabilidade",
    color = "Sexo"
  ) +
  scale_y_continuous(labels = scales::percent_format(decimal.mark = ",")) +
  scale_x_continuous(labels = scales::number_format(decimal.mark = ",", accuracy = 1)) +  # Ajustar os rótulos do eixo x
  facet_grid(AreaInteresse ~ ., scales = "free_y") +  # Facetas com escalas independentes no eixo y
  theme_bw() +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    strip.text = element_text(size = 12),
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_legend(override.aes = list(fill = NA)))


########

library(ggplot2)
library(scales)
library(forcats)

# Filtrar os dados para remover a categoria "Ainda não sabe" de AreaInteresse
dados_prev_filtered <- dados_prev %>%
  filter(AreaInteresse != "Ainda não sabe") %>%
  mutate(AreaInteresse = fct_recode(AreaInteresse,
                                    "C. Política" = "Ciência Política"))

# Arredondar as probabilidades para números inteiros
dados_prev_filtered$Probabilidade <- round(dados_prev_filtered$Probabilidade * 100)  # Arredonda as probabilidades para inteiros

# Criar o gráfico
ggplot(dados_prev_filtered, aes(x = rfm, y = Probabilidade, color = sexo)) +
  geom_point(alpha = 0.6, size = 1.5) +  # Adicionar pontos com transparência
  geom_smooth(method = "loess", size = 0.8, linetype = "solid", se = TRUE) +  # Linha de suavização com intervalo de confiança
  labs(
    title = "Probabilidade de Área de Interesse por Renda Familiar e Sexo",
    x = "Renda Familiar, em Salários Mínimos",
    y = "Probabilidade (%)",
    color = "Sexo"
  ) +
  scale_y_continuous(labels = scales::number_format(decimal.mark = ",", accuracy = 1)) +  # Mostrar como inteiros
  scale_x_continuous(labels = scales::number_format(decimal.mark = ",", accuracy = 1)) +  # Ajustar os rótulos do eixo x
  facet_grid(AreaInteresse ~ ., scales = "free_y") +  # Facetas com escalas independentes no eixo y
  theme_bw() +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    strip.text = element_text(size = 12),
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_legend(override.aes = list(fill = NA)))


