####################### Regressão Logística Multinomial #######################

# Passo 1: Carregar os pacotes que serão usados

if(!require(pacman)) install.packages("pacman")
library(pacman)

pacman::p_load(dplyr, car, psych, nnet, AER, lmtest,
               gtsummary, reshape2, ggplot2, DescTools)


# Passo 2: Carregar o banco de dados

# Importante: selecionar o diretório de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory

dados <- read.csv2('DadosMultinomial.csv', stringsAsFactors = TRUE,
                   fileEncoding = "latin1")

View(dados)                                 # Visualização dos dados em janela separada
glimpse(dados)                              # Visualização de um resumo dos dados



# Passo 3: Análise das frequências das categorias da VD

summary(dados)

table(dados$Voto, dados$Etnia)


# Passo 4: Checagem das categorias de referência

levels(dados$Voto) # Trump = categoria de referência
# dados$Voto <- relevel(dados$Voto, ref = "Hillary Clinton")


levels(dados$Etnia)  # Caucasiano = categoria de referência
# dados$Etnia <- relevel(dados$Etnia, ref = "Não-Caucasiano")



# Passo 5: Checagem dos pressupostos

## 1. Variável dependente nominal (categorias mutuamente exclusivas)
## 2. Independência das observações (sem medidas repetidas)

## 3. Ausência de multicolinearidade
psych::pairs.panels(dados[3:5])

m <- lm(as.numeric(Voto) ~ Etnia + ConfPol + LibEcon, data = dados)
car::vif(m)


## 4. Independência de alternativas irrelevantes (teste Hausman-McFadden)
# Críticas válidas aos testes que checam esse pressuposto: https://statisticalhorizons.com/iia
install.packages("mlogit")
library(mlogit)

modiia <- mlogit::mlogit(Voto ~ 1 | Etnia + ConfPol + LibEcon,
                         data = dados, shape = "wide",
                         reflevel = "Donald Trump")

modiia2 <- mlogit::mlogit(Voto ~ 1 | Etnia + ConfPol + LibEcon,
                          data = dados, shape = "wide",
                          reflevel = "Donald Trump",
                          alt.subset = c("Donald Trump", "Hillary Clinton"))

modiia3 <- mlogit::mlogit(Voto ~ 1 | Etnia + ConfPol + LibEcon,
                          data = dados, shape = "wide",
                          reflevel = "Donald Trump",
                          alt.subset = c("Donald Trump", "Outros"))

mlogit::hmftest(modiia, modiia2)
mlogit::hmftest(modiia, modiia3)



# Passo 6. Construção do modelo e interpretação dos resultados


## Construção do modelo e do modelo nulo (usando o pacote nnet):

mod <- multinom(Voto ~ ConfPol + LibEcon + Etnia, data = dados, model = TRUE)

mod0 <- multinom(Voto ~ 1, data = dados, model = TRUE)



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

mod2 <- multinom(Voto ~ LibEcon + Etnia, data = dados, model = TRUE)


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

tab <- table(Observado = dados$Voto, Previsto = predict(mod2))

prop.table(tab)

acuracia <- sum(diag(tab)) / sum(tab)

# caret::confusionMatrix(predict(mod2), dados$Voto)



# Visualização gráfica
## https://rpubs.com/heruwiryanto/multinom-reg

dados_prev <- cbind(dados[3:5], predict(mod2, type = "probs", se = TRUE))

dados_prev <- reshape2::melt(dados_prev,
                             id.vars = c("Etnia", "ConfPol", "LibEcon"),
                             value.name = "Probabilidade",
                             variable.name = "Candidato")


ggplot(dados_prev, aes(x = LibEcon, y = Probabilidade, color = Etnia)) +
  geom_smooth(method = "loess", size = 0.5) +
  labs(x = "Escore de Liberalismo Econômico") +
  scale_y_continuous(labels = scales::percent_format(decimal.mark = ",")) +
  scale_x_continuous(labels = scales::number_format(decimal.mark = ",",
                                                    accuracy = 1),
                     breaks = seq(1, 10, 1)) +
  facet_grid(Candidato ~ .) +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(fill = NA)))