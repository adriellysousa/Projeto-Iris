#Projeto de Análise exploratória de dados
#CLASSIFICAÇÃO DE ESPÉCIES DE PLANTAS UTILIZANDO RANDOM FOREST

install.packages(c("tidyverse", "corrplot", "GGally", "caret", "rpart", "rpart.plot"), dependencies = TRUE)
#definir o endereco do conjunto de dados e baixa-lo
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"
iris          <- read.csv(url, header = FALSE)
names(iris)   <- c("c_sepala", "l_sepala", "c_petala","l_petala", "especie")

#remover a string 'Iris-' do inicio de cada tipo de especie
iris$especie <- as.factor(gsub("iris-", "", iris$especie))
#estatistica descritiva basica 
summary(iris)

#Com isso, percebemos que temos um conjunto de dados formado por cinco variáveis. Quatro destas variáveis são quantitativas e uma é categórica. As variáveis são

#c_sepala: comprimento da sépala das flores

#l_sepala: largura da sépala das flores

#c_petala: comprimento da pétala das flores

#l_petala: largura da pétala das flores

#especie: espécie da flor

#verificar se existe correlação entre as nossas variáveis.Retirar a coluna 5 desta análise porque ela não é uma variável quantitativa.
library(corrplot)
correlacao <- cor(iris[,-5])

corrplot.mixed(correlacao, upper = "ellipse")

#Grafico de dispersão paea visualizar melhor as correlações entre as variáveis

library(tidyverse)
theme_set(theme_bw())
ggplot(iris, aes(x = c_petala, y = l_petala)) +
  geom_point()

#Podemos colocar mais informações neste gráfico, por exemplo, podemos colorir os pontos 
#de acordo com a espécie da planta.

ggplot(iris, aes(x = c_petala, y = l_petala, colour = especie)) +
  geom_point()

#vamos extender o gráfico anterior afim de verificar o comportamento das outras variáveis usando a função ggpairs do pacote GGally:

library(GGally)
#remover a variavel especie pois não é quantitativa
ggpairs(iris[, -5], aes(colour = iris$especie))

#Ajuste de modelo
library(rpart)
library(rpart.plot)
modelo <- rpart(especie ~ . , method = "class", data = iris)
prp(modelo, extra = 1)

#conjuntos de treino e testes. Vamos usar a função createDataPartition do pacote caret.
#definir 75% dos dados para treino e 25% para teste:

library(caret)
trainIndex  <- createDataPartition(iris$especie, p = 0.75, list = FALSE)
iris_treino <- iris[ trainIndex, ]
iris_teste  <- iris[-trainIndex, ]

#Agora tenho dois data frame, mas essa divisão provoca uma desvantagem, acabamos tendo menos dados para ajustar o modelo, 
#Menos informações temos.Uma maneira que temos para reduzir isso é atráves da validação cruzada.
#eu vou utilizar um método chamado validação cruzada k-fold.Esta técnica consiste em cinco passos:
  
#Separar o conjunto de treinamento em k-folds (ou partições)

#Ajustar o modelo em k-folds

#Testar o modelo no fold restante

#Repetir os passos 2 e 3 até que todos os folds tenham sido utilizados para teste

#Calcular a acurácia do modelo

#a função trainControl estabelece os parâmetros utilizados no ajuste do modelo

fitControl <- trainControl(method = "cv",
                           number = 5)

#Com os parametros definidos, podemos partir para o ajuste em si.

ajuste_iris <- train(especie ~ .,
                     data = iris_treino,
                     method = "rf",
                     importance = TRUE,
                     trControl = fitControl)
ajuste_iris

#Note que há um parâmetro chamado mtry no output acima. Este parâmetro significa que os modelos foram ajustados 
#3 e 4 variáveis preditoras selecionadas aleatoriamente (na verdade, quando mtry = 4 temos 
#todas as variáveis preditoras utilizadas no modelo)
#Ao final, o melhor modelo é aquele que utiliza 2 variáveis preditoras.

#Finalizamos o ajuste testando se este modelo criado é capaz de classificar novos dados.Para isto, vamos utilizar o conjunto de teste definido anteriormente:

predicao <- predict(ajuste_iris,iris_teste)
confusionMatrix(predicao, iris_teste$especie)
#Apenas uma flor da especie Iris Virginica foi classificada de maneira inadequada. Todas as outras foram classificadas
#corretamente.

#podemos verificar quais foram , dentre essas quatro variaveis preditoras,quais tiveram a maior importância no modelo. 

ggplot(varImp(ajuste_iris))
#Como podemos ver, as variáveis relacionadas à pétala são mais importantes do que as
#variáveis relacionadas à sépala. Além disso, em termos de importância, a largura 
#Portanto, podemos concluir este projeto dizendo que fomos bem sucedidos ao utilizar o
#Random Forest para classificar as espécies de plantas de acordo com as variáveis.

