library(tidyverse)
library(e1071)
library(caret)
library(rpart)
library(rpart.plot)
library(ggplot2)

#setting working directory
location <- "/home/carloseduardo/projects/soccer_matches_prediction/data"
setwd(location)

#loading files
file_list <- list.files(path=location)

scouts_files <- file_list[c(4,8,12,16)]
match_files <- file_list[c(3,7,11,15)]
players_files <- file_list[c(5,9)]

#reading the files and creating data frame
matches_2014 = read.csv(match_files[1], header = TRUE) 
matches_2015 = read.csv(match_files[2], header = TRUE) 
matches_2016 = read.csv(match_files[3], header = TRUE)
matches_2017 = read.csv(match_files[4], header = TRUE) 

scouts_2014 = read.csv(scouts_files[1], header = TRUE) 
scouts_2015 = read.csv(scouts_files[2], header = TRUE) 
scouts_2016 = read.csv(scouts_files[3], header = TRUE) 
scouts_2017 = read.csv(scouts_files[4], header = TRUE)

players_2016 = read_csv(players_files[2])
players_2015 = read_csv(players_files[1])

matches_2014 <- matches_2014[,-1]
matches_2015 <- matches_2015[,-1]
matches_2016 <- matches_2016[,-1]
matches_2017 <- matches_2017[,-c(4,5,6,7,10,11,12)]
 
scouts_2014  <- scouts_2014[,c(1:3,5,7:10)]
scouts_2015  <- scouts_2015[,c(1:3,5:8)]
scouts_2016  <- scouts_2016[,c(1:3,5:8)]
scouts_2017  <- scouts_2017[,c(1:4,6:9)]
 
players_2016 <- players_2016[,c(1,4)]
players_2015 <- players_2015[,c(1,4)]

scouts_2015 <- merge(players_2015,scouts_2015,by.x="id",by.y="atleta_id")
scouts_2016 <- merge(players_2016,scouts_2016,by.x="id",by.y="atleta_id")

colnames(scouts_2016)[1] <- "atleta_id"
colnames(scouts_2015)[1] <- "atleta_id"
colnames(scouts_2017)[2] <- "rodada"
colnames(matches_2017)[1] <- "rodada"

scouts_2014 <- na.omit(scouts_2014)
scouts_2015 <- na.omit(scouts_2015)
scouts_2016 <- na.omit(scouts_2016)
scouts_2017 <- na.omit(scouts_2017)

create_df <- function(matches, scouts){
  #Player stats by field: attack, defense and midfield
  teams_stats <- data.frame(matrix(ncol = 14, nrow = 0))
  columns <- c("rodada" ,"clube_id","Pontuação Defesa","Preco Defesa","Variacao Defesa","Media Defesa",
               "Pontuacao Ataque","Preco Ataque","Variacao Ataque","Media Ataque",
               "Pontuacao Meio","Preco Meio","Variacao Meio","Media Meio")
  colnames(teams_stats) <- columns
  
  #Team statistics for each match
  match_stats <- data.frame(matrix(ncol = 26, nrow = 0))
  columns <- c("rodada", "clube_casa_id", "casaDef", "casaDefMedia", "casaDefPreco", "casaDefVar",
               "casaAta", "casaAtaMedia", "casaAtaPreco", "casaAtaVar",
               "casaMei", "casaMeiMedia", "casaMeiPreco", "casaMeiVar",
               "visitanteDef","visitanteDefMedia","visitanteDefPreco","visitanteDefVar",
               "visitanteAta","visitanteAtaMedia","visitanteAtaPreco","visitanteAtaVar",
               "visitanteMei","visitanteMeiMedia","visitanteMeiPreco","visitanteMeiVar")
  colnames(match_stats) <- columns
  
  for(i in 1:19){
    for(j in min(scouts$clube_id):max(scouts$clube_id)){
      defense <- filter(scouts,rodada==i & clube_id==j & (posicao_id == 1 |posicao_id == 2 |posicao_id == 3))
      attack <- filter(scouts,rodada==i & clube_id==j & (posicao_id == 5))
      midfield <- filter(scouts,rodada==i & clube_id==j & (posicao_id == 4))
      df_row <- data.frame("rodada"=defense$rodada[1] ,"clube_id"=defense$clube_id[1],"Pontuação Defesa"=sum(defense$pontos_num),"Preco Defesa"=sum(defense$preco_num),"Variacao Defesa"=sum(defense$variacao_num),"Media Defesa"=sum(defense$media_num),
                           "Pontuacao Ataque"=sum(attack$pontos_num),"Preco Ataque"=sum(attack$preco_num),"Variacao Ataque"=sum(attack$variacao_num),"Media Ataque"=sum(attack$media_num),
                           "Pontuacao Meio"=sum(midfield$pontos_num),"Preco Meio"=sum(midfield$preco_num),"Variacao Meio"=sum(midfield$variacao_num),"Media Meio"=sum(midfield$media_num))
      teams_stats<-rbind(teams_stats, df_row)
    }

    teams_stats <- na.omit(teams_stats)
    rod <- filter(matches, rodada == i)
    for (j in 1:length(rod$clube_casa_id)) {
      casa <- filter(teams_stats,rodada==i,clube_id==rod$clube_casa_id[j])
      visitante <-filter(teams_stats,rodada==i,clube_id==rod$clube_visitante_id[j])
      if(nrow(casa)> 0 && nrow(visitante)>0){
        row <- data.frame("rodada"=rod$rodada[j],"clube_casa_id"=rod$clube_casa_id[j],"casaDef"= casa$Pontuação.Defesa,"casaDefMedia"= casa$Media.Defesa,"casaDefPreco"=casa$Preco.Defesa,"casaDefVar"=casa$Variacao.Defesa,
                          "casaAta"= casa$Pontuacao.Ataque ,"casaAtaMedia"= casa$Media.Ataque,"casaAtaPreco"=casa$Preco.Ataque,"casaAtaVar"=casa$Variacao.Ataque,
                          "casaMei"= casa$Pontuacao.Meio,"casaMeiMedia"= casa$Media.Meio,"casaMeiPreco"=casa$Preco.Meio,"casaMeiVar"=casa$Variacao.Meio,
                          "visitanteDef"= visitante$Pontuação.Defesa,"visitanteDefMedia"= visitante$Media.Defesa,"visitanteDefPreco"=visitante$Preco.Defesa,"visitanteDefVar"=visitante$Variacao.Defesa,
                          "visitanteAta"= visitante$Pontuacao.Ataque,"visitanteAtaMedia"= visitante$Media.Ataque,"visitanteAtaPreco"=visitante$Preco.Ataque,"visitanteAtaVar"=visitante$Variacao.Ataque,
                          "visitanteMei"= visitante$Pontuacao.Meio,"visitanteMeiMedia"= visitante$Media.Meio,"visitanteMeiPreco"=visitante$Preco.Meio,"visitanteMeiVar"=visitante$Variacao.Meio
        )
        match_stats <- rbind(match_stats, row) 
      }
    }
  }
  df <- merge(matches,match_stats,by.x=c("rodada","clube_casa_id"),by.y=c("rodada","clube_casa_id"))
  matchResults <- data.frame("resultado" = ifelse(df$placar_oficial_mandante > df$placar_oficial_visitante,"V", ifelse(df$placar_oficial_mandante == df$placar_oficial_visitante,"E","D")))
  df <- cbind(df, matchResults)
  return(df)
}

df_2017 <- create_df(matches_2017, scouts_2017)
df_2016 <- create_df(matches_2016, scouts_2016)
df_2015 <- create_df(matches_2015, scouts_2015)
df_2014 <- create_df(matches_2014, scouts_2014)

dataset <- rbind(df_2017, df_2016, df_2015, df_2014)
dataset <- dataset[, -c(4:5)]

derrotas <- filter(dataset, resultado == "D")
empates <- filter(dataset, resultado == "E")
vitorias <- filter(dataset, resultado == "V")

ggplot(data = vitorias, aes(vitorias$clube_casa_id,vitorias$casaAta)) + 
  geom_line()
  
set.seed(4321)
sample_size <- floor(0.8 * nrow(dataset))
idxs <- sample(seq_len(nrow(dataset)), size = sample_size)

train_rows <- dataset[idxs,]
test_rows <- dataset[-idxs,]

class_train <- train_rows[,28]
class_test <- factor(test_rows[,28]) 

train <- train_rows[,-28]
test <- test_rows[,-28]

##################################################
#classification using SVM - Support Vector Machine
##################################################
classifier = svm(formula = resultado~.,
                 data = train_rows,
                 type = 'C-classification',
                 kernel = 'linear')

svm_pred = predict(classifier, newdata = test)

confusionMatrix(svm_pred,class_test)

###################################
#classification using decision tree
###################################
model <- rpart(resultado~., train_rows, method = "class", control = rpart.control(minsplit = 1))

plot <- rpart.plot(model, type = 3)

dtree_pred <- predict(model, test, type = "class")

confusionMatrix(dtree_pred,class_test)

