


listaFinalCompletadiscursos<-read.csv("app/dados/processados/dadosProcessadosDiscursos_54.csv")

lista<-listaFinalCompletadiscursos[!is.na(listaFinalCompletadiscursos$COD_GRAU_INSTRUCAO),]



tab<-as.data.frame(table(lista$Nome_Orador))

require(ggplot2)

hist(x =as.numeric(as.character(listaCompletadiscursos$COD_GRAU_INSTRUCAO)) )
hist(x =log(as.numeric(as.character(listaFinalCompletadiscursos$text.sum))),breaks=100 )

density(as.numeric(as.character(listaFinalCompletadiscursos$text.sum)))
hist(x =as.numeric(as.character(listaCompletadiscursos$hapax.percentage)) )


cor (x = as.numeric(as.character(listaCompletadiscursos$text.type.ratio)), y=as.numeric(as.character(listaCompletadiscursos$text.sum)) ) 

cor (x = as.numeric(as.character(listaCompletadiscursos$hapax.percentage)), y=as.numeric(as.character(listaCompletadiscursos$text.sum)) ) 



cor (x = as.numeric(as.character(listaFinalCompletadiscursos$hapax.percentage)), y=as.numeric(as.character(listaFinalCompletadiscursos$text.type.ratio)) ) 


qplot(y = as.numeric(as.character(listaFinalCompletadiscursos$hapax.percentage)), x=as.numeric(as.character(listaFinalCompletadiscursos$idade)) ) + geom_point(shape=1) +geom_smooth(method = "lm", se = TRUE)
qplot(y = as.numeric(as.character(listaCompletadiscursos$text.type.ratio)), x=as.numeric(as.character(listaCompletadiscursos$idade)) ) + geom_point(shape=1) +geom_smooth(method = "lm", se = TRUE)

cor (x = as.numeric(as.character(listaFinalCompletadiscursos$hapax.percentage)), y=as.numeric(as.character(listaFinalCompletadiscursos$idade)) ) 
cor (x = as.numeric(as.character(listaFinalCompletadiscursos$text.type.ratio)), y=as.numeric(as.character(listaFinalCompletadiscursos$idade)) ) 


qplot(y = as.numeric(as.character(listaFinalCompletadiscursos$hapax.percentage)), x=as.numeric(as.character(listaFinalCompletadiscursos$COD_GRAU_INSTRUCAO)) ) + geom_point(shape=1) +geom_smooth(method = "lm", se = TRUE)
qplot(y = as.numeric(as.character(listaFinalCompletadiscursos$text.type.ratio)), x=as.numeric(as.character(listaFinalCompletadiscursos$COD_GRAU_INSTRUCAO)) ) + geom_point(shape=1) +geom_smooth(method = "lm", se = TRUE)



cor (x = as.numeric(as.character(listaFinalCompletadiscursos$text.type.ratio)), y=as.numeric(as.character(listaFinalCompletadiscursos$COD_GRAU_INSTRUCAO)) ) 
cor (x = as.numeric(as.character(listaFinalCompletadiscursos$hapax.percentage)), y=as.numeric(as.character(listaFinalCompletadiscursos$COD_GRAU_INSTRUCAO)) ) 


return(listaCompletadiscursos)