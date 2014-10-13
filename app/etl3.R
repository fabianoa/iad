

gerarQuantitativoDeputadosPorPartido<-function(){
    
    
    deputados<- obterDadosDeputados()
        
    quantitativoDeputadorPartido<-as.data.frame(table(deputados$Partido))
    names(quantitativoDeputadorPartido)<-c("SIGLA_PARTIDO","QT_DEPUTADOS")
    
    
    saveRDS(quantitativoDeputadorPartido,file="app/dados/processados/quantitativoDeputadorPartido.Rda")
    
    
    return(quantitativoDeputadorPartido)
}

gerarQuantitativoDeputadosPorPartido()


setwd('c:/R/iad')
discursos2011<-obterDadosListaDiscursos(2011)
discursos2012<-obterDadosListaDiscursos(2012)
discursos2013<-obterDadosListaDiscursos(2013)
discursos2014<-obterDadosListaDiscursos(2014)


listaQtAbsolutaDiscursosPorPartidos2011<-do.call(rbind.data.frame,cbind(discursos2011$'Partido do Orador'))
listaQtAbsolutaDiscursosPorPartidos2011 <- as.data.frame(table(listaQtAbsolutaDiscursosPorPartidos2011))
listaQtAbsolutaDiscursosPorPartidos2011<-cbind(2011,listaQtAbsolutaDiscursosPorPartidos2011)
names(listaQtAbsolutaDiscursosPorPartidos2011)<-c("Ano","Partido","Quantidade")

listaQtAbsolutaDiscursosPorPartidos2012<-do.call(rbind.data.frame,cbind(discursos2012$'Partido do Orador'))
listaQtAbsolutaDiscursosPorPartidos2012 <- as.data.frame(table(listaQtAbsolutaDiscursosPorPartidos2012))
listaQtAbsolutaDiscursosPorPartidos2012<-cbind(2012,listaQtAbsolutaDiscursosPorPartidos2012)
names(listaQtAbsolutaDiscursosPorPartidos2012)<-c("Ano","Partido","Quantidade")

listaQtAbsolutaDiscursosPorPartidos2013<-do.call(rbind.data.frame,cbind(discursos2013$'Partido do Orador'))
listaQtAbsolutaDiscursosPorPartidos2013 <- as.data.frame(table(listaQtAbsolutaDiscursosPorPartidos2013))
listaQtAbsolutaDiscursosPorPartidos2013<-cbind(2013,listaQtAbsolutaDiscursosPorPartidos2013)
names(listaQtAbsolutaDiscursosPorPartidos2013)<-c("Ano","Partido","Quantidade")

listaQtAbsolutaDiscursosPorPartidos2014<-do.call(rbind.data.frame,cbind(discursos2014$'Partido do Orador'))
listaQtAbsolutaDiscursosPorPartidos2014 <- as.data.frame(table(listaQtAbsolutaDiscursosPorPartidos2014))
listaQtAbsolutaDiscursosPorPartidos2014<-cbind(2014,listaQtAbsolutaDiscursosPorPartidos2014)
names(listaQtAbsolutaDiscursosPorPartidos2014)<-c("Ano","Partido","Quantidade")

listaQtAbsolutaDiscursosPorPartidos<-rbind(listaQtAbsolutaDiscursosPorPartidos2011,listaQtAbsolutaDiscursosPorPartidos2012,listaQtAbsolutaDiscursosPorPartidos2013,listaQtAbsolutaDiscursosPorPartidos2014)

listaQtAbsolutaDiscursosPorPartidos<-listaQtAbsolutaDiscursosPorPartidos[ order(-listaQtAbsolutaDiscursosPorPartidos$Ano,-
                                                                                    listaQtAbsolutaDiscursosPorPartidos$Quantidade), ]

saveRDS(listaQtAbsolutaDiscursosPorPartidos,file="app/dados/processados/listaQtAbsolutaDiscursosPorPartidos.Rda")

