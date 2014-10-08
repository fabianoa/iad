obterDadosListaDeputados <- function() {
 
    
    require(XML)    
    
    trim <- function( x ) {
        gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
    }
    
    pasta.base <- "app/dados/brutos/lista_deputados/"    
    pasta.origem<-pasta.base
    
    
    doc = xmlTreeParse(paste(pasta.origem,"/listadesputados.xml",sep = '') , useInternalNodes = T)
    idNodes <- getNodeSet(doc, "//deputados")
    
      
    deputado_Id_cadastro <- xpathApply(doc, path = '//deputados/deputado/ideCadastro', xmlValue)
    deputado_nome_parlamentar <- xpathApply(doc, path = '//deputados/deputado/nomeParlamentar', xmlValue) 
    deputado_url_foto <- xpathApply(doc, path = '//deputados/deputado/urlFoto', xmlValue) 

    deputado_nome <- xpathApply(doc, path = '//deputados/deputado/nome', xmlValue)
    deputado_partido <- xpathApply(doc, path = '//deputados/deputado/partido', xmlValue)  
    deputado_partido<-mapply(trim,deputado_partido)
        
    listadeputados<-cbind(deputado_Id_cadastro, deputado_nome_parlamentar,deputado_url_foto,deputado_nome, deputado_partido)
    
    names(listadeputados[,1])<-c("Id","Nome Parlamentar","Url Foto","Nome","Partido")
    row.names(listadeputados)<-NULL
    
    return(listadeputados)

    
    
}    

obterDadosListaDeputados()

fileList <- list.files(path="app/dados/brutos/lista_candidatos/", pattern=".txt")
fileList <- paste("app/dados/brutos/lista_candidatos/",fileList,sep = '')
h<-sapply(fileList, read.csv,sep=';')
h<-as.data.frame(h)


read.csv(sep = )

sapply(paste("app/dados/brutos/lista_candidatos/", 
             1:100, sep=".txt"), 
       read.csv)


obterDadosListaDiscursos <- function( ano ) {
    
    require(XML)    
    trim <- function( x ) {
        gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
    }
    
    pasta.base <- "app/dados/brutos/lista_discursos/"    
    pasta.origem<-paste(pasta.base,ano,sep = "") 
    
    
    doc = xmlTreeParse(paste(pasta.origem,"/listadiscurso_",ano,".xml",sep = '') , useInternalNodes = T)
    idNodes <- getNodeSet(doc, "//discursos")
    
    sessao_codigo <- lapply(idNodes, xpathApply, path = '../../../codigo', xmlValue)
    sessao_codigo<-mapply(trim,sessao_codigo)
    sessao_data <- lapply(idNodes, xpathApply, path = '../../../data', xmlValue)
    orador_nome <- lapply(idNodes, xpathApply, path = 'discurso/orador/nome', xmlValue)
    orador_partido <- lapply(idNodes, xpathApply, path = 'discurso/orador/partido', xmlValue)
    orador_partido<-mapply(trim,orador_partido)
    
    orador_uf <- lapply(idNodes, xpathApply, path = 'discurso/orador/uf', xmlValue)
    orador_numero <- lapply(idNodes, xpathApply, path = 'discurso/orador/numero', xmlValue)
    discurso_quarto <- lapply(idNodes, xpathApply, path = 'discurso/numeroQuarto', xmlValue)
    discurso_insercao <- lapply(idNodes, xpathApply, path = 'discurso/numeroInsercao', xmlValue)
    discurso_sumario <- lapply(idNodes, xpathApply, path = 'discurso/sumario', xmlValue)
    
    
    listadiscursos<- do.call(rbind.data.frame, mapply(cbind,ano, sessao_codigo,sessao_data,orador_numero, orador_nome,orador_partido,orador_uf,discurso_quarto,discurso_insercao,discurso_sumario))
    
    names(listadiscursos)<-c("Ano","Codigo da Sessao","Data da Sessao","Numero do Orador","Nome do Orador", "Partido do Orador", "UF do Orador","Quarto","Insercao","Sumario")
    row.names(listadiscursos)<-NULL
    
    return(listadiscursos)
    
}

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
