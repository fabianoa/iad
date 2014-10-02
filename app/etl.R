




obterDiscursos <- function( x , ano) {
    
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    
out <- tryCatch(
{
    pasta.destino <- ano 
    
    urlBase<- paste ("http://www.camara.gov.br/SitCamaraWS/SessoesReunioes.asmx/obterInteiroTeorDiscursosPlenario?","codSessao=",x$V1,"&numOrador=",x$V3,"&numQuarto=", x$V7 ,"&numInsercao=",x$V8,sep = '')
    
    nome.arquivo <- paste ("dados/brutos/conteudo_discursos/",pasta.destino,"/discurso_",x$V1,"_",x$V3,"_", x$V7 ,"_",x$V8,".xml",sep = '')
    
    download.file(urlBase,nome.arquivo, quiet=TRUE)
    
    Sys.sleep(0.5)
    
},
error=function(cond) {
    message(paste("URL does not seem to exist:", urlBase))
    message("Here's the original error message:")
    message(cond)
    # Choose a return value in case of error
    return(NA)
},
warning=function(cond) {
    message(paste("URL caused a warning:", urlBase))
    message("Here's the original warning message:")
    message(cond)
    # Choose a return value in case of warning
    return(NULL)
},
finally={
    
}
    )    
return(out)


}

obterListaDiscursos <- function( ano ) {

 pasta.base <- "app/dados/brutos/lista_discursos/"
 pasta.destino<-paste(pasta.base,ano,sep = "") 
 
 
 if (!file.exists(pasta.destino)){
    dir.create(pasta.destino)
 }
 
 
 urlBase<- paste("http://www.camara.gov.br/sitcamaraws/SessoesReunioes.asmx/ListarDiscursosPlenario?dataIni=01/01/",ano,"&dataFim=25/12/",ano,"&codigoSessao=&parteNomeParlamentar=&siglaPartido=&siglaUF=",sep='')
 nome.arquivo <- paste (pasta.destino,"/listadiscurso_",ano,".xml",sep = '')
 download.file(urlBase,nome.arquivo)

}

obterListaDiscursos(2013)



obterDadosListaDiscursos <- function( ano ) {

require(XML,quietly = TRUE)   
require('data.table')

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


listadiscursos<- do.call(rbind.data.frame, mapply(cbind, sessao_codigo,sessao_data,orador_numero, orador_nome,orador_partido,orador_uf,discurso_quarto,discurso_insercao,discurso_sumario))

names(listadiscursos)<-c("Codigo da Sessao","Data da Sessao","Numero do Orador","Nome do Orador", "Partido_do_Orador", "UF do Orador","Quarto","Insercao","Sumario")
row.names(listadiscursos)<-NULL

return(data.table(listadiscursos))

}

discursos<-obterDadosListaDiscursos(2013)
names(discursos)


t<-(discursos[discursos$'Partido_do_Orador'!=''])$Partido_do_Orador

x<-do.call(rbind.data.frame,t)

discursoas.data.frame(table(x))






for (i in seq(along=discursos[,1])){
    
    obterdiscursos(discursos[i,])
}

