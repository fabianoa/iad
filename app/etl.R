#Classse com métodos para obtenção dos dados 


obterDiscursos <- function( x , ano) {
    
    ## Método para obter conteudos de discursos em proferidos em determinado ano
    ##
    
    
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


#obterListaDiscursos(2013)


obterListaDeputados <- function( ) {
    
    pasta.base <- "app/dados/brutos/lista_deputados"
    pasta.destino<-pasta.base
        
    urlBase<- "http://www.camara.gov.br/SitCamaraWS/Deputados.asmx/ObterDeputados"
    nome.arquivo <- paste (pasta.destino,"/listadesputados.xml",sep = '')
    download.file(urlBase,nome.arquivo)
    
}


obterDeputados <- function(idDeputado, legislatura) {
    
    ## Método para obter conteudos de discursos em proferidos em determinado ano
    ##
    
    
   ideCadastro=141428&numLegislatura=
     
    
    out <- tryCatch(
{
    pasta.destino <- legislatura 
    
    urlBase<- paste (" http://www.camara.gov.br/SitCamaraWS/Deputados.asmx/ObterDetalhesDeputado?","ideCadastro=",idDeputado,"&numLegislatura=",legislatura,sep = '')
    
    nome.arquivo <- paste ("dados/brutos/dados_deputado/",pasta.destino,"/deputado_",idDeputado,".xml",sep = '')
    
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


obterCandidatos <- function(ano) {
    
    pasta.base <- "app/dados/brutos"
    pasta.destino<-pasta.base
    
    urlBase<- "http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_cand/consulta_cand_2010.zip"
    nome.arquivo <- paste (pasta.destino,"/consulta_cand_2010.zip",sep = '')
    download.file(urlBase,nome.arquivo)
    
    unzip(paste (pasta.destino,"/consulta_cand_2010.zip",sep = '') , exdir = paste('./',pasta.base,'/lista_candidatos',sep = ''), unzip = "internal", setTimes = FALSE)
    
    
}


obterDeputados()





for (i in seq(along=discursos[,1])){
    
    obterdiscursos(discursos[i,])
}

