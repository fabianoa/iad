
obterPeriodoLegislaturas<-function( nuLegislatura) {
    
    
    listaLegislaturas <- as.data.frame(rbind(c(51,'01/02/1999','31/01/2003'),c(52,'01/02/2003','31/01/2007'),c(53,'01/02/2007','31/01/2011'),c(54,'01/02/2011','31/01/2015')))
    names(listaLegislaturas)<-c("NUMERO_LEGISLATURA", "DT_INICIO","DT_FIM")
    
    return listaLegislaturas[listaLegislaturas$NUMERO_LEGISLATURA==nuLegislatura,]
    
     
    
}

#Classse com métodos para obtenção dos dados 

obterDiscursos <- function( codSessao, numOrador, numQuarto, numInsercao , ano) {
    
    ## Método para obter conteudos de discursos em proferidos em determinado ano
    ##    
    
    out <- tryCatch(
{
    pasta.destino <- ano 
    urlBase<- paste ("http://www.camara.gov.br/SitCamaraWS/SessoesReunioes.asmx/obterInteiroTeorDiscursosPlenario?","codSessao=",codSessao,"&numOrador=",numOrador,"&numQuarto=", numQuarto ,"&numInsercao=",numInsercao,sep = '')
    nome.arquivo <- paste ("dados/brutos/conteudo_discursos/",pasta.destino,"/discurso_",codSessao,"_",numOrador,"_", numQuarto ,"_",numInsercao,".xml",sep = '')
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
finally={
    
}
    )    
return(out)


}

#Método que coleta arquivos no formato XML com a lista das sessões realizadas na Câmara dos Deputados 
#em uma determinado ano gravando o arquivos em uma pasta correspondente ao ano.
obterListaSessoes <- function( ano ) {

 pasta.base <- paste(getwd(),"/app/dados/brutos/lista_sessoes",sep='')
 pasta.destino<-paste(pasta.base,'/',ano,sep = "") 
 
 if (!file.exists(pasta.base)){
     dir.create(pasta.base)
 }
 
 
 if (!file.exists(pasta.destino)){
    dir.create(pasta.destino)
 }
 
 
 urlBase<- paste("http://www.camara.gov.br/sitcamaraws/SessoesReunioes.asmx/ListarDiscursosPlenario?dataIni=01/01/",ano,"&dataFim=25/12/",ano,"&codigoSessao=&parteNomeParlamentar=&siglaPartido=&siglaUF=",sep='')
 nome.arquivo <- paste (pasta.destino,"/listaSessoes_",ano,".xml",sep = '')
 download.file(urlBase,nome.arquivo)
 print(paste('Arquivo:',nome.arquivo,'carregado com sucesso'))

}

#Método que coleta arquivos no formato XML com a lista de Deputados da  Câmara dos Deputados 
#do serviço de dados abertos da Camara de Deputados.
obterListaDeputados <- function( ) {
    
    pasta.base <- "app/dados/brutos/lista_deputados"
    pasta.destino<-pasta.base
    
    if (!file.exists(pasta.base)){
        dir.create(pasta.base)
    }
    
    
    if (!file.exists(pasta.destino)){
        dir.create(pasta.destino)
    }
    
    urlBase<- "http://www.camara.gov.br/SitCamaraWS/Deputados.asmx/ObterDeputados"
    nome.arquivo <- paste (pasta.destino,"/listadeDeputados.xml",sep = '')
    download.file(urlBase,nome.arquivo)
    print(paste('Arquivo:',nome.arquivo,'carregado com sucesso'))
    
    
}

#Método que coleta arquivos no formato XML com os dados dos Deputados 
# em uma determinada Legislatura do do serviço de dados abertos da Camara de Deputados.
# idDeputado - Identificador do deputado
# legislatura - numero da legislatura
obterDeputados <- function(idDeputado, legislatura) {
    
    ## Método para obter conteudos de discursos em proferidos em determinado ano
    ##
    
    pasta.base <- paste(getwd(),"/app/dados/brutos/dados_deputado",sep='')
    pasta.destino<-paste(pasta.base,'/',legislatura,sep = "") 
      
      
    if (!file.exists(pasta.base)){
        dir.create(file.path(pasta.base))
    }
    
    
    if (!file.exists(pasta.destino)){
        dir.create(pasta.destino)
    }
    
    
    out <- tryCatch(
{
   
    
    url<-paste("http://www.camara.gov.br/SitCamaraWS/Deputados.asmx/ObterDetalhesDeputado?ideCadastro=",idDeputado,"&numLegislatura=",legislatura,sep = '')
    nome.arquivo <- paste (pasta.destino,"/deputado_",idDeputado,".xml",sep = '')
    
    download.file(url,nome.arquivo, quiet=TRUE)
    
    Sys.sleep(0.5)
    
},
error=function(cond) {
    message(paste("URL does not seem to exist:", urlBase))
    message("Here's the original error message:")
    message(cond)
    # Choose a return value in case of error
    return(NA)
},
finally={
    
}
    )    
return(out)


}

#Método que coleta arquivos no formato CSV agregado por UF 
#com a lista de Candidatos de um determinado ano do serviço de dados abertos
#do TRE.

obterListaCandidatos <- function(ano) {
    
    pasta.base <- "app/dados/brutos"
    pasta.destino<-pasta.base
    
    if (!file.exists(pasta.base)){
        dir.create(pasta.base)
    }
      
    urlBase<- "http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_cand/consulta_cand_2010.zip"
    nome.arquivo <- paste (pasta.destino,"/consulta_cand_2010.zip",sep = '')
    download.file(urlBase,nome.arquivo)
    
    unzip(paste (pasta.destino,"/consulta_cand_",ano,".zip",sep = '') , exdir = paste('./',pasta.base,'/lista_candidatos',sep = ''), unzip = "internal", setTimes = FALSE)
    file.remove(nome.arquivo)
    
}



coletarDados<-function(nuLegislatura){
    
    nuLegislatura<-54
    anosLegislatura<-obterAnos(2011,2012,2013,2014)
    
    
    print('Obtendo lista de Deputados...')
    obterListaDeputados()
    
    print('Obtendo lista de Candidatos..')
    obterListaCandidatos(2010)
      
    print('Obtendo lista de Sesões')
    for(i in anosLegislatura){
        obterListaSessoes(i)
    }
    
    
    listaDeputados<-obterDadosDeputados()
    
   
    for (i in listaDeputados[,1]){
       obterDeputados(i,nuLegislatura)
    }
    
    
    listaSessoes<-obterDadosDasSessoes(2011)
    
    
#     for(i in anosLegislatura){
#         
#         listaSessoes<-obterDadosDasSessoes(i)
#         
#         for (i in seq(along=listaSessoes[,1])){
#             
#             obterdiscursos(discursos[i,])
#         }
#         
#         
#     }
   
    
    
   
    
    
}
coletarDados()

