


#Classse com métodos para obtenção dos dados 

obterDiscursos <- function( codSessao, numOrador, numQuarto, numInsercao , numeroLegislatura) {
    
    ## Método para obter conteudos de discursos em proferidos em determinado ano
    ##    
    source('app/preparadorDeDados.R')
    numeroLegislatura<-54
    listaAnos<- obterPeriodoLegislaturas(numeroLegislatura)   
    listaAnos<-c((listaAnos$ANO_INICIO):(listaAnos$ANO_FIM-1))
    
    pasta.base <- paste(getwd(),"/app/dados/brutos/conteudo_discursos",sep='')
    
    if (!file.exists(pasta.base)){
        dir.create(pasta.base)
    }
    
    
    for(i in listaAnos){
        
        
        listaSessoes<-obterDadosDasSessoes(numeroLegislatura,i)
        
        pasta.destino<-paste(pasta.base,"/",numeroLegislatura,sep = '')
        
        if (!file.exists(pasta.destino)){
            dir.create(pasta.destino)
        }
        
        for (j in seq(along=listaSessoes[,1])){
        
            
            out <- tryCatch(
            {
             
              sec<-listaSessoes[j,] 
                
              url<- paste ("http://www.camara.gov.br/SitCamaraWS/SessoesReunioes.asmx/obterInteiroTeorDiscursosPlenario?","codSessao=",sec$'Codigo da Sessao',"&numOrador=",sec$'Numero do Orador',"&numQuarto=", sec$'Quarto' ,"&numInsercao=",sec$'Insercao',sep = '')
              
              nome.arquivo <- paste (pasta.destino,"/discurso_",i,"_",sec$'Codigo da Sessao',"_",sec$'Numero do Orador',"_", sec$'Quarto' ,"_",sec$'Insercao',".xml",sep = '')
              if (!file.exists(nome.arquivo)){
                  download.file(url,nome.arquivo, quiet=TRUE)
                  Sys.sleep(0.2)
              }
              
        
            },
            error=function(cond) {
                message(paste("URL does not seem to exist:", url))
                message("Here's the original error message:")
                message(cond)
                # Choose a return value in case of error
                return(NA)
            },
            finally={
               
            }
            )
        }

   }    
    
   
        
return(out)


}

#Método que coleta arquivos no formato XML com a lista das sessões realizadas na Câmara dos Deputados 
#em uma determinado ano gravando o arquivos em uma pasta correspondente ao ano.
obterListaSessoes <- function( legislatura ) {
    
    listaAnos<- obterPeriodoLegislaturas(legislatura)   
    listaAnos<-c((listaAnos$ANO_INICIO):(listaAnos$ANO_FIM-1))
    
    for(i in listaAnos){
     
        ano<-i
            
        pasta.base <- paste(getwd(),"/app/dados/brutos/lista_sessoes",sep='')
        pasta.destino<-pasta.base 
        
        if (!file.exists(pasta.base)){
            dir.create(pasta.base)
        }
        
        
        if (!file.exists(pasta.destino)){
            dir.create(pasta.destino)
        }
        
        #Excluindo período de recesso parlamentar no final do ano e inicio do ano seguinte
        urlBase<- paste("http://www.camara.gov.br/sitcamaraws/SessoesReunioes.asmx/ListarDiscursosPlenario?dataIni=01/02/",ano,"&dataFim=23/12/",ano,"&codigoSessao=&parteNomeParlamentar=&siglaPartido=&siglaUF=",sep='')
        nome.arquivo <- paste (pasta.destino,"/listaSessoes_",legislatura,"_",ano,".xml",sep = '')
        download.file(urlBase,nome.arquivo)
        print(paste('Arquivo:',nome.arquivo,'carregado com sucesso'))
    }
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

obterListaCandidatos <- function(numeroLegislatura) {
    
    legislatura<-obterPeriodoLegislaturas(numeroLegislatura)   
    anoCandidatura <-legislatura$ANO_INICIO-1
   
    pasta.base <- "app/dados/brutos"
  
        
    if (!file.exists(pasta.base)){
        dir.create(pasta.base)
    }
     
    
    nomeArquivo<-paste("consulta_cand_",anoCandidatura,".zip",sep='')
      
    urlBase<- paste("http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_cand/",nomeArquivo,sep = '')
    nome.arquivo <- paste (pasta.base,"/",nomeArquivo,sep = '')
    download.file(urlBase,nome.arquivo)
    
    pasta.destino<-paste(pasta.base,"/lista_candidatos/",numeroLegislatura,sep='')
    
    if (!file.exists(pasta.destino)){
        dir.create(pasta.destino)
    }
    
    unzip(paste (pasta.base,"/consulta_cand_",anoCandidatura,".zip",sep = '') , exdir = paste('./',pasta.destino,sep = ''), unzip = "internal", setTimes = FALSE)
    file.remove(nome.arquivo)
    
}



coletarDados<-function(nuLegislatura){
    
    nuLegislaturas<-c(54,53,52)
       
    
    print('Obtendo lista de Candidatos..')
    for(i in nuLegislaturas){
        obterListaCandidatos(i)
    } 
    
    print('Obtendo lista de Deputados...')
    obterListaDeputados()
    
    
    
    
    print('Obtendo lista de Sesões')
    for(i in nuLegislaturas){
        obterListaSessoes(i)
    }
    
    print('Obtendo lista de Candidatos..')
    for(i in nuLegislaturas){
    
    listaDeputados<-obterDadosDeputados()
        i<-52
        listaDeputados<-obterDadosDeputados(i)
        
        for (j in listaDeputados[,1]){
            obterDeputados(j,i)
        }
        
    } 
   
    
   
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

