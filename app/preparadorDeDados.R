obterPeriodoLegislaturas<-function( nuLegislatura) {
    
    
    listaLegislaturas <- as.data.frame(rbind(c(51,1999,2003),c(52,2003,2007),c(53,2007,2011),c(54,2011,2015)))
    names(listaLegislaturas)<-c("NUMERO_LEGISLATURA", "ANO_INICIO","ANO_FIM")
    
    return(listaLegislaturas[listaLegislaturas$NUMERO_LEGISLATURA==nuLegislatura,])
    
    
    
}


obterDadosDeputados <- function() {
 
    
    require(XML)    
    
    trim <- function( x ) {
        gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
    }
    
    pasta.base <- "app/dados/brutos/lista_deputados/"    
    pasta.origem<-pasta.base
    
    
    doc = xmlTreeParse(paste(pasta.origem,"/listadeDeputados.xml",sep = '') , useInternalNodes = T)
    idNodes <- getNodeSet(doc, "//deputados")
    
    
    
        
    deputado_Id_cadastro <- xpathApply(doc, path = '//deputados/deputado/ideCadastro', xmlValue)
    deputado_Id_cadastro<-cbind(deputado_Id_cadastro)
    deputado_Id_cadastro<-do.call(rbind.data.frame,deputado_Id_cadastro)
    
        
    deputado_nome_parlamentar <- xpathApply(doc, path = '//deputados/deputado/nomeParlamentar', xmlValue) 
    deputado_nome_parlamentar<-cbind(deputado_nome_parlamentar)
    deputado_nome_parlamentar<-do.call(rbind.data.frame,deputado_nome_parlamentar)
    
    
    deputado_url_foto <- xpathApply(doc, path = '//deputados/deputado/urlFoto', xmlValue) 
    deputado_url_foto<-cbind(deputado_url_foto)
    deputado_url_foto<-do.call(rbind.data.frame,deputado_url_foto)
    
        
    deputado_nome <- xpathApply(doc, path = '//deputados/deputado/nome', xmlValue)
    deputado_nome<-cbind(deputado_nome)
    deputado_nome<-do.call(rbind.data.frame,deputado_nome)
    
    
    deputado_partido <- xpathApply(doc, path = '//deputados/deputado/partido', xmlValue)  
    deputado_partido<-cbind(deputado_partido)  
    deputado_partido<-do.call(rbind.data.frame,deputado_partido)
    
    
    deputado_uf <- xpathApply(doc, path = '//deputados/deputado/uf', xmlValue)  
    deputado_uf<-cbind(deputado_uf)   
    deputado_uf<-do.call(rbind.data.frame,deputado_uf)
    
    
    listadeputados<-NULL
    listadeputados<-cbind(deputado_Id_cadastro, deputado_nome_parlamentar,deputado_url_foto,deputado_nome, deputado_partido,deputado_uf)
        
    names(listadeputados)<-c("Id","Nome Parlamentar","Url","Nome","Partido", "UF")

 
    return(listadeputados)
    
    
}    


obterDadosCandidatos<-function() {
    
    fileList <- list.files(path="app/dados/brutos/lista_candidatos/", pattern=".txt")
    fileList <- paste("app/dados/brutos/lista_candidatos/",fileList,sep = '')
    
    
    listaCandidatos<-''
    
    
    
    for (i in fileList){
        
        f<-read.csv(i,sep=';',header = FALSE, )
        
        f<-f[f$V10=='DEPUTADO FEDERAL',]
        #f<-f[f$V42=='ELEITO' | f$V42=='SUPLENTE',]
        
        c<- data.frame(iconv(f$V11, to='ASCII//TRANSLIT'),f$V14,f$V18,f$V24,f$V25,f$V26,f$V29,f$V30,f$V31,f$V35,f$V36)
        listaCandidatos<-rbind(listaCandidatos,c)
        
    }
    names(listaCandidatos)<- c("NOME_CANDIDATO","NOME_URNA_CANDIDATO","SIGLA_PARTIDO","CODIGO_OCUPACAO","DESCRICAO_OCUPACAO","DATA_NASCIMENTO","CODIGO_SEXO","DESCRICAO_SEXO","COD_GRAU_INSTRUCAO","CODIGO_ESTADO_CIVIL","DESCRICAO_ESTADO_CIVIL")
    
    
    return(listaCandidatos)
    
}


obterDadosCompletosDeputadors <- function( ano ){
    
    unwanted_array = list(    'Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                              'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                              'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                              'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                              'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )
    
    if(!require(data.table)){install.packages("data.table")}
    
    library("data.table")
    
    listaDeputados<-obterDadosDeputados()
    listaCandidatos<-obterDadosCandidatos()
    names(listaCandidatos)[1]<-"Nome"

    names(listaCandidatos)[2]<-"Nome Parlamentar"
    
  
    listaDeputados$'Nome Parlamentar'<-chartr(paste(names(unwanted_array), collapse=''),
           paste(unwanted_array, collapse=''),
           listaDeputados$'Nome Parlamentar')
    
    
    listaDeputados$'Nome'<-chartr(paste(names(unwanted_array), collapse=''),
                                              paste(unwanted_array, collapse=''),
                                              listaDeputados$'Nome')
    
    listaCandidatos$'Nome Parlamentar'<-chartr(paste(names(unwanted_array), collapse=''),
                                paste(unwanted_array, collapse=''),
                                listaCandidatos$'Nome Parlamentar')
    
    
    listaDeputados$'Nome'<-chartr(paste(names(unwanted_array), collapse=''),
                                              paste(unwanted_array, collapse=''),
                                              listaDeputados$'Nome')
    
     
    dt1<-data.table(listaDeputados) 
    setkeyv(dt1, 'Nome')
    dt1 <- dt1[order(Nome),] 
    
    dt2<-data.table(listaCandidatos)
    setkeyv(dt2, 'Nome')
    dt2 <- dt2[order(Nome),] 
    
    #write.table(dt2, file = "listaCandidatos.cvs",)
    
    
    listaParcialDeputados<-merge(x = dt1, y = dt2, by = "Nome", all.x=TRUE)
    listaParcialDeputados$'Nome Parlamentar.y'<-NULL
    names(listaParcialDeputados)[3]<-"Nome Parlamentar"
    
    listaDeputadosSemInformacaoDemografica<-data.table(listaParcialDeputados[is.na(listaParcialDeputados$DATA_NASCIMENTO),]$"Nome Parlamentar")
    names(listaDeputadosSemInformacaoDemografica)[1]<-"Nome Parlamentar"
    listaParcialDeputados<-listaParcialDeputados[!is.na(listaParcialDeputados$DATA_NASCIMENTO),]
    setkeyv(listaParcialDeputados, names(listaParcialDeputados))
    
    setkeyv(listaDeputadosSemInformacaoDemografica, 'Nome Parlamentar')
    setkeyv(dt2, 'Nome Parlamentar')
    
    
    listaCompletaDeputados1<-merge(x = listaDeputadosSemInformacaoDemografica, y = dt2, by = "Nome Parlamentar", all.x=TRUE)
    
    
    listaDeputadosSemInformacaoDemografica<-data.table(listaCompletaDeputados1[is.na(listaCompletaDeputados1$DATA_NASCIMENTO),]$"Nome Parlamentar")
    
 
    
    return(listaParcialDeputados)
    
    

}


obterDadosDasSessoes <- function( legislatura, ano ) {
    
    require(XML)    
    trim <- function( x ) {
        gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
    }
    
    pasta.base <- "app/dados/brutos/lista_sessoes/"    
    pasta.origem<-pasta.base
    
    
    doc = xmlTreeParse(paste(pasta.origem,"listaSessoes_",legislatura,"_",ano,".xml",sep = '') , useInternalNodes = T)
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


obterConteudoDiscurso <- function( legislatura ) {
    
    require('base64enc') 
    
    file.pattern<-"*.xml"
    filename<-paste(dir,'/',files.v[3],sep = '')
    
    #doc = xmlTreeParse(filename, useInternalNodes = T)
    doc <- xmlParseDoc(filename, HUGE)       
    
    
    discurso_conteudo <- xpathApply(doc, path = '//sessao/discursoRTFBase64', xmlValue)
    
    discurso_nome_orador <- xpathApply(doc,path = '//sessao/nome', xmlValue)
    regmatches(discurso_nome_orador, gregexpr("\\(([^()]+)\\)",discurso_nome_orador, perl=TRUE))<-''
    
    discurso_partido_orador <- xpathApply(doc,path = '//sessao/partido', xmlValue)
    discurso_dia_hora <- xpathApply(doc,path = '//sessao/horaInicioDiscurso', xmlValue)
    
    
    t<-base64decode(paste(discurso_conteudo))
    
    str<- rawToChar(t, multiple = FALSE)
    
    cao<-gregexpr("\\\\s?'\\w\\w",str)
    
    cao_hexa_prefixo<-paste("0x",substr(regmatches(str, cao)[[1]],3,4),sep = '')
    
    cao_hexa__to_int<-strtoi(cao_hexa_prefixo)
    
    cao_hexa__to_int_multiple<-rawToChar(as.raw(cao_hexa__to_int),multiple =TRUE )
    
    regmatches(str, cao) <- list(cao_hexa__to_int_multiple)
    
    cao1<-gregexpr("({\\\\)(.+?)(})|(\\\\)(.+?)(\\b)|\\r|\\n|\\(Palmas.\\)",str, perl=TRUE)
    
    regmatches(str, cao1)<-''
    
    dados<- cbind(discurso_dia_hora,discurso_nome_orador,discurso_partido_orador,str)
    
    return(dados)
    
}


