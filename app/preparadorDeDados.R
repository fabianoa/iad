obterLegislaturas<-function() {
    

    return(c(52,53,54))
    
}


obterPeriodoLegislaturas<-function( nuLegislatura) {
    
    
    listaLegislaturas <- as.data.frame(rbind(c(51,1999,2003),c(52,2003,2007),c(53,2007,2011),c(54,2011,2015)))
    names(listaLegislaturas)<-c("NUMERO_LEGISLATURA", "ANO_INICIO","ANO_FIM")
    
    return(listaLegislaturas[listaLegislaturas$NUMERO_LEGISLATURA==nuLegislatura,])
        
    
}


obterDadosDeputados <- function(legislatura) {
    
    require(XML)    
    
     
    trim <- function( x ) {
        gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
    }
    
    pasta.base <- "app/dados/brutos/lista_deputados/"    
    pasta.origem<-pasta.base
    
    
    doc = xmlTreeParse(paste(pasta.origem,"/Deputados.xml",sep = '') , useInternalNodes = T)
    idNodes <- getNodeSet(doc, "//deputados")
    
    
    deputado_Id_cadastro <- xpathApply(doc, path = '//orgao/Deputados/Deputado/ideCadastro', xmlValue)
    deputado_Id_cadastro<-cbind(deputado_Id_cadastro)
    deputado_Id_cadastro<-do.call(rbind.data.frame,deputado_Id_cadastro)
    
    
    deputado_nome_parlamentar <- xpathApply(doc, path = '//orgao/Deputados/Deputado/nomeParlamentar', xmlValue) 
    deputado_nome_parlamentar<-cbind(deputado_nome_parlamentar)
    deputado_nome_parlamentar<-do.call(rbind.data.frame,deputado_nome_parlamentar)
    
    numLegislatura <- xpathApply(doc, path = '//orgao/Deputados/Deputado/numLegislatura', xmlValue) 
    numLegislatura<-cbind(numLegislatura)
    numLegislatura<-do.call(rbind.data.frame,numLegislatura)
    
    
    deputado_nome <- xpathApply(doc, path = '//orgao/Deputados/Deputado/nomeParlamentar', xmlValue)
    deputado_nome<-cbind(deputado_nome)
    deputado_nome<-do.call(rbind.data.frame,deputado_nome)
    
    
    deputado_partido <- xpathApply(doc, path = '//orgao/Deputados/Deputado/LegendaPartidoEleito', xmlValue)  
    deputado_partido<-cbind(deputado_partido)  
    deputado_partido<-do.call(rbind.data.frame,deputado_partido)
    
    
    deputado_uf <- xpathApply(doc, path = '//orgao/Deputados/Deputado/UFEleito', xmlValue)  
    deputado_uf<-cbind(deputado_uf)   
    deputado_uf<-do.call(rbind.data.frame,deputado_uf)
    
    
    listadeputados<-NULL
    listadeputados<-cbind(deputado_Id_cadastro, deputado_nome_parlamentar,numLegislatura,deputado_nome, deputado_partido,deputado_uf)
    
    names(listadeputados)<-c("Id","Nome Parlamentar","Legislatura","Nome","Partido", "UF")
    listadeputados<-listadeputados[listadeputados$Legislatura==legislatura,]
    
    
    return(listadeputados)
    
    
}    


obterDetalheDeputado <- function(nomeArquivo) {
    
    require(XML)    
    
    trim <- function( x ) {
        gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
    }
       
    
    doc = xmlTreeParse(nomeArquivo , useInternalNodes = T)
    
    
    ideCadastro <- xpathApply(doc, path = '//Deputados/Deputado/ideCadastro', xmlValue)
    ideCadastro<-cbind(ideCadastro)
    ideCadastro<-do.call(rbind.data.frame,ideCadastro)
    
    
    nomeParlamentarAtual <- xpathApply(doc, path = '//Deputados/Deputado/nomeParlamentarAtual', xmlValue) 
    nomeParlamentarAtual<-cbind(nomeParlamentarAtual)
    nomeParlamentarAtual<-do.call(rbind.data.frame,nomeParlamentarAtual)
    
    numLegislatura <- xpathApply(doc, path = '//Deputados/Deputado/numLegislatura', xmlValue) 
    numLegislatura<-cbind(numLegislatura)
    numLegislatura<-do.call(rbind.data.frame,numLegislatura)
    
    
    nomeCivil <- xpathApply(doc, path = '//Deputados/Deputado/nomeCivil', xmlValue)
    nomeCivil<-cbind(nomeCivil)
    nomeCivil<-do.call(rbind.data.frame,nomeCivil)
    
    
    partidoAtual <- xpathApply(doc, path = '//Deputados/Deputado/partidoAtual/sigla', xmlValue)  
    partidoAtual<-cbind(partidoAtual)  
    partidoAtual<-do.call(rbind.data.frame,partidoAtual)
    
    
    ufRepresentacaoAtual <- xpathApply(doc, path = '//Deputados/Deputado/ufRepresentacaoAtual', xmlValue)  
    ufRepresentacaoAtual<-cbind(ufRepresentacaoAtual)   
    ufRepresentacaoAtual<-do.call(rbind.data.frame,ufRepresentacaoAtual)
    
    
    dataNascimento <- xpathApply(doc, path = '//Deputados/Deputado/dataNascimento', xmlValue)  
    dataNascimento<-cbind(dataNascimento)   
    dataNascimento<-do.call(rbind.data.frame,dataNascimento)
    
    sexo <- xpathApply(doc, path = '//Deputados/Deputado/sexo', xmlValue)  
    sexo<-cbind(sexo)   
    sexo<-do.call(rbind.data.frame,sexo)
    
    nomeProfissao <- xpathApply(doc, path = '//Deputados/Deputado/nomeProfissao', xmlValue)  
    nomeProfissao<-cbind(nomeProfissao)   
    nomeProfissao<-do.call(rbind.data.frame,nomeProfissao)
    
    email <- xpathApply(doc, path = '//Deputados/Deputado/email', xmlValue)  
    email<-cbind(email)   
    email<-do.call(rbind.data.frame,email)
    
    
    
    listadeputados<-NULL
    listadeputados<-cbind(numLegislatura,ideCadastro, nomeParlamentarAtual,nomeCivil,dataNascimento,sexo,partidoAtual, ufRepresentacaoAtual,nomeProfissao,email)
    
    names(listadeputados)<-c("numLegislatura","ideCadastro","nomeParlamentarAtual","nomeCivil","dataNascimento","sexo","partidoAtual","ufRepresentacaoAtual","nomeProfissao","email")
    
    
    return(listadeputados)
    
    
}    


obterListaDetalhadaDeputado <- function(legislatura){
    
    require(data.table)
      
    pasta.base<-paste('app/dados/brutos/dados_deputado/',legislatura,sep = '')
    file.pattern<-'deputado_'
    
    files.v <- dir(path=pasta.base, pattern=file.pattern)
    
    listaDetalhadaDeputados<-NULL
    
    for(i in 1:length(files.v)){
        arquivo<-paste(pasta.base,'/',files.v[i],sep = '')
        deputado<-obterDetalheDeputado(arquivo)
        listaDetalhadaDeputados<-rbind(listaDetalhadaDeputados,deputado)
     }
    
    
    
    return(listaDetalhadaDeputados)
    
    
}


obterDadosCandidatos<-function(legislatura) {
    
    pasta.base <- "app/dados/brutos/lista_candidatos/"    
    pasta.origem<-paste(pasta.base,legislatura,sep = '')
    
    
    fileList <- list.files(path=pasta.origem, pattern=".txt")
    fileList <- paste(pasta.origem,"/",fileList,sep = '')
        
    
    listaCandidatos<-NULL
    
        
    for (i in fileList){
        
        f<-read.csv(i,sep=';',header = FALSE, )
        
        f<-f[f$V10=='DEPUTADO FEDERAL',]
        f<-f[f$V42=='ELEITO' | f$V42=='SUPLENTE',]
        
        c<- data.frame(iconv(f$V11, to='ASCII//TRANSLIT'),f$V14,f$V31,f$V35)
        listaCandidatos<-rbind(listaCandidatos,c)
        
    }
    
    names(listaCandidatos)<- c("NOME_CANDIDATO","NOME_URNA_CANDIDATO","COD_GRAU_INSTRUCAO","CODIGO_ESTADO_CIVIL")
    write.csv(listaCandidatos, paste("cand",legislatura,".csv",sep = ''), row.names=FALSE)
    
    
    return(listaCandidatos)
    
}


obterDadosCompletosDeputados <- function( legislatura ){
    
   
    unwanted_array = list(    'Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                              'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                              'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                              'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                              'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )
    
    if(!require(data.table)){install.packages("data.table")}
    
    library("data.table")
    
    listaDeputados<-obterListaDetalhadaDeputado(legislatura)
    listaCandidatos<-obterDadosCandidatos(legislatura)
    
    
    #Removendo caracteres acentuados
    
    listaDeputados$nomeParlamentarAtual<-chartr(paste(names(unwanted_array), collapse=''),
                                              paste(unwanted_array, collapse=''),
                                              listaDeputados$nomeParlamentarAtual)
    
    
    listaDeputados$nomeCivil<-chartr(paste(names(unwanted_array), collapse=''),
                                  paste(unwanted_array, collapse=''),
                                  listaDeputados$nomeCivil)
    
    listaCandidatos$NOME_CANDIDATO<-chartr(paste(names(unwanted_array), collapse=''),
                                               paste(unwanted_array, collapse=''),
                                               listaCandidatos$NOME_CANDIDATO)
    
    
    listaCandidatos$NOME_URNA_CANDIDATO<-chartr(paste(names(unwanted_array), collapse=''),
                                  paste(unwanted_array, collapse=''),
                                  listaCandidatos$NOME_URNA_CANDIDATO)
    
    
    names(listaCandidatos)[1]<-"Nome"
    
    names(listaCandidatos)[2]<-"Nome Parlamentar"
    
    
    names(listaDeputados)[4]<-"Nome"
    
    names(listaDeputados)[3]<-"Nome Parlamentar"
    
    out <- tryCatch(
{
    
    listaCandidatos[listaCandidatos$"Nome Parlamentar"=="GAROTINHO",][2]<-"ANTHONY GAROTINHO"
    listaCandidatos[listaCandidatos$"Nome Parlamentar"=="ONOFRE AGOSTINI",][2]<-"ONOFRE SANTO AGOSTINI"
    listaCandidatos[listaCandidatos$"Nome Parlamentar"=="EVANDRO MILHOMEM",][2]<-"EVANDRO MILHOMEN"
    listaCandidatos[listaCandidatos$"Nome Parlamentar"=="PRACIANO",][2]<-"FRANCISCO PRACIANO"
    listaCandidatos[listaCandidatos$"Nome Parlamentar"=="IRACEMA PORTELA",][2]<-"IRACEMA PORTELLA"
    
    
},
error=function(cond) {
    
},
finally={
    
}
    
    )        
   
    
    dt1<-data.table(listaDeputados) 
    setkeyv(dt1, 'Nome')
    dt1 <- dt1[order(Nome),] 
    
    dt2<-data.table(listaCandidatos)
    setkeyv(dt2, 'Nome')
    dt2 <- dt2[order(Nome),] 
    
    write.csv(dt1, paste("data",legislatura,".csv",sep = ''), row.names=FALSE)
    
    
    listaParcialDeputados<-merge(x = dt1, y = dt2, by = "Nome", all.x=TRUE)
    
    listaParcialDeputados$'Nome Parlamentar.y'<-NULL
    setnames(listaParcialDeputados,"Nome Parlamentar.x","Nome Parlamentar")
    
    listaDeputadosComInformacaoDemografica<-data.table(listaParcialDeputados[!is.na(listaParcialDeputados$COD_GRAU_INSTRUCAO),])
    listaDeputadosSemInformacaoDemografica<-data.table(listaParcialDeputados[is.na(listaParcialDeputados$COD_GRAU_INSTRUCAO),])
    listaDeputadosSemInformacaoDemografica<-listaDeputadosSemInformacaoDemografica[, 1:10, with = FALSE]
    
    setkeyv(listaDeputadosSemInformacaoDemografica, 'Nome Parlamentar')
    setkeyv(dt2, 'Nome Parlamentar')
    
    
    listaCompletaDeputados1<-merge(x = listaDeputadosSemInformacaoDemografica, y = dt2, by = "Nome Parlamentar", all.x=TRUE)
    listaCompletaDeputados1$'Nome.y'<-NULL
    setnames(listaCompletaDeputados1,"Nome.x","Nome")
    
    
    listaDeputados<- rbind(listaDeputadosComInformacaoDemografica,listaCompletaDeputados1)
        
    
   # t<-as.data.frame(table(listaDeputados$Nome))
    
    return(listaDeputados)
    
    
    
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


obterConteudoDiscurso <- function( filename ) {
    
    require('base64enc') 
    require(XML) 
    
    trim <- function( x ) {
        gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
    }    
    
    #doc = xmlTreeParse(filename, useInternalNodes = T)
    doc <- xmlParseDoc(filename, HUGE)       
    
    
    discurso_conteudo <- xpathApply(doc, path = '//sessao/discursoRTFBase64', xmlValue)
    
    discurso_nome_orador <- xpathApply(doc,path = '//sessao/nome', xmlValue)
    regmatches(discurso_nome_orador[[1]], gregexpr("\\(([^()]+)\\)|[[:punct:]]",discurso_nome_orador, perl=TRUE))<-''
    discurso_nome_orador<-trim(discurso_nome_orador[[1]])
    
    discurso_partido_orador <- xpathApply(doc,path = '//sessao/partido', xmlValue)
    discurso_partido_orador<-trim(discurso_partido_orador[[1]])
    
    discurso_dia_hora <- xpathApply(doc,path = '//sessao/horaInicioDiscurso', xmlValue)
    discurso_dia_hora<- discurso_dia_hora[[1]]
    
    t<-base64decode(paste(discurso_conteudo))
    
    str<- rawToChar(t, multiple = FALSE)
    
    cao<-gregexpr("\\\\s?'\\w\\w",str)
    
    cao_hexa_prefixo<-paste("0x",substr(regmatches(str, cao)[[1]],3,4),sep = '')
    
    cao_hexa__to_int<-strtoi(cao_hexa_prefixo)
    
    cao_hexa__to_int_multiple<-rawToChar(as.raw(cao_hexa__to_int),multiple =TRUE )
    
    regmatches(str, cao) <- list(cao_hexa__to_int_multiple)
    
    cao1<-gregexpr("({\\\\)(.+?)(})|(\\\\)(.+?)(\\b)|\\r|\\n|\\(Palmas.\\)",str, perl=TRUE)
    
    regmatches(str, cao1)<-''    
    
    dados<- cbind(discurso_dia_hora,discurso_nome_orador,discurso_partido_orador,calcularIndiceComplexidade(str))
    
    return(dados)
    
}


calcularIndiceComplexidade<- function(Discurso){
    
    
    require(tm)
   
    #Discurso<-'}  }'
        
    #ctrl <- list(removePunctuation = list(preserve_intra_word_dashes = TRUE),
    #             wordLengths = c(1, Inf))
    
    
    chapter.words.v <- tolower(Discurso)
    chapter.words.l <- strsplit(chapter.words.v, "\\W")
    chapter.word.v <- unlist(chapter.words.l)
    chapter.word.v <- chapter.word.v[which(chapter.word.v!="")]
    
    if(length(chapter.word.v)>0){
        text <- as.data.frame(table(chapter.word.v))
        
        
        
        #text<-as.data.frame(termFreq(PlainTextDocument(Discurso),control = ctrl))
        
        #Soma dos elementos do texto
        text.sum<-sum(text$Freq)
        #Soma do elementos do texto que possuem apenas uma ocorrência
        hapax.sum<-sum(text[text$Freq==1,]$Freq)
        #Soma dos elementos distintos do texto
        text.distinct<-nrow(text)
        
        
        hapax.percentage<-hapax.sum/text.sum
        text.type.ratio<-text.distinct/text.sum
    }else{
        text.sum<-0
        hapax.percentage<-0
        text.type.ratio<-0
    }
        

    
    return(cbind(hapax.percentage,text.type.ratio,text.sum))
    
}


obterListaDeDiscursos <- function( legislatura, ano ) {
    
    require(data.table)
 
    legislaturas<- obterLegislaturas()
    listaFinalCompletadiscursos<-NULL
    
    for(legislatura in legislaturas){
    
        anosLegislatura<-  obterPeriodoLegislaturas(legislatura)  
        
        for(ano in anosLegislatura$ANO_INICIO:(anosLegislatura$ANO_FIM-1)){
        
            listaDeDeputados<-obterDadosCompletosDeputados(legislatura)
            
            pasta.base<-paste('app/dados/brutos/conteudo_discursos/',legislatura,sep = '')
            file.pattern<-paste('discurso_',ano, sep = '')
            
            files.v <- dir(path=pasta.base, pattern=file.pattern)
            
            conteudo.discursos<-NULL
            
            for(i in 1:length(files.v)){
                arquivo<-paste(pasta.base,'/',files.v[i],sep = '')
                conteudo<-cbind(obterConteudoDiscurso(arquivo),ano)
                conteudo.discursos<-rbind(conteudo.discursos,conteudo)
                print(i)
            }
            
            conteudo.discursos<-as.data.frame(conteudo.discursos)
            names(conteudo.discursos)
            
            conteudo.discursos<-conteudo.discursos[conteudo.discursos$discurso_partido_orador!='\n\n ',]
            
            listaDpt<-data.table(listaDeDeputados) 
            setkeyv(listaDpt, 'Nome Parlamentar')
            
            names(conteudo.discursos)[2]<-'Nome Parlamentar'
            listaDiscur<-data.table(conteudo.discursos)
            setkeyv(listaDiscur, 'Nome Parlamentar')
            
            listaCompletadiscursos<-merge(x =listaDiscur  , y =listaDpt  , by = "Nome Parlamentar", all.x=TRUE, allow.cartesian =TRUE)
            
            listaCompletadiscursos$'discurso_partido_orador'<-NULL
            setnames(listaCompletadiscursos,'Nome Parlamentar','Nome_Orador')
            setnames(listaCompletadiscursos,'discurso_dia_hora','Dia_Hora')
            #setnames(listaCompletadiscursos,'str','Conteudo')
            
            listaCompletadiscursos<-listaCompletadiscursos[!is.na(listaCompletadiscursos$Nome),]
            listaCompletadiscursos<-listaCompletadiscursos[as.numeric(as.character(listaCompletadiscursos$text.sum))>100,]
            
            Sys.setlocale("LC_TIME", "C")
            
            require('chron')
            listaCompletadiscursos$dataNascimento<-as.Date(as.Date(listaCompletadiscursos$dataNascimento, format="%d/%m/%Y"))
            idade<-ano-as.numeric(format(listaCompletadiscursos$dataNascimento,"%Y"))
            listaCompletadiscursos<-cbind(listaCompletadiscursos,as.data.frame(idade))
            
            listaFinalCompletadiscursos<-rbind(listaFinalCompletadiscursos,listaCompletadiscursos)
        
        }
    
    
    }
    
    
    
    
    
    
        
    listaFinalCompletadiscursos<-listaFinalCompletadiscursos[!is.na(listaFinalCompletadiscursos$COD_GRAU_INSTRUCAO),]
    
    
    
    tab<-as.data.frame(table(lista$Nome_Orador))
    
    require(ggplot2)
   
    hist(x =as.numeric(as.character(listaCompletadiscursos$text.type.ratio)) )
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
    
}








