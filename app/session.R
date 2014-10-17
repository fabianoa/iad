library('base64enc')    


setwd('C:/R')
require(XML)

trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}




obterDiscursos <- function( x ) {
  
  
out <- tryCatch(
{
  
  urlBase<- paste ("http://www.camara.gov.br/SitCamaraWS/SessoesReunioes.asmx/obterInteiroTeorDiscursosPlenario?","codSessao=",x$V1,"&numOrador=",x$V3,"&numQuarto=", x$V7 ,"&numInsercao=",x$V8,sep = '')
  
  filename<- paste ("discursos/discurso_",x$V1,"_",x$V3,"_", x$V7 ,"_",x$V8,".xml",sep = '')
  
  download.file(urlBase,filename)
  
  Sys.sleep(x)

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
  # NOTE:
  # Here goes everything that should be executed at the end,
  # regardless of success or error.
  # If you want more than one expression to be executed, then you 
  # need to wrap them in curly brackets ({...}); otherwise you could
  # just have written 'finally=<expression>' 
  #message(paste("Processed URL:", urlBase))
  #message("Some other message at the end")
}
  )    
return(out)
     

}

urlBase<- 'http://www.camara.gov.br/sitcamaraws/SessoesReunioes.asmx/ListarDiscursosPlenario?dataIni=01/01/2012&dataFim=25/12/2012&codigoSessao=&parteNomeParlamentar=&siglaPartido=&siglaUF='

#download.file(urlBase,"ListarDiscursosPlenario.xml")


doc = xmlTreeParse("ListarDiscursosPlenario.xml", useInternalNodes = T)


idNodes <- getNodeSet(doc, "//discursos")
sessao_codigo <- lapply(idNodes, xpathApply, path = '../../../codigo', xmlValue)
sessao_codigo<-mapply(trim,sessao_codigo)

sessao_data <- lapply(idNodes, xpathApply, path = '../../../data', xmlValue)

orador_nome <- lapply(idNodes, xpathApply, path = 'discurso/orador/nome', xmlValue)
orador_partido <- lapply(idNodes, xpathApply, path = 'discurso/orador/partido', xmlValue)
orador_uf <- lapply(idNodes, xpathApply, path = 'discurso/orador/uf', xmlValue)
orador_numero <- lapply(idNodes, xpathApply, path = 'discurso/orador/numero', xmlValue)


discurso_quarto <- lapply(idNodes, xpathApply, path = 'discurso/numeroQuarto', xmlValue)
discurso_insercao <- lapply(idNodes, xpathApply, path = 'discurso/numeroInsercao', xmlValue)
discurso_sumario <- lapply(idNodes, xpathApply, path = 'discurso/sumario', xmlValue)


discursos<- do.call(rbind.data.frame, mapply(cbind, sessao_codigo,sessao_data,orador_numero, orador_nome,orador_partido,orador_uf,discurso_quarto,discurso_insercao,discurso_sumario))


for (i in seq(along=discursos[,1])){

     obterdiscursos(discursos[i,])
}


##################################################################################

obterConteudoDiscurso <- function( filename ) {
  
  require('base64enc')    
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




require(XML)
getwd()
input.ano<-''
input.sessao<-'001.1.54.N'

dir<-'app/dados/brutos/conteudo_discursos/2012'
file.pattern<-paste('*',input.sessao, sep = '')
file.pattern<-"*.xml"

files.v <- dir(path=dir, pattern=file.pattern)

conteudo.discursos<-''


for(i in 1:1){
  
  conteudo.discursos<-paste(conteudo.discursos,obterConteudoDiscurso(paste(dir,'/',files.v[i],sep = '')))
  print(i)
}

conteudo.discursos<-paste(conteudo.discursos,obterConteudoDiscurso(paste(dir,'/',files.v[2],sep = '')))




saveRDS(conteudo.discursos,file="app/dados/processados/conteudo2012.Rda")

str<-conteudo.discursos


install.packages('tm')
library(tm)

myCorpus = Corpus(VectorSource(str))
myCorpus = tm_map(myCorpus, content_transformer(tolower))
myCorpus = tm_map(myCorpus, removePunctuation)
myCorpus = tm_map(myCorpus, removeNumbers)
myCorpus = tm_map(myCorpus, removeWords,c("deputado","federal","era","dia","temos","pode","forma", "vexa","hoje","dizer","fazer","mil","anos","ter","vez","tem","desta","vai","estamos","senhoras","câmara","ser","nos","essas","num","estão","esses","pois","está","muitas","tenho","pois","toda","eles","todo","nesse","nossos","dessa","alguns","desse","tudo","tão","assim","quanto","menos","portanto","pronunciamento","sra","orador","onde","seja","segundo","essa","deputados","obrigado","srs","sras","mesmo","todas","obrigado","obrigada","quero","esta","aqui","foram","suas","além","bem","pela","até","agora","foi","grande","nós","minha","das","ainda","meu","nesta","contra","este","são","todos","aos","seus","isso","sobre","pelo","seu","nosso","muito","por","quando","mas","cada","apenas", "não","sim","então","nossa","neste","ele","também","porque","presidente","mais","de", "para", "da", "é","com","uma","um","essa","esse","sua","como","que","dos","sem","entre","nas"))


myDTM = TermDocumentMatrix(myCorpus)
m = as.table(myDTM)
t<-sort(rowSums(m), decreasing = TRUE)


#install.packages('wordcloud')
library(wordcloud)


wordcloud(names(t), t, scale=c(2,0.5),
                min.freq = 1, max.words=10,
                colors=brewer.pal(8, "Dark2"))

