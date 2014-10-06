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
  
  doc = xmlTreeParse(filename, useInternalNodes = T)
  
  idNodes <- getNodeSet(doc, "//sessao")
  
  discurso_conteudo <- lapply(idNodes, xpathApply, path = 'discursoRTFBase64', xmlValue)
  
  library('base64enc')
  
  t<-base64decode(paste(discurso_conteudo))
  
  str<- rawToChar(t, multiple = FALSE)
  
  cao<-gregexpr("\\\\s?'\\w\\w",str)
  
  cao_hexa_prefixo<-paste("0x",substr(regmatches(str, cao)[[1]],3,4),sep = '')
  
  cao_hexa__to_int<-strtoi(cao_hexa_prefixo)
  
  cao_hexa__to_int_multiple<-rawToChar(as.raw(cao_hexa__to_int),multiple =TRUE )
  
  regmatches(str, cao) <- list(cao_hexa__to_int_multiple)
  
  cao1<-gregexpr("({\\\\)(.+?)(})|(\\\\)(.+?)(\\b)|\\r|\\n",str, perl=TRUE)
  
  regmatches(str, cao1)<-''
  
  return(str)

}



setwd('C:/R')
require(XML)

input.ano<-''
input.sessao<-'125.2.54.O'

dir<-'discursos'
file.pattern<-paste('*',input.sessao,'*.xml', sep = '')
files.v <- dir(path=dir, pattern="*.xml")

conteudo.discursos<-''


for(i in 1:length(files.v)){
  
  conteudo.discursos<-paste(conteudo.discursos,obterConteudoDiscurso(paste('discursos/',files.v[i],sep = '')))
  
}


str<-conteudo.discursos


#install.packages('tm')
library(tm)

myCorpus = Corpus(VectorSource(str))
myCorpus = ?tm_map(myCorpus, content_transformer(tolower))
myCorpus = tm_map(myCorpus, removePunctuation)
myCorpus = tm_map(myCorpus, removeNumbers)
myCorpus = tm_map(myCorpus, removeWords,c("nos","essas","num","est?o","esses","pois","est?","muitas","tenho","pois","toda","eles","todo","nesse","nossos","dessa","alguns","desse","tudo","t?o","assim","quanto","menos","portanto","pronunciamento","sra","orador","onde","seja","segundo","essa","deputados","obrigado","srs","sras","mesmo","todas","obrigado","obrigada","quero","esta","aqui","foram","suas","al?m","bem","pela","at?","agora","foi","grande","n?s","minha","das","ainda","meu","nesta","contra","este","s?o","todos","aos","seus","isso","sobre","pelo","seu","nosso","muito","por","quando","mas","cada","apenas", "n?o","sim","ent?o","nossa","neste","ele","tamb?m","porque","presidente","mais","de", "para", "da", "?","com","uma","um","essa","esse","sua","como","que","dos","sem","entre","nas"))


myDTM = TermDocumentMatrix(myCorpus)
m = as.table(myDTM)
t<-sort(rowSums(m), decreasing = TRUE)


#install.packages('wordcloud')
library(wordcloud)


wordcloud(names(t), t, scale=c(2,0.5),
                min.freq = 1, max.words=600,
                colors=brewer.pal(8, "Dark2"))

