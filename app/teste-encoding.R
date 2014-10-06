setwd('C:/R')
require(XML)

doc = xmlTreeParse("discursos/2012/discurso_127.2.54.O_1_42_42.xml", useInternalNodes = T)

idNodes <- getNodeSet(doc, "//sessao", sessionEncoding = "latin1")

discurso_conteudo<-'ã'

discurso_conteudo <- lapply(idNodes, xpathApply, path = 'discursoRTFBase64', xmlValue)

discurso_conteudo<-discurso_conteudo[[1]][[1]]


install.packages('stringi')

library(stringi)
stri_enc_mark(discurso_conteudo)
all(stri_enc_isutf8(discurso_conteudo))
stri_trans_general(discurso_conteudo, "Latin-ASCII")

Encoding(discurso_conteudo)


install.packages('base64enc')

library('base64enc')

t<-base64decode(discurso_conteudo)

ans = base64Decode(discurso_conteudo, "raw")

stri_enc_mark(ans)
all(stri_enc_isutf8(ans))
ans2<-stri_trans_general(ans, "ASCII-Latin")

Encoding(ans2)<-'latin1'

ans




t<-rawToChar(t, multiple = FALSE)
Encoding(t)<-'latin1'
fix(t)

tt <- iconv(t, "unknown", "latin1")


library('stringr')
str<-str_c() 



class(t)

s<-toString(g)

?base64decode

x <- "ação"
z<-paste(x,ans)

Encoding(z)
y <- charToRaw(z)
y
Encoding(y)
z<-rawToChar(y)
