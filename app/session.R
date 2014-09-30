setwd('C:/R')
require(XML)

doc = xmlTreeParse("ListarDiscursosPlenario.xml", useInternalNodes = T)

idNodes <- getNodeSet(doc, "//sessao")



idNodes <- getNodeSet(doc, "//discursos")
sessao_codigo <- lapply(idNodes, xpathApply, path = '../../../codigo', xmlValue)
sessao_data <- lapply(idNodes, xpathApply, path = '../../../data', xmlValue)

orador_nome <- lapply(idNodes, xpathApply, path = 'discurso/orador/nome', xmlValue)
orador_partido <- lapply(idNodes, xpathApply, path = 'discurso/orador/partido', xmlValue)
orador_uf <- lapply(idNodes, xpathApply, path = 'discurso/orador/uf', xmlValue)
orador_numero <- lapply(idNodes, xpathApply, path = 'discurso/orador/numero', xmlValue)


discurso_quarto <- lapply(idNodes, xpathApply, path = 'discurso/numeroQuarto', xmlValue)
discurso_insercao <- lapply(idNodes, xpathApply, path = 'discurso/numeroInsercao', xmlValue)
discurso_sumario <- lapply(idNodes, xpathApply, path = 'discurso/sumario', xmlValue)


discursos<- do.call(rbind.data.frame, mapply(cbind, sessao_codigo,sessao_data,orador_numero, orador_nome,orador_partido,orador_uf,discurso_quarto,discurso_insercao,discurso_sumario))
head(discursos,20)

fm2 <- lm(tlimth ~ sex * ethnicty, data = tli)
library('xtable')
