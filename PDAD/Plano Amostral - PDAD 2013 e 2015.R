# Limpar o ambiente e a memória
# rm(list = ls())
# gc()

# Listar os pacotes necessários
list.of.packages <- c("data.table", "survey")

# Instalar os pacotes necessários, caso eles não estejam instalados
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Carregar os pacotes
library(data.table)
library(survey)

# Remover os objetos desnecessários
rm(list.of.packages,new.packages)

# Declarar o plano amostral da PDAD 2013 ####

# Carregar base de domicílios - PDAD 2013
pdad_dom_2013 <- fread("https://raw.githubusercontent.com/codeplanprojetos/util/master/PDAD/2013/PDAD_DOM_2013.csv",
                       dec = ",", encoding = "Latin-1")

# Carregar base de pessoas - PDAD 2013
pdad_pes_2013 <- fread("https://raw.githubusercontent.com/codeplanprojetos/util/master/PDAD/2013/PDAD_PES_2013.csv",
                       dec = ",", encoding = "Latin-1")

# Unir informações de pessoas e domicílios, desconsiderando as variáveis comuns
pdad_2013 <- merge(pdad_pes_2013, 
                   pdad_dom_2013[,-c(2,4:6,110:114)],
                   all.x = T,by="CD_SEQ_DOM")

# Criar um contador
pdad_2013$count <- 1

# Declarar o desenho incial
sample.pdad2013 <-
  svydesign(
    id = ~CD_SEQ_DOM,
    strata = ~ESTRATO,
    data = pdad_2013,
    weights = ~PESO_PRE,
    nest=TRUE
  )

# Criar um objeto para pós estrato
post.pop<-unique(subset(pdad_2013,select=c(POPULACAO_AJUSTADA)))

# Criar a variável de frequência (a mesma variável de pós-estrato, para funcionar como id e peso)
post.pop$Freq <- post.pop$POPULACAO_AJUSTADA

# Declarar o objeto de pós estrato
svy2013 <- postStratify(sample.pdad2013,~POPULACAO_AJUSTADA,post.pop)

# Ajustar estratos com apenas uma UPA (adjust=centered)
options( survey.lonely.psu = "adjust")

# Verificar total da população
svytotal(~count,svy2013) # 2.786.684.

# Verificar total da população por sexo
svyby(~count,~TP_MOR_SEXO,svy2013,svytotal) # 1.338.352 homens, 1.448.332 mulheres.

# Declarar o plano amostral da PDAD 2015 ####

# Carregar a base de domicílios
pdad_dom_2015 <- fread("https://raw.githubusercontent.com/codeplanprojetos/util/master/PDAD/2015/PDAD_DOM_2015.csv",
                       dec = ",", encoding = "Latin-1")

# Carregar base de pessoas - PDAD 2015
pdad_pes_2015 <- fread("https://raw.githubusercontent.com/codeplanprojetos/util/master/PDAD/2015/PDAD_PES_2015.csv",
                       dec = ",", encoding = "Latin-1")

# Unir informações de pessoas e domicílios, desconsiderando as variáveis comuns
pdad_2015 <- merge(pdad_pes_2015, 
                   pdad_dom_2015[,-c(3:6,102:105)],
                       all.x = T,by="CD_SEQ_DOM")

# Criar um contador
pdad_2015$count <- 1

# Declarar o desenho incial
sample.pdad2015 <-
  svydesign(
    id = ~CD_SEQ_DOM,
    strata = ~ESTRATO,
    data = pdad_2015,
    weights = ~PESO_PRE,
    nest=TRUE
  )

# Criar um objeto para pós estrato
post.pop<-unique(subset(pdad_pes_2015,select=c(POPULACAO_AJUSTADA)))

# Criar a variável de frequência (a mesma variável de pós-estrato, para funcionar como id e peso)
post.pop$Freq <- post.pop$POPULACAO_AJUSTADA

# Declarar o objeto de pós-estrato
svy2015 <- postStratify(sample.pdad2015,~POPULACAO_AJUSTADA,post.pop)

# Ajustar estratos com apenas uma UPA (adjust=centered)
options( survey.lonely.psu = "adjust")

# Verificar total da população
svytotal(~count,svy2015) # 2.906.855.

# Verificar total da população por sexo
svyby(~count,~D04_MOR_SEXO,svy2015,svytotal) # 1.391.640 homens, 1.515.215  mulheres.

# Remover objetos
# rm(list = ls())
# gc()