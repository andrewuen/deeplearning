library(tidyverse)
library(lubridate)

#Load the data
# N = 1721489   
dataBrasil <- read.csv2(file = "Data/INFLUD21-09-03-2022.csv")

dim(dataBrasil)

#-----------------------------------------------------
# Filter data
#-----------------------------------------------------

#Has to be closed and died of covid or cured (N = 1376492)
dataBrasilFilter <- dataBrasil %>% 
  filter(DT_EVOLUCA!="",
         EVOLUCAO <= 2) 
dim(dataBrasilFilter)
  

# Diagnosed with COVID (N=1049989)
table(dataBrasilFilter$CLASSI_FIN)
dataBrasilFilter <- dataBrasilFilter %>% 
  filter(CLASSI_FIN==5)
dim(dataBrasilFilter)


# Was hospitalised  (N=1004807)
dataBrasilFilter <- dataBrasilFilter %>% 
  filter(DT_INTERNA!="") 
dim(dataBrasilFilter)


# With covid vaccination info (N=552322)
table(dataBrasilFilter$VACINA_COV, useNA = "ifany")
dataBrasilFilter <- dataBrasilFilter %>% 
  filter(VACINA_COV <= 2) 
dim(dataBrasilFilter)

# With SEX information (N=552289)
table(dataBrasilFilter$CS_SEXO, useNA = "ifany")
dataBrasilFilter <- dataBrasilFilter %>% 
  filter(CS_SEXO != "I") 
dim(dataBrasilFilter)

# With full symptoms information (N = 383765)
table(dataBrasilFilter$FEBRE, useNA = "ifany") #Fever
table(dataBrasilFilter$TOSSE, useNA = "ifany") #Cough
table(dataBrasilFilter$GARGANTA, useNA = "ifany") #Sore Throat
table(dataBrasilFilter$DISPNEIA, useNA = "ifany") #Dispnoea
table(dataBrasilFilter$DESC_RESP, useNA = "ifany") #Respiratory distress
table(dataBrasilFilter$SATURACAO, useNA = "ifany") #blood oxygen saturation < 95%
table(dataBrasilFilter$DIARREIA, useNA = "ifany") #Diarrhea
table(dataBrasilFilter$VOMITO, useNA = "ifany") #Vomit

dataBrasilFilterSym <- dataBrasilFilter %>% 
  filter(FEBRE <= 2,
         TOSSE <= 2,
         GARGANTA <= 2,
         DISPNEIA <= 2,
         DESC_RESP <= 2,
         SATURACAO <= 2,
         DIARREIA <= 2,
         VOMITO <= 2) 
dim(dataBrasilFilterSym)

# With full comorbidity information (N = 196535)
table(dataBrasilFilterSym$CARDIOPATI, useNA = "ifany") #Cardiac disease
table(dataBrasilFilterSym$HEMATOLOGI, useNA = "ifany") #Hematological disease
table(dataBrasilFilterSym$SIND_DOWN, useNA = "ifany") #Down Syndrome
table(dataBrasilFilterSym$HEPATICA, useNA = "ifany") #Liver Disease
table(dataBrasilFilterSym$ASMA, useNA = "ifany") #Asthma
table(dataBrasilFilterSym$DIABETES, useNA = "ifany") #DIABETES
table(dataBrasilFilterSym$NEUROLOGIC, useNA = "ifany") #Neuropathy
table(dataBrasilFilterSym$PNEUMOPATI, useNA = "ifany") #Pneumopathy
table(dataBrasilFilterSym$IMUNODEPRE, useNA = "ifany") #Immunodepression
table(dataBrasilFilterSym$RENAL, useNA = "ifany") #Kidney disease
table(dataBrasilFilterSym$OBESIDADE, useNA = "ifany") #Obesity

dataBrasilFilterSymCom <- dataBrasilFilterSym %>% 
  filter(CARDIOPATI <= 2,
         HEMATOLOGI <= 2,
         SIND_DOWN <= 2,
         HEPATICA <= 2,
         ASMA <= 2,
         DIABETES <= 2,
         NEUROLOGIC <= 2,
         PNEUMOPATI <= 2,
         IMUNODEPRE <= 2,
         RENAL <= 2,
         OBESIDADE <= 2) 
dim(dataBrasilFilterSymCom)

# With ICU entrance information (N = 189185)
dataBrasilFilterSymComICU <- dataBrasilFilterSymCom %>% 
  filter((UTI == 1 & DT_ENTUTI != "") | 
           UTI == 2)
dim(dataBrasilFilterSymComICU)

#-----------------------------------------------------
# Process data
#-----------------------------------------------------

#Filter variables
dataFull <- dataBrasilFilterSymComICU %>% 
  select(DT_NASC,
         CS_SEXO,
         VACINA_COV,
         DT_INTERNA,
         DT_EVOLUCA,
         EVOLUCAO,
         UTI,
         DT_ENTUTI, 
         DT_SAIDUTI,
         FEBRE,
         TOSSE,
         GARGANTA,
         DISPNEIA,
         DESC_RESP,
         SATURACAO,
         DIARREIA,
         VOMITO,
         CARDIOPATI,
         HEMATOLOGI,
         SIND_DOWN,
         HEPATICA,
         ASMA,
         DIABETES,
         NEUROLOGIC,
         PNEUMOPATI,
         IMUNODEPRE,
         RENAL,
         OBESIDADE)


#Tansform variables
dataProcessed <- dataFull %>%
  mutate(DT_SAIDUTI = if_else(DT_SAIDUTI == "" & UTI == 1, DT_EVOLUCA, DT_SAIDUTI), 
         DT_NASC = as.Date(DT_NASC, format = "%d/%m/%Y"),
         DT_INTERNA = as.Date(DT_INTERNA, format = "%d/%m/%Y"),
         AGE = round(time_length(DT_NASC %--% DT_INTERNA, "years")),
         DT_EVOLUCA = as.Date(DT_EVOLUCA, format = "%d/%m/%Y"),
         DT_ENTUTI = as.Date(DT_ENTUTI, format = "%d/%m/%Y"),   
         DT_SAIDUTI = as.Date(DT_SAIDUTI, format = "%d/%m/%Y"),
         VACINA_COV = (VACINA_COV == 1),
         EVOLUCAO = (EVOLUCAO == 2),
         UTI = (UTI == 1),
         FEBRE = (FEBRE == 1),
         TOSSE = (TOSSE == 1),
         GARGANTA = (GARGANTA == 1),
         DISPNEIA = (DISPNEIA == 1),
         DESC_RESP = (DESC_RESP == 1),
         SATURACAO = (SATURACAO == 1),
         DIARREIA = (DIARREIA == 1),
         VOMITO = (VOMITO == 1),
         CARDIOPATI = (CARDIOPATI == 1),
         HEMATOLOGI = (HEMATOLOGI == 1),
         SIND_DOWN = (SIND_DOWN == 1),
         HEPATICA = (HEPATICA == 1),
         ASMA = (ASMA == 1),
         DIABETES = (DIABETES == 1),
         NEUROLOGIC = (NEUROLOGIC == 1),
         PNEUMOPATI = (PNEUMOPATI == 1),
         IMUNODEPRE = (IMUNODEPRE == 1),
         RENAL = (RENAL == 1),
         OBESIDADE = (OBESIDADE == 1))

#Change variable names
dataProcessed <- dataProcessed %>% 
  rename(dateBirth = DT_NASC,
         age = AGE, 
         sex = CS_SEXO,
         vaccine = VACINA_COV,
         dateHosp = DT_INTERNA,
         dateEndObs = DT_EVOLUCA,
         covidDeath = EVOLUCAO,
         icu = UTI,
         dateAdmIcu = DT_ENTUTI, 
         dateDisIcu = DT_SAIDUTI,
         fever = FEBRE,
         cough = TOSSE,
         sorethroat = GARGANTA,
         dyspnoea = DISPNEIA,
         respdistress = DESC_RESP,
         oxygensat = SATURACAO,
         diarrhea = DIARREIA,
         vomit = VOMITO,
         cardio = CARDIOPATI,
         hematologic = HEMATOLOGI,
         downsyn = SIND_DOWN,
         hepatic = HEPATICA,
         asthma = ASMA,
         diabetes = DIABETES,
         neurological = NEUROLOGIC,
         pneumopathy = PNEUMOPATI,
         immuno = IMUNODEPRE,
         renal = RENAL,
         obesity = OBESIDADE) %>% 
  relocate(age, .after = dateBirth)


#filter data which is too old 

dataFinal <- dataProcessed %>% 
  filter(dateBirth >= "1900-01-01",
         dateHosp >= "2021-01-01",
         dateEndObs <= "2021-12-31")


write_csv(dataFinal, "CovidHospDataBrasil.csv")
