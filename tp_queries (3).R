library(RColorBrewer)
library(VennDiagram)
library(dplyr)
library(highcharter)
library(ggplot2)
library(plotly)
library(ggridges)
library(plotrix)
library(prettydoc)
library(readxl)

dataset <- read_excel("C:/Users/Usuario/Desktop/Timo Facu/Segundo año/1er Cutri/AyED R/dataset.xlsx")
dataset$SEXO <- factor(dataset$SEXO, levels=c("MASC", "FEME"), labels=c("M","F"))
##diabetes cual fue la cirugia más hecha
diabetic<- dataset[which(dataset$DIABETES=="1"),22]
mostDoneSurgeryEnDiabeticos<- tail(names(sort(table(diabetic$'PROCEDIMIENTO'))), 1)




##obesos cual fue la cirugia más hecha
obesidad<- dataset[which(dataset$`OBESIDAD MORBIDA`=="1"),22]
mostDoneSurgeryEnObesos<- tail(names(sort(table(obesidad$'PROCEDIMIENTO'))), 1)




##hipertensionados cual fue la cirugia más hecha
hipertension<- dataset[which(dataset$`TAS INGRESO RCV` >= 140 | dataset$`TAD INGRESO RCV` >= 90),22]
mostDoneSurgeryEnHipertensionados<- tail(names(sort(table(hipertension$'PROCEDIMIENTO'))), 1)



ejeX <- c(rep("Diabetes" , 3) , rep("Hipertension" , 3) , rep("Obesidad" , 3) )
Procedimientos <- rep(c("ANGIOPLASTIA" , "CIRUGIA " , "ENDOVALVULA ") , 3)
ejeY <- c(nrow(diabetic[which(diabetic$PROCEDIMIENTO == "ANGIOPLASTIA"),]), 
          nrow(diabetic[which(diabetic$PROCEDIMIENTO == "CIRUGIA"),]), 
          nrow(diabetic[which(diabetic$PROCEDIMIENTO == "ENDOVALVULA"),])
          ,  nrow(hipertension[which(hipertension$PROCEDIMIENTO == "ANGIOPLASTIA"),]), 
          nrow(hipertension[which(hipertension$PROCEDIMIENTO == "CIRUGIA"),]), 
          nrow(hipertension[which(hipertension$PROCEDIMIENTO == "ENDOVALVULA"),])
          , nrow(obesidad[which(obesidad$PROCEDIMIENTO == "ANGIOPLASTIA"),]), 
          nrow(obesidad[which(obesidad$PROCEDIMIENTO == "CIRUGIA"),]), 
          nrow(obesidad[which(obesidad$PROCEDIMIENTO == "ENDOVALVULA"),])
)
data <- data.frame(ejeX,Procedimientos,ejeY)

ggplot(data, aes(fill=Procedimientos, y=ejeY, x=ejeX)) + 
  geom_bar(position="dodge", stat="identity") + 
  labs(x = "Enfermedades preexistentes", y = "Cantidad de pacientes", title = "Tipo de procedimiento por cada enfermedad") 


##media de fey
meanFey <- round(mean.default(na.omit(dataset$'FEY')),2)

ggplot(dataset, aes(x = FEY, fill = SEXO)) +
  geom_density(alpha = 0.8) +
  theme_ridges() +
  scale_fill_discrete(name = "Gender", labels = c("Male","Female")) +
  theme(legend.position = "top") +
  geom_vline(xintercept=meanFey, size=1.5, color="black", show.legend = "mean") +
  geom_text(aes(x=meanFey - 5, label=paste0("Mean\n",meanFey), y=0.06))  +
  labs(y = "Density") +
  scale_fill_manual(values = c("light blue", "pink"))
  

####MAYORES A 70 HOMBRES

mayoresA70paraHombres<- dataset[which(dataset$`EDAD` >= 70 & dataset$`SEXO` == "M"),47]
mayoresA70paraHombres <- na.omit(mayoresA70paraHombres)

mayoresA70paraHombre0<- dataset[which(mayoresA70paraHombres$`NUMERO DE LESIONES` == "0"),2]
percentagemayoresA70paraHombres0<- nrow(mayoresA70paraHombre0) *100/nrow(mayoresA70paraHombres)

mayoresA70paraHombre1<- dataset[which(mayoresA70paraHombres$`NUMERO DE LESIONES` == "1"),2]
percentagemayoresA70paraHombres1<- nrow(mayoresA70paraHombre1) *100/nrow(mayoresA70paraHombres)

mayoresA70paraHombre2<- dataset[which(mayoresA70paraHombres$`NUMERO DE LESIONES` == "2"),2]
percentagemayoresA70paraHombres2<- nrow(mayoresA70paraHombre2) *100/nrow(mayoresA70paraHombres)

mayoresA70paraHombre3<- dataset[which(mayoresA70paraHombres$`NUMERO DE LESIONES` == "3"),2]
percentagemayoresA70paraHombres3<- nrow(mayoresA70paraHombre3) *100/nrow(mayoresA70paraHombres)

mayoresA70paraHombre4<- dataset[which(mayoresA70paraHombres$`NUMERO DE LESIONES` == "4"),2]
percentagemayoresA70paraHombres4<- nrow(mayoresA70paraHombre4) *100/nrow(mayoresA70paraHombres)

mayoresA70paraHombre5<- dataset[which(mayoresA70paraHombres$`NUMERO DE LESIONES` == "5"),2]
percentagemayoresA70paraHombres5<- nrow(mayoresA70paraHombre5) *100/nrow(mayoresA70paraHombres)


datamayoresA70paraHombres <- data.frame(
  x = c(0, 1, 2, 3, 4, 5),
  y = c(percentagemayoresA70paraHombres0, percentagemayoresA70paraHombres1, percentagemayoresA70paraHombres2, percentagemayoresA70paraHombres3, percentagemayoresA70paraHombres4, percentagemayoresA70paraHombres5),
  name = as.factor(c("0 lesiones","1 lesiones","2 lesiones","3 lesiones","4 lesiones","5 lesiones"))
)

hmayoresA70paraHombres <- datamayoresA70paraHombres %>%
  hchart(
    "pie", hcaes(x = name, y = y),
    name = "Porcentaje de pacientes") %>%
  hc_title(
    text = "Cantidad de lesiones para hombres mayores A 70 años",
    margin = 20,
    align = "center",
    style = list(color = "#22A884", useHTML = TRUE) 
  ) %>% 
  hc_colors(c("blue", "red", "green", "yellow", "orange", "black"))


hmayoresA70paraHombres



##MAYORES A 70 MUJERES


mayoresA70paraMujeres<- dataset[which(dataset$`EDAD` >= 70 & dataset$`SEXO` == "F"),47]
mayoresA70paraMujeres <- na.omit(mayoresA70paraMujeres)

mayoresA70paraMujeres0<- dataset[which(mayoresA70paraMujeres$`NUMERO DE LESIONES` == "0"),2]
percentagemayoresA70paraMujeres0<- nrow(mayoresA70paraMujeres0) *100/nrow(mayoresA70paraMujeres)

mayoresA70paraMujer1<- dataset[which(mayoresA70paraMujeres$`NUMERO DE LESIONES` == "1"),2]
percentagemayoresA70paraMujeres1<- nrow(mayoresA70paraMujer1) *100/nrow(mayoresA70paraMujeres)

mayoresA70paraMujeres2<- dataset[which(mayoresA70paraMujeres$`NUMERO DE LESIONES` == "2"),2]
percentagemayoresA70paraMujeres2<- nrow(mayoresA70paraMujeres2) *100/nrow(mayoresA70paraMujeres)

mayoresA70paraMujeres3<- dataset[which(mayoresA70paraMujeres$`NUMERO DE LESIONES` == "3"),2]
percentagemayoresA70paraMujeres3<- nrow(mayoresA70paraMujeres3) *100/nrow(mayoresA70paraMujeres)

mayoresA70paraMujeres4<- dataset[which(mayoresA70paraMujeres$`NUMERO DE LESIONES` == "4"),2]
percentagemayoresA70paraMujeres4<- nrow(mayoresA70paraMujeres4) *100/nrow(mayoresA70paraMujeres)

mayoresA70paraMujeres5<- dataset[which(mayoresA70paraMujeres$`NUMERO DE LESIONES` == "5"),2]
percentagemayoresA70paraMujeres5<- nrow(mayoresA70paraMujeres5) *100/nrow(mayoresA70paraMujeres)


datamayoresA70paraMujeres <- data.frame(
  x = c(0, 1, 2, 3, 4, 5),
  y = c(percentagemayoresA70paraMujeres0, percentagemayoresA70paraMujeres1, percentagemayoresA70paraMujeres2, percentagemayoresA70paraMujeres3, percentagemayoresA70paraMujeres4, percentagemayoresA70paraMujeres5),
  name = as.factor(c("0 lesiones","1 lesiones","2 lesiones","3 lesiones","4 lesiones","5 lesiones"))
)

hmayoresA70paraMujeres <- datamayoresA70paraMujeres %>%
  hchart(
    "pie", hcaes(x = name, y = y),
    name = "Porcentaje de pacientes") %>%
  hc_title(
    text = "Cantidad de lesiones para mujeres mayores A 70 años",
    margin = 20,
    align = "center",
    style = list(color = "#22A884", useHTML = TRUE) 
  ) %>% 
  hc_colors(c("blue", "red", "green", "yellow", "orange", "black"))


hmayoresA70paraMujeres





##MENORES A 70 HOMBRES

menoresA70paraHombres<- dataset[which(dataset$EDAD < 70 & dataset$SEXO == "M"),47]
menoresA70paraHombres <- na.omit(menoresA70paraHombres)

menoresA70paraHombre0<- dataset[which(menoresA70paraHombres$`NUMERO DE LESIONES` == "0"),2]
percentagemenoresA70paraHombres0<- nrow(menoresA70paraHombre0) *100/nrow(menoresA70paraHombres)

menoresA70paraHombre1<- dataset[which(menoresA70paraHombres$`NUMERO DE LESIONES` == "1"),2]
percentagemenoresA70paraHombres1<- nrow(menoresA70paraHombre1) *100/nrow(menoresA70paraHombres)

menoresA70paraHombre2<- dataset[which(menoresA70paraHombres$`NUMERO DE LESIONES` == "2"),2]
percentagemenoresA70paraHombres2<- nrow(menoresA70paraHombre2) *100/nrow(menoresA70paraHombres)

menoresA70paraHombre3<- dataset[which(menoresA70paraHombres$`NUMERO DE LESIONES` == "3"),2]
percentagemenoresA70paraHombres3<- nrow(menoresA70paraHombre3) *100/nrow(menoresA70paraHombres)

menoresA70paraHombre4<- dataset[which(menoresA70paraHombres$`NUMERO DE LESIONES` == "4"),2]
percentagemenoresA70paraHombres4<- nrow(menoresA70paraHombre4) *100/nrow(menoresA70paraHombres)

menoresA70paraHombre5<- dataset[which(menoresA70paraHombres$`NUMERO DE LESIONES` == "5"),2]
percentagemenoresA70paraHombres5<- nrow(menoresA70paraHombre5) *100/nrow(menoresA70paraHombres)


datamenoresA70paraHombres <- data.frame(
  x = c(0, 1, 2, 3, 4, 5),
  y = c(percentagemenoresA70paraHombres0, percentagemenoresA70paraHombres1, percentagemenoresA70paraHombres2, percentagemenoresA70paraHombres3, percentagemenoresA70paraHombres4, percentagemenoresA70paraHombres5),
  name = as.factor(c("0 lesiones","1 lesiones","2 lesiones","3 lesiones","4 lesiones","5 lesiones"))
)

hmenoresA70paraHombres <- datamenoresA70paraHombres %>%
  hchart(
    "pie", hcaes(x = name, y = y),
    name = "Porcentaje de pacientes") %>%
    hc_title(
    text = "Cantidad de lesiones para Hombres menores A 70 años",
    margin = 20,
    align = "center",
    style = list(color = "#22A884", useHTML = TRUE)
  ) %>% 
  hc_colors(c("blue", "red", "green", "yellow", "orange", "black"))


hmenoresA70paraHombres



##MENORES A 70 MUJERES




menoresA70paraMujeres<- dataset[which(dataset$EDAD < 70 & dataset$SEXO == "F"),47]
menoresA70paraMujeres<- na.omit(menoresA70paraMujeres)

menoresA70paraMujeres0<- dataset[which(menoresA70paraMujeres$`NUMERO DE LESIONES` == "0"),2]
percentagemenoresA70paraMujeres0<- nrow(menoresA70paraMujeres0) *100/nrow(menoresA70paraMujeres)

menoresA70paraMujeres1<- dataset[which(menoresA70paraMujeres$`NUMERO DE LESIONES` == "1"),2]


percentagemenoresA70paraMujeres1<- nrow(menoresA70paraMujeres1) *100/nrow(menoresA70paraMujeres)


menoresA70paraMujeres2<- dataset[which(menoresA70paraMujeres$`NUMERO DE LESIONES` == "2"),2]


percentagemenoresA70paraMujeres2<- nrow(menoresA70paraMujeres2) *100/nrow(menoresA70paraMujeres)


menoresA70paraMujeres3<- dataset[which(menoresA70paraMujeres$`NUMERO DE LESIONES` == "3"),2]


percentagemenoresA70paraMujeres3<- nrow(menoresA70paraMujeres3) *100/nrow(menoresA70paraMujeres)


menoresA70paraMujeres4<- dataset[which(menoresA70paraMujeres$`NUMERO DE LESIONES` == "4"),2]


percentagemenoresA70paraMujeres4<- nrow(menoresA70paraMujeres4) *100/nrow(menoresA70paraMujeres)

menoresA70paraMujeres5<- dataset[which(menoresA70paraMujeres$`NUMERO DE LESIONES` == "5"),2]


percentagemenoresA70paraMujeres5<- nrow(menoresA70paraMujeres5) *100/nrow(menoresA70paraMujeres)


datamenoresA70paraMujeres <- data.frame(
  x = c(0, 1, 2, 3, 4, 5),
  y = c(percentagemenoresA70paraMujeres0, percentagemenoresA70paraMujeres1, percentagemenoresA70paraMujeres2, percentagemenoresA70paraMujeres3, percentagemenoresA70paraMujeres4, percentagemenoresA70paraMujeres5),
  name = as.factor(c("0 lesiones","1 lesiones","2 lesiones","3 lesiones","4 lesiones","5 lesiones"))
)

hmenoresA70paraMujeres <- datamenoresA70paraMujeres %>%
  hchart(
    "pie", hcaes(x = name, y = y),
    name = "Porcentaje de pacientes") %>%
  hc_title(
    text = "Cantidad de lesiones para mujeres menores A 70 años",
    margin = 20,
    align = "center",
    style = list(color = "#22A884", useHTML = TRUE) 
  ) %>% 
  hc_colors(c("blue", "red", "green", "yellow", "orange", "black"))


hmenoresA70paraMujeres





##Los que tienen enfermedades preexistentes, tienen alguna realción con el tipo de complicaciones?

ep<- dataset[which(dataset$DIABETES=="1" | dataset$`OBESIDAD MORBIDA`=="1" | (dataset$`TAS INGRESO RCV` >= 140 | dataset$`TAD INGRESO RCV` >= 90)),37:38]
epConComplicaciones<- ep[which(ep$`COMPLICACIONES INMEDIATAS` == "1" | ep$`COMPLICACIONES TARDIAS` == "1"),]
tuvieronComplicaciones<- dataset[which(dataset$`COMPLICACIONES INMEDIATAS` == "1" | dataset$`COMPLICACIONES TARDIAS` == "1"),]

nrow(epConComplicaciones)/nrow(tuvieronComplicaciones)   ##un 25% de los pacientes que tuvieron complicaciones tienen enfermedades preexistentes

dia<- dataset[which(dataset$DIABETES == "1" & (dataset$`COMPLICACIONES INMEDIATAS` == "1" | dataset$`COMPLICACIONES TARDIAS` == "1")),1]
hip<- dataset[which((dataset$`TAS INGRESO RCV` >= 140 | dataset$`TAD INGRESO RCV` >= 90) & (dataset$`COMPLICACIONES INMEDIATAS` == "1" | dataset$`COMPLICACIONES TARDIAS` == "1")),1]
obe<- dataset[which(dataset$`OBESIDAD MORBIDA` == "1" & (dataset$`COMPLICACIONES INMEDIATAS` == "1" | dataset$`COMPLICACIONES TARDIAS` == "1")),1]

dia<- dia$...1
hip<- hip$...1
obe<- obe$...1

# Prepare a palette of 3 colors with R colorbrewer:
myCol <- brewer.pal(4, "BuGn")

# Chart
venn.diagram(
  x = list(dia, hip, obe),
  category.names = c("Diabetes" , "Hypertension" , "Obesity"),
  filename = 'venn_diagramm.png',
  output=TRUE,
  
  # Output features
  imagetype="png" ,
  height = 650 , 
  width = 650 , 
  resolution = 300,
  compression = "lzw",
  
  # Circles
  lwd = 3,
  lty = 'blank',
  fill = myCol[2:4],
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",
  
  # Set names
  cat.cex = 0.6,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  cat.pos = c(-10, 0, 10),
  cat.dist = c(0.055, 0.055, 0.025),
  cat.fontfamily = "sans",
  rotation = 1
)








##De los que tuvieron intervencion previa cuantos tuvieron complicaciones dependiendo el tipo de cirugia
intervencionPrevia<- na.omit(dataset$`INTERVENCION PREVIA`)
intervencionPrevia<- dataset[!is.na(dataset$`INTERVENCION PREVIA`),c(4,37:38)]
intervencionPreviaConComplicaciones<- intervencionPrevia[which(intervencionPrevia$`COMPLICACIONES INMEDIATAS` == "1" | intervencionPrevia$`COMPLICACIONES TARDIAS` == "1"),]

plot_ly(alpha = 1, color=factor(intervencionPreviaConComplicaciones$`INTERVENCION PREVIA`), colors = c("red", "blue", "pink")) %>%
  add_histogram(x=intervencionPreviaConComplicaciones$`INTERVENCION PREVIA`,data=na.omit(intervencionPreviaConComplicaciones)) %>%
  layout(barmode = "overlay",showlegend = F, title = "Complicaciones segun el tipo de intervencion previa", xaxis = list(title = "Tipos de intervenciones previas"), yaxis = list(title = "Cantidad de personas")) 
  
  
  


##¿Cuál es el motivo de ingreso más común en hombres mayores a 45 años y en mujeres mayores a 55 años?
mayoresA45Hombres<- dataset[which(dataset$`EDAD` >= 45 & dataset$SEXO == "M"),5]

mayoresA55Mujeres<- dataset[which(dataset$`EDAD` >= 55 & dataset$SEXO == "F"),5]

plot_ly(alpha = 1, color=mayoresA45Hombres$`MOTIVO DE INGRESO`) %>%
  add_histogram(x=mayoresA45Hombres$`MOTIVO DE INGRESO`,data=na.omit(mayoresA45Hombres)) %>%
  layout(barmode = "overlay",showlegend = F, title = "Motivos de ingreso para hombres mayores a 45 años", xaxis = list(title = "Motivos de ingreso"), yaxis = list(title = "Cantidad de pacientes"))

plot_ly(alpha = 1, color=mayoresA55Mujeres$`MOTIVO DE INGRESO`) %>%
  add_histogram(x=mayoresA55Mujeres$`MOTIVO DE INGRESO`,data=na.omit(mayoresA55Mujeres)) %>%
  layout(barmode = "overlay",showlegend = F, title = "Motivos de ingreso para mujeres mayores a 55 años", xaxis = list(title = "Motivos de ingreso"), yaxis = list(title = "Cantidad de pacientes"))

##De los pacientes que fueron a cirugía, cuántos fueron sometidos a una cirugía de revascularización coronaria (CRM)?

fueronACirugia<- dataset[!is.na(dataset$`TIPO DE CIRUGIA`),c(23,25)]
sometidosACRM <- fueronACirugia[fueronACirugia$CRM == "true",]

options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 2)))
total <- nrow(sometidosACRM)
percentageAA <- 100 * sum(sometidosACRM$`TIPO DE CIRUGIA` == "ANEURISMA AORTICO") / total
percentageCRM <- 100 * sum(sometidosACRM$`TIPO DE CIRUGIA` == "CRM PURA") / total
percentageOTRA <- 100 * sum(sometidosACRM$`TIPO DE CIRUGIA` == "OTRA") / total
percentagePM <- 100 * sum(sometidosACRM$`TIPO DE CIRUGIA` == "PLASTICA MITRAL") / total
percentageRVAO <- 100 * sum(sometidosACRM$`TIPO DE CIRUGIA` == "RVAO") / total
percentageRVAOA <- 100 * sum(sometidosACRM$`TIPO DE CIRUGIA` == "RVAo + Aorta") / total
percentageRVM <- 100 * sum(sometidosACRM$`TIPO DE CIRUGIA` == "RVM") / total

df <- data.frame(
  x = c(0, 1, 2, 3, 4, 5, 6),
  y = c(percentageAA, percentageCRM, percentageOTRA, percentagePM, percentageRVAO, percentageRVAOA, percentageRVM),
  name = as.factor(c("AORTIC ANEURYSM","Myocardial revascularization surgery","OTHER","PLASTICA MITRAL","RVAO","RVAo + Aorta","RVM"))
)

hc <- df %>%
  hchart(
    "pie", hcaes(x = name, y = y),
    name = "sometidos a CRM")


hc


##Cuales fueron los vasos angiplastiados mas comunes en pacientes?

personasConVasosAngioplastiados <- dataset[which(!is.na(dataset$`VASOS ANGIOPLASTIADOS`)),c(48:56) ]

CD <- group_by(personasConVasosAngioplastiados, CD) %>% summarise("count"=n())
DA <- group_by(personasConVasosAngioplastiados, DA) %>% summarise("count"=n())
CDPROXIMAL <- group_by(personasConVasosAngioplastiados, CDPROXIMAL) %>% summarise("count"=n())
CXPROXIMAL <- group_by(personasConVasosAngioplastiados, CXPROXIMAL) %>% summarise("count"=n())
LVCX <- group_by(personasConVasosAngioplastiados, LVCX) %>% summarise("count"=n())
DAPROXIMAL <- group_by(personasConVasosAngioplastiados, DAPROXIMAL) %>% summarise("count"=n())
DG <- group_by(personasConVasosAngioplastiados, DG) %>% summarise("count"=n())
TCI <- group_by(personasConVasosAngioplastiados, TCI) %>% summarise("count"=n())


# Create data
data1 <- data.frame(
  name=c("CD","DA","CDPROXIMAL","CXPROXIMAL","LVCX", "DAPROXIMAL", "DG", "TCI") ,  
  value=c(CD$count[2], DA$count[2], CDPROXIMAL$count[2], CXPROXIMAL$count[2], LVCX$count[2], DAPROXIMAL$count[2], DG$count[2], TCI$count[2]))

plot_ly(alpha = 1, color=factor(data1$name)) %>%
  add_histogram(yend=data1$value,x=data1$value) %>%
  layout(barmode = "overlay",showlegend = F, title = "Motivos de ingreso para mujeres mayores a 55 años", xaxis = list(title = "Motivos de ingreso"), yaxis = list(title = "Cantidad de pacientes"))




##TABLAS


##tabla de sus respectivos procedimientos 
table(diabetic)
table(obesidad)
table(hipertension)
##cantidad de lesiones
table(mayoresA70paraHombres)
table(menoresA70paraHombres)
table(mayoresA70paraMujeres)
table(menoresA70paraMujeres)

##media de fey
mean.default(na.omit(dataset$'FEY'))

##un 25% de los pacientes que tuvieron complicaciones tienen enfermedades preexistentes
nrow(epConComplicaciones)/nrow(tuvieronComplicaciones)

##muestra las intervenciones previas de aquellos pacientes que tambien tuvieron complicaciones 
table(intervencionPreviaConComplicaciones$`INTERVENCION PREVIA`) 

##¿Cuál es el motivo de ingreso más común en hombres mayores a 45 años y en mujeres mayores a 55 años?
table(mayoresA45Hombres)
table(mayoresA55Mujeres)

##muestra tabla de cirugias para los que fueron sometidos a CRM
table(sometidosACRM$`TIPO DE CIRUGIA`)

##muestra vasos angioplastiados de pacientes entre 50 y 70
table(personasConVasosAngioplastiados)







