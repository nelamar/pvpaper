-------------ANALISIS PAPER CENSO-------------------------------# 
  
rm(list=ls())

library(lattice)
library(permute)
library(vegan)

censo<-read.csv("C:/IAA/Cav.pv.24/paper.censo.pv/Abun.1.csv", header=T)
View(censo)
head(censo)

install.packages("dplyr")
library("dplyr")

censo3<-dplyr::tbl_df(censo)# aca transforma el dataframe en otro formato mas facil para trabajar

conteo<-select(censo3,CA,GA,GC, PG, SK)
zonas<-select(censo3,Idzona)
View(zonas)
View(conteo)

dd.orden3<-metaMDS (conteo, distance= "bray" )
dd.orden3
plot(dd.orden3, display = "sites", type = "text")

par(mfcol=c(1,1)) 
letras <- atach$Idzona # 
numeros<- as.integer(letras) # Asignar un numero a cada categoria de Zona (vector numerico de la misma longitud)
dd.orden$points # Extraer coordenadas de cada punto
addzonas <- cbind(dd.orden$points,numeros) # Crea nueva matriz con las coordenadas de cada punto y su zonas correspondiente
plot(dd.orden,type="n") # Ocultar los puntos
points(x=addzonas[,1], y=addzona[,2], pch=addzona[,3], col=addzona[,3]) # Mostrar en la grafica las coordenadas de cada punto dando forma y color a cada tipo de zona 
legend(x="bottomleft", col=1:5, pch=1:5) 

#Anosim por zonas entre ISS y PA#

conteo.dist<-vegdist( conteo, method= "bray" ) # aca arma la matriz de bray curtis 
attach(zonas)
dd.ano<- anosim( conteo.dist, grouping= Idzona, distance= "bray")
summary(dd.ano)
plot(dd.ano)

# ANOSIM statistic R: 0.0938  este R indica que hay diferencias pero no son independientes, son indeptes si R es mayor a 0,75, en realidad son bastante similares,
#Significance: 0.001  # esto tambien rechaza la ho de que los ensambles son iguales o no hay diferencias 

#Prueba Adonis#PERMANOVA 

adonis(dd~ Zona, data=zonasdiet, permutations=99)# dd es la matriz de datos en crudo y data (ej zonasdiet) de donde vas a sacar las variables "explicativas"

Df SumsOfSqs MeanSqs F.Model    R2 Pr(>F)   
Zona        1     3.676  3.6760  22.117 0.139   0.01 **
  Residuals 137    22.770  0.1662         0.861          
Total     138    26.446                 1.000          
---
  
  #SIMPER resultado 
  
  (sim3 <- with(zonasdiet, simper(dd,Zona)))
summary(sim3)

Contrast: GU _PV 

average       sd  ratio      ava      avb cumsum
Nc.M.    0.2183074 0.189535 1.1518 83.69000 54.69132 0.4270
Tn.M.    0.1105056 0.146933 0.7521  0.63794 22.16026 0.6431
Ha.M.    0.0739918 0.121573 0.6086 12.32063  4.90750 0.7878
Nnu.M.   0.0510224 0.091165 0.5597  0.95698  9.59974 0.8876
Ggib.M.  0.0207723 0.065728 0.3160  0.06619  4.17000 0.9282
Pcha.M.  0.0116502 0.047444 0.2456  0.00000  2.33868 0.9510
Nr.      0.0108455 0.049564 0.2188  2.17524  0.00000 0.9722
Pb.M.    0.0107534 0.037050 0.2902  0.00000  2.16197 0.9932
Ccarl.M. 0.0024282 0.013038 0.1862  0.00000  0.52382 0.9980
Ecarl.   0.0004448 0.003503 0.1270  0.08921  0.00000 0.9988
Eant.    0.0003103 0.001803 0.1721  0.06222  0.00000 0.9994
Cwill.M. 0.0002820 0.002442 0.1155  0.00000  0.05763 1.0000
Permutation: free
Number of permutations: 0

#--------------------------------# ENTRE todas las colonias #---------------------------------------------------------------------#

dd.dist<-vegdist( dd, method= "bray" ) # aca arma la matriz de bray curtis 
attach(zonasdiet)
dd.ano2<- anosim( dd.dist, grouping= Colonia, distance= "bray")
summary(dd.ano2)# R=0,28 (0,05????) , P=0,009 
plot(dd.ano2)

par(mfcol=c(1,1)) 
plot(dd.orden,type="n")
letras2 <- zonasdiet$Colonia # 
numeros2 <- as.integer(letras2) # Asignar un numero a cada categoria de colonia(vector numerico de la misma longitud)
dd.orden$points # Extraer coordenadas de cada punto
points(x=dd.orden$points[,1], y=dd.orden$points[,2]) # Mostrar en la grafica las coordenadas de cada punto
addcolonias <- cbind(dd.orden$points,numeros2) # Creamos nueva matriz con las coordenadas de cada punto y su zonas correspondiente

plot(dd.orden,type="n")# Ocultar los puntos
points(x=addcolonias[,1], y=addcolonias[,2], pch=addcolonias[,3], col=addcolonias[,3]) # Mostrar en la grafica las coordenadas de cada punto dando forma y color a cada colonia 
legend(x="bottomleft", legend=c(  "1"," 2", " 3 ",  "CH ","IM", "PP"), col=1:6, pch=1:6) 

adonis(dd~ Colonia, data=zonasdiet, permutations=99)

#         Df SumsOfSqs MeanSqs F.Model      R2 Pr(>F)   
Colonia     6    7.9249 1.32082  19.148 0.46535   0.01 **
  Residuals 132    9.1052 0.06898         0.53465          
Total     138   17.0301                 1.00000    


(sim3 <- with(zonasdiet, simper(dd,Colonia)))
summary(sim3)

#----------------------ENTRE AREAS---------------------------------------------------------#	 

dd.dist<-vegdist( dd, method= "bray" ) # aca arma la matriz de bray curtis 
attach(zonasdiet)
dd.ano3<- anosim( dd.dist, grouping= Area, distance= "bray")
summary(dd.ano3)# R=0,28 (0,05????) , P=0,009 
plot(dd.ano2)

ANOSIM statistic R: 0.2543 
Significance: 0.001 

par(mfcol=c(1,1)) 
plot(dd.orden,type="n")
letras4 <- zonasdiet$Area # 
numeros4 <- as.integer(letras4) # Asignar un numero a cada categoria de colonia(vector numerico de la misma longitud)
dd.orden$points # Extraer coordenadas de cada punto
points(x=dd.orden$points[,1], y=dd.orden$points[,2]) # Mostrar en la grafica las coordenadas de cada punto
addareas <- cbind(dd.orden$points,numeros4) # Creamos nueva matriz con las coordenadas de cada punto y su zonas correspondiente

plot(dd.orden,type="n")# Ocultar los puntos
points(x=addareas[,1], y=addareas[,2], pch=addareas[,3], col=addareas[,3]) # Mostrar en la grafica las coordenadas de cada punto dando forma y color a cada colonia 
legend(x="bottomleft", legend=c(  "vacio","CH",  "GU","IM", "PP"), col=1:5, pch=1:5) 

adonis(dd~ Area, data=zonasdiet, permutations=99)

Df SumsOfSqs MeanSqs F.Model      R2 Pr(>F)   
Area        4    4.1481  1.0370  6.2324 0.15686   0.01 **
  Residuals 134   22.2970  0.1664         0.84314          
Total     138   26.4451                 1.00000         


(sim5 <- with(zonasdiet, simper(dd,Area)))
summary(sim5)

#-------------------------DENTRO DE PRIMAVERA, POR AnoS Y COLONIAS------------------------------------------------#

datadietapv<-read.csv("C:/becadbox2/estadistica/%M.pv.csv", header=T)
datadietapv

ddpv<-datadietapv
head(ddpv)

zonasdietpv<-read.csv("C:/becadbox2/estadistica/atachpv.15.06.csv", header=T)
head(zonasdietpv)

dd.ordenpv<-metaMDS (ddpv, distance= "bray")
dd.ordenpv
attach(zonasdietpv)


plot(dd.ordenpv, display = "sites", type = "text")

letras3 <- zonasdietpv$Colonia # 
numeros3 <- as.integer (letras3) # 
dd.ordenpv$points 
points(x=dd.ordenpv$points[,1], y=dd.ordenpv$points[,2]) # Mostrar en la grafica las coordenadas de cada punto
addcolonia <- cbind(dd.ordenpv$points,numeros3) 

# Creamos nueva matriz con las coordenadas de cada punto y su zonas correspondiente
par(mfcol=c(1,1)) 
plot(dd.ordenpv,type="n") # Ocultar los puntos
points(x=addcolonia[,1], y=addcolonia[,2], pch=addcolonia[,3], col=addcolonia[,3]) # Mostrar en la grafica las coordenadas de cada punto dando forma y color a cada tipo de zona 
legend(x="bottomleft",col=1:3, pch=1:3) 


dd.distpv<-vegdist( ddpv, method= "bray" ) # aca arma la matriz de bray curtis 
attach(zonasdietpv)
dd.anopv<- anosim( dd.distpv, grouping= Colonia, distance= "bray")
summary(dd.anopv)

ANOSIM statistic R: -0.01357 
Significance: 0.699 

adonis(ddpv~ Colonia, data=zonasdietpv, permutations=99)# dd es la matriz de datos en crudo y data (ej zonasdiet) de donde vas a sacar las variables "explicativas"

Df SumsOfSqs MeanSqs F.Model      R2 Pr(>F)
Colonia    3    0.4495 0.14985 0.61909 0.02515   0.77 # no diferencias 
Residuals 72   17.4273 0.24205         0.97485       
Total     75   17.8769                 1.00000       

(sim4 <- with(zonasdietpv, simper(ddpv,Colonia)))
summary(sim4)


##----------------------- Analisis a lo largo de la temporada --------------##

rm(list=ls())
spestado<-read.csv("C:/becadbox2/estadistica/spestado.csv", header=T)
spe<-spestado
head(spe)  

spe.orden<-metaMDS (spe, distance= "bray")
spe.orden

zonasestado2<-read.csv("C:/becadbox2/estadistica/zonasestado.csv", header=T)
ze<-zonasestado2 # zonasdiet mas los estados, cargar directamente esta#
head(ze)


letras <- ze$Categoria.crianza # Extraer la columna correspondiente a Forest.type
numeros <- as.integer(letras) # Asignar un numero a cada categoria de Forest.Type (vector numerico de la misma longitud)
spe.orden$points # Extraer coordenadas de cada punto
points(x=spe.orden$points[,1], y=spe.orden$points[,2]) # Mostrar en la grafica las coordenadas de cada punto
addzonas2 <- cbind(spe.orden$points,numeros) # Creamos nueva matriz con las coordenadas de cada punto y su zonas correspondiente

par(mfcol=c(1,1)) # Ventana grafica con 4 espacios
plot(spe.orden,type="n") # Ocultar los puntos
points(x=addzonas2[,1], y=addzonas2[,2], pch=addzonas2[,3], col=addzonas2[,3]) # Mostrar en la grafica las coordenadas de cada punto dando forma y color a cada zona o variable de agrupamiento 
legend(x="topleft", legend=c("CM","CT","Inc"), col=1:5, pch=1:5) #Añade  leyenda

#Anosim para efecto del estado reproductivo# 


braydist <- vegdist(dd)
nmds.vares <- metaMDS(dd, distance ="bray")
nmds.vares
plot(nmds.vares)
data.scores = as.data.frame(scores(nmds.vares))
data.scores$zona = zonasdiet$Zona
head (data.scores)
goodness(nmds.vares, display = c("sites"), choices = c(1,2), statistic = c("distance"))

library(ggplot2)
ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 2, aes( colour = zona))+
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        legend.text = element_text(size = 12, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 12, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", y = "NMDS2", shape = "zona") 



spe.dist<-vegdist( spe, method= "bray" ) 
attach(ze)
spe.ano<- anosim( spe.dist, grouping= Categoria.crianza, distance= "bray")
summary(spe.ano)
plot(spe.ano)

ANOSIM statistic R: 0.2442 
Significance: 0.001 

#comparacion de a pares# 

#Adonis y simper#

adonis(dune ~ Management*A1, data=dune.env, permutations=99)

adonis(spe~ Categoria.crianza, data=ze, permutations=99)# dd es la matriz de datos en crudo y data (ej zonasdiet) de donde vas a sacar las variables "explicativas"

adonis(spe~ Categoria.crianza, data=ze, permutations=999) # al aumentar el numero de permutaciones siempre se me hace mas chico el p 

(sim2 <- with(ze, simper(spe,Categoria.crianza )))
summary(sim2)

spe.dist<-vegdist( spe, method= "bray" ) 
attach(ze)
spe.ano<- anosim( spe.dist, grouping= Categoria.crianza, distance= "bray")
summary(spe.ano)
plot(spe.ano)


adonis(spe~ Categoria.crianza, data=ze, permutations=99)

adonis(spe~ Categoria.crianza, data=ze, permutations=999) # al aumentar el numero de permutaciones siempre se me hace mas chico el p 

(sim2 <- with(zonasdiet, simper(dd,Zona )))
summary(sim2)

sim <- simper(dd, zonasdiet$Zona)
summary(sim, ordered = TRUE)


#------------------------ De a pares --------------------#

ze.ict<-ze[-which(ze$Categoria.crianza=="CM"),]

spe.ict<-spestado [-which(spestado$Muestra=="35"|spestado$Muestra=="42"|spestado$Muestra=="44"|spestado$Muestra=="48"|spestado$Muestra=="49" |spestado$Muestra=="50" |spestado$Muestra=="51" |spestado$Muestra=="52" |spestado$Muestra=="55"|spestado$Muestra=="57"|spestado$Muestra=="58"|spestado$Muestra=="63"),]

speict.dist<-vegdist( spe.ict, method= "bray" ) 
attach(ze.ict)
speict.ano<- anosim( speict.dist, grouping= Categoria.crianza, distance= "bray")
summary(speict.ano)
plot(speict.ano)

adonis(spe.ict~ Categoria.crianza, data=ze.ict, permutations=999)

ze.icm<-ze[-which(ze$Categoria.crianza=="CT"),]
spe.icm<-spestado [-which(spestado$Muestra=="14"|spestado$Muestra=="15"|spestado$Muestra=="22"|spestado$Muestra=="25"|spestado$Muestra=="27" |spestado$Muestra=="29" |spestado$Muestra=="32" |spestado$Muestra=="34" |spestado$Muestra=="33" |spestado$Muestra=="41"|spestado$Muestra=="45"|spestado$Muestra=="46"|spestado$Muestra=="47"|spestado$Muestra=="53"|spestado$Muestra=="54"|spestado$Muestra=="56"|spestado$Muestra=="59"|spestado$Muestra=="62"|spestado$Muestra=="64"),]

speicm.dist<-vegdist( spe.icm, method= "bray" ) 
attach(ze.icm)
speicm.ano<- anosim( speicm.dist, grouping= Categoria.crianza, distance= "bray")
summary(speicm.ano)
plot(speict.ano)

adonis(spe.icm~ Categoria.crianza, data=ze.icm, permutations=999)

#CM-CT 

ze.ctcm<-ze[-which(ze$Categoria.crianza=="inc")]
spe.ctcm<-spestado [-which(spestado$Muestra=="1"|spestado$Muestra=="18"|spestado$Muestra=="23"|spestado$Muestra=="2"|spestado$Muestra=="3" |spestado$Muestra=="4" |spestado$Muestra=="5" |spestado$Muestra=="6" |spestado$Muestra=="7" |spestado$Muestra=="8"|spestado$Muestra=="9"|spestado$Muestra=="10"|spestado$Muestra=="11"|spestado$Muestra=="12"|spestado$Muestra=="13"),]

spectcm.dist<-vegdist( spe.ctcm, method= "bray" ) 
attach(ze.ctcm)
spectcm.ano<- anosim( spectcm.dist, grouping= Categoria.crianza, distance= "bray")
summary(spectcm.ano)
plot(spectcm.ano)

adonis(spe.ctcm~ Categoria.crianza, data=ze.ctcm, permutations=999)

#####################-----------EFECTO DEL PORCENTAJE EN NUMERO------------################################################################

ddN.pv<-read.csv("C:/Users/Nela/Dropbox/becadbox2/estadistica/%N.pv.csv", header=T)


head(ddN.pv)

atddNpv<-read.csv("C:/Users/Nela/Dropbox/becadbox2/estadistica/atach%N.pv.csv", header=T)
head(atddNpv)

dd.ordenb<-metaMDS (ddN.pv, distance= "bray" )
dd.ordenb


letrasb <- atddNpv$Colonia # 
numerosb <- as.integer(letrasb) # Asignar un numero a cada categoria de Zona (vector numerico de la misma longitud)
dd.ordenb$points # Extraer coordenadas de cada punto
points(x=dd.ordenb$points[,1], y=dd.ordenb$points[,2]) # Mostrar en la grafica las coordenadas de cada punto
addcoloniab <- cbind(dd.ordenb$points,numerosb) # Creamos nueva matriz con las coordenadas de cada punto y su zonas correspondiente

par(mfcol=c(1,1)) 
points(x=addcoloniab[,1], y=addcoloniab[,2], pch=addcoloniab[,3], col=addcoloniab[,3]) # Mostrar en la grafica las coordenadas de cada punto dando forma y color a cada tipo de zona 
legend(x="bottomleft", legend=c("CH","IM","PP"), col=1:3, pch=1:3) 
plot(dd.ordenb,type="n")


dd.distb<-vegdist( ddN.pv, method= "bray" ) # aca arma la matriz de bray curtis 
attach(atddNpv)
dd.anob<- anosim( dd.distb, grouping= Colonia, distance= "bray")
summary(dd.anob)
#ANOSIM statistic R: 0.02309 
# Significance: 0.124

#Prueba Adonis#PERMANOVA 

adonis(ddN.pv~ Colonia, data=atddNpv, permutations=99)# dd es la matriz de datos en crudo y data (ej zonasdiet) de donde vas a sacar las variables "explicativas"

Df SumsOfSqs MeanSqs F.Model      R2 Pr(>F)
Colonia    2    0.6501 0.32504   1.257 0.03243   0.32
Residuals 75   19.3947 0.25860         0.96757       
Total     77   20.0448                 1.00000      

(simb <- with(atddNpv, simper(ddN.pv,Colonia)))
summary(simb)

#-----------Efecto del %N entre Zonas--------------------------------------------#

ddN<-read.csv("C:/becadbox2/estadistica/%N.gu.pv.csv", header=T)

head(ddN)

zonasdietN<-read.csv("C:/becadbox2/estadistica/atach%N.gu.pv.csv", header=T)
head(zonasdietN)

dd.ordenN<-metaMDS (ddN, distance= "bray" )
dd.ordenN  #Stress:     0.1552049 , esto sirve para el grafico 
atach(zonasdietN)

plot(dd.ordenN)
orditorp(dd.ordenN,display="sites",cex=1.25,air=0.01)


letrasN <- zonasdietN$Zona # 
numerosN <- as.integer(letrasN) # Asignar un numero a cada categoria de Zona (vector numerico de la misma longitud)
dd.ordenN$points # Extraer coordenadas de cada punto
points(x=dd.ordenN$points[,1], y=dd.ordenN$points[,2]) # Mostrar en la grafica las coordenadas de cada punto
addzonasN<- cbind(dd.ordenN$points,numerosN) # Creamos nueva matriz con las coordenadas de cada punto y su zonas correspondiente

par(mfcol=c(1,1)) 
plot(dd.ordenN,type="n") # Ocultar los puntos
points(x=addzonasN[,1], y=addzonasN[,2], pch=addzonasN[,3], col=addzonasN [,3]) # Mostrar en la grafica las coordenadas de cada punto dando forma y color a cada tipo de zona 
legend(x="bottomleft", legend=c("ISS","PA"), col=1:2, pch=1:2) 

#Anosim por zonas entre ISS y PA#

dd.distN<-vegdist( ddN, method= "bray" ) # aca arma la matriz de bray curtis 
attach(zonasdietN)
dd.anoN<- anosim( dd.distN, grouping= Zona, distance= "bray")
summary(dd.anoN)


ANOSIM statistic R: 0.2518 
Significance: 0.001 


adonis(ddN~ Zona, data=zonasdietN, permutations=99)# dd es la matriz de datos en crudo y data (ej zonasdiet) de donde vas a sacar las variables "explicativas"

Df SumsOfSqs MeanSqs F.Model      R2 Pr(>F)   
Zona        1     5.891  5.8909  29.133 0.17225   0.01 **
  Residuals 140    28.309  0.2022         0.82775          
Total     141    34.199                 1.00000      


(sim3N <- with(zonasdietN, simper(ddN,Zona)))
summary(sim3N)

#Contrast: PV_GU 

average       sd  ratio      ava     avb cumsum
nc.      0.232142 0.161925 1.4336 32.29897 57.9545 0.3342
ha.      0.141647 0.146326 0.9680  7.54359 27.1005 0.5381
X.tn     0.125077 0.130948 0.9552 24.89013  0.7161 0.7182
Rotos.   0.106128 0.102109 1.0394 19.54949 10.4002 0.8710
nnu.     0.043936 0.065503 0.6708  8.19372  0.7812 0.9343
ggib.    0.020273 0.035616 0.5692  3.59167  0.6778 0.9634
pb.      0.014451 0.057034 0.2534  2.86615  0.0000 0.9843
pcha.    0.004544 0.015808 0.2874  0.90128  0.0000 0.9908
eant..   0.003515 0.019696 0.1784  0.00000  0.7031 0.9959
ecarl.   0.001562 0.012400 0.1260  0.00000  0.3125 0.9981
crastro. 0.000724 0.004342 0.1667  0.14359  0.0000 0.9991
crino.   0.000404 0.003549 0.1138  0.08013  0.0000 0.9997
cwil.    0.000190 0.001669 0.1138  0.03769  0.0000 1.0000

# ejemplo graficos 

library(vegan) 
data(dune) 
data(dune.env)

sim <- simper(dune, dune.env$Management)
summary(sim, ordered = TRUE)

ddp<-read.csv("C:/Users/Nela/Dropbox/becadbox2/estadistica/prueba2nmds.csv", header=T)
ddp


mdsB <- metaMDS(ddp, distance = "bray")
plot(mdsB, display = "sites", type = "text")

ddp3<-read.csv("C:/Users/Nela/Dropbox/becadbox2/estadistica/prueba3nmds.csv", header=T)
ddp3
zonasdietp<-read.csv("C:/Users/Nela/Dropbox/becadbox2/estadistica/atachprueba.csv", header=T)

mdsB3 <- metaMDS(ddp3, distance = "bray")# creo que aca van los datos en crudos 
plot(mdsB3, display = "sites", type = "text")

diversity(ddp3,index = "simpson")
shannon <- diversity(ddp3) 

#eje DUNE#

ord <- metaMDS(dune)

data(dune.env)
attach(dune.env)

plot(ord, display="sites", type="text")

ordihull(ord, Management, col=1:4, lwd=3)
ordiellipse(ord, Management, col=1:4, kind = "ehull", lwd=3)
ordiellipse(ord, Management, col=1:4, draw="polygon")
ordispider(ord, Management, col=1:4, label = TRUE)
points(ord, disp="sites", pch=21, col="red", bg="yellow", cex=1.3)

orditorp(ord,display="species",col="red",air=0.01) #The function adds text or points to ordination plots
orditorp(ord,display="sites",cex=1.25,air=0.01)

data(dune) 

ord2<-cca(dune) 
plot(ord2,display="sites") 
ordihull(ord2,grp, lty=2, col="red")

# diferencia entre meses en ISS

datadieta<-read.csv("C:/becadbox2/estadistica/%M.gu.pv.csv", header=T)
dd<-datadieta
head(dd)

zonasdiet<-read.csv("C:/becadbox2/estadistica/atach%M.gu.pv.csv", header=T)
head(zonasdiet)

dd.orden<-metaMDS (dd, distance= "bray" )
dd.orden
plot(dd.orden, display = "sites", type = "text")

par(mfcol=c(1,1)) 
letras <- zonasdiet$Zona # 
numeros <- as.integer(letras) # Asignar un numero a cada categoria de Zona (vector numerico de la misma longitud)
dd.orden$points # Extraer coordenadas de cada punto
points(x=dd.orden$points[,1], y=dd.orden$points[,2]) # Mostrar en la grafica las coordenadas de cada punto
addzonas <- cbind(dd.orden$points,numeros) # Crea nueva matriz con las coordenadas de cada punto y su zonas correspondiente

plot(dd.orden,type="n") # Ocultar los puntos
points(x=addzonas[,1], y=addzonas[,2], pch=addzonas[,3], col=addzonas[,3]) # Mostrar en la grafica las coordenadas de cada punto dando forma y color a cada tipo de zona 
legend(x="bottomleft", legend=c("ISS","PA"), col=1:5, pch=1:5) 

#Anosim por zonas entre ISS y PA#

dd.dist<-vegdist( dd, method= "bray" ) # aca arma la matriz de bray curtis 
attach(zonasdiet)
dd.ano<- anosim( dd.dist, grouping= Zona, distance= "bray")
summary(dd.ano)
plot(dd.ano)

# ANOSIM statistic R: 0.0938  este R indica que hay diferencias pero no son independientes, son indeptes si R es mayor a 0,75, en realidad son bastante similares,
#Significance: 0.001  # esto tambien rechaza la ho de que los ensambles son iguales o no hay diferencias 


#Prueba Adonis#PERMANOVA 