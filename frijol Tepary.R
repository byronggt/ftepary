# Dr. Byron González
# http://byrong.cc

if(!require(car)){install.packages("car")}
if(!require(lattice)){install.packages("lattice")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(performance)){install.packages("performance")}
if(!require(nlme)){install.packages("nlme")}
if(!require(lme4)){install.packages("lme4")}
if(!require(lmerTest)){install.packages("lmerTest")}
if(!require(emmeans)){install.packages("emmeans")}
if(!require(multcomp)){install.packages("multcomp")}
if(!require(readxl)){install.packages("readxl")}

# Experimento en bloques completos al azar

# Importar la tabla de datos "frijol.xslx"
grupo<-read_excel("data/frijol.xlsx")
head(grupo)
tail(grupo)

grupo$Loc<- as.factor(grupo$Loc)
grupo$Lin<- as.factor(grupo$Lin)
grupo$Bloq<-as.factor(grupo$Bloq)
grupo$Loc_Bloq<-as.factor(grupo$Loc_Bloq)

str(grupo)
attach(grupo)

# Gráfico de interacción
windows(10,10)
interaction.plot(Loc,Lin,Rend, fixed=T, xlab="Localidades", ylab="Rendimento") 

# Análisis exploratorio
sapply(as.list(grupo[c("Loc","Lin","Bloq")]), levels) # Revisar
with(grupo, table(Loc, Lin)) 		# Todas las localidades tienen todas las líneas
with(grupo, table(Loc, Bloq)) 	# Mismo número de bloques en cada experimento

# Análisis gráfico para rendimiento

xyplot(Rend~Lin|Loc) 
xyplot(Rend~Loc|Lin)


# Análisis de varianza por localidad
# Modelo estadístico: y_ij = mu + Bloq_i + Lin_j + e_ij
#  Bloq: efecto fijo de bloques, i=4
#  Lin: efecto fijo de las líneas, j=14
#  e: error experimental

names(grupo)

# Análisis para cada localidad (Ejemplo para el Jícaro)
jic <- subset(grupo, Loc=="Jicaro")

aov.jic<- lm(Rend~Bloq+Lin, data=jic) # modelo de efectos fijos
summary(aov.jic)
plot(aov.jic,1)
plot(aov.jic,2)
anova(aov.jic)

# Análisis para todas las localidades de una sola vez
das <- split(grupo, grupo$Loc) # Genera una lista para cada localidad
str(das)
lapply(das, function(loc) with(loc, tapply(Rend, list(Bloq,Lin), identity)))

# Ajusta los modelos y muestra un anova por localidad

m01 <- lapply(das, FUN=aov, formula=Rend~Bloq+Lin) # ajusta para todas las localidades
lapply(m01, summary)               # Muestra los andevas
glrs <- sapply(m01, df.residual); glrs   # Grados de libertad
qmrs <- sapply(m01, deviance)/glrs ; qmrs # Cuadrados medios

# Ajuste para el análisis de los residuos (modelo reducido)

mc11 <- lm(Rend~Loc/Bloq+Loc*Lin)
anova(mc11)
windows(10,10)
check_model(mc11)
check_normality(mc11)

## Análisis con modelos mixtos 

# Agregar la combinación de bloque*loc como efecto aleatorio

mc12<-lme(Rend~1+Loc+Lin+Loc:Lin
          ,random=list(Loc_Bloq=pdIdent(~1))
          ,method="REML"
          ,control=lmeControl(niterEM=150
          ,msMaxIter=200)
          ,na.action=na.omit
          ,data=grupo
          ,keep.data=FALSE)
summary(mc12)
anova(mc12)

# Revisión del cumplimiento de los supuestos del anova
plot(Loc,mc12$residuals) # Atención
plot(Lin,mc12$residuals)
plot(mc12) # Atención
qqPlot(mc12$residuals) # Atención

# Anova mixto, con modelación de la varianza
# de acuerdo a cada localidad

mc13<-lme(Rend~1+Loc+Lin+Loc:Lin
          ,random=list(Loc_Bloq=pdIdent(~1))
          ,weights=varComb(varIdent(form=~1|Loc)) # Atención
          ,method="REML"
          ,control=lmeControl(niterEM=150
          ,msMaxIter=200)
          ,na.action=na.omit
          ,data=grupo
          ,keep.data=FALSE)
summary(mc13)
anova(mc13) # Significancia en la interacción de localidad*línea
#plot(Loc,mc13$residuals) # Atención
plot(Lin,mc13$residuals)
plot(mc13) # Atención
qqPlot(mc13$residuals)

# Prueba de medias (pendiente de mostrar solamente medias y EE)

results_m0<-emmeans(mc13, specs =~Lin*Loc) %>% 
  multcomp::cld(Letters=letters, sort=T, reverse=T) %>% 
  as.data.frame()
results_m0
