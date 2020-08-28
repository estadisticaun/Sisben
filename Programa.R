# ANÁLISIS DE PBM VS SISBEN EN LA UNAL
#Librerías Requeridas

library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)

# Importar Datos

Sisben20191 <- read_excel('Datos/PSISBEN 20191.xlsx', sheet = 'PSISBEN 2019-1')
Sisben20192 <- read_excel('Datos/PSISBEN 20192.xlsx', sheet = 'PSISBEN 2019-2')
Sisben20201 <- read_excel('Datos/PSISBEN 20201.xlsx', sheet = 'PSISBEN 2020-1')

names(Sisben20192)

Sisben20201 %>% group_by(TIPO_NIVEL) %>% count()

# Unir bases, seleccionar pregrado y eliminar repeticiones

Sisben <- bind_rows(Sisben20191, Sisben20192, Sisben20201)
Sisben <- Sisben %>% distinct(ID, .keep_all = TRUE) %>% filter(TIPO_NIVEL == 'Pregrado')

# Sisben Sí/No

Sisben %>% group_by(SisbenSi_No) %>% count()


# Comparación de poblaciones (Sí / No)

Sisben_sede <- Sisben %>% group_by(SisbenSi_No, SEDE_NOMBRE_MAT) %>% count() %>% pivot_wider(names_from = SEDE_NOMBRE_MAT, values_from = n, values_fill = list(n = 0))
Sisben_edad <- Sisben %>% group_by(SisbenSi_No, CAT_EDAD) %>% count() %>% pivot_wider(names_from = CAT_EDAD, values_from = n, values_fill = list(n = 0))
Sisben_Sexo <- Sisben %>% group_by(SisbenSi_No, SEXO) %>% count() %>% pivot_wider(names_from = SEXO, values_from = n, values_fill = list(n = 0))
Sisben_Estrato <- Sisben %>% group_by(SisbenSi_No, ESTRATO) %>% count() %>% pivot_wider(names_from = ESTRATO, values_from = n, values_fill = list(n = 0))
Sisben_PBM <- Sisben %>% group_by(SisbenSi_No, PBM) %>% count() %>% pivot_wider(names_from = PBM, values_from = n, values_fill = list(n = 0))

# Exportar resultados ---- 

write.csv2(Sisben_PBM, "Análisis/Sisben_PBM.csv") 
write.csv2(Sisben_sede, "Análisis/Sisben_SEDE.csv") 
write.csv2(Sisben_edad, "Análisis/Sisben_EDAD.csv") 
write.csv2(Sisben_Sexo, "Análisis/Sisben_SEXO.csv") 
write.csv2(Sisben_Estrato, "Análisis/Sisben_ESTRATO.csv") 

# Retener estudiantes con información del Sisben completa 

SisbenSI <- Sisben %>% filter(SisbenSi_No == 'Sí', !is.na(PBM_ORIG), !is.na(PuntajeSisben))




# Distribuciones de PBM y SISBEN
# PMB
ggplot(SisbenSI, aes(x=PBM_ORIG)) + geom_density() + xlab("Puntaje Básico de Matrícula (PBM)") +
  ylab("Densidad")

ggplot(SisbenSI, aes(x= PBM_ORIG)) + geom_histogram() + xlab("Puntaje Básico de Matrícula (PBM)") +
  ylab("Total de estudiantes")

# SISBEN
ggplot(SisbenSI, aes(x= PuntajeSisben)) + geom_density() + xlab("Puntaje Sisben") +
  ylab("Densidad")

ggplot(SisbenSI, aes(x= PuntajeSisben)) + geom_histogram() + xlab("Puntaje Sisben") +
  ylab("Total de estudiantes")

# PBM vs SISBEN

Mixto <- SisbenSI %>% select(ID, PBM_ORIG, PuntajeSisben) %>%  
             pivot_longer(PBM_ORIG:PuntajeSisben, names_to = "Instrumento", values_to = "Puntaje") 
Mixto

ggplot(Mixto, aes(x= Puntaje, col = Instrumento)) + geom_density(size = 1)


# Puntos de Corte apoyos sociales del Estado

# Icetex

SisbenSI %>% group_by(PuntajeSisben <= 57.21) %>% count() # Subsidio de sostenimiento ciudades

# Ministerio de Educación
# DPS
# Ministerio de Salud
# Instituto Colombiano de Bienestar Familiar

# Análisis Gráfico y Coeficiente de Correlación Entre Sisben y PBM

PlotCor <- SisbenSI %>% ggplot(aes(x = PBM_ORIG, y = PuntajeSisben)) + geom_point() + xlab("Puntaje Básico de Matrícula (PBM)") + ylab("Puntaje Sisben") 
PlotCor

#Sedes Andinas
PlotCorAnd <- SisbenSI %>% filter(SEDE_NOMBRE_MAT %in% c('Bogotá', 'Medellín', 'Manizales', 'Palmira')) %>% ggplot(aes(x = PBM_ORIG, y = PuntajeSisben)) + geom_point()
PlotCorAnd + facet_grid(. ~ SEDE_NOMBRE_MAT) + xlab("Puntaje Básico de Matrícula (PBM)") + ylab("Puntaje Sisben") 

# Sedes de Presencia Nacional

PlotCorSPN <- SisbenSI %>% filter(SEDE_NOMBRE_MAT %in% c('De La Paz', 'Caribe', 'Amazonía', 'Tumaco', 'Orinoquía')) %>% ggplot(aes(x = PBM_ORIG, y = PuntajeSisben)) + geom_point()
PlotCorSPN + facet_grid(. ~ SEDE_NOMBRE_MAT) + xlab("Puntaje Básico de Matrícula (PBM)") + ylab("Puntaje Sisben") 


SisbenSI %>% group_by(SEDE_NOMBRE_MAT) %>% summarise(Correlación = cor(x = PBM_ORIG, y = PuntajeSisben))

cor(x = SisbenSI$PBM_ORIG, y = SisbenSI$PuntajeSisben)

SisbenSI %>% filter(PuntajeSisben <= 25) %>% count()

# Creación de cuadrantes entre PBM y SISBEN

PlotCor_Lineas <- SisbenSI %>% ggplot(aes(x = PBM_ORIG, y = PuntajeSisben)) + geom_point() + 
           geom_vline(xintercept = 20, col = 'red', size = 1.5) +
           geom_hline(yintercept = 50, col = 'blue', size = 1.5) +
           xlab("Puntaje Básico de Matrícula (PBM)") + 
           ylab("Puntaje Sisben") 
PlotCor_Lineas

Cuad_1 <- SisbenSI %>% group_by(PuntajeSisben <= 50, PBM_ORIG <= 20) %>% count()

PBM_Bienestar <- SisbenSI %>% group_by(PBM_ORIG <= 6) %>% count()

# Densidades Acumuladas

ggplot(SisbenSI, aes(PBM_ORIG)) + stat_ecdf(geom = "step", pad = FALSE, size = 1.1)+ xlab("Puntaje Básico de Matrícula (PBM)") +
  ylab("Densidad")
ggplot(SisbenSI, aes(PuntajeSisben)) + stat_ecdf(geom = "step", pad = FALSE, size = 1.1) + xlab("Puntaje Sisben") +
  ylab("Densidad")
### Mixta
ggplot(Mixto, aes(x = Puntaje, col = Instrumento)) + stat_ecdf(geom = "step", pad = FALSE, size = 1.1) +
                                                     xlab("Puntaje Básico de Matrícula (PBM)") +
                                                     ylab("Probabilidad acumulada")
  
var(SisbenSI$PBM_ORIG)
var(SisbenSI$PuntajeSisben)

# Distribución PMB en la U

ggplot(Sisben, aes(x= PBM_ORIG)) + geom_histogram() + xlab("Puntaje Básico de Matrícula (PBM)") +
                                                      ylab("Total de Estudiantes") + 
                                                     ggtitle("Distribución de matriculados según PBM - Histograma")

ggplot(Sisben, aes(x= PBM_ORIG)) + geom_density(size = 1) + xlab("Puntaje Básico de Matrícula (PBM)") +
  ylab("Densidad") + 
  ggtitle("Distribución de matriculados según PBM - Función de densidad")
