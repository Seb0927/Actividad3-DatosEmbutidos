datos <- read.table("data_embutidos.txt", header = TRUE, dec = ".")

#--- Valores necesarios para gráficas ---#

lim_s = 230 #Limite superior del peso
lim_i = 210 #Limite inferrior del peso

max = max(datos$peso)
min = min(datos$peso)
peso_media = median(datos$peso)

cantInf = sum(ifelse(datos$peso < lim_i, 1, 0))
which(ifelse(datos$peso < lim_i, TRUE, FALSE))
cantSup = sum(ifelse(datos$peso > lim_s, 1, 0))

#--- Gráficas ---#

##--- Pesos ---##

###--- General ---###
x11() ; #par(mfrow = c(1, 1))

x11()
boxplot(datos$peso,
        ylab = "Peso (gr)",
        ylim = c(min, max+5),
        col  = "White",
        main = "Boxplot de pesos")

abline(h = c(peso_media, lim_s, lim_i),
    lty = 3, 
    col = "Red")

x11()
hist(datos$peso,
     xlab = "Peso (gr)",
     ylab = "Frecuencia",
     xlim = c(min,max+5),
     col = "white",
     main = "Histograma de pesos")

abline(v = c(lim_s, lim_i),
       lty = 3,
       col = "Red")


###--- Maquina ---##

x11()
boxplot(datos$peso ~ datos$maquina,
        xlab  = "Máquina", 
        ylab  = "Peso (gr)",
        names = c("1", "2"),
        main  = "Pesos por maquina",
        ylim  = c(min, max+5)
)

abline(h = c(peso_media, lim_s, lim_i),
    lty = 3, 
    col = "Red",
)

###--- Operarios general ---###

x11()
boxplot(datos$peso ~ datos$operario,
        xlab  = "operario", 
        ylab  = "Peso (gr)",
        names = c("A", "B"),
        main  = "Pesos por operario",
        ylim  = c(min, max+5)
)

abline(h = c(peso_media, lim_s, lim_i),
    lty = 3, 
    col ="Red",
)


###--- Operarios separados ---###

## NOTA: Instalar dplyr primero
operario_a <- dplyr :: filter(datos, operario == "A") 
operario_b <- dplyr :: filter(datos, operario == "B")

####--- Operario A ---####

x11()
boxplot(operario_a$peso ~ operario_a$maquina,
        ylab  = "Peso (gr)",
        xlab = "Máquina",
        names = c("1", "2"),
        main  = "Pesos operario A",
        ylim  = c(min, max+5)
)
abline(h = c(peso_media, lim_s, lim_i),
    lty = 3, 
    col ="Red",
)

####--- Operario B ---####

x11()
boxplot(operario_b$peso ~ operario_b$maquina,
        ylab  = "Peso (gr)",
        xlab = "Máquina",
        names = c("1", "2"),
        main  = "Pesos operario B",
        ylim  = c(min, max+5)
)
abline(h = c(peso_media, lim_s, lim_i),
    lty = 3, 
    col ="Red",
)

###--- Maquinas separadas ---###

maquina_1 <- dplyr :: filter(datos, maquina == 1)
maquina_2 <- dplyr :: filter(datos, maquina == 2)

####--- Maquina A ---####

x11()
boxplot(maquina_1$peso,
        ylab  = "Peso (gr)",
        names = c("1"),
        main  = "Pesos maquina 1",
        ylim  = c(min, max+5)
)
abline(h = c(peso_media, lim_s, lim_i),
       lty = 3, 
       col ="Red",
)

####--- Maquina B ---####

x11()
boxplot(maquina_2$peso,
        ylab  = "Peso (gr)",
        names = c("2"),
        main  = "Pesos maquina 2",
        ylim  = c(min, max+5)
)
abline(h = c(peso_media, lim_s, lim_i),
       lty = 3, 
       col ="Red",
)

##
x11(); par(mfrow = c(1, 2))
boxplot(datos$peso ~ datos$maquina,
        xlab  = "Máquina", 
        ylab  = "Peso (gr)",
        names = c("1", "2"),
        main  = "Pesos por maquina",
        ylim  = c(min, max+5)
)

abline(h = c(peso_media, lim_s, lim_i),
       lty = 3, 
       col = "Red",
)

###--- Operarios general ---###
boxplot(datos$peso ~ datos$operario,
        xlab  = "Operario", 
        ylab  = "Peso (gr)",
        names = c("A", "B"),
        main  = "Pesos por operario",
        ylim  = c(min, max+5)
)

abline(h = c(peso_media, lim_s, lim_i),
       lty = 3, 
       col ="Red",
)