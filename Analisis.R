#Librerias
library(reshape2)
library(lubridate)
library(ggplot2)
#Lectura de Archivos

activity <- read.csv("activity.csv", sep = ",")


## Calculamos la media durante los dias


actividades <- melt(activity[which(!is.na(activity$steps)), ], id.vars = c("date", "interval")) # Limpieza de NA
pasos <- dcast(actividades, date ~ variable, sum)
summary(pasos$steps) # Resumen



## Desarrollo de Histograma

hist(pasos$steps, main = "Pasos totales por dia", 
     xlab = "Pasos por Dia", ylab = "Numero de Dias", 
     breaks = 10, col = "Red")
abline(v = mean(pasos$steps), lty = 1, lwd = 2, col = "blue")
abline(v = median(pasos$steps), lty = 2, lwd = 2, col = "green")
legend(x = "topright", c("Mean", "Median"), col = c("blue", "green"), 
       lty = c(1, 2), lwd = c(2, 2))

ggplot(pasos, aes(steps)) + geom_histogram(bins = 10)

## Actividad diaria promedio
pasos_intervalo <- dcast(actividades, interval ~ variable, 
                           mean, na.rm = TRUE)


plot(pasos_intervalo$interval, pasos_intervalo$steps, ty = "l",
     xlab = "Intervalo Tiempo", ylab = "Promedio Pasos", 
     main = "Pasos promedios durante todos los dais vs intervalos de tiempo")

maximos_pasos <- 
  pasos_intervalo$interval[which.max(pasos_intervalo$steps)]
maximos_pasos



## Valores perdidos
actividad2 <- split(activity, activity$interval)
actividad2 <- lapply(actividad2, function(x) {
  x$steps[which(is.na(x$steps))] <- mean(x$steps, na.rm = TRUE)
  return(x)
})
actividad2 <- do.call("rbind", actividad2)
row.names(actividad2) <- NULL

actividad2 <- split(actividad2, actividad2$date)
a <- lapply(actividad2, function(x) {
  x$steps[which(is.na(x$steps))] <- mean(x$steps, na.rm = TRUE)
  return(x)
})
actividad2 <- do.call("rbind", actividad2)
row.names(actividad2) <- NULL
head(actividad2)


pasos2 <- melt(actividad2, id.vars = c("date", "interval"))
pasos_sum <- dcast(pasos2, date ~ variable, sum, na.rm = TRUE)
head(pasos_sum)

## Histograma pasos dados con valores perdidos
hist(pasos_sum$steps, main = "Histograma de pasos totales dados por día", 
     xlab = "Total pasos por dia", ylab = "Numero de dias", 
     breaks = 10, col = "Green")
abline(v = mean(pasos_sum$steps), lty = 1, lwd = 2, col = "red")
abline(v = median(pasos_sum$steps), lty = 2, lwd = 2, col = "black")
legend(x = "topright", c("Mean", "Median"), col = c("red", "black"), 
       lty = c(2, 1), lwd = c(2, 2))



## Numero de filas con NA valores
sum(is.na(activity$steps))
sum(is.na(activity$steps))*100/nrow(activity) 

##Diferencias en los patrones de actividad: días laborables frente a fines de semana

Fin_semana <- which(weekdays(as.Date(actividad2$date)) == "sábado" |
                    weekdays(as.Date(actividad2$date)) == "domingo")
entre_semana <- which(weekdays(as.Date(actividad2$date)) != "sábado" &
                    weekdays(as.Date(actividad2$date)) != "domingo")


temp <- c(rep("a", length(actividad2)))

temp[Fin_semana] <- "Fin_semana"
temp[entre_semana] <- "entre_semana"
length(temp)
actividad2 <- cbind(actividad2, temp)
head(actividad2)
names(actividad2)[4] <- "day"

actividad2_split <- split(actividad2, actividad2$day)
media_pasos <- lapply(actividad2_split, function(x) {
  temp <- aggregate(x$steps, list(x$interval), mean)
  names(temp) <- c("interval", "steps")
  return(temp)
})

## Entre semana y fin de semana
media_pasos <- do.call("rbind", media_pasos)
entre_semana <- grep("entre_semana" ,row.names(media_pasos))
Fin_semana <- grep("Fin_semana" ,row.names(media_pasos))
temp <- c(rep("a", length(media_pasos$steps)))
temp[entre_semana] <- "entre_semana"
temp[Fin_semana] <- "Fin_semana"
names(temp) <- "DIA"
media_pasos <- cbind(media_pasos, temp)
row.names(media_pasos) <- NULL
names(media_pasos)[3] <- "day"
head(media_pasos)


ggplot(media_pasos, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) 

medio_pasos <- melt(media_pasos, id.vars = c("interval",
                                             "day"))
dcast(medio_pasos, day ~ variable, mean) # promedio
