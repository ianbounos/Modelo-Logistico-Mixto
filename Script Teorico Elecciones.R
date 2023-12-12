


library(lme4)
eleccion <- read.csv("C:/Users/ian bounos/Downloads/eleccion.txt", sep="")
eleccion$state = as.factor(eleccion$state)
fitlmer =glmer(proportion~(1|state),data=eleccion,family="binomial",weights=n)
eleccion$predicho = predict(fitlmer,newdata=eleccion,type="response")
eleccion

fitlm =glm(proportion~state,data=eleccion,family="binomial",weights=n)
summary(fitlm)

plot(eleccion$proportion~as.factor(eleccion$state))
points(eleccion$predicho~as.factor(eleccion$state),col="red" )
points(eleccion$pi~as.factor(eleccion$state),col="green" )
library(ggplot2)

### Grafico ordenado por peso
ggplot(eleccion, aes(x=state, y=proportion,col= "proporción muestral")) +
  geom_point() +
  geom_point(aes(x=state, y=predicho,col="proporción estimada"))+
  scale_x_discrete(limits = eleccion$state[order(-eleccion$n)])



ggplot(eleccion, aes(x=state, y=proportion,col= "Proporción muestral")) +
  geom_point() +
  geom_point(aes(x=state, y=pi,col="Resultado electoral"))+
  
 geom_point(aes(x=state, y=predicho,col="Proporción estimada"))+
  scale_x_discrete(limits = eleccion$state[order(-eleccion$n)])

ggplot(eleccion, aes(x=state, y=predicho,col= "Estimado efectos aleatorios")) +
  geom_point() +
  geom_point(aes(x=state, y=pi,col="Resultado electoral"))+
  scale_x_discrete(limits = eleccion$state[order(-eleccion$n)])




#### Vemos error cuadrático medio
mean((eleccion$predicho-eleccion$pi)^2)
mean((eleccion$proportion-eleccion$pi)^2)

resumenlmer = summary(fitlmer)
resumenlmer






