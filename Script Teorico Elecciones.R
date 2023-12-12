### Armamos una simulacion

set.seed(087)
n = 50

su = 0.5


secuenciahospitales = function(cant){
  rta = c()
  
  for(k in 1:cant){
    rta = c(rta, rep(k,k^2))
  }
  
  return(rta)
}

secuenciahospitales(5)

u = rnorm(n,0,sd = su)

hospital =  secuenciahospitales(n)
N = length(hospital)
efectorandom = u[hospital]
hospital = as.factor(hospital)


X = sample(20:80, N,replace=TRUE)
b0 = 0.2
b1 = -0.05 
mult = 20


probs = 1/(1+exp(-b0-b1*X-mult*efectorandom))
sobrevive = c()
for(j in 1:N){ 
  sobrevive[j] = rbinom(1,1,probs[j]) 
}
sobrevive = as.factor(sobrevive)
boxplot(X~sobrevive)


datos = data.frame(sobrevive,X,hospital)
datos

fitlm =glm(sobrevive~X+hospital,data=datos,family="binomial")

summary(fitlm)
predicholm = predict(fitlm,newdata=datos,type="response")
plot(probs,predicholm)


fitlmer =glmer(sobrevive~X+(1|hospital),data=datos,family="binomial")
summary(fitlmer)
predicholmer = predict(fitlmer,newdata=datos,type="response")
points(probs,predicholmer,col="red")




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






