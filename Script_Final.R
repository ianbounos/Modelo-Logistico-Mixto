library(dplyr)
library(lme4)
library(pROC)
library(ggplot2)



###################################################################
########## PRIMERA PARTE: TRATAMIENTO PREVIO DE DATOS ############
####################################################################


insurance_claims <- read.csv("C:/Users/ian bounos/OneDrive/Escritorio/ESTADISTICA/Tesis Maestria/insurance_claims_original.csv", header=TRUE, sep=",")


insurance_claims$fraud_reported= as.numeric(insurance_claims$fraud_reported=="Y")
insurance_claims$fraud_reported=as.factor(insurance_claims$fraud_reported)

### Definimos Siniestros "Grandes" y "Pequeños" por las observaciones hechas:
insurance_claims$injury_dummy= insurance_claims$injury_claim > 2000
insurance_claims$property_dummy = insurance_claims$property_claim > 2000
insurance_claims$vehicle_dummy = insurance_claims$vehicle_claim > 10000


## Separamos conjunto de entrenamiento y testeo
set.seed(1996)
n= nrow(insurance_claims)
n
index = sample(1:n,size=0.8*n)
insurance.train = insurance_claims[index,]
insurance.test = insurance_claims[-index,]


### algunas cuestiones exploratorias

ggplot(insurance.train, aes(x=fraud_reported, y=property_claim, color=fraud_reported)) +
  geom_boxplot()+geom_point(alpha=0.1)

cor(insurance.train%>%
      select(vehicle_claim,
             injury_claim,
             property_claim,
             vehicle_dummy,
             property_dummy,
             injury_dummy))

table(insurance.train$property_dummy,insurance.train$fraud_reported)
table(insurance.train$vehicle_dummy,insurance.train$fraud_reported)
table(insurance.train$injury_dummy,insurance.train$fraud_reported)


#### Variables preseleccionadas

insurance.train = insurance.train%>%
  select(fraud_reported,
         incident_severity,
         insured_hobbies,
         incident_type,
         collision_type,
         authorities_contacted,
         incident_state,
         witnesses,
         auto_make,
         injury_claim,
         vehicle_claim,
         property_claim,
         injury_dummy,
         vehicle_dummy,
         property_dummy)
insurance.test = insurance.test%>%
  select(fraud_reported,
         incident_severity,
         insured_hobbies,
         incident_type,
         collision_type,
         authorities_contacted,
         incident_state,
         witnesses,
         auto_make,
         injury_claim,
         vehicle_claim,
         property_claim,
         injury_dummy,
         vehicle_dummy,
         property_dummy)



##########################################################
###### SEGUNDA PARTE: SELECCION AUC ######################
##########################################################



covariables_fijas_todas = names(insurance.train%>%
                                  select(incident_severity,
                                         incident_type,
                                         collision_type,
                                         authorities_contacted,
                                         incident_state,
                                         witnesses,
                                         injury_claim,
                                         vehicle_claim,
                                         property_claim,
                                         injury_dummy,
                                         vehicle_dummy,
                                         property_dummy))
covariables_random_todas = names(insurance.train%>%
                                   select(insured_hobbies,auto_make))

insurance_seleccion.train = insurance.train
insurance_seleccion.test = insurance.test



#### definimos funcion auxiliar calculo_auc 
#  calculo_auc toma los indices de las variables a utilizar y devuelve el auc
calculo_auc = function(indice_fijas,indice_random)
{
  
  variables_fijas_este_modelo  =    covariables_fijas_todas[indice_fijas]
  if(length(indice_random) >0)
  {
    variables_random_este_modelo  =  paste("(1|",  covariables_random_todas[indice_random],")",sep="")
    variables_todas_este_modelo = c(variables_fijas_este_modelo,variables_random_este_modelo )
  }else{variables_todas_este_modelo = variables_fijas_este_modelo}
  
  formula_este_modelo = paste("fraud_reported~",paste(variables_todas_este_modelo, collapse = "+"))
  
  if(length(indice_random) >0){f=glmer}else{f=glm}
  modelo_logistico <- f(as.formula(formula_este_modelo), data = insurance_seleccion.train, family =binomial(link="logit"))
  auc <- roc(response = insurance_seleccion.test$fraud_reported, predictor = predict(modelo_logistico,newdata= insurance_seleccion.test,type = "response"))
 return(auc)
}



##### A partir de ahora realizaremos las condiciones iniciales para el proceso de iteracion:


n_fijas = length(covariables_fijas_todas)
n_random = length(covariables_random_todas)
n_total = n_fijas+n_random


indice_fijas = c()
indice_random = c()

### generamos un nuevo indice que es el indice de todas las variables, para que sea mas comodo ir iterando sobre el
indice_total= c()

### definimos el auc inicial
modelo_logistico_base <- glm(fraud_reported~1, data = insurance_seleccion.train, family =binomial(link="logit"))
auc0 =roc(response = insurance_seleccion.test$fraud_reported, predictor = predict(modelo_logistico_base,newdata= insurance_seleccion.test,type = "response"))$auc


actualizar = -1  ### Mientras esta variable no sea 0, se seguiran agregando variables al indice
registro_auc<- data.frame(matrix(0, nrow = 14, ncol = 10))
paso = 1
#### Nos fijaremos quién es la que maximiza el valor
while(actualizar!=0){ 
    
  actualizar= 0  
  auc_inicial = auc0
  
    #### iteramos sobrelas que no estan en el indice
    for( k in (1:n_total)) {
          
            if(! k %in% indice_total){
          indice_total_provisorio = c(indice_total,k)
          indice_fijas_provisorio =   indice_total_provisorio[indice_total_provisorio<=n_fijas]
          indice_random_provisorio =   indice_total_provisorio[indice_total_provisorio>n_fijas]-n_fijas
          
          auc_provisorio =  calculo_auc(indice_fijas_provisorio,indice_random_provisorio)$auc
          registro_auc[k,paso]=  auc_provisorio
          
          
          ### si supera el auc, cambiamos actualizar a 1
            if(auc0<auc_provisorio){ 
              actualizar = 1
              auc0 = auc_provisorio 
              mejor_indice_total = indice_total_provisorio
              }
            
            }
   
    }
    if(actualizar == 1 ){
      if(auc0-auc_inicial>0.01){  ### pediremos tolerancia de 0.01
      print(auc0)
      indice_total  =   mejor_indice_total
      print(indice_total)}else{actualizar=0}}
  paso = paso+1
  
}


rownames(registro_auc) =  c(covariables_fijas_todas,covariables_random_todas)


#### Aquí guardamos los aportes al AUC de cada paso

registro_auc

#################### Graficamos AUC#################################
##### Aquí graficamos los pasos.
#No es importante para el desarrollo del codigo más allá del grafico en sí:



#### Primer paso 
indice_fijas = c()
indice_random = c()
indice_total= c()


modelo_logistico_base <- glm(fraud_reported~1, data = insurance_seleccion.train, family =binomial(link="logit"))
auc0 =roc(response = insurance_seleccion.test$fraud_reported, predictor = predict(modelo_logistico_base,newdata= insurance_seleccion.test,type = "response"))$auc
plot(roc(response = insurance_seleccion.test$fraud_reported, predictor = predict(modelo_logistico_base,newdata= insurance_seleccion.test,type = "response")))


for( k in (1:n_total)) {
  
  if(! k %in% indice_total){
    indice_total_provisorio = c(indice_total,k)
    indice_fijas_provisorio =   indice_total_provisorio[indice_total_provisorio<=n_fijas]
    indice_random_provisorio =   indice_total_provisorio[indice_total_provisorio>n_fijas]-n_fijas
    
    roc_provisorio =  calculo_auc(indice_fijas_provisorio,indice_random_provisorio)
    auc_provisorio = roc_provisorio$auc
    plot(roc_provisorio,add=TRUE,col="light blue")

  }
}

plot(calculo_auc(c(1),c()),add=TRUE,col="red")





#### Segundo paso 
indice_fijas = c()
indice_random = c(1)
indice_total= c(13)


modelo_logistico_base <- glm(fraud_reported~1, data = insurance_seleccion.train, family =binomial(link="logit"))
auc0 =roc(response = insurance_seleccion.test$fraud_reported, predictor = predict(modelo_logistico_base,newdata= insurance_seleccion.test,type = "response"))$auc
plot(roc(response = insurance_seleccion.test$fraud_reported, predictor = predict(modelo_logistico_base,newdata= insurance_seleccion.test,type = "response")))


for( k in (1:n_total)) {
  
  if(! k %in% indice_total){
    indice_total_provisorio = c(indice_total,k)
    indice_fijas_provisorio =   indice_total_provisorio[indice_total_provisorio<=n_fijas]
    indice_random_provisorio =   indice_total_provisorio[indice_total_provisorio>n_fijas]-n_fijas
    
    roc_provisorio =  calculo_auc(indice_fijas_provisorio,indice_random_provisorio)
    auc_provisorio = roc_provisorio$auc
    plot(roc_provisorio,add=TRUE,col="light blue")
    
  }
}

plot(calculo_auc(c(1),c(1)),add=TRUE,col="red")




###################################################################
############### TERCERA PARTE: SELECCION AIC ######################
###################################################################


### Analogamente, definimos funcion calculo_aic

calculo_aic = function(indice_fijas,indice_random)
{
  
  variables_fijas_este_modelo  =    covariables_fijas_todas[indice_fijas]
  if(length(indice_random) >0)
  {
    variables_random_este_modelo  =  paste("(1|",  covariables_random_todas[indice_random],")",sep="")
    variables_todas_este_modelo = c(variables_fijas_este_modelo,variables_random_este_modelo )
  }else{variables_todas_este_modelo = variables_fijas_este_modelo}
  
  formula_este_modelo = paste("fraud_reported~",paste(variables_todas_este_modelo, collapse = "+"))
  
  if(length(indice_random) >0){f=glmer}else{f=glm}
  modelo_logistico <- f(as.formula(formula_este_modelo), data = insurance.train, family =binomial(link="logit"))
  aic <- AIC(modelo_logistico)
  return(aic)
}




indice_fijas = c()
indice_random = c()

### generamos un nuevo indice que es el indice de todas las variables, para que sea mas comodo ir iterando sobre el
indice_total= c()

### definimos el auc inicial
modelo_logistico_base <- glm(fraud_reported~1, data = insurance.train, family =binomial(link="logit"))
aic0 = AIC(modelo_logistico_base)
aic0
actualizar = -1  ### Mientras esta variable no sea 0, se seguiran agregando variables al indice


registro_aic<- data.frame(matrix(0, nrow = 14, ncol = 10))
paso = 1

while(actualizar!=0){ 
  
  actualizar= 0  
  aic_inicial = aic0
  
  #### iteramos sobrelas que no estan en el indice
  for( k in (1:n_total)) {
    
    if(! k %in% indice_total){
      indice_total_provisorio = c(indice_total,k)
      indice_fijas_provisorio =   indice_total_provisorio[indice_total_provisorio<=n_fijas]
      indice_random_provisorio =   indice_total_provisorio[indice_total_provisorio>n_fijas]-n_fijas
      
      aic_provisorio =  calculo_aic(indice_fijas_provisorio,indice_random_provisorio)
      
      registro_aic[k,paso] = aic_provisorio
      
      
      ### si supera el aic, cambiamos actualizar a 1
      if(aic0>aic_provisorio){ 
        actualizar = 1
        aic0 = aic_provisorio 
        mejor_indice_total = indice_total_provisorio
      }
      
    }
    
  }
  if(actualizar == 1 ){
      ### pediremos tolerancia de 0.01
      print(aic0)
      indice_total  =   mejor_indice_total
      print(indice_total)}else{actualizar=0} 
  paso=paso+1
  
}


registro_aic


### Anova de cada etapa de la seleccion de variables

modelo_base = glm(fraud_reported~1,
                  family=binomial(link="logit"),
                  data=insurance.train  )
modelo_paso1 = glm(fraud_reported~incident_severity,
                   family=binomial(link="logit"),
                   data=insurance.train  )
modelo_paso2 = glmer(fraud_reported~incident_severity+(1|insured_hobbies),
                     family=binomial(link="logit"),
                     data=insurance.train  )
modelo_paso3 = glmer(fraud_reported~incident_severity+(1|insured_hobbies)+collision_type,
                     family=binomial(link="logit"),
                     data=insurance.train  )

anova(modelo_paso3,modelo_paso2,modelo_paso1, modelo_base)



#######################################################################################################
#############################CUARTA PARTE: RESULTADOS MODELO FINAL #########################################################
#################################################################################




modelo_final <- glmer(fraud_reported ~ incident_severity +(1|insured_hobbies),data=insurance.train, family=binomial(link="logit")   )
summary(modelo_final)


######## Comparamos el modelo final con la exclusion de incident severity

modelo_sin_severity  = glmer(fraud_reported ~ (1|insured_hobbies),data=insurance.train, family=binomial(link="logit")   )
anova(modelo_final,modelo_sin_severity,test="LRT")

#### Curva ROC

roc_final <- roc(response = insurance.test$fraud_reported, predictor = predict(modelo_logistico,newdata= insurance.test,type = "response"))
plot(roc_final,col="blue")


summary(modelo_final)
anova(modelo_final)


### Efectos aleatorios
x = ranef(modelo_final) $insured_hobbies

efrandom =  as.numeric(x[order(-abs(x[1:20,] )),])
efrandom
hobby = rownames(x)[order(-abs(x[1:20,] ))]
data.frame(hobby,efrandom)



###################################################
#### QUINTA PARTE: ANALISIS DE RESIDUOS ##########
####################################################



library(arm)
binnedplot(fitted(modelo_final), 
           residuals(modelo_final, type = "response"), 
 
           xlab = "Valor Esperado", 
           ylab = "Residuo Promedio", 
           main= "",
           cex.pts = 1.4, 
           col.pts = 4, 
           col.int = "white")


library(DHARMa)


simulationOutput <- simulateResiduals(fittedModel = modelo_final, n = 1000)

               
simulationOutput = recalculateResiduals(simulationOutput , group = insurance.train$insured_hobbies)

  plot(simulationOutput)

  
  
  
################################################################
############## SEXTA PARTE: ANALISIS LOOCV #####################
################################################################
  
  
#### Definimos probabilidad estimada con LOOCV  
 probmodelo=c()
  for(k in 1:n){
    print(k)
    datos = insurance_claims[-k,]
    individuo = insurance_claims[k,]
   modelo_final_loocv =  glmer(fraud_reported ~incident_severity+(1|insured_hobbies),data = datos, family = binomial)
    probmodelo = c(probmodelo,predict( modelo_final_loocv,newdata=individuo,type="response"))
    
  }
  
##### Funcion que dice, segun el umbral de probabilidad que clasificacion corresponde a dicha probabilidad  
  pred = function(vector,p=0.5       ){
    return(as.numeric(vector>p))
    }  

  
  #### tasas de acierto para p = 0.5
  mean((pred(probmodelo)==insurance_claims$fraud_reported))
  

#### Grafico tasa de acierto en función del umbral de probabilidad
  
  grilla =seq(0,1,length=100)
  acierto = c()
  for( p in grilla){    acierto = c(acierto,mean((pred(probmodelo,p)==insurance_claims$fraud_reported)))  }

  ### Guardamos aca el maximo
  p_max = grilla[which.max(acierto) ]
  tasa_max = max(acierto)
  
  df = data.frame(Umbral = grilla,Tasa = acierto)
  ggplot(data=df,aes(x=Umbral,y=Tasa))+
    geom_line()+
    geom_point(data=data.frame(Umbral=c(p_max),Tasa = c(tasa_max)),color="blue",size=2)         
         
 ### Tabla de confusión para p_max
  
  tabla_confusion_maximizatasa = table(insurance_claims$fraud_reported,pred(probmodelo,p_max)) 
  tabla_confusion_maximizatasa 
  sens_maximizatasa =    tabla_confusion_maximizatasa[2,2]/( tabla_confusion_maximizatasa[2,2]+ tabla_confusion_maximizatasa[2,1])
  spec_maximizatasa =   tabla_confusion_maximizatasa[1,1]/( tabla_confusion_maximizatasa[1,2]+ tabla_confusion_maximizatasa[1,1])
  #### Curva roc  
  roc_loocv = roc(response = insurance_claims$fraud_reported, predictor =probmodelo)
roc_loocv$specificities
roc_loocv$auc

plot(roc_loocv)
points(spec_maximizatasa,sens_maximizatasa,pch=16,col="red")




