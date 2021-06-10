#Modelo de regresión para saber qué lleva a los países a tener un NDC de transport

#Especificamos de donde viene la información
dir.data<-"C:\\Users\\camil\\OneDrive\\Documentos\\GitHub\\Climate-Change-and-AI\\Data\\Model\\"
data<-read.csv(paste0(dir.data,"ModelData.csv"))
#Summary
summary(data)

#Limpiar columnas
bad.vars<-sapply(data, function(x) {mean(ifelse(is.na(x)==TRUE,1,0))})
bad.vars<-names(bad.vars[bad.vars>0.03])
bad.vars<-c(bad.vars,"SE.ENR.PRSC.FM.ZS","SE.PRM.CMPT.ZS","SE.SEC.ENRR","SH.DYN.AIDS.ZS","TT.PRI.MRCH.XD.WD","TX.VAL.TECH.MF.ZS")

#Respuesta de interés, en este caso es Transport
ids<-c("iso_code3","country")
response<-"m_transport"
predictors<-subset(colnames(data),!(colnames(data)%in%c(ids,response,bad.vars)))
predictors
length(predictors)
summary(data[,response])

threshold<- 0.5
#Guardar como un factor
data$response.binary<-as.factor(ifelse(data[,response]>threshold,"High","Low"))
summary(data$response.binary)

# remove NA values in the response
#Quitar todos los NA de la variable de interés
data<-subset(data,is.na(response.binary)==FALSE)

#now look at how many high emission cases we have
#Resumen de la respuesta binaria
summary(data$response.binary)

data.model<-data[,c("response.binary","m_energy", "m_lulucf","m_agriculture", "m_waste", "a_drm", "m_industries", "m_buildings")]
head(data.model)
#eliminar NA's
data.model<-data.model[complete.cases(data.model),]
summary(data.model)
dim(data.model)

#MODELO Lineal
#Estimación del modelo
#estimate full model
data.modelml<-data[,c(response,"m_energy", "m_lulucf","m_agriculture", "m_waste", "a_drm", "m_industries", "m_buildings")]
head(data.modelml)
#Modelo como una formula de las emisiones per capita en funcion de esta información
#Remover las variables altamente relacionadas
modelml<-as.formula(paste0(response,"~",paste(c("m_energy", "m_lulucf","m_agriculture", "m_waste", "a_drm", "m_industries", "m_buildings"),collapse="+")))
#Ya no salen NA's pero tampoco hay variables muy significativas
full.modelml <- lm(modelml, data = data.modelml)
summary(full.modelml)
dim(data.modelml)
#now let's select various smaller models
#install.packages("leaps")

#load library
library(leaps)


#first let's divide the sample into a test and a a train set
set.seed (55555)
train <- sample (c(TRUE ,FALSE), nrow(data.modelml ),rep=TRUE)
test  <- (!train )

#define maximum length of the model
max.vars<-7

#let's do full search
regfit.best <- regsubsets (modelml, data.modelml[train,], nvmax =max.vars,really.big = T) #you can choose how large you want the search to be

#let's do forward stepwise selection
regfit.fwd <- regsubsets (modelml, data.modelml[train,], nvmax =max.vars,really.big = T, method = "forward") #you can choose how large you want the search to be

#let's do backard stepwise selection
regfit.bwd <- regsubsets (modelml, data.modelml[train,], nvmax =max.vars,really.big = T, method = "backward") #you can choose how large you want the search to be


#explore different models
msize<-7
coef(regfit.best ,msize)
coef(regfit.fwd , msize)
coef(regfit.bwd , msize)


#now how do we select which model is best

predict.regsubsets <-function (object, modelml ,newdata ,id ){
  #object<-regfit.best
  #newdata<-data.model[test ,]
  #id<-1
  form<-modelml
  options(na.action='na.pass')
  mat<-model.matrix (form,newdata )
  coefi<-coef(object ,id=id)
  xvars<-names (coefi )
  pred<-mat[,xvars ]%*% coefi
  val.errors<- mean((newdata[,response]-pred)^2,na.rm=TRUE)
  val.errors
}

#now estimate test error for the different versions of the models
#best subset
cv.best<-data.frame(subset.type="best",
                    nvars=1,
                    test.mse=predict.regsubsets(regfit.best,modelml,data.modelml[test ,],1))

for(i in 2:max.vars)
{
  pivot<-data.frame(subset.type="best",
                    nvars=i,
                    test.mse=predict.regsubsets(regfit.best,modelml,data.modelml[test ,],i))
  cv.best<-rbind(cv.best,pivot)
  
}

#best model
subset(cv.best,test.mse==min(test.mse))
#actual model
coef(regfit.best ,subset(cv.best,test.mse==min(test.mse))$nvars)

#forward method
cv.fwd<-data.frame(subset.type="fwd",
                   nvars=1,
                   test.mse=predict.regsubsets(regfit.fwd,modelml,data.modelml[test ,],1))

for(i in 2:max.vars)
{
  pivot<-data.frame(subset.type="fwd",
                    nvars=i,
                    test.mse=predict.regsubsets(regfit.fwd,modelml,data.modelml[test ,],i))
  cv.fwd<-rbind(cv.fwd,pivot)
  
}

#best model
subset(cv.fwd,test.mse==min(test.mse))
#actual model
coef(regfit.fwd ,subset(cv.fwd,test.mse==min(test.mse))$nvars)

#
#backward method

cv.bwd<-data.frame(subset.type="bwd",
                   nvars=1,
                   test.mse=predict.regsubsets(regfit.bwd,modelml,data.modelml[test ,],1))


for(i in 2:max.vars)
{
  pivot<-data.frame(subset.type="bwd",
                    nvars=i,
                    test.mse=predict.regsubsets(regfit.bwd,modelml,data.modelml[test ,],i))
  cv.bwd<-rbind(cv.bwd,pivot)
  
}



#best model
subset(cv.bwd,test.mse==min(test.mse))
#actual model
coef(regfit.bwd ,subset(cv.bwd,test.mse==min(test.mse))$nvars)

#

