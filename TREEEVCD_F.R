dir.data<-"C:\\Users\\camil\\OneDrive\\Documentos\\GitHub\\Climate-Change-and-AI\\Data\\Model\\"
data<-read.csv(paste0(dir.data,"ModelData.csv"))

#let's look at the data first,
summary(data)

#remove columns with only NA
bad.vars<-sapply(data, function(x) {mean(ifelse(is.na(x)==TRUE,1,0))})
bad.vars<-names(bad.vars[bad.vars>0.03])
bad.vars<-c(bad.vars,"SE.ENR.PRSC.FM.ZS","SE.PRM.CMPT.ZS","SE.SEC.ENRR","SH.DYN.AIDS.ZS","TT.PRI.MRCH.XD.WD","TX.VAL.TECH.MF.ZS")

#let's select a response of interest
#EN.ATM.CO2E.PC=CO2 emissions (metric tons per capita)
ids<-c("iso_code3","country")
#response<-"EN.ATM.CO2E.PC"
response<-"m_transport"
predictors<-subset(colnames(data),!(colnames(data)%in%c(ids,response,bad.vars)))

# Since we are analyzing this as a classification problem
#let's look at how this response is distributed
summary(data[,response])

# now let's partition the response of interest into two groups
#focus on the high emissions per capita nations
threshold<-as.numeric(quantile(data[,response],0.10,na.rm=TRUE))
#data$response.binary<-as.factor(ifelse(data[,response]>threshold,"High","Low"))
data$response.binary<-as.factor(ifelse(data[,response]>1.5,"High","Low")) 

# remove NA values in the response
data<-subset(data,is.na(response.binary)==FALSE)

#now look at how many high emission cases we have
summary(data$response.binary)

#Now before we start modeling, we subset the data to be used in the model to only complete cases
data.model<-data[,c("response.binary",predictors)]
data.model<-data.model[complete.cases(data.model),]
summary(data.model)
dim(data.model)

# now we have a clean data set

#define the model we want to estimate
model<-as.formula(paste0("response.binary","~",paste(predictors,collapse="+")))
model
# Approach 3: Random Forest
#========================

library(randomForest)
set.seed (55555)
train <- sample (1: nrow(data.model), 100)
rf.data.model <- randomForest(model,
                              data=data.model ,
                              subset =train ,
                              mtry=round(length(predictors)^0.5),
                              importance =TRUE
)
rf.data.model

#base on many different trees, which are the most important drivers
importance (rf.data.model)
varImpPlot (rf.data.model )

