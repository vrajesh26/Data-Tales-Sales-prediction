gr_train=read.csv("D:Great Lakes Sales price/gr_train.csv",na.strings = c(" ","","NA"))
head(gr_train)
gr_test=read.csv("D:Great Lakes Sales price/gr_test.csv",na.strings = c(" ","","NA"))
nrow(gr_train)
nrow(gr_test)
summary(gr_train)
summary(gr_test)
colSums(is.na(gr_train))
colSums(is.na(gr_test))
str(gr_train)
str(gr_test)
names(gr_train)

nzv <- nearZeroVar(gr_train)
nzv
Salesprice=gr_train$SALES_PRICE
ncol(gr_train)
nrow(gr_train)
nrow(gr_test)
gr=rbind(gr_train[,1:21],gr_test)
head(gr)
nrow(gr)
str(gr)
class(gr)
colSums(is.na(gr))

#1
library("ggplot2")
ggplot(data = gr,mapping = aes(AREA))+geom_bar()
ggplot(data = gr_train,mapping = aes(AREA,fill=SALES_PRICE))+geom_bar()
gr$AREA=as.character(gr$AREA)
levels(gr$AREA)
table(gr$AREA)
gr[gr$AREA=="Adyr",]
library("car")

gr$AREA<-recode(gr$AREA,"c('Adyr')='Adyar'")

gr$AREA<-recode(gr$AREA,"c('Ana Nagar','Ann Nagar')='Anna Nagar'")
gr[gr$AREA=="Ana Nagar",]
table(gr$AREA)

gr$AREA<-recode(gr$AREA,"c('Chormpet','Chrmpet','Chrompt')
                ='Chrompet'")
table(gr$AREA)

gr$AREA<-recode(gr$AREA,"c('Karapakam')='Karapakkam'")
gr$AREA<-recode(gr$AREA,"c('KKNagar')='KK Nagar'")
gr$AREA<-recode(gr$AREA,"c('TNagar')='T Nagar'")
gr$AREA<-recode(gr$AREA,"c('Velchery')='Velachery'")
gr$AREA=as.factor(gr$AREA)
levels(gr$AREA)
ggplot(data = gr[1:nrow(gr_train),],mapping = aes(x=AREA,y=Salesprice))+geom_boxplot()

#2
library("lubridate")
class(gr$DATE_SALE)
gr$DATE_SALE=dmy(gr$DATE_SALE)

class(gr$DATE_BUILD)
gr$DATE_BUILD=dmy(gr$DATE_BUILD)

#3
ggplot(data = gr,mapping = aes(DIST_MAINROAD))+geom_histogram()
ggplot(data = gr_train,mapping = aes(DIST_MAINROAD,fill=SALES_PRICE))+geom_histogram()
ggplot(data = gr_train,mapping = aes(x=DIST_MAINROAD,y=SALES_PRICE))+geom_point()
cor.test(gr_train$DIST_MAINROAD,gr_train$SALES_PRICE)
summary(gr_train$DIST_MAINROAD)
summary(gr_train$SALES_PRICE)

#4 Missing values
table(gr$N_BEDROOM)
class(gr$N_BEDROOM)
#gr_train[gr_train$AREA=="Adyr",]

levels(gr$N_BEDROOM)
gr[is.na(gr$N_BEDROOM),]
#gr$N_BEDROOM[is.na(gr$N_BEDROOM)]=1
which(is.na(gr$N_BEDROOM))
#gr$N_BEDROOM=as.factor(gr$N_BEDROOM)
ggplot(data = gr,mapping = aes(N_BEDROOM))+geom_bar()
ggplot(data = gr_train,mapping = aes(N_BEDROOM,fill=SALES_PRICE))+geom_bar()
ggplot(data = gr_train,mapping = aes(N_BEDROOM,SALES_PRICE))+geom_boxplot()

#5 Missing value
table(gr$N_BATHROOM)
class(gr$N_BATHROOM)
which(is.na(gr$N_BATHROOM))
gr[is.na(gr$N_BATHROOM),]
#gr$N_BATHROOM[is.na(gr$N_BATHROOM)]=1
#gr$N_BATHROOM=as.factor(gr$N_BATHROOM)
ggplot(data = gr,mapping = aes(N_BATHROOM))+geom_bar()
ggplot(data = gr_train,mapping = aes(N_BATHROOM,fill=SALES_PRICE))+geom_bar()

table(gr$N_ROOM)
class(gr$N_ROOM)
#gr$N_ROOM=as.factor(gr$N_ROOM)
ggplot(data = gr,mapping = aes(N_ROOM))+geom_bar()
ggplot(data = gr_train,mapping = aes(N_ROOM,fill=SALES_PRICE))+geom_bar()

#6 New variable
gr$N_Total=gr$N_BATHROOM*gr$N_BEDROOM*gr$N_ROOM

#
levels(gr$SALE_COND)
table(gr$SALE_COND)
gr$SALE_COND<-recode(gr$SALE_COND,"c('Ab Normal')='AbNormal'")
gr$SALE_COND<-recode(gr$SALE_COND,"c('Adj Land')='AdjLand'")
gr$SALE_COND<-recode(gr$SALE_COND,"c('Partiall','PartiaLl')='Partial'")
levels(gr$SALE_COND)
table(gr$SALE_COND)
ggplot(data = gr,mapping = aes(SALE_COND))+geom_bar()
ggplot(data = gr_train,mapping = aes(SALE_COND,fill=SALES_PRICE))+geom_bar()

levels(gr$PARK_FACIL)
table(gr$PARK_FACIL)
gr$PARK_FACIL<-recode(gr$PARK_FACIL,"c('Noo')='No'")
ggplot(data = gr,mapping = aes(PARK_FACIL))+geom_bar()
ggplot(data = gr_train,mapping = aes(PARK_FACIL,fill=SALES_PRICE))+geom_bar()

levels(gr$BUILDTYPE)
table(gr$BUILDTYPE)
gr$BUILDTYPE<-recode(gr$BUILDTYPE,"c('Comercial','Commercil')='Commercial'")
gr$BUILDTYPE<-recode(gr$BUILDTYPE,"c('Other')='Others'")
ggplot(data = gr,mapping = aes(BUILDTYPE))+geom_bar()
ggplot(data = gr_train,mapping = aes(BUILDTYPE,fill=SALES_PRICE))+geom_bar()

levels(gr$UTILITY_AVAIL)
table(gr$UTILITY_AVAIL)
gr$UTILITY_AVAIL<-recode(gr$UTILITY_AVAIL,"c('NoSewr ')='NoSewr'")
gr$UTILITY_AVAIL<-recode(gr$UTILITY_AVAIL,"c('All Pub')='AllPub'")
ggplot(data = gr,mapping = aes(UTILITY_AVAIL))+geom_bar()
ggplot(data = gr_train,mapping = aes(UTILITY_AVAIL,fill=SALES_PRICE))+geom_bar()

levels(gr$STREET)
table(gr$STREET)
gr$STREET<-recode(gr$STREET,"c('NoAccess')='No Access'")
gr$STREET<-recode(gr$STREET,"c('Pavd')='Paved'")

levels(gr$MZZONE)
table(gr$MZZONE)
ggplot(data = gr,mapping = aes(MZZONE))+geom_bar()
ggplot(data = gr_train,mapping = aes(MZZONE,fill=SALES_PRICE))+geom_bar()

ggplot(data = gr,mapping = aes(QS_ROOMS))+geom_histogram()
ggplot(data = gr_train,mapping = aes(x=QS_ROOMS,y=SALES_PRICE))+geom_point()
summary(gr$QS_ROOMS)
table(gr$QS_ROOMS)

#
gr$QS_ROOMS <- with(gr,ifelse(QS_ROOMS>4.4,5,ifelse(QS_ROOMS>3.5,4,
                                                    ifelse(QS_ROOMS>2.4,3,2))))
gr$QS_ROOMS=as.factor(gr$QS_ROOMS)
table(gr$QS_BATHROOM)
gr$QS_BATHROOM <- with(gr,ifelse(QS_BATHROOM>4.4,5,ifelse(QS_BATHROOM>3.5,4,
                                                          ifelse(QS_BATHROOM>2.4,3,2))))
gr$QS_BATHROOM=as.factor(gr$QS_BATHROOM)
table(gr$QS_BEDROOM)
gr$QS_BEDROOM <- with(gr,ifelse(QS_BEDROOM>4.4,5,ifelse(QS_BEDROOM>3.5,4,
                                                        ifelse(QS_BEDROOM>2.4,3,2))))

gr$QS_BEDROOM=as.factor(gr$QS_BEDROOM)


#New variable
gr$houseage=as.numeric(format(gr$DATE_SALE,'%Y'))-as.numeric(format(gr$DATE_BUILD,'%Y'))
#gr$houseage=gr$DATE_SALE-gr$DATE_BUILD
#gr$houseage=as.integer(gr$houseage)
class(gr$houseage)

cor.test(gr_train$REG_FEE,gr_train$SALES_PRICE)
cor.test(gr_train$COMMIS,gr_train$SALES_PRICE)

colSums(is.na(gr))
cor(gr[ ,sapply(gr, is.integer)])


table(gr$QS_OVERALL)
which(is.na(gr$QS_OVERALL))
gr[is.na(gr$QS_OVERALL),]
gr$QS_OVERALL[is.na(gr$QS_OVERALL)]=mean(gr$QS_OVERALL,na.rm=T)
summary(gr$QS_OVERALL)
hist(gr$QS_OVERALL)
gr$QS_OVERALL <- with(gr,ifelse(QS_OVERALL>4.4,5,ifelse(QS_OVERALL>3.5,4,
                                                        ifelse(QS_OVERALL>2.4,3,2))))

gr$QS_OVERALL=as.factor(gr$QS_OVERALL)
names(gr)
gr=gr[,-c(1,4,11)]
#gr_train=gr[1:nrow(gr_train),]
gr_train$SALES_PRICE=Salesprice
#gr_test=gr[7110:10034,]
head(gr)
#install.packages("missForest")
library("missForest")
#library("mice")
gr<-missForest(gr,verbose = TRUE)
#mice(gr,meth=c('rf'))
head(gr)
class(gr)
gr<-data.frame(gr$ximp)
gr$OOBerror

head(gr)
cor(gr_train[ ,sapply(gr_train, is.integer)])
rm(dumm)
rm(dfWithDummies)
dumm1 = as.data.frame(model.matrix(~AREA+SALE_COND+
                                     PARK_FACIL+BUILDTYPE+STREET+
                                     MZZONE+0,data=gr_train))
dumm2 = as.data.frame(model.matrix(~AREA+SALE_COND+
                                     PARK_FACIL+BUILDTYPE+STREET+
                                     MZZONE+0,data=gr_test))
dfWithDummies1 = cbind(gr_train, dumm1) 
dfWithDummies2 = cbind(gr_test, dumm2) 
head(dfWithDummies1)
str(dfWithDummies)
names(dfWithDummies1)
gr_train=dfWithDummies1[,-c(1,2,3,4,5,6)]
gr_test=dfWithDummies2[,-c(1,2,3,4,5,6)]
gr=dfWithDummies
gr_train=gr[1:nrow(gr_train),]
gr_train$SALES_PRICE=Salesprice
gr_test=gr[7110:10034,]

split<-createDataPartition(y =gr_train$SALES_PRICE, p = 0.7, list = FALSE)

gr_sample_train<-gr_train[split,]

gr_sample_test<-gr_train[-split,]

model_lm=lm(SALES_PRICE~.,data=gr_train)
summary(model_lm)
ls(model_lm)

plot(model_lm)

library(MASS)
step <- stepAIC(model_lm, direction="both")
summary(step)


#Backward Selection based on AIC
step <- stepAIC(model_lm, direction="backward")
summary(step)


#Forward Selection based on AIC
step <- stepAIC(model_lm, direction="forward")
summary(step)


#Stepwise Selection with BIC
n = dim(gr_train)[1]
stepBIC = stepAIC(model_lm,k=log(n))
summary(stepBIC)
plot(stepBIC)

library(lars)
gr_train=gr_train-apply(gr_train,2,mean)
#Fit Lasso
model_lasso=lars(x=gr_train[,-53],y=gr_train[,53],type="lasso",trace=F,normalize=T,intercept=F)
#Plot Lasso Path
plot(model_lasso)
ls(model_lasso)
model_lasso$RSS

cv.lars(x=gr_train[,-53],y=gr_train[,53],intercept=F,type="lasso")
title("Lars CV")
grid()

coef.lars(model_lasso,s=0.5,mode="fraction")

y_hat=predict.lars(object=model_lasso,newx=gr_test,s=0.5,mode="fraction")

model_ridge <- lm.ridge(formula=SALES_PRICE~.,data=gr_train,lambda=seq(from=0,to=100,by=1))

matplot(x=t(model_ridge$coef),type='l',
        main="Ridge Regression Lambda vs Coefficient Plot",
        xlab="Lambda Values",
        ylab="Coefficients",
        col=c("black","red","blue","green"))
grid()
legend("topright",legend=rownames(model_ridge$coef),  
       fill=c("black","red","blue","green"),
       bty='n',
       cex=1)

bootStepAIC::boot.stepAIC(object=model_lm,data=gr_train)
names(gr_train)
gr_train=gr_train[,-c(2,12,18,19,20,23,38:47)]
gr_test=gr_test[,-c(2,12,18,19,20,23,38:47)]


library("missForest")
head(gr)
colSums(is.na(gr))
library("missForest")
gr_nona<-missForest(gr[,-1])
class(gr)
library("mice")
gr_mice=mice(gr,meth="rf")

cor(gr[ ,sapply(gr, is.numeric)])
str(gr)

gr$INT_SQFT_square=gr$INT_SQFT*gr$INT_SQFT

gr_train=gr[1:nrow(gr_train),]
gr_train$SALES_PRICE=Salesprice
gr_test=gr[7110:10034,]
gr_train=gr_train[,-c(2:6,10,13:16)]
gr_test=gr_test[,-c(2:6,10,13:16)]
for (col in colnames(gr_train)){
  if(is.numeric(gr_train[,col])){
    if( abs(cor(gr_train[,col],gr_train$SALES_PRICE)) > 0.5){
      print(col)
      print( cor(gr_train[,col],gr_train$SALES_PRICE) )
    }
  }
}

for (col in colnames(gr_train)){
  if(is.numeric(gr_train[,col])){
    if( abs(cor(gr_train[,col],gr_train$SALES_PRICE)) < 0.1){
      print(col)
      print( cor(gr_train[,col],gr_train$SALES_PRICE) )
    }
  }
}

cor(gr_train[ , sapply(gr_train, is.numeric)])
high_cor = which(abs(cors) > 0.6 & (abs(cors) < 1))
rows = rownames(cors)[((high_cor-1) %/% 38)+1]
cols = colnames(cors)[ifelse(high_cor %% 38 == 0, 38, high_cor %% 38)]
vals = cors[high_cor]

normalize <- function(x){return((x-min(x))/(max(x) - min(x)))}
gr_train=gr_train[,-27]
gr_train <- as.data.frame(lapply(gr_train, normalize))
gr_test <- as.data.frame(lapply(gr_test, normalize))
gr_train$SALES_PRICE=Salesprice

path <- "D:/Great Lakes Sales price/gr_train.csv"
gr_train1 <- h2o.uploadFile(localH2O, path = path,key="air2008")
dim(air2008.hex)

library("h2o")
localh2o<-h2o.init(nthreads = -1)
h2o.init()
train.h2o<-as.h2o(gr_train)
test.h2o<-as.h2o(gr_test)
colnames(train.h2o)
y.dep <- 27
x.indep <- 1:26
rforest.model <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = train.h2o, 
                                  seed = 1122)
h2o.performance(rforest.model)
h2o.varimp(rforest.model)
predict.rforest <- as.data.frame(h2o.predict(rforest.model, test.h2o))

gbm.model <- h2o.gbm(y=y.dep, x=x.indep, training_frame = train.h2o, ntrees = 1000, max_depth = 4, learn_rate = 0.01, seed = 1122)
h2o.performance(gbm.model)

dlearning.model <- h2o.deeplearning(y = y.dep,
                                    x = x.indep,
                                    training_frame = train.h2o,
                                    epoch = 60,
                                    hidden = c(100,100),
                                    activation = "Rectifier",
                                    seed = 1122
)
h2o.performance(dlearning.model)
predict.dl2 <- as.data.frame(h2o.predict(dlearning.model, test.h2o))
length(PRT_ID)
sub_dlearning <- data.frame(PRT_ID = PRT_ID, SALES_PRICE = predict.dl2$predict)
write.csv(sub_dlearning, file = "sub_dlearning_new.csv")
