data <- openxlsx::read.xlsx("7. BtB - Study 2 - EMA data - all data - valid, restructured and coded.xlsx",detectDates = T)
data <- data[data$X5 != "Code not found",]

colnames(data) <- data[1,]

data <- data[3:nrow(data),]

newdata <- data.frame(matrix(nrow=nrow(data)))
newdata$ID <- data$participant
newdata$date <- as.numeric(data$timestamp)
newdata$entry_type <- data$`On demand entry type 1*`
newdata$artificial_post <- data$`On demand entry type 2*` == "1"
newdata$self_report_binge <- as.numeric(data$"Self-reported binge")
newdata$willingness_to_restrict <- as.numeric(data$q7_answer)
newdata$state_anger_pre <- as.numeric(data$q15_answer)
newdata$state_calm_pre <- as.numeric(data$q16_answer)
newdata$state_anxiety_pre <- as.numeric(data$q17_answer)
newdata$state_depression_pre <- as.numeric(data$q18_answer)
newdata$state_stress_pre <- as.numeric(data$q19_answer)
newdata$state_boredom_pre <- as.numeric(data$q21_answer)
newdata$state_tired_pre <- as.numeric(data$q22_answer)
newdata$state_disgust_pre <- as.numeric(data$q23_answer)
newdata$state_loneliness_pre <- as.numeric(data$q24_answer)
newdata$state_guilt_pre <- as.numeric(data$q25_answer)
newdata$state_shame_pre <- as.numeric(data$q26_answer)

newdata$state_happy_post <- as.numeric(data$q44_answer)
newdata$state_anger_post <- as.numeric(data$q45_answer)
newdata$state_calm_post <- as.numeric(data$q46_answer)
newdata$state_anxiety_post <- as.numeric(data$q47_answer)
newdata$state_depression_post <- as.numeric(data$q48_answer)
newdata$state_stress_post <- as.numeric(data$q49_answer)
newdata$state_boredom_post <- as.numeric(data$q51_answer)
newdata$state_tired_post <- as.numeric(data$q52_answer)
newdata$state_disgust_post <- as.numeric(data$q53_answer)
newdata$state_loneliness_post <- as.numeric(data$q54_answer)
newdata$state_guilt_post <- as.numeric(data$q55_answer)
newdata$state_shame_post <- as.numeric(data$q56_answer)

ndc <- newdata[,7:30]
ndc[is.na(ndc)] <- 0
newdata[,7:30] <- ndc




mindate <- c()
for(i in 1:nrow(newdata)){
  mindate[i] <- min(newdata$date[newdata$ID == newdata$ID[i]],na.rm=T)
}
newdata$start_date <- mindate
newdata$duration <- newdata$date  -  newdata$start_date

cor(newdata[,-c(2)],use = "pairwise.complete.obs")[,3]

train_indices <- sample(unique(newdata$ID),105)
train_data <- newdata[newdata$ID %in% train_indices,]
test_data <- newdata[!(newdata$ID %in% unique(train_data$ID)),]

library(splines)
##########Time of Day ###################
model_timeofday <- lme4::glmer(self_report_binge ~ bs(timeofday,knots = c(8,14,20)) + (1|ID),data=train_data,family=binomial)
summary(model)

y_hat <- predict(model_timeofday,test_data,allow.new.levels=T,type="response")

roc <- pROC::roc(test_data$self_report_binge,y_hat)
sens1 <- roc$sensitivities
spec1 <- roc$specificities
auc1 <- roc$auc

tod <- ggplot()+
  geom_line(aes(1-sens1,spec1))+
  geom_abline(aes(intercept=0,slope=1))+
  labs(x="Sensitivity",y="Specificity",title="Mixed Time of Day only")+
  scale_x_continuous(labels=c(1,0.75,0.5,0.25,0))+
  geom_label(aes(0.35,0.6),label=paste("AUC:",round(auc1,4)))+
  theme_classic()
#tod
#lmmadd::johnson_neyman(model_timeofday,"scale(state_stress_pre)","scale(willingness_to_restrict)")+
#labs(x="Stress",y="Slope of <Willingness to Restrict>")

##########Time of Year###################
model_timeofyear <- lme4::glmer(self_report_binge ~ bs(date%%365,knots=c(30,180,320)) + (1|ID),data=train_data,family=binomial)
summary(model_timeofyear)

y_hat <- predict(model_timeofyear,test_data,allow.new.levels=T,type="response")

roc <- pROC::roc(test_data$self_report_binge,y_hat)
sens2 <- roc$sensitivities
spec2 <- roc$specificities
auc2 <- roc$auc

toy <- ggplot()+
  geom_line(aes(1-sens2,spec2))+
  geom_abline(aes(intercept=0,slope=1))+
  labs(x="Sensitivity",y="Specificity",title="Mixed Time of Year only")+
  scale_x_continuous(labels=c(1,0.75,0.5,0.25,0))+
  geom_label(aes(0.35,0.6),label=paste("AUC:",round(auc2,4)))+
  theme_classic()
#toy

############Time of Year and Day################
model_both <- lme4::glmer(self_report_binge ~ bs(date%%365,knots=c(30,180,320)) + bs(timeofday,knots = c(8,14,20)) + (1|ID),data=train_data,family=binomial)
summary(model_both)

y_hat <- predict(model_both,test_data,allow.new.levels=T,type="response")

roc <- pROC::roc(test_data$self_report_binge,y_hat)
sens3 <- roc$sensitivities
spec3 <- roc$specificities
auc3 <- roc$auc

toytod <- ggplot()+
  geom_line(aes(1-sens3,spec3))+
  geom_abline(aes(intercept=0,slope=1))+
  labs(x="Sensitivity",y="Specificity",title="Mixed Time of Day and Year")+
  scale_x_continuous(labels=c(1,0.75,0.5,0.25,0))+
  geom_label(aes(0.35,0.6),label=paste("AUC:",round(auc3,4)))+
  theme_classic()
#toytod


#############Stepwise#################
train_data[,7:18] <- scale(train_data[,7:18])
test_data[,7:18] <- scale(test_data[,7:18])

model_full <- glmer(self_report_binge ~ 
                      state_calm_pre +
                      #willingness_to_restrict +
                      #state_depression_pre +
                      state_stress_pre * willingness_to_restrict +
                      state_boredom_pre +
                      state_disgust_pre * state_guilt_pre +
                      
                      #state_loneliness_pre +
                      state_guilt_pre +
                      bs(timeofday,knots = c(10,16),degree = 3) + 
                      #bs(date%%365,knots=c(30,180,320)) + 
                      (1|ID),
                    data=train_data,family=binomial)

summary(model_full)
AIC(model_full)

save(model_full,file="model.Rdata")


y_hat <- predict(model_full,test_data,allow.new.levels=T,type="response")
#y_hat_raw <- as.matrix(cbind(rep(1,nrow(test_data)),test_data[,c(32,9,12,13,15,16,17)])) %*% fixef(model_stepwise_allcovariates$Model)
#y_hat <- exp(y_hat_raw)/(1+exp(y_hat_raw))


roc <- pROC::roc(test_data$self_report_binge,y_hat)
sens4 <- roc$sensitivities
spec4 <- roc$specificities
auc4 <- roc$auc

mixedfull <- ggplot()+
  geom_line(aes(1-sens4,spec4))+
  geom_abline(aes(intercept=0,slope=1))+
  labs(x="Sensitivity",y="Specificity",title="Mixed Emotions and Time of Day and Year")+
  scale_x_continuous(labels=c(1,0.75,0.5,0.25,0))+
  geom_label(aes(0.35,0.6),label=paste("AUC:",round(auc4,4)))+
  theme_classic()
#mixedfull

jn1 <- lmmadd::johnson_neyman(model_full,"state_guilt_pre","state_disgust_pre")+
  labs(x="Guilt",y="Slope of \"Disgust\"")

jn2 <- lmmadd::johnson_neyman(model_full,"willingness_to_restrict","state_stress_pre")+
  labs(x="Stress",y="Slope of \"Willingness to Restrict\"")

#library(patchwork)

#jn1+jn2

#############Full pooled model###############
train_data[,7:18] <- scale(train_data[,7:18])
test_data[,7:18] <- scale(test_data[,7:18])

model_full_pooled <- glm(self_report_binge ~ 
                           state_calm_pre +
                           #willingness_to_restrict +
                           #state_depression_pre +
                           state_stress_pre * willingness_to_restrict +
                           state_boredom_pre +
                           state_disgust_pre * state_guilt_pre +
                           
                           #state_loneliness_pre +
                           state_guilt_pre +
                           bs(timeofday,knots = c(10,16),degree = 3),
                         data=train_data,family=binomial)

summary(model_full_pooled)
AIC(model_full_pooled)


y_hat <- predict(model_full_pooled,test_data,allow.new.levels=T,type="response")
#y_hat_raw <- as.matrix(cbind(rep(1,nrow(test_data)),test_data[,c(32,9,12,13,15,16,17)])) %*% fixef(model_stepwise_allcovariates$Model)
#y_hat <- exp(y_hat_raw)/(1+exp(y_hat_raw))


roc <- pROC::roc(test_data$self_report_binge,y_hat)
sens5 <- roc$sensitivities
spec5 <- roc$specificities
auc5 <- roc$auc

pooledfull <- ggplot()+
  geom_line(aes(1-sens5,spec5))+
  geom_abline(aes(intercept=0,slope=1))+
  labs(x="Sensitivity",y="Specificity",title="Pooled Emotions and Time of Day and Year")+
  scale_x_continuous(labels=c(1,0.75,0.5,0.25,0))+
  geom_label(aes(0.35,0.6),label=paste("AUC:",round(auc5,4)))+
  theme_classic()
#pooledfull

#jn1 <- lmmadd::johnson_neyman(model_full,"state_guilt_pre","state_disgust_pre")+
#labs(x="Guilt",y="Slope of \"Disgust\"")

#jn2 <- lmmadd::johnson_neyman(model_full,"willingness_to_restrict","state_stress_pre")+
#labs(x="Stress",y="Slope of \"Willingness to Restrict\"")

#library(patchwork)

#jn1+jn2

#############MERF#################
train_data_ <- train_data[!is.na(train_data$self_report_binge),]
test_data_ <- test_data[!is.na(test_data$self_report_binge),]

model_merf <- LongituRF::MERF(
  #ntree = 500,
  #mtry = 8,
  X = as.matrix(train_data_[c(7:17,33)]),
  Z = as.matrix(rep(1,nrow(train_data_))),
  id = match(train_data_$ID,unique(train_data_$ID)),
  Y = train_data_$self_report_binge,
  time = train_data_$duration,
  sto = "none"
)

y_hat <- predict(model_merf,
                 X = as.matrix(test_data_[c(7:17,33)]),
                 Z = as.matrix(rep(1,nrow(test_data_))),
                 id = match(test_data_$ID,unique(test_data_$ID)),
                 #Y = test_data_$self_report_binge,
                 time = test_data_$duration#,
                 #sto = "none"
                 
)

y_hat <- y_hat - min(y_hat)
y_hat <- y_hat / max(y_hat)

roc <- pROC::roc(test_data_$self_report_binge,y_hat)
sens6 <- roc$sensitivities
spec6 <- roc$specificities
auc6 <- roc$auc

merffull <- ggplot()+
  geom_line(aes(1-sens6,spec6))+
  geom_abline(aes(intercept=0,slope=1))+
  labs(x="Sensitivity",y="Specificity",title="MERF Emotions and Time of Day")+
  scale_x_continuous(labels=c(1,0.75,0.5,0.25,0))+
  geom_label(aes(0.35,0.6),label=paste("AUC:",round(auc6,4)))+
  theme_classic()
#merffull

############SVM - Linear####################
library(e1071)
X <- as.matrix(train_data[,c(7:17,33)])
Y <- train_data$self_report_binge
model_svm <- svm(Y ~ X,kernel='linear',probability=T)
X <- as.matrix(test_data[!is.na(test_data$self_report_binge),c(7:17,33)])
Y <- test_data$self_report_binge[!is.na(test_data$self_report_binge)]
y_hat <- predict(model_svm,X,type="response")

roc <- pROC::roc(Y,y_hat)
sens7 <- roc$sensitivities
spec7 <- roc$specificities
auc7 <- roc$auc

svmlinear <- ggplot()+
  geom_line(aes(1-sens7,spec7))+
  geom_abline(aes(intercept=0,slope=1))+
  labs(x="Sensitivity",y="Specificity",title="Linear SVM Emotions and Time of Day")+
  scale_x_continuous(labels=c(1,0.75,0.5,0.25,0))+
  geom_label(aes(0.35,0.6),label=paste("AUC:",round(auc7,4)))+
  theme_classic()
svmlinear

############SVM - Linear####################
library(e1071)
X <- as.matrix(train_data[,c(7:17,33)])
Y <- train_data$self_report_binge
model_svm <- svm(Y ~ X,kernel='poly',probability=T)
X <- as.matrix(test_data[!is.na(test_data$self_report_binge),c(7:17,33)])
Y <- test_data$self_report_binge[!is.na(test_data$self_report_binge)]
y_hat <- predict(model_svm,X,type="response")

roc <- pROC::roc(Y,y_hat)
sens8 <- roc$sensitivities
spec8 <- roc$specificities
auc8 <- roc$auc

svmpoly <- ggplot()+
  geom_line(aes(1-sens8,spec8))+
  geom_abline(aes(intercept=0,slope=1))+
  labs(x="Sensitivity",y="Specificity",title="Poly SVM Emotions and Time of Day")+
  scale_x_continuous(labels=c(1,0.75,0.5,0.25,0))+
  geom_label(aes(0.35,0.6),label=paste("AUC:",round(auc8,4)))+
  theme_classic()
svmpoly

############SVM - Linear####################
library(e1071)
X <- as.matrix(train_data[,c(7:17,33)])
Y <- train_data$self_report_binge
model_svm <- svm(Y ~ X,kernel='radial',probability=T)
X <- as.matrix(test_data[!is.na(test_data$self_report_binge),c(7:17,33)])
Y <- test_data$self_report_binge[!is.na(test_data$self_report_binge)]
y_hat <- predict(model_svm,X,type="response")

roc <- pROC::roc(Y,y_hat)
sens9 <- roc$sensitivities
spec9 <- roc$specificities
auc9 <- roc$auc

svmradial <- ggplot()+
  geom_line(aes(1-sens9,spec9))+
  geom_abline(aes(intercept=0,slope=1))+
  labs(x="Sensitivity",y="Specificity",title="Radial SVM Emotions and Time of Day")+
  scale_x_continuous(labels=c(1,0.75,0.5,0.25,0))+
  geom_label(aes(0.35,0.6),label=paste("AUC:",round(auc9,4)))+
  theme_classic()
svmradial

##############Compare all models#################
library(patchwork)
(toy + tod + toytod) /
  (pooledfull + mixedfull + merffull) /
  (svmlinear + svmradial + svmpoly)




##############Other approaches:
###### 1. RFCluster
###### 2. GPBoost Library
###### 3. GLMerTree
###### 4. MetBoost

################################################

library(ggplot2)
ggplot(newdata[!is.na(newdata$self_report_binge),],aes(duration %% 1,willingness_to_restrict,color=as.factor(self_report_binge),group=as.factor(self_report_binge)))+
  geom_point()+
  geom_smooth(method = "loess")+
  #geom_line()+
  theme_classic()


openxlsx::write.xlsx(newdata,"dataset_cleaned.xlsx")


cormat <- cor(newdata[,5:19],use = "pairwise.complete.obs")
cormat[upper.tri(cormat)] <- NA

openxlsx::write.xlsx(as.data.frame(cormat),"correlation_matrix.xlsx",rowNames=T)


library(tidyverse)

m <- newdata %>% group_by(ID) %>% 
  summarize(n_na = mean(is.na(state_anger)))

mout <- step.Lmer(newdata,"self_report_binge","willingness_to_restrict","",2,colnames(newdata)[7:18])
summary(mout$Model)

#Descriptives
counts <- newdata %>% group_by(ID) %>% 
  summarize(N=n(),minDate=min(date,na.rm=T),maxDate=max(date,na.rm=T))
hist(counts$N)

#number of 14-day runs
table((counts$maxDate-counts$minDate) > 13)

table(newdata$self_report_binge)

datediffs <- newdata %>% group_by(ID) %>% 
  summarize(date_diffs = mean(date[2:length(date)]-date[1:(length(date)-1)],na.rm=T))

quantile(datediffs$date_diffs,na.rm=T,c(0.05,0.95))*24

sort(colMeans(is.na(newdata)))

data$self_report_binge <- data$`Self-reported binge`

d <- data %>% select(c(q12_answervalue,self_report_binge)) %>% group_by(q12_answervalue) %>% 
  summarize(prop = mean(as.numeric(self_report_binge),na.rm=T),n=n())

d <- d[d$n > 100,]
View(d)

library(ggplot2)

data.frame(Thing=c("Colleagues","family","Partner","Flatmates","Nobody","Friends"),Value=c())

ggplot(d,aes(d$q12_answervalue,d$prop))+
  geom_bar(stat='identity')+
  theme_classic()

bingedesc <- newdata %>% group_by(ID) %>% 
  summarize(prop = mean(self_report_binge,na.rm=T))


hist(bingedesc$prop)

ggplot()+
  geom_boxplot(aes(bingedesc$prop),fill="lightgrey")+
  labs(x="Binge Proportion",title = "Binge proportion distribution of participants")+
  scale_y_continuous(breaks=c())+
  scale_x_continuous()+
  theme_classic()

dim(newdata)


dh <- newdata$date %% 1
dh*24

dhf <- data.frame(time = dh*24,binge = newdata$self_report_binge)
dhf <- dhf[complete.cases(dhf),]

ggplot(dhf)+
  geom_histogram(aes(time,group=binge,fill=as.factor(binge)),color="black")+
  xlim(c(0,24))+
  scale_x_continuous(breaks = c(0,4,8,12,16,20,24))+
  labs(x="Time of day",y="Count",title="Observation density by time of day",fill="Binge")+
  scale_fill_manual(values=c("green","red"))+
  theme_classic()

library(splines)
####### Spline fit time of day #########
model <- lme4::glmer(self_report_binge ~ bs((duration %% 1)*24,knots=c(4,8,12,16,20),degree=3) + (1|ID),data=newdata,family="binomial")
summary(model)

y_hat <- predict(model)

dursamples <- seq(0,24,length.out=1000)
ds <- bs(dursamples,knots=c(4,8,12,16,20),degree=3)
#ds <- ds[,1:5]
ds <- cbind(rep(1,nrow(ds)),ds)

pred <- ds %*% lme4::fixef(model)

ggplot(mapping=aes(dursamples,exp(pred)/(exp(pred)+1)))+
  geom_line()+
  labs(x="Time of day",y="Binge probability")+
  scale_x_continuous(breaks=c(0,4,8,12,16,20,24))+
  theme_classic()

######## spline fit duration #########
model <- lme4::glmer(self_report_binge ~ bs(duration,knots=1:13,degree=3) + (1|ID),data=newdata,family="binomial")
summary(model)

dursamples <- seq(0,14,length.out=1000)
ds <- bs(dursamples,knots=1:13,degree=3)
#ds <- ds[,1:5]
ds <- cbind(rep(1,nrow(ds)),ds)

pred <- ds %*% lme4::fixef(model)

ggplot(mapping=aes(dursamples,exp(pred)/(exp(pred)+1)))+
  geom_line()+
  labs(x="Follow up day",y="Binge probability")+
  scale_x_continuous(breaks=c(1:13),limits = c(0.5,13.5))+
  theme_classic()


plot(dursamples,exp(pred)/(1+exp(pred)))


####### binge rate by season ###########
model <- lme4::glmer(self_report_binge ~ bs(date%%365,knots=c(60,120,180,240,300),degree=3) + (1|ID),data=newdata,family="binomial")
summary(model)

dursamples <- seq(0,365,length.out=1000)
ds <- bs(dursamples,knots=c(60,120,180,240,300),degree=3)
#ds <- ds[,1:5]
ds <- cbind(rep(1,nrow(ds)),ds)

pred <- ds %*% lme4::fixef(model)
season <- ifelse(dursamples < 59,"winter",ifelse(
  dursamples < 59+91,"spring",ifelse(
    dursamples < 59+91+94,"summer",ifelse(
      dursamples < 59+91+94+90,"fall","winter2"
    )
  )
))


ggplot(mapping=aes(dursamples,exp(pred)/(exp(pred)+1),fill=season))+
  geom_area()+
  labs(x="Time of year",y="Binge probability")+
  scale_x_continuous(breaks=c(seq(15,365,length.out=12)),labels = month.abb,limits=c(0,365))+
  scale_fill_manual(values=c("orange","green","red","lightblue","lightblue"))+
  theme_classic()

######## binge depending on time ########
model <- lme4::glmer(self_report_binge ~ bs(date,knots=c(44400,44500,44600),degree=3) + (1|ID),data=newdata,family="binomial")
summary(model)

dursamples <- seq(min(newdata$date,na.rm=T),max(newdata$date,na.rm=T),length.out=1000)
ds <- bs(dursamples,knots=c(44400,44500,44600),degree=3)
#ds <- ds[,1:5]
ds <- cbind(rep(1,nrow(ds)),ds)

pred <- ds %*% lme4::fixef(model)

ggplot()+
  geom_line(aes(dursamples,exp(pred)/(exp(pred)+1)))

######## binge by meal ########



####### descriptives ########

desctab <- psych::describe(newdata)
openxlsx::write.xlsx(transpose(as.data.frame(desctab)),"descriptives.xlsx",rowNames=T)


####### modeling #########
library(splines)
newdata$timeofday <- (newdata$duration %% 1)*24
basemodel <- lme4::glmer(self_report_binge ~ 1 + (1|ID),data=newdata,family="binomial")

performance::icc(basemodel)

model_data <- newdata[!is.na(newdata$timeofday) & !is.na(newdata$self_report_binge),c("self_report_binge","ID")]

model_data <- cbind(model_data,bs(newdata$timeofday[!is.na(newdata$timeofday) & !is.na(newdata$self_report_binge)],knots=c(8,13,18),degree=3))

model_splines <- lme4::glmer(self_report_binge ~ . -ID + (1|ID),data=model_data,subset = train_indices,family="binomial")

summary(model_splines)

test_data <- model_data[-train_indices,c(1,3:8)]
test_data[,1] <- rep(1,nrow(test_data))

pred_test <- as.matrix(test_data) %*% as.matrix(lme4::fixef(model_splines))

p_test <- exp(pred_test)/(1+exp(pred_test))

mean(round(p_test) == model_data$self_report_binge[-train_indices])

roc <- pROC::roc( model_data$self_report_binge[-train_indices],p_test)

sens <- roc$sensitivities
spec <- roc$specificities
auc <- roc$auc

ggplot()+
  geom_line(aes(1-sens,spec))+
  geom_abline(aes(intercept=0,slope=1))+
  labs(x="Sensitivity",y="Specificity")+
  scale_x_continuous(labels=c(1,0.75,0.5,0.25,0))+
  geom_label(aes(0.35,0.6),label=paste("AUC:",round(auc,4)))+
  theme_classic()


summary(model_splines)

