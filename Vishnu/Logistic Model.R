#Clear the list
rm(list = ls(all=TRUE)) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~stepI:import dataset,and prameters~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data <- read.csv("Documents/data_final.csv",header=TRUE)
#spaceless <- function(x) {colnames(x) <- gsub(".", "_", colnames(x));x}
#data <- spaceless(data)

x=nrow(data)
y=(.90)*x
y=round(y, digits=0)
sample_numbers <- sample(1:x, y, FALSE)

validation <- data[-sample_numbers,]
data <- data[sample_numbers,]

#Ratio of 1s
sum(validation$WnvPresent)/length(validation$WnvPresent)
sum(data$WnvPresent)/length(data$WnvPresent)

#~~~~~~~~~~~~~~~VIF function definition
vif <- function(object, ...)
  UseMethod("vif")
vif.default <- function(object, ...)
  stop("No default method for vif. Sorry.")

vif.lm <- function(object, ...) {
  V <- summary(object)$cov.unscaled
  Vi <- crossprod(model.matrix(object))
  nam <- names(coef(object))
  if(k <- match("(Intercept)", nam, nomatch = F)) {
    v1 <- diag(V)[-k]
    v2 <- (diag(Vi)[-k] - Vi[k, -k]^2/Vi[k,k])
    nam <- nam[-k]
  } else {
    v1 <- diag(V)
    v2 <- diag(Vi)
    warning("No intercept term detected. Results may surprise.")
  }
  structure(v1*v2, names = nam)
}
vif.lm <- function(object, ...) {
  V <- summary(object)$cov.unscaled
  Vi <- crossprod(model.matrix(object))
  nam <- names(coef(object))
  if(k <- match("(Intercept)", nam, nomatch = F)) {
    v1 <- diag(V)[-k]
    v2 <- (diag(Vi)[-k] - Vi[k, -k]^2/Vi[k,k])
    nam <- nam[-k]
  } else {
    v1 <- diag(V)
    v2 <- diag(Vi)
    warning("No intercept term detected. Results may surprise.")
  }
  structure(v1*v2, names = nam)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ VIF ~~~~~~~~~~~~~~~~~
#input cut_off_vif, data
#~~~~~~~~~~~~ VIF  value sorting~~~~~~~~~~

dep=names(data[10])
var_names=names(data[,c(11:45,47:94)])
model_equtn=as.formula(paste(dep,(paste(var_names, collapse = '+')),sep='~'))

Reg<- lm(model_equtn,na.action=na.pass,data=data)
summary(Reg)
vfit1 <- data.frame(vif(Reg))  

non_mlticolnr=(rownames(vfit1[,0]))
vif_value=(vfit1[,1])

vif_vars=data.frame(non_mlticolnr,vif_value)
#library(sqldf)
vif_vars_desc=sqldf("select * from vif_vars order by vif_value desc")


# loop for variable selection starts
cut_off_vif <- 10
while(vif_vars_desc[1,2]>cut_off_vif)
{
  vif_vars_desc=(vif_vars_desc[2:nrow(vif_vars_desc),])
  
  model_equtn=as.formula(paste(dep,(paste(vif_vars_desc[,1], collapse = '+')),sep='~'))
  Reg<- lm(model_equtn,na.action=na.pass,data=data)
  vfit1 <- data.frame(vif(Reg))  
  non_mlticolnr=rownames(vfit1[,0])
  vif_value=vfit1[,1]
  
  vif_vars=data.frame(non_mlticolnr,vif_value)
  vif_vars_desc=sqldf("select * from vif_vars order by vif_value desc")
  
}


var_list=vif_vars_desc$non_mlticolnr

#~~~~~~~~~~~~ p value sorting~~~~~~~~~~

#dep=names(data[2])
signi_vars2=var_list
model_equtn=as.formula(paste(dep,(paste(signi_vars2, collapse = '+')),sep='~'))
reg_model<- glm(model_equtn, family=binomial(link="logit"), na.action=na.pass,data=data)

all_vars=summary(reg_model)$coef
imp_vars=rownames(all_vars[,0])
p_value=all_vars[,4]
signi_vars=data.frame(imp_vars,p_value)
signi_vars1=signi_vars[2:nrow(signi_vars),]
signi_vars2=sqldf("select * from signi_vars1 order by p_value desc")

# loop for variable selection starts

cut_off_pvalue = 0.05

while(signi_vars2[1,2]>cut_off_pvalue)
{
  signi_vars2=signi_vars2[2:nrow(signi_vars2),]
  
  model_equtn=as.formula(paste(dep,(paste(signi_vars2$imp_vars, collapse = '+')),sep='~'))
  reg_model<- glm(model_equtn, family=binomial(link="logit"), na.action=na.pass,data=data)
  
  all_vars=summary(reg_model)$coef
  
  imp_vars=rownames(all_vars[,0])
  p_value=all_vars[,4]
  signi_vars=data.frame(imp_vars,p_value)
  
  signi_vars1=signi_vars[2:nrow(signi_vars),]
  signi_vars2=sqldf("select * from signi_vars1 order by p_value desc")
}

###########################################################


########################################################

#dep=names(data[2])
model_equtn=as.formula(paste(dep,(paste(signi_vars2$imp_vars, collapse = '+')),sep='~'))
reg_model<- glm(model_equtn, family=binomial(link="logit"), na.action=na.pass,data=data)
summary(reg_model)
pred_train <- predict(reg_model, data, type='response')
train_data <- data.frame(data[c(1:2,10)], pred_train, type='response')

pred_val=data.frame(predict(reg_model, validation, type='response'))
val_data <- data.frame(validation[c(1:2,10)], pred_val)

#####################################################

all_actual=data[,10]
Acc_table=data.frame(cbind(all_actual, pred_train))
Acc_table$new=NULL


k=1
prob_cutoff=1
TPR=NULL
FPR=NULL
diff <- NULL
probability_cutoff=NULL
overall_accuracy=NULL
#library(sqldf)

while(prob_cutoff<100)
{
  Acc_table$prob=ifelse(Acc_table$pred_train<=(prob_cutoff/100),0,1)
  count_Actual_1=sqldf("select count(all_actual) as no_of_1 from Acc_table where all_actual=1")
  count_Actual_0=sqldf("select count(all_actual) as no_of_1 from Acc_table where all_actual=0")
  prdctn_1=sqldf("select count(prob) as no_of_1 from Acc_table where all_actual=1 and prob=1")
  prdctn_0=sqldf("select count(prob) as no_of_1 from Acc_table where all_actual=0 and prob=0")
  TPR[k]=(prdctn_1/count_Actual_1)*100
  FPR[k]=(prdctn_0/count_Actual_0)*100
  overall_accuracy[k]=(prdctn_1+prdctn_0)/(count_Actual_1+count_Actual_0)*100
  probability_cutoff[k]=(prob_cutoff/100)
  diff[k]=abs(as.numeric(TPR[k])-as.numeric(FPR[k]))
  prob_cutoff=prob_cutoff+1
  k=k+1
}

performance_table=data.frame(cbind(probability_cutoff,TPR,FPR,overall_accuracy, diff))
performance_table=performance_table[order(diff),]
prob_cutoff=as.numeric(performance_table[1,1])

train_data$pred_index=ifelse(train_data$pred_train<=prob_cutoff,0,1)
colnames(val_data)[4] <- "pred_val"
val_data$pred_index=ifelse(val_data$pred_val<=prob_cutoff,0,1)

train_metric <- matrix(nrow = 2, ncol = 2)
val_metric <- matrix(nrow = 2, ncol = 2)
train_metric <- data.frame(train_metric)
val_metric <- data.frame(val_metric)

colnames(train_metric) <- c("actual-0", "actual-1")
rownames(train_metric) <- c("predicted-0", "predicted-1")
colnames(val_metric) <- c("actual-0", "actual-1")
rownames(val_metric) <- c("predicted-0", "predicted-1")

train_metric[1,1] <- sqldf("select count(pred_index) from train_data where WnvPresent=0 and pred_index=0")*100/nrow(train_data)
train_metric[2,1] <- sqldf("select count(pred_index) from train_data where WnvPresent=0 and pred_index=1")*100/nrow(train_data)
train_metric[1,2] <- sqldf("select count(pred_index) from train_data where WnvPresent=1 and pred_index=0")*100/nrow(train_data)
train_metric[2,2] <- sqldf("select count(pred_index) from train_data where WnvPresent=1 and pred_index=1")*100/nrow(train_data)


val_metric[1,1] <- sqldf("select count(pred_index) from val_data where WnvPresent=0 and pred_index=0")*100/nrow(val_data)
val_metric[2,1] <- sqldf("select count(pred_index) from val_data where WnvPresent=0 and pred_index=1")*100/nrow(val_data)
val_metric[1,2] <- sqldf("select count(pred_index) from val_data where WnvPresent=1 and pred_index=0")*100/nrow(val_data)
val_metric[2,2] <- sqldf("select count(pred_index) from val_data where WnvPresent=1 and pred_index=1")*100/nrow(val_data)
#################################################



