#Installing all necessary Packages

pkgs <- c("rpart", "rpart.plot", "party", "randomForest", "e1071", "dplyr", "quantmod")
#install.packages(pkgs,lib="C:/Rick/R/Rlib",depend=TRUE)
install.packages(pkgs, depend=TRUE)
install.packages("ggplot2")
install.packages("anytime")
library(ggplot2)
library(dplyr)
library(quantmod)
library(anytime)

#renaming dataframe
df = Pruned_cols

grouped_df <- aggregate(df$WnvPresent, by=list(Date=df$Date), FUN=sum) 
grouped_df$Date <-as.Date(grouped_df$Date, format = "%m/%d/%y")
sort <-grouped_df[order(grouped_df$Date),]


ggplot(freqs, aes(x=grouped_df$Date, y=x)) + geom_bar(stat="identity") +
  scale_x_date(breaks="1 month", labels=date_format("%Y-%b"),
               limits=c(as.Date("2008-04-30"),as.Date("2012-04-01"))) +
  ylab("Frequency") + xlab("Year and Month") +
  theme_bw() + opts(axis.text.x = theme_text(angle=90))
sort <-grouped_df[order(grouped_df$Date),]

grouped_df$Dateconverted <- as.Date(grouped_df$Date, format="%m-%d-%Y")
grouped_df <- order(grouped_df$Date)
grouped_df %>%
ggplot(aes(grouped_df$WnvPresent))+
  geom_histogram()+ 
  labs(x = "Month", y = "Presence", title = "WNV Presence")+
  theme_minimal()
for(i in grouped_df){
 
}
df_day %>% 
  ggplot(aes(ds, y))+
  geom_line()+ 
  labs(x = "Date", y = "Traffic", title = "Seattle Bike Traffic")+
  theme_minimal()
#Delt(x1, x2 = NULL, k = 0, type = c("arithmetic", "log")) from quantmod

#creating those independent variables
df <- mutate(df,
             week = rep(1:52,each = 5, len =3001))
df <- mutate(df,
             month = rep(1:12, each = 30, len = 3001))
df <- mutate(df,
             year = rep(1:9,each = 365, len = 3001))

df <- mutate(df,
             Change_week.1 = ifelse(is.na(round(Delt(df$Open,df$Close, k = 4, "arithmetic" ),3)),
                                    0,
                                    round(Delt(df$Open,df$Close, k = 4, "arithmetic" ),3)))
df <- mutate(df,
             Change_month.1 = ifelse(is.na(round(Delt(df$Open,df$Close, k = 29, "arithmetic" ),3)),
                                     0,
                                     round(Delt(df$Open,df$Close, k = 29, "arithmetic" ),3)))
df <- mutate(df,
             Change_year.1 = ifelse(is.na(round(Delt(df$Open,df$Close, k = 364, "arithmetic" ),3)),
                                    0,
                                    round(Delt(df$Open,df$Close, k = 364, "arithmetic" ),3)))
#creating the training data
set.seed(1234)
train <- sample(nrow(df), 0.5*nrow(df))

# Define training data frame using training data
df.train <- df[train,]
# Define validation data frame using the rest of the data
df.validate <- df[-train,]

# CUSTOMIZE DATA: Table counts the observations for each categorical value of target
# CUSTOMIZE DATA: The categorical dependent variable is called target
table(df.train$target)
table(df.validate$target)

# Decision Tree

# library loads add-on packages
#library(rpart,lib="C:/Rick/R/Rlib")
library(rpart)

# Define seed for random number generator
set.seed(1234)

# Fit a recursive partitioning model
# CUSTOMIZE DATA: the first parameter target specifies the categorical dependent variable 

# created data tree based on training data
dtree <- rpart(target ~ ., data=df.train, method="class",
               parms=list(split="information"))

# Summarize the decision tree including decision nodes and leaf nodes
# The decision tree nodes are described row by row, then left to right
summary(dtree)

# Display decision tree.  The true values follow the left branches.
plot(dtree);text(dtree)

# Display decision tree complexity parameter table which is a matrix of information on the optimal prunings based on a complexity parameter
# Identify CP that corresponds to the lowest xerror
dtree$cptable

# Plot complexity parameter table
plotcp(dtree)

# Determine CP that corresponds to the lowest xerror
# Get index of CP with lowest xerror
opt <- which.min(dtree$cptable[,"xerror"])
# get its CP value
cp <- dtree$cptable[opt, "CP"]

# Prune decision tree to decrease overfitting
dtree.pruned <- prune(dtree, cp)

# Display pruned decision tree.  The true values follow the left branches.
plot(dtree.pruned);text(dtree.pruned)

# class displays the object class
class(dtree$cptable)

# names displays the names of an object
names(dtree)

#library(rpart.plot,lib="C:/Rick/R/Rlib")
library(rpart.plot)

# Determine proportion of categorical dependent variable
# CUSTOMIZE DATA: The categorical dependent variable is called target
table(df.train$target)/nrow(df.train)

# prp plots an rpart model
prp(dtree.pruned, type=2, extra=104, fallen.leaves=TRUE, main="Decision Tree")

# predict evaluates the application of a model to a data frame
dtree.pred <- predict(dtree.pruned, df.validate, type="class")
# define classification matrix
# CUSTOMIZE DATA: The categorical dependent variable is called target
dtree.perf <- table(df.validate$target, dtree.pred, dnn=c("Actual", "Predicted"))
dtree.perf

prp(dtree, type=2, extra=104, fallen.leaves=TRUE, main="Decision Tree")


#library(randomForest,lib="C:/Rick/R/Rlib")
library(randomForest)

# Define seed for random number generator
set.seed(1234)
# Fit a random forest model
# The na.action=na.roughfix option replaces missing values on numeric variables with column medians and missing values on categorical variables with the modal category for that variable
# CUSTOMIZE DATA: The categorical dependent variable is called target
forest <- randomForest(target ~ ., data=df.train,
                       na.action=na.roughfix,
                       importance=TRUE)

# Display a summary for a random forest model
forest

# Display variable importance measures for a random forest model
importance(forest, type=2)

# predict evaluates the application of a model to a data frame
forest.pred <- predict(forest, df.validate)
# define classification matrix
# CUSTOMIZE DATA: The categorical dependent variable is called target
forest.perf <- table(df.validate$target, forest.pred,
                     dnn=c("Actual", "Predicted"))
forest.perf

#first Plot Out of bounds error
Error_data <- data.frame(
  Trees=rep(1:nrow(forest$err.rate),times=3),
  Type=rep(c("OOB","Yes","No"),each=nrow(forest$err.rate)),
  Error=c(forest$err.rate[,"OOB"],
          forest$err.rate[,"Yes"],
          forest$err.rate[,"No"]))

ggplot(data = Error_data, aes(x=Trees,y=Error))+
  geom_line(aes(color=Type)) +ggtitle("Out of Bounds Error")

#Line plot: 
install.packages("reshape2")
library("reshape2")
library("ggplot2")
attach(df)
dfline <- melt(df, id.vars="year", value.name="Change_week.1", variable.name="week")
ggplot(data=df, aes(x=as.numeric(df$month), y=as.numeric(df$Open), group = year, colour = year)) +
  geom_step() + geom_smooth() + 
  ggtitle("Month Over Month Change") + 
  xlab("Month")+ylab("Opening Values") +
  labs(fill = "Year")

#Line plot over week

# ax, alpha, color, fill, linetype,
# shape, size
# Visualizing error
# df <- data.frame(grp = c("A", "B"), fit = 4:5, se = 1:2)
k <- ggplot(df, aes(x = df$Open, fill = df$year, bins = 30))
k + geom_dotplot()

k <- ggplot(df,aes(Open, fill = target))+geom_histogram() + ggtitle("Open Values hitting Target")
k
#x, ymax, ymin, alpha, color, linetype, size,
#width (also geom_errorbarh()) 

# library
install.packages("ggridges")
install.packages("viridis")
install.packages("hrbrthemes")
library(ggridges)
library(ggplot2)
library(viridis)
library(hrbrthemes)

Month = as.factor(df$month)
# Ridgeline Plot
ggplot(df, aes(x = Change_month.1, y = Month, height = 1,fill = ..x..)) +
  geom_ridgeline_gradient(data = df, gradient_lwd = .5) +
  scale_fill_viridis(name = "% Change", option = "C") +
  labs(title = 'Month Over Month % Change') +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)) + ylab("Month") + xlab("% Change Per Month for each Year") +
  theme_ridges(font_size = 13, grid = TRUE) + theme(axis.title.y = element_blank()) + ylab("Month")






