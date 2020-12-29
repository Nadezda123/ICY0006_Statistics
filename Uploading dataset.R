#check the working directory
getwd()


#import .cvs file to your Global Enviroment
insurance <- read.csv("insurance.csv", header = TRUE, sep = ",")

insurance

head(insurance)
str(insurance)

#visualizing data r - plot example
plot(insurance$age)

barplot(insurance$age)
str(insurance)

head(insurance, n=3)

plot(insurance$age, xlab = 'Age Groups', main = 'Age Graph', col = 'blue')
hist(insurance$age, xlab = 'Age Groups', main = 'Age Graph', col = 'blue')

attach(insurance)

#Installing & Loading the package 

install.packages("ggplot2") 
library(ggplot2)

install.packages("plotly")
library(plotly)

hist(insurance$age, x <- c(1:100))

hist(insurance$age, xlab = 'age', ylab = c(1:100), main = 'Frequency', col = 'blue')

plot(sex, xlab = Female)

hist(insurance$age, breaks = seq(0, 100, 10), main = 'Frequency', col = 'blue')

barplot(insurance$sex, names.arg = insurance$sex)

par(mar = c(5, 5, 5, 5))
pie(insurance$sex)

help(plot)
plot
hist(insurance$age, xlab = "age", ylab = "frequency", seq = 5, color = "green")

prop.table(with(df, table(year, sex)), 1)

df <- data.frame(variable=c(rep('Males', 10), rep('Females', 10)), value=sample(1:1000, 20))

mean(age)
mean(sex)
table(sex)
table(bmi)
mean(bmi)
install.packages("knitr")
install.packages("rmarkdown")
tinytex::install_tinytex()
library(knitr)
uninstall.packages(knitr)
install.packages('rmarkdown')
install.packages('plotly')

#import .cvs file to your Global Enviroment
insurance <- read.csv("insurance.csv", header = TRUE, sep = ",")
library(knitr) 
knit('Stage_1.Rmd')

#check the working directory
getwd()
knitr::opts_chunk$set(error = TRUE)
library("knitr")
knit2html("Stage_1.Rmd")
rmarkdown::render('Stage_1.rmd', output_format = 'html_document')
rmarkdown::render('Stage_1.rmd', output_format = 'html_document')

head(insurance)

install.packages('knitr', dependencies = TRUE)
library(knitr)

#Summary of Central Tendency Measures
summary(insurance)
#Calculating Mean
mean1<-mean(age)
age<-insurance$age
mean1<-mean(age)
mean2<-sum(insurance$age)/length(insurance$age)

# Calculating Median
median1<-median(insurance$age)
# function
median.func<-function (age) {
  age.sorted<-sort(age)
  l<-length(age.sorted)
  r<-(age.sorted[floor(1/2)]+age.sorted[ceiling(1/2)])/2
}
median2<-median.func(insurance$age[1:1338])

# Mode
# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(insurance$region)
getmode(insurance$sex)

mean.bmi<-mean(insurance$bmi)

#Install packages "psych"
install.packages("psych")
library(psych)
describe(insurance)

age<-insurance$age
sex<-insurance$sex
bmi<-insurance$bmi
children<-insurance$children
smoker<-insurance$smoker
region<-insurance$region
charges<-insurance$charges

trimmean<-trimmean(insurance$age)
install.packages("matlab")
library(matlab)
trimmean<- function (v) {
  trimMean(vec, p1, p2)
}
trimmean1<-trimmean(insurance$age)

var(insurance$age)

sd(insurance)
head(insurance)

sapply(insurance[,1:7], sd)

range(insurance$age, insurance$bmi)


#box plot

boxplot(insurance$age, horizontal = TRUE, main = "Medidcal Insurance")

# Density plot

insurance.new <-insurance[insurance$sex != "Sex",]
insurance.new <-insurance[insurance$region != "Region",]
insurance.new <-insurance[insurance$smoker != "Smoker",]

par(mfrow=c(3, 3))
colnames <- dimnames(insurance.new)[[2]]
for (i in 1) {
  d <- density(insurance.new[,i])
  plot(d, type="n", main=colnames[i])
  polygon(d, col="red", border="gray")
}
boxplot(insurance$age, horizontal = TRUE, main = "Medidcal Insurance")

------

par(mfrow = c(1,2)) # combine the two plots
par("mar")
par(mar=c(1,1,1,1))
hist(insurance$charges, main = "Histogram of charges", col = "yellow")
plot(density(insurance$charges), main = "Density plot of charges")
polygon(density(insurance$charges), col = "red")



par(mfrow = c(1,3))
barplot(table(insurance$sex), main = "sex")
barplot(table(insurance$smoker), main = "smoker")
barplot(table(insurance$region), main = "region")




-------
#creating scatterplot

input <- insurance[,c('age','charges')]
print(head(input))

plot(x = input$age, y = input$charges,
     xlab = "Age",
     ylab = "Charges",
     xlim = c(15, 70),
     ylim = c(1000,15000),
     main = "Age vs Charges"
     )
######
 
# find Q1, Q3 and IQR for values in column Age
Q1 <-quantile(insurance$age, .25)
Q3 <- quantile(insurance$charges, .75)
IQR <- IQR(insurance$age)

# only keep rows that have values within 1.5*IQR of Q1 and Q3
no_outliers <- subset(insurance, insurance$age> (Q1 - 1.5*IQR) & insurance$age< (Q3 + 1.5*IQR))

#view row and column count of new data  frame
dim(no_outliers)

# to check for outliers with boxplot
par(mfrow = c(1,3))
par("mar")
par(mar=c(1,1,1,1))
boxplot(insurance$age, main = "Histogram of age")
boxplot(insurance$bmi, main = "Histogram of bmi")
boxplot(insurance$children, main = "Histogram of children")


# removing outliers with IQR
# find Q1, Q3 and IQR for values in column Age
Q1 <-quantile(insurance$bmi, .25)
Q3 <- quantile(insurance$bmi, .75)
IQR <- IQR(insurance$bmi)

# only keep rows that have values within 1.5*IQR of Q1 and Q3
no_outliers <- subset(insurance, insurance$bmi> (Q1 - 1.5*IQR) & insurance$bmi< (Q3 + 1.5*IQR))

#view row and column count of new data  frame
dim(no_outliers)

insurance_2 <- no_outliers(insurance)
str(insurance_2)


insurance_2 <- subset(insurance, insurance$bmi> (Q1 - 1.5*IQR) & insurance$bmi< (Q3 + 1.5*IQR))
str(insurance_2)


input <- insurance[,c('age','charges')]
print(head(input))
plot(x = input$age, y = input$charges,
     xlab = "Age",
     ylab = "Charges",
     xlim = c(15, 70),
     ylim = c(1000,15000),
     main = "Age vs Charges"
)
install.packages("hrbrthemes")
library(hrbrthemes)
p <- ggplot(data = insurance, aes(x = age, y = charges)) + geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE) +
  theme_ipsum()



install.packages("corrplot")
library("corrplot")
Filter(is.numeric, x)
(insurance[sapply(insurance, is.numeric)])

cor(insurance$age, insurance$charges, method = "pearson")
cor(insurance$age, insurance$charges, method = "spearman")

library("corrplot")
mat_1 <- as.dist(cor((insurance_2[sapply(insurance_2, is.numeric)])))
mat_1
library(GGally)

corrplot.mixed(cor(mat_1), title = "Correlations between numeric variables", order="hclust", tl.col="black")

library(corrgram)
library(hmisc)
install.packages("corrgram")
install.packages("Hmisc")
library(Hmisc)
library(corrgram)
corrclass <- rcorr(as.matrix(insurance))
print(corrclass)

library("corrplot")
mat_1 <- as.matrix(cor((insurance_2[sapply(insurance_2, is.numeric)])))
mat_1

corrplot(mat_1)
x <- mat_1$age
y <- mat_1$charges
plot(x,y, main = "Age vs Charges",
     xlab = "age", ylab = "charges",
     pch = 1, frame = FALSE)
abline(lm(y ~ x, data = insurance), col = "blue")
linearReg <- lm(x ~ y, data = insurance)
print(linearReg)

install.packages("GGally")
library(GGally)
ggcorr(mat_1)
ggcorr(mat_1,
       nbreaks = 10,
       low = "steelblue",
       mid = "white",
       high = "darkred",
       geom = "circle")

ggcorr(mat_1,
       nbreaks = 6,
       label = TRUE,
       label_size = 3,
       color = "grey50")

corrplot.mixed(cor(mat_1), title = "Correlations between numeric variables", order="hclust", tl.col="black")



install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(mat_1, histogram=TRUE, pch=10)

install.packages("corrr")
library(corrr)
network_plot(correlate(mat_1), min_cor=0.5)


input <- insurance[,c('age','charges')]
print(head(input))

plot(x = input$age, y = input$charges,
xlab = "Age",
ylab = "Charges",
xlim = c(15, 70),
ylim = c(1000,15000),
main = "Age vs Charges"
)
abline(lm(input$charges ~ input$age))

input <- insurance[,c('age','charges')]
input.graph <- input + geom_smooth(method = "lm", col = "black")



summary(insurance)
summary(mat_1)


scatter.smooth(x = insurance$age, y = insurance$charges, main = "Age vs Charges")

plot(x = input$age, y = input$charges,
     xlab = "Age",
     ylab = "Charges",
     xlim = c(15, 70),
     ylim = c(1000,15000),
     main = "Age vs Charges")




install.packages("readr")
library(Hmisc)
library(readr)
library(ggplot2)
library(tidyr)
library(data.table)

corrplot(mat_1)
x <- mat_1$age
y <- mat_1$charges

plot(x,y, main = "Age vs Charges",
     xlab = "age", ylab = "charges",
     xlim = c(15, 70),
     ylim = c(1000,15000),
     pch = 10, frame = FALSE)
abline(lm(y ~ x, data = insurance), col = "red")
print(abline)
linearReg <- lm(x ~ y, data = insurance)
print(linearReg)



data <- lm(insurance$age ~ insurance$charges)
summary(data)

data <- lm(charges ~., data = insurance)
summary(data)

data <- lm(charges ~ bmi, data = insurance)
summary(data)

dwtest(insurance$age ~., data = insurance)


install.packages("IMTest")
library(IMTest)
dwtest(insurance$age ~., data = insurance)
install.packages("car")
library(car)
library(car)

durbinWatsonTest(insurance$age ~., data = insurance)

data <- lm(insurance$age ~ insurance$charges, data = insurance)

mat_2 <- data.frame(insurance$age, insurance$charges)

plot(x = mat_2$age, y = mat_2$charges,
xlab = "age", ylab = "charges",
xlim = c(15, 70),
ylim = c(1000,15000),
pch = 10, frame = FALSE)

abline(mat_2)

plot(x = input$age, y = input$charges,
     xlab = "Age",
     ylab = "Charges",
     xlim = c(15, 70),
     ylim = c(1000,15000),
     main = "Age vs Charges")

model <- lm(input$charges ~ input$age, data = insurance)
abline(model)

input <- insurance[,c('bmi','charges')]
print(head(input))

plot(x = input$bmi, y = input$charges,
     xlab = "BMI",
     ylab = "Charges",
#    xlim = c(1, 70),
#    ylim = c(1000,60000),
     main = "BMI vs Charges"
)
abline(lm(input$charges ~ input$bmi, data = insurance), col = "green")
linearReg <- lm(input$bmi ~ input$charges, data = insurance)
print(linearReg)


input2 <- insurance[,c('children','charges')]
print(head(input2))
plot(x = input2$children, y = input2$charges,
     xlab = "Children",
     ylab = "Charges",
#    xlim = c(1, 5),
#    ylim = c(1000,60000),
     main = "Children vs Charges"
)
abline(lm(input2$charges ~ input2$children, data = insurance), col = "red")
linearReg <- lm(input2$children ~ input2$charges, data = insurance)
print(linearReg)


input3<- insurance[,c('bmi','charges')]
print(head(input3))
plot(x = input3$bmi, y = input3$charges,
     xlab = "BMI",
     ylab = "Charges",
     #    xlim = c(1, 70),
     #    ylim = c(1000,60000),
     main = "BMI vs Charges"
)

abline(lm(input3$charges ~ input3$bmi, data = insurance), col = "green")
linearReg <- lm(input3$bmi ~ input3$charges, data = insurance)
print(linearReg)

input4<- insurance[,c('bmi','age')]
print(head(input4))
plot(x = input4$bmi, y = input4$age,
     xlab = "BMI",
     ylab = "Age",
     #    xlim = c(1, 70),
     #    ylim = c(1000,60000),
     main = "BMI vs Age"
)

abline(lm(input4$age ~ input4$bmi, data = insurance), col = "green")
linearReg <- lm(input4$bmi ~ input4$age, data = insurance)
print(linearReg)

cor(insurance[sapply(insurance, is.numeric)])

model <- lm(charges ~ age + bmi + children, data = insurance)
summary(model)


par(mfrow = c(1,2)) # combine the two plots
par("mar")
par(mar=c(1,1,1,1))
hist(insurance$charges, main = "Histogram of charges", col = "yellow")
plot(density(insurance$charges), main = "Density plot of charges")
polygon(density(insurance$charges), col = "red")

par(mfrow = c(1,3))
barplot(table(insurance$sex), main = "sex")
barplot(table(insurance$smoker), main = "smoker")
barplot(table(insurance$region), main = "region")


multiple_model <- lm(charges ~ age + sex + bmi + children + smoker + region, data = insurance_2)
summary(multiple_model)


library(psych)
pairs.panels(insurance_2[c("age", "sex", "bmi", "children", "smoker", "region")], digits = 2, cor = TRUE, main = "Insurance Matrix")


