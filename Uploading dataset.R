#check the working directory
getwd()

#import .cvs file to your Global Enviroment
insurance <- read.csv("insurance.csv", header = TRUE, sep = ",")

insurance

head(insurance)

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
