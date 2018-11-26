install.packages('tableone')
library(tableone)
data.grad = read.csv('https://raw.githubusercontent.com/AustinGratton/STT520/master/graduation.csv')
str(data.grad)

allVars = c('time','age','gpa','prttime','dblmaj')
data.grad$prttime = factor(data.grad$prttime)
data.grad$dblmaj = factor(data.grad$dblmaj)

tab1 <- CreateTableOne(vars = allVars, strata = "event" , data = data.grad)
tab1
