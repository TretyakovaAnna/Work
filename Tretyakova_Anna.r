readfile<-read.csv("C:\\data.csv",header=TRUE,stringsAsFactors=FALSE,sep=';')
attach(readfile)
names(readfile)
table(Ист.выбр., Год)
shapiro.test(X.соотнош.)
mean<-aggregate(X.соотнош., by=list(Ист.выбр.,Год), FUN=mean)
var<-aggregate(X.соотнош., by=list(Ист.выбр.,Год), FUN=var)
sd<-aggregate(X.соотнош., by=list(Ист.выбр.,Год), FUN=sd)
data.frame(Группа_1=mean$Group.1,Группа_2=mean$Group.2,Среднее=mean$x,Дисперсия=var$x,Ст.откл=sd$x)
ANOVA<-aov(X.соотнош.~ Ист.выбр.*Год)
summary(ANOVA)
library(gplots)
plotmeans(X.соотнош.~ interaction(Ист.выбр.,Год, sep=""),
connect=list(c(1,3),c(2,4)),
col=c("red", "green"),
main = "Диаграмма взаимодействия факторов с 95% доверительнымм интервалом",
xlab= "Год, в течении которого производилось измерение исследуемого признака",ylab="Объём выброса CO в атмосферу выраженный в тысячах килотонн")
legend("topright", inset=.05, legend=c("N-Нестационарный ист.","S-Стационарный ист."),lty=c(1, 2), pch=c(15,15), col =c("red","green"))