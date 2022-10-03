### 데이터 불러오기
setwd("C:/Users/sec/Desktop/학부연구생/data/data 1")
housereg=read.csv("housereg.csv", header = T)
attach(housereg)
colnames(housereg) = c("house", "baserate", "credit", "money")
colnames(housereg) = c("y", "x1", "x2", "x3")
summary(housereg)

### 표준편차 구하기
sqrt(cov(housereg))

### 산점도 구하기
library(lattice)
splom(~housereg, pch=8)

### 회귀모형 만들기
library(lattice)
splom(~housereg, pch=8)
r.full=lm(housereg$y~housereg$x1+housereg$x2+housereg$x3)
summary(r.full)
fitted(r.full)
resid(r.full)
anova(r.full)
qf(0.95,3,208)
qt(0.95,208)
confint(r.full)

### 공동신뢰영역
install.packages("ellipse")
op=par(mfrow=c(3,2))
library(ellipse)
fit<-lm(housereg$y~housereg$x1)
plot(ellipse(fit), type="l")
abline(v=confint(fit)[1,], lty=3, col="cyan")
abline(h=confint(fit)[2,], lty=3, col="cyan")
points(coef(fit)[1], coef(fit)[2], pch=3)
library(ellipse)
fit<-lm(housereg$y~housereg$x2)
plot(ellipse(fit), type="l")
abline(v=confint(fit)[1,], lty=3, col="cyan")
abline(h=confint(fit)[2,], lty=3, col="cyan")
points(coef(fit)[1], coef(fit)[2], pch=3)
library(ellipse)
fit<-lm(housereg$y~housereg$x3)
plot(ellipse(fit), type="l")
abline(v=confint(fit)[1,], lty=3, col="cyan")
abline(h=confint(fit)[2,], lty=3, col="cyan")
points(coef(fit)[1], coef(fit)[2], pch=3)
library(ellipse)
fit<-lm(housereg$x1~housereg$x2)
plot(ellipse(fit), type="l")
abline(v=confint(fit)[1,], lty=3, col="cyan")
abline(h=confint(fit)[2,], lty=3, col="cyan")
points(coef(fit)[1], coef(fit)[2], pch=3)
library(ellipse)
fit<-lm(housereg$x1~housereg$x3)
plot(ellipse(fit), type="l")
abline(v=confint(fit)[1,], lty=3, col="cyan")
abline(h=confint(fit)[2,], lty=3, col="cyan")
points(coef(fit)[1], coef(fit)[2], pch=3)
library(ellipse)
fit<-lm(housereg$x2~housereg$x3)
plot(ellipse(fit), type="l")
abline(v=confint(fit)[1,], lty=3, col="cyan")
abline(h=confint(fit)[2,], lty=3, col="cyan")
points(coef(fit)[1], coef(fit)[2], pch=3)
par(op)

### 적합도와 분산분석
anova(r.full)
summary(r.full)
qf(0.95,3,208)

### 잔차분석
par(mfrow=c(2,2))
plot(r.full)

### 표준화 잔차 대 설명변수
op1=par(mfrow=c(2,2))
res<-rstandard(r.full)
plot(res~x1, housereg, ylab="Standardized Residuals")
abline(h=0, lty=3)
res<-rstandard(r.full)
plot(res~x2, housereg, ylab="Standardized Residuals")
abline(h=0, lty=3)
res<-rstandard(r.full)
plot(res~x3, housereg, ylab="Standardized Residuals")
abline(h=0, lty=3)
par(op1)

### 관측값 대 예측값 산점도
plot(y~fitted(r.full),housereg)
abline(a=0,b=1,lty=3)

###  RF 그림
rfs(r.full)

### 부분 F 검정
fit<-lm(y~x1+x2+x3, housereg)
anova(fit)
drop1(fit, test="F")

### 적합 결여 검정
r.origin=lm(y~x1+x2+x3, housereg)
r.pe=lm(y~factor(x1)+factor(x2)+factor(x3), housereg) 
anova(r.origin,r.pe)

### 내표준화 잔차, 외 표준화 잔차, 지렛값
r.model=lm(y~., housereg)
rstandard(r.model)
rstudent(r.model)
hatvalues(r.model)
(abs(rstandard(r.model))>2)
(abs(rstudent(r.model))>2)
hatvalues(r.model)>0.037735
sum(resid(r.model)^2)
sum((resid(r.model)/(1-hatvalues(r.model)))^2)

### 영향력 측도
a=influence.measures(r.model)
print(which(apply(a$is.inf,1,sum)>=1))
print(which(apply(a$is.inf,1,sum)>=2))
print(which(apply(a$is.inf,1,sum)>=3))

### 쿡의 거리
cutoff<- 4/((nrow(housereg)-length(r.full$coefficients)-2))
plot(r.full, which=4, cook.levels = cutoff)

### DFFITS
plot(dffits(r.full), type="h", lwd=3)

### 쿡의 거리 vs 지렛대점 그래프
plot(r.full, which=6, cook.levels = cutoff)

### 스튜던트 잔차 vs 모자행렬 그래프
install.packages("car")
library(car)
influencePlot(r.full,main="influence plot", sub="Circle size is 
                   proportial to Cook's Distance" )

### 더빈 왓슨 검정
install.packages("lmtest")
library(lmtest)
lmtest::dwtest(r.model)

### 다중 공산성
vif(r.full)

### 관측치 제거 후 회귀모형 적합을 위한 데이터
houseobs=housereg[c(-4, -11, -12, -13, -14, -15, -16, -18, -19, -58, -59, -155, -158, -168, -191, -194, -195, -197, -198, -200, -203, -204, -205, -208, -209, -210, -211, -212),]

### 회귀모형 선택을 위한 설정
library(leaps)
install.packages("devtools")
library(devtools)
install_github("regbook/regbook")
library(regbook)
regamoobs<-regsubsets(y~.,houseobs, nbest=8)
summaryf(regamoobs)

### AIC 구하기
r.1=lm(houseobs$y~houseobs$x1)
r.2=lm(houseobs$y~houseobs$x2)
r.3=lm(houseobs$y~houseobs$x3)
r.12=lm(houseobs$y~houseobs$x1+houseobs$x2)
r.13=lm(houseobs$y~houseobs$x1+houseobs$x3)
r.23=lm(houseobs$y~+houseobs$x2+houseobs$x3)
r.full=lm(houseobs$y~houseobs$x1+houseobs$x2+houseobs$x3)
install.packages("olsrr")
library(olsrr)
ols_aic(r.1)
ols_aic(r.2)
ols_aic(r.3)
ols_aic(r.12)
ols_aic(r.13)
ols_aic(r.23)
ols_aic(r.full)

### PRESS P 구하기
press(regamoobs)

### S P 구하기
library("olsrr")
ols_msep(r.1)
ols_msep(r.2)
ols_msep(r.3)
ols_msep(r.12)
ols_msep(r.13)
ols_msep(r.23)
ols_msep(r.full)

### 결정계수 그래프 그리기
RS<-data.frame(P,Rsquared, row.names = c("x1","x2","x3",
                                         "x12","x13","x23","x123"))
Rsquared<-c(
  0.5789,
  0.4393,
  0.0390,
  0.6004,
  0.5804,
  0.4463,
  0.6016
)
P<-c(2,2,2,3,3,3,4)
RSplot=RS[order(RS$Rsquared,decreasing=T),]
plot(RS,xlim=c(0,5))
text(RS, labels=rownames(RS), cex=0.8, col="blue")
X=RSplot[c(4,2,1),]
lines(X)
RSplot

### 수정된 결정계수 그래프 그리기
RS<-data.frame(P,AdjRS, row.names = c("x1","x2","x3",
                                      "x12","x13","x23","x123"))
AdjRS<-c(
  
  0.5766,
  0.4363,
  0.0337,
  0.5960,
  0.5758,
  0.4402,
  0.5950
)
P<-c(2,2,2,3,3,3,4)
RSplot=RS[order(RS$AdjRS,decreasing=T),]
plot(RS,xlim=c(0,5))
text(RS, labels=rownames(RS), cex=0.8, col="blue")
X=RSplot[c(2,1,3),]
lines(X)
RSplot

### S^2 P 그래프 그리기
RS<-data.frame(P,sp2, row.names = c("x1","x2","x3", "x12","x13","x23","x123"))
sp2<-c(
  8649.594,
  11515.51,
  19738.67,
  8253.521,
  8665.935,
  11434.78,
  8274.039)
P<-c(2,2,2,3,3,3,4)
RSplot=RS[order(RS$sp2,decreasing=T),]
plot(RS,xlim=c(0,5))
text(RS, labels=rownames(RS), cex=0.8, col="blue")
X=RSplot[c(5,7,6),]
lines(X)
RSplot

### C P 그래프 그리기
a=data.frame(houseobs)
require(car)
subsets(regamoobs, statistic="cp", main="Cp Plot for All Subsets Regression")

### PRESS P 그래프 그리기
RS<-data.frame(P,PRESSp, row.names = c("x1","x2","x3",    "x12","x13","x23","x123"))
PRESSp<-c(
  8749.514,
  11660.233,
  19927.495,
  8407.069,
  8793.307,
  11617.581,
  8452.290
)
P<-c(2,2,2,3,3,3,4)
RSplot=RS[order(RS$PRESSp,decreasing=T),]
plot(RS,xlim=c(0,5))
text(RS, labels=rownames(RS), cex=0.8, col="blue")
X=RSplot[c(5,7,6),]
lines(X)
RSplot

### 최적모형 적합
r.best=lm(houseobs$y~houseobs$x1+houseobs$x2)
summary(r.best)
par(mfrow=c(2,2))
plot(r.best)