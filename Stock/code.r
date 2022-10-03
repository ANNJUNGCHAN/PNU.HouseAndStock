### 데이터 불러오기
setwd("C:/Users/sec/Desktop/학부연구생/data/data 2")
install.packages("readxl")
library(readxl)
stockreg=read_excel("stockreg.xlsx")
attach(stockreg)
colnames(stockreg) = c("y", "x1", "x2", "x3")
summary(stockreg)

### 표준편차 구하기
sqrt(cov(stockreg))

### 산점도 구하기
library(lattice)
splom(~stockreg, pch=8)

### 회귀모형 만들기
library(lattice)
splom(~stockreg, pch=8)
r.full=lm(stockreg$y~stockreg$x1+stockreg$x2+stockreg$x3)
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
fit<-lm(stockreg$y~stockreg$x1)
plot(ellipse(fit), type="l")
abline(v=confint(fit)[1,], lty=3, col="cyan")
abline(h=confint(fit)[2,], lty=3, col="cyan")
points(coef(fit)[1], coef(fit)[2], pch=3)
library(ellipse)
fit<-lm(stockreg$y~stockreg$x2)
plot(ellipse(fit), type="l")
abline(v=confint(fit)[1,], lty=3, col="cyan")
abline(h=confint(fit)[2,], lty=3, col="cyan")
points(coef(fit)[1], coef(fit)[2], pch=3)
library(ellipse)
fit<-lm(stockreg$y~stockreg$x3)
plot(ellipse(fit), type="l")
abline(v=confint(fit)[1,], lty=3, col="cyan")
abline(h=confint(fit)[2,], lty=3, col="cyan")
points(coef(fit)[1], coef(fit)[2], pch=3)
library(ellipse)
fit<-lm(stockreg$x1~stockreg$x2)
plot(ellipse(fit), type="l")
abline(v=confint(fit)[1,], lty=3, col="cyan")
abline(h=confint(fit)[2,], lty=3, col="cyan")
points(coef(fit)[1], coef(fit)[2], pch=3)
library(ellipse)
fit<-lm(stockreg$x1~stockreg$x3)
plot(ellipse(fit), type="l")
abline(v=confint(fit)[1,], lty=3, col="cyan")
abline(h=confint(fit)[2,], lty=3, col="cyan")
points(coef(fit)[1], coef(fit)[2], pch=3)
library(ellipse)
fit<-lm(stockreg$x2~stockreg$x3)
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
plot(res~x1, stockreg, ylab="Standardized Residuals")
abline(h=0, lty=3)
res<-rstandard(r.full)
plot(res~x2, stockreg, ylab="Standardized Residuals")
abline(h=0, lty=3)
res<-rstandard(r.full)
plot(res~x3, stockreg, ylab="Standardized Residuals")
abline(h=0, lty=3)
par(op1)

### 관측값 대 예측값 산점도
plot(y~fitted(r.full),stockreg)
abline(a=0,b=1,lty=3)

### RF 그림
rfs(r.full)

### 부분 F 검정
fit<-lm(y~x1+x2+x3, stockreg)
anova(fit)
drop1(fit, test="F")

### 적합 결여 검정
r.origin=lm(y~x1+x2+x3, stockreg)
r.pe=lm(y~factor(x1)+factor(x2)+factor(x3), stockreg) 
anova(r.origin,r.pe)

### 내표준화 잔차, 외 표준화 잔차, 지렛값
r.model=lm(y~., stockreg)
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
cutoff<- 4/((nrow(stockreg)-length(r.full$coefficients)-2))
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

### 관측치 제거 후 회귀모형 적합을 위한 데이터
stockobs=stockreg[c(-2, -3, -9, -11, -13, -14, -15, -48, -59, -155, -158, -170, -191, -194, -195, -197, -198, -200, -203, -204, -205, -208, -209, -211, -212),]
stockobs

### 회귀모형 선택을 위한 설정
library(leaps)
install.packages("devtools")
library(devtools)
install_github("regbook/regbook")
library(regbook)
regamoobs<-regsubsets(y~.,stockobs, nbest=8)
summaryf(regamoobs)

### AIC 구하기
r.1=lm(stockobs$y~stockobs$x1)
r.2=lm(stockobs$y~stockobs$x2)
r.3=lm(stockobs$y~stockobs$x3)
r.12=lm(stockobs$y~stockobs$x1+stockobs$x2)
r.13=lm(stockobs$y~stockobs$x1+stockobs$x3)
r.23=lm(stockobs$y~+stockobs$x2+stockobs$x3)
r.full=lm(stockobs$y~stockobs$x1+stockobs$x2+stockobs$x3)
install.packages("olsrr")
library(olsrr)
ols_aic(r.1)
ols_aic(r.2)
ols_aic(r.3)
ols_aic(r.12)
ols_aic(r.13)
ols_aic(r.23)
ols_aic(r.full)

### PRESSp 구하기
press(regamoobs)

### Sp 구하기
library("olsrr")
ols_msep(r.1)
ols_msep(r.2)
ols_msep(r.3)
ols_msep(r.12)
ols_msep(r.13)
ols_msep(r.23)
ols_msep(r.full)

### 결정계수 그래프 그리기
RS<-data.frame(P,Rsquared, row.names = c("x1","x2","x3","x12","x13","x23","x123"))
Rsquared<-c(
  0.3889,
  0.3605,
  0.0406,
  0.4293,
  0.4022,
  0.3655,
  0.4359
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
  0.3856,
  0.3571,
  0.0354,
  0.4231,
  0.3957,
  0.3586,
  0.4266
)
P<-c(2,2,2,3,3,3,4)
RSplot=RS[order(RS$AdjRS,decreasing=T),]
plot(RS,xlim=c(0,5))
text(RS, labels=rownames(RS), cex=0.8, col="blue")
X=RSplot[c(4,2,1),]
lines(X)
RSplot

### S^2p 그래프 그리기
sp2<-c(
  22865829,
  21848934,
  34306191,
  20516014,
  22812274,
  21491042,
  20392016
)
P<-c(2,2,2,3,3,3,4)
RSplot=RS[order(RS$sp2,decreasing=T),]
plot(RS,xlim=c(0,5))
text(RS, labels=rownames(RS), cex=0.8, col="blue")
X=RSplot[c(4,6,7),]
lines(X)
RSplot

### Cp 그래프 그리기
a=data.frame(stockobs)
require(car)
subsets(regamoobs, statistic="cp", main="Cp Plot for All Subsets Regression")
abline(1,1,lty=2,col="red")

### PRESSp 그래프 그리기
RS<-data.frame(P,PRESSp, row.names = c("x1","x2","x3",
                                  "x12","x13","x23","x123"))
PRESSp<-c(
  22114811,
  23169701,
  34653786,
  21048972,
  21903697,
  23198679,
  21015519
)
P<-c(2,2,2,3,3,3,4)
RSplot=RS[order(RS$PRESSp,decreasing=T),]
plot(RS,xlim=c(0,5))
text(RS, labels=rownames(RS), cex=0.8, col="blue")
X=RSplot[c(4,6,7),]
lines(X)
RSplot

### 최적 모형 적합
r.best=lm(stockobs$y~stockobs$x1+stockobs$x2+stockobs$x3)
summary(r.best)
par(mfrow=c(2,2))
plot(r.best)
