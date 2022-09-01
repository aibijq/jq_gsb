"
WEEK1 과제
1. 
  포스팅된 데이터를 R을 사용해서 간단히 Descriptive statistic 을 구해보세요. 
  가격이 아닌 수익률 데이터를 사용해야 합니다. 
  수익률을 계산할때는 Adjusted price 를 사용해 주세요. 
2.
  분석후, 각각의 주식의 성격이 어떻게 다른지 수치를 증거로 들어 설명해 주고
  이에 기반한 포트폴리오를 만든 후 이 포트폴리오 수익률의 성격 역시 간단한 통계치를 사용해 계산한후 설명해 주세요.
"

library(ggplot2)

#################################################################
##step1.데이터생성 및 전처리
##step2.EDA
##step3.Descriptive statistic
#################################################################

#데이터Load
aapl <- read.csv(file = "data/AAPL.csv")
goog <- read.csv(file = "data/GOOG.csv")
ief <- read.csv(file = "data/IEF.csv")
ko <- read.csv(file = "data/KO.csv")
spy <- read.csv(file = "data/SPY.csv")
tsla <- read.csv(file = "data/TSLA.csv")


#컬럼추가(시가총액)
aapl$amount <- c(aapl$Adj.Close * aapl$Volume)
goog$amount <- c(goog$Adj.Close * goog$Volume)
ief$amount <- c(ief$Adj.Close * ief$Volume)
ko$amount <- c(ko$Adj.Close * ko$Volume)
spy$amount <- c(spy$Adj.Close * spy$Volume)
tsla$amount <- c(tsla$Adj.Close * tsla$Volume)

#데이터확인
summary(aapl$Adj.Close, aapl$Volume)
summary(goog)
summary(ief)
summary(ko)
summary(spy)
summary(tsla)

# 추세확인(개별)
par(mfrow=c(3,2))
plot(aapl$Adj.Close, type = 'l', main='aapl')
plot(goog$Adj.Close, type = 'l', main='goog')
plot(ief$Adj.Close, type = 'l', main='ief')
plot(ko$Adj.Close, type = 'l', main='ko')
plot(spy$Adj.Close, type = 'l', main='spy')
plot(tsla$Adj.Close, type = 'l', main='tsla')

# 추세확인(종합)
plot(aapl$Adj.Close, type = 'l', main='aapl', col='red')
lines(goog$Adj.Close, type = 'l', main='goog')
lines(ief$Adj.Close, type = 'l', main='ief')
lines(ko$Adj.Close, type = 'l', main='ko')
lines(spy$Adj.Close, type = 'l', main='spy')
lines(tsla$Adj.Close, type = 'l', main='tsla')

#데이터마이그레이션(종목코드추가)
aapl$company <- c('aapl')
goog$company <- c('goog')
ief$company <- c('ief')
ko$company <- c('ko')
spy$company <- c('spy')
tsla$company <- c('tsla')
df_total <- rbind(aapl,goog,ief,ko,spy,tsla)


#ㄱ.종목별 성격 (각각의 주식의 성격이 어떻게 다른지 수치를 증거로 들어 설명)
#1.중심화 경향

#2.퍼짐정도(range, Q1, median, Q3, lower/upper whisker line, outlier) & 중심 경향(mean) 관련 통계량
ggplot(df_total, aes(x = company, y = Adj.Close)) + 
  geom_boxplot(width=0.8, outlier.size=1, outlier.shape=16, outlier.colour="red") +
  stat_summary(fun.y="mean", geom="point", shape=21, size=3, fill="blue") + 
  ggtitle("종목별 Boxplot")

#3.분포형태 및 대칭 정도 통계량
par(mfrow=c(3,2))
hist(aapl$Adj.Close, main = 'aapl')
hist(goog$Adj.Close, main = 'goog')
hist(ief$Adj.Close, main = 'ief')
hist(ko$Adj.Close, main = 'ko')
hist(spy$Adj.Close, main = 'spy')
hist(tsla$Adj.Close, main = 'tsla')

#ㄴ.종목별통계치
by(df_total$Adj.Close, df_total$company, mean)
by(df_total$Adj.Close, df_total$company, max)
by(df_total$Adj.Close, df_total$company, mean)
by(df_total$Adj.Close, df_total$company, median)
by(df_total$Adj.Close, df_total$company, var)
by(df_total$Adj.Close, df_total$company, sd)
by(df_total$Adj.Close, df_total$company, IQR)

#ㄱ.포트폴리오 생성

#ㄴ.포트폴리오 통계치

#ㄷ.시각화
#################################################################
##4.최종결과 (분석결과)
# 분석방향: 시계열(일자별) 증감에 대한 상관관계
# 제약사항: 종목별 기간이 상이(종목별 80년대~최근)함.
#################################################################
