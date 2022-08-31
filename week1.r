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

#################################################################
##step1.데이터생성 및 전처리
#################################################################
#ㄱ.데이터Load
aapl <- read.csv(file = "data/AAPL.csv")
goog <- read.csv(file = "data/GOOG.csv")
ief <- read.csv(file = "data/IEF.csv")
ko <- read.csv(file = "data/KO.csv")
spy <- read.csv(file = "data/SPY.csv")
tsla <- read.csv(file = "data/TSLA.csv")

#종목별 기본통계 확인
summary(aapl$Adj.Close, aapl$Volume)
summary(goog)
summary(ief)
summary(ko)
summary(spy)
summary(tsla)

#ㄴ.데이터컬럼추가(종목코드)
aapl$company <- c('aapl')
goog$company <- c('goog')
ief$company <- c('ief')
ko$company <- c('ko')
spy$company <- c('spy')
tsla$company <- c('tsla')

#ㄴ.데이터컬럼추가(종목코드)
aapl$amount <- c(aapl$Adj.Close * aapl$Volume)
goog$amount <- c(goog$Adj.Close * goog$Volume)
ief$amount <- c(ief$Adj.Close * ief$Volume)
ko$amount <- c(ko$Adj.Close * ko$Volume)
spy$amount <- c(spy$Adj.Close * spy$Volume)

#ㄷ.데이터합치기
df_total <- rbind(aapl,goog,ief,ko,spy,tsla)

#dim(df_total)  #30849행 X 8개 컬럼
#is.data.frame(df_total)
summary(df_total)

#ㅁ.일별수익율 계산 및 추가 (Adjusted price활용)
#수익금계산
df_total$amount <- c(df_total$Adj.Close * df_total$Volume)

#수익율계산
#################################################################
##step2.EDA
#################################################################
#ㄱ.종목별 성격 (각각의 주식의 성격이 어떻게 다른지 수치를 증거로 들어 설명)

#ㄴ.종목별통계치
by(df_total$Adj.Close, df_total$company, mean)
by(df_total$Adj.Close, df_total$company, max)
by(df_total$Adj.Close, df_total$company, mean)
by(df_total$Adj.Close, df_total$company, median)
by(df_total$Adj.Close, df_total$company, var)
by(df_total$Adj.Close, df_total$company, sd)
by(df_total$Adj.Close, df_total$company, IQR)

#ㄷ.시각화
library(ggplot2)
install.packages(ggplot2)
plot(aapl$amount, goog$amount, ief$amount, ko$amount)

#################################################################
##step3.Descriptive statistic
#################################################################
#ㄱ.포트폴리오 생성

#ㄴ.포트폴리오 통계치

#ㄷ.시각화
#################################################################
##4.최종결과 (분석결과)
# 분석방향: 시계열(일자별) 증감에 대한 상관관계
# 제약사항: 종목별 기간이 상이(종목별 80년대~최근)함.
#################################################################
