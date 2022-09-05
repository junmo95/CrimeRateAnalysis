### 시각화 및 정리
# 패키지 모음 ----------------------------------------------------------------
library(readxl)
library(dplyr)
library(ggplot2)
library(rgdal)
library(ggmap)
library(rgeos)
library(maptools)
library(raster)
library(reshape)
library(reshape2)
getwd()
# 원본데이터 --------------------------------------------------------------
crime_data <- read_excel("Project/USE_DATA/사용할꺼/seoulCrimeStatistic.xls") 
dist_id <- read.csv("./Project/USE_DATA/사용할꺼/district_id.csv")
map <- readOGR("./Project/USE_DATA/사용할꺼/TL_SCCO_SIG.shp") 
district_center <- read.csv("./Project/USE_DATA/사용할꺼/seoul_district_center.csv", header = T)
victim_data <- read_excel("./Project/USE_DATA/사용할꺼/victim_sex_age.xlsx") 
crime_time <- read_excel("./Project/USE_DATA/사용할꺼/crime_time.xlsx")
lamp_data <- read_excel("./Project/USE_DATA/사용할꺼/streetlight.xlsx") 
crime_cctv_data <- read_excel("./Project/USE_DATA/사용할꺼/crime_cctv_data.xlsx")
bell_data <- read_excel("./Project/USE_DATA/사용할꺼/bell.xlsx")
realty_raw <- read.csv("./Project/USE_DATA/사용할꺼/non_realty.csv")
tax_raw <- read_excel("./Project/USE_DATA/사용할꺼/non_tax.xlsx") 
gorvernment_data <- read_xls("./Project/USE_DATA/사용할꺼/gorvernment.xls") 
scout_data <- read_xlsx("./Project/USE_DATA/사용할꺼/seoul_scout.xlsx") 
crime_n_time <- read.csv("./Project/USE_DATA/Day_time_Crime.csv")
gu_total <- read.csv("./Project/USE_DATA/gu_tatal.csv")

# 공동 사용 코드 -----------------------------------------------------------------
### 지도 불러오기
#좌표계 변환 
map_1 <- spTransform(map, CRSobj = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
#map 데이터셋의 SIG_CD변수들을 id로 변환
new_map <- fortify(map_1, region = 'SIG_CD')
# id가 11740 이하가 서울시 구에 해당
new_map$id <- as.numeric(new_map$id)
# 서울지도 완성
seoul_map <- new_map[new_map$id <= 11740, ]

### 지도에 표시할 범죄통계
code_seoul_b <- dplyr::rename(dist_id, district = 시군구명)
dis2_1 <- dplyr::rename(district_center, long = X, lat = Y, id = "코드", district = "시군구명")
b <- crime_data
b <- dplyr::rename(b, district = "자치구", cnt = "발생")
b <- cbind(code_seoul_b$id, b)
b <- cbind(b, dis2_1$long, dis2_1$lat)
b <- dplyr::rename(b, long = "dis2_1$long", lat = "dis2_1$lat")
cntMax <- max(b$cnt)

# 자치구별 범죄 통계 -------------------------------------------------------------------
# 범죄 통계 불러오기
crime_1 <- crime_data
# 막대그래프 시각화
# 범죄 발생 순위별로 나열
ggplot(data=crime_1, aes(x = reorder(자치구,-발생), y = 발생, fill = 발생)) + 
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "#FFC0CB", high = "#DC143C") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(title = "2020년도 서울시 자치구별 5대 범죄 발생건수", x = "자치구", y = "발생")

# x축 y축 변경 후, 나열
ggplot(data=crime_1, aes(x = reorder(자치구,+발생), y = 발생, fill = 발생)) + 
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "#FFC0CB", high = "#DC143C") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5)) +
  coord_flip() +
  labs(title = "2020년도 서울시 자치구별 5대 범죄 발생건수", x = "자치구", y = "발생")

#지도시각화
crime_1 <- dplyr::rename(crime_1, district = "자치구", cnt = "발생")
code_seoul_1 <- dist_id

# 사용할 데이터 셋 만들기
code_seoul_1 <- dplyr::rename(code_seoul_1, district = "시군구명")
P <- merge(x = code_seoul_1, y = crime_1, by = 'district')
P_merge <- merge(seoul_map, P, by= 'id')

center_1 <- dplyr::rename(district_center, long = X, lat = Y, id = "코드", district = "시군구명")
center_1 <- cbind(center_1, crime_1$cnt)
center_1 <- dplyr::rename(center_1, cnt = "crime_1$cnt")
sum<-sum(center_1$cnt)
center_1$pct<-(center_1$cnt/sum) * 100
sum(center_1$pct)

ggplot() + geom_polygon(data = P_merge, aes(x = long, y = lat, group = group, fill = cnt)) +
  scale_fill_gradient(low = "#FFC0CB", high = "#DC143C") + 
  theme_bw() +
  labs(title = "서울시 범죄 건수") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5)) +
  geom_text(data = center_1, aes(x = long, y = lat, label = paste(district, paste0(round(pct, 2), "%"), sep = "\n")), col = "black")
getwd()
# 범죄 발생 요소  ----------------------------------------------------------------
# 범죄 피해자 성별통계 및 범죄 시간 파악하기
victime_data_2 <- victim_data

# 컬럼명 변경
victime_data_2 <- reshape::rename(victime_data_2,
                         c(...2 = "구분",
                           남자...4 = "male1",
                           남자...5 = "male2",
                           남자...6 = "male3",
                           남자...7 = "male4",
                           남자...8 = "male5",
                           남자...9 = "male6",
                           남자...10 = "male7",
                           남자...11 = "male8",
                           남자...12 = "male9",
                           남자...13 = "male10",
                           남자...14 = "male11"))
victime_data_2 <- reshape::rename(victime_data_2,
                         c(여자...15 = "female1", 
                             여자...16 = "female2",
                             여자...17 = "female3",
                             여자...18 = "female4",
                             여자...19 = "female5",
                             여자...20 = "female6",
                             여자...21 = "female7",
                             여자...22 = "female8",
                             여자...23 = "female9",
                             여자...24 = "female10",
                             여자...25 = "female11"))

# 성별 구분
male_2 <- victime_data_2 %>% 
  dplyr::select(...1, 구분, male1, male2, male3, male4, male5, male6, male7, male8, male9, male10, male11)
female_2 <- victime_data_2 %>% 
  dplyr::select(...1, 구분, female1, female2, female3, female4, female5, female6, female7, female8, female9, female10, female11)

# 범죄 피해 통계 시각화 _ 파이차트 _ 데이터 정제
# 남
male_2 <- male_2 %>% dplyr::select(구분, male1)
male_2 <- male_2[-c(1:3, 12, 13),]  # 소계 제거
male_2$male1 <- as.numeric(male_2$male1)
# 여
female_2 <- female_2 %>% dplyr::select(구분, female1)
female_2 <- female_2[-c(1:3, 12, 13),]  #소계 제거
female_2$female1 <- as.numeric(female_2$female1)

#### 퍼센트 추가하기
#남
sum_2 <- sum(male_2$male1)
male_2$pct_1 <- (male_2$male1 / sum_2) * 100

male_2_1 <- male_2 %>% arrange(desc(pct_1)) %>% head()
ar_mal <- male_2 %>% arrange(desc(pct_1))
etc <- ar_mal[-1:-6,2:3]

male_2_1 <- rbind(male_2_1, c("기타", sum(etc$male1), sum(etc$pct_1)))
male_2_1$male1 <- as.numeric(male_2_1$male1)
male_2_1$pct_1 <- as.numeric(male_2_1$pct_1)
# 남성_ 파이차트 그리기
pie(x = male_2_1$pct_1, labels = paste(male_2_1$구분, round(male_2_1$pct_1, 3), "%")) +
  title("2020년도 5대범죄 피해자 _ 남성")

#여
sum_2 <- sum(female_2$female1)
female_2$pct_2 <- (female_2$female1 / sum_2)*100

female_2_1 <- female_2 %>% arrange(desc(pct_2)) %>% head(7)
ar_mal <- female_2 %>% arrange(desc(pct_2))

etc <- ar_mal[-1:-7,2:3]
female_2_1 <- rbind(female_2_1, c("기타", sum(etc$female1), sum(etc$pct_2)))
female_2_1$female1 <- as.numeric(female_2_1$female1)
female_2_1$pct_2 <- as.numeric(female_2_1$pct_2)
female_2_1 <- female_2_1 %>% arrange(desc(pct_2))
# 여성_ 파이차트 그리기
pie(x = female_2_1$pct_2, labels = paste(female_2_1$구분, round(female_2_1$pct_2, 3), "%")) +
  title("2020년도 5대범죄 피해자 _ 여성")

#여성,남성 범죄율 파이차트 그리기
man_tot <- sum(male_2$male1)
woman_tot <- sum(female_2$female1)
tot <- sum(man_tot, woman_tot)
m_pc <- (man_tot / tot) * 100
w_pr <- (woman_tot / tot) * 100
pcq <- data.frame(c(m_pc, w_pr))
dimnames(pcq) = list(row = c("남성", "여성"), col = "피해율")

pie(x = pcq$피해율, labels = paste(row.names(pcq),round(pcq$피해율, 2),"%")) +
  title("2020년도 성별별 피해율")

#### 범죄 피해의 구분이 성별에 따라 조금씩 다름

# 범죄 발생 시간
time <- crime_time

# 필요한 부분만 추출
time <- time[,-c(1,3)]
time <- time[-c(1:2, 11, 12),]
time <- dplyr::rename(time, 죄종별 = "죄종별...2")

## 중첩 시각화
lengends <- reshape2::melt(time, id.vars = "죄종별", na.rm = TRUE)
lengends <- dplyr::rename(lengends, 범죄_발생시간 = variable)
ggplot(lengends) + geom_line(mapping = aes(x = 죄종별, y = value, colour = 범죄_발생시간, group = 범죄_발생시간)) +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.1, vjust = 0.5)) +
  labs(title = "시간대별 5대범죄 발생시간", y = "발생건수") + 
  theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5))

# 가로등 ---------------------------------------------------------------------
lamp_data_3 <- lamp_data

##범죄데이터 가져오기
crime_data_3 <- crime_data

##상관분석을 위해 범죄데이터 + 가로등 데이터 구 기준 결합
#가로등 변수 변경 
lamp_data_3 <- dplyr::rename(lamp_data_3, 자치구 = 구분)
#결합
crime_lamp_data <- left_join(crime_data_3, lamp_data_3, by = "자치구")

#fill=자치구
ggplot(crime_lamp_data, aes(x=자치구, y=개소, fill=자치구))+geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle=0, hjust=1)) +
  coord_flip()

#정렬, fill = 가로등 개수
ggplot(crime_lamp_data, aes(x=reorder(자치구,개소), y=개소, fill=개소))+geom_bar(stat = "identity")+
  scale_fill_gradient(low = "#FFFFF0", high = "#FFD700") +
  theme(axis.text.x = element_text(angle=0, hjust=1)) + 
  coord_flip() + 
  labs(title="2020년도 자치구별 가로등 개수", x="자치구", y="가로등 개수") +
  theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5))

###상관분석###
cor.test(crime_lamp_data$발생, crime_lamp_data$개소)
#t = 5.3951, df = 23, p-value = 1.76e-05 
#cor 0.747394 

#결과 분석
#p-value이 0.05미만이므로 유의미하다.
#cor값이 0.747 이므로 높은 양의 관계를 가지고 있다고 판단된다.

##절편과 기울기 구하기
#가로등 수를 독립변수, 범죄발생수를 종속변수
lm(발생 ~ 개소, data = crime_lamp_data)
#(Intercept)         개소  
# 1130.222          0.325  

#y절편 : 1130.222
#기울기 : 0.325

#회귀분석
summary(lm(발생 ~ 개소, data = crime_lamp_data))
#결정계수가 0.5586으로 설명력이 높지도 낮지도 않은 회귀직선이다.
#p-value가 1.76e-05로 0.001에서도 유의하다. 


###그래프 시각화###
#plot, abline 이용 방법
plot(crime_lamp_data$개소, crime_lamp_data$발생)
abline(lm(발생 ~ 개소, data = crime_lamp_data), col = 'red')

#ggplot 이용방법
ggplot(crime_lamp_data, aes(x = 개소, y = 발생)) +
  geom_point(size = 3) +
  geom_abline(intercept = 1130.222, slope = 0.325, col = 'red', size = 2)

###지도 시각화###
# 사용할 데이터 셋 만들기
crime_lamp <- dplyr::rename(crime_lamp_data, district = "자치구", cnt = 발생, streetlights = "개소")
crime_lamp_1 <- merge(x = code_seoul_b, y = crime_lamp, by = 'district')
lamp_merge <- merge(seoul_map, crime_lamp_1, by= 'id')

ggplot() + geom_polygon(data = lamp_merge, aes(x = long, y = lat, group = group, fill = streetlights)) +
  scale_fill_gradient(low = "#FFFFF0", high = "#FFD700") +
  theme_bw() +
  labs(title = "2020년도 자치구별 가로등 개수") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5))  +
  geom_point(data = b, aes(x = long, y = lat), size = b$cnt/ 280, color = "red", alpha = b$cnt/cntMax) +
  geom_text(data = b, aes(x = long, y = lat), label = b$district, color = "black", size = 5)

# CCTV --------------------------------------------------------------------
cctv <- crime_cctv_data # 상관분석에 사용할 예정(도봉구 삭제)
cctv_1 <- cctv # 차트표에 사용 (도봉구 0)
cctv_2 <- cctv # 지도시각화에 사용 (도봉구 NA)

cctv <- cctv[-10,]
cctv_1[10,3] <- 0

#
ggplot(cctv_1, aes(x=자치구, y=총대수, fill=자치구))+geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle=0, hjust=1)) +
  coord_flip()

#정렬, fill = cctv 개수
ggplot(cctv_1, aes(x=reorder(자치구,총대수), y=총대수, fill=총대수))+geom_bar(stat = "identity")+
  scale_fill_gradient(low = "#F0FFF0", high = "#6B8E23") +
  theme(axis.text.x = element_text(angle=0, hjust=1)) +
  coord_flip() +
  labs(title="2020년도 자치구별 cctv 개수", x="자치구", y="CCTV 개수") + 
  theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5))

cor.test(cctv$범죄발생수, cctv$총대수)
#      cor 
# 0.415838 

lm(범죄발생수 ~ 총대수, cctv)
# (Intercept)       총대수  
#   2722.9250       0.2725  
# y 절편 : 2722.9250
# 기울기 : 0.2725

#회귀분석
summary(lm(범죄발생수 ~ 총대수, cctv))
#결정계수가 0.1729로 설명력이 매우 낮다.
#p-value는 0.04328로 p값0.05에서 유의하다.
###그래프 시각화###

#plot, abline 이용 방법
plot(cctv$총대수, cctv$범죄발생수)
abline(lm(범죄발생수 ~ 총대수, cctv), col = 'red')

#ggplot 이용방법
ggplot(cctv, aes(x = 총대수, y = 범죄발생수)) +
  geom_point(size = 3) +
  geom_abline(intercept = 2722.9250, slope = 0.2725, col = 'red', size = 2)

###지도 시각화###
# 사용할 데이터 셋 만들기
crime_cctv <- dplyr::rename(cctv_2, district = "자치구", cnt = 범죄발생수, cctvs = "총대수")
crime_cctv_1 <- merge(x = code_seoul_b, y = crime_cctv, by = 'district')
cctv_merge <- merge(seoul_map, crime_cctv_1, by= 'id')

ggplot() + geom_polygon(data = cctv_merge, aes(x = long, y = lat, group = group, fill = cctvs)) +
  scale_fill_gradient(low = "#F0FFF0", high = "#6B8E23") +
  theme_bw() +
  labs(title = "2020년도 자치구별 cctv 개수") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5))  +
  geom_point(data = b, aes(x = long, y = lat), size = b$cnt/ 280, color = "red", alpha = b$cnt/cntMax) +
  geom_text(data = b, aes(x = long, y = lat), label = b$district, color = "black", size = 5)

# 안심벨 ---------------------------------------------------------------------
##안심벨 분석##
bell <- bell_data

unique(bell$관리기관명)

bell_num <- bell %>% dplyr::select(관리기관명) %>% group_by(관리기관명) %>% summarise(벨_개수 = n())
bell_num_1 <- bell_num # 차트표에 사용
bell_num_2 <- bell_num # 지도시각화에 사용

bell_num_1[25, ]$관리기관명 <- "도봉구"
bell_num_2[25, ]$관리기관명 <- "도봉구"
bell_num_1[25, ]$벨_개수 <-  0

#fill = 자치구
ggplot(bell_num_1, aes(x=관리기관명, y=벨_개수, fill=관리기관명))+geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle=0, hjust=1)) +
  coord_flip()

#정렬, fill = 벨 개수
ggplot(bell_num_1, aes(x=reorder(관리기관명,벨_개수), y=벨_개수, fill=벨_개수))+geom_bar(stat = "identity")+
  scale_fill_gradient(low = "#E0FFFF", high = "#008B8B") +
  theme(axis.text.x = element_text(angle=0, hjust=1)) +
  coord_flip() + 
  labs(title="2020년도 자치구별 안심벨 개수", x="자치구", y="안심벨 개수") +
  theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5))

crime_data_5 <- dplyr::rename(crime_data, 관리기관명 = 자치구)

#범죄율 데이터 도봉구 제거
crime_data_5_1 <- crime_data_5[-10, ]

crime_bell_data <- left_join(crime_data_5_1, bell_num, by = "관리기관명") # 상관관계에 사용
crime_bell_data_2 <- left_join(crime_data_5, bell_num_2, by = "관리기관명") # 지도 시각화에 사용용

cor.test(crime_bell_data$발생, crime_bell_data$벨_개수)
#       cor 
# 0.5198766

lm(발생 ~ 벨_개수, crime_bell_data)
# (Intercept)      벨_개수  
#   2915.1932       0.9864  
# y 절편 : 2915.1932
# 기울기 : 0.9864 

#회귀분석
summary(lm(발생 ~ 벨_개수, crime_bell_data))
#결정계수0.2703으로 설명력이 매우 낮다.
#p-value값은 0.009217로 0.01에서 유의하다.

###그래프 시각화###

#plot, abline 이용 방법
plot(crime_bell_data$벨_개수, crime_bell_data$발생)
abline(lm(발생 ~ 벨_개수, crime_bell_data), col = 'red')

#ggplot 이용방법
ggplot(crime_bell_data, aes(x = 벨_개수, y = 발생)) +
  geom_point(size = 3) +
  geom_abline(intercept = 2915.1932, slope = 0.9864, col = 'red', size = 2)

###지도 시각화###
# 사용할 데이터 셋 만들기
crime_bell_data_2 <- dplyr::rename(crime_bell_data_2, district = "관리기관명", cnt = 발생, bells = "벨_개수")

bell_1 <- merge(x = code_seoul_b, y = crime_bell_data_2, by = 'district')
bell_merge <- merge(seoul_map, bell_1, by= 'id')

ggplot() + geom_polygon(data = bell_merge, aes(x = long, y = lat, group = group, fill = bells)) +
  scale_fill_gradient(low = "#E0FFFF", high = "#008B8B") +
  theme_bw() +
  labs(title = "2020년도 자치구별 안심벨 개수") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5))  +
  geom_point(data = b, aes(x = long, y = lat), size = b$cnt/ 280, color = "red", alpha = b$cnt/cntMax) +
  geom_text(data = b, aes(x = long, y = lat), label = b$district, color = "black", size = 5)

# 부동산 ---------------------------------------------------------------------
#부동산데이터를 realty라고 명명
str(realty_raw)
realty <- realty_raw
realty$물건금액 <- as.integer(realty$물건금액)
realty <- realty_raw %>%dplyr::select(자치구명,물건금액)%>%group_by(자치구명)%>%summarise(mean(물건금액))

#변수명 변경
realty <- dplyr::rename(realty, price="mean(물건금액)")
realty <- dplyr::rename(realty, 자치구="자치구명")

#fill=자치구
ggplot(realty, aes(x=자치구, y=price, fill=자치구))+geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle=0, hjust=1)) +
  coord_flip()

#정렬, fill=price / 파란색 bar
ggplot(realty, aes(x=reorder(자치구,price), y=price, fill=price))+geom_bar(stat = "identity")+
  scale_fill_gradient(low = "#E6E6FA", high = "#4169E1") +
  theme(axis.text.x = element_text(angle=0, hjust=1)) +
  coord_flip() +
  labs(title="2020년도 서울시 자치구별 부동산 가격", x="자치구", y="부동산가격") +
  theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5))

crime_realty_data <- left_join(crime_data, realty, by = "자치구")

#상관관계 분석
cor.test(crime_realty_data$cnt, crime_realty_data$price)
# t = 2.4769, df = 23, p-value = 0.02104
#       cor 
# 0.4588882 

lm(발생 ~ price, crime_realty_data)
# (Intercept)        price  
# 2.422e+03    1.947e-06 
# y 절편 : 2.422e+03
# 기울기 : 1.947e-06 

#회귀분석
summary(lm(발생 ~ price, crime_realty_data))
#결정계수가 0.2106으로 설명력이 매우 낮은 회귀직선이다.
#p-value가 0.02104로 0.05에서 유의하다.

###그래프 시각화###

#plot, abline 이용 방법
plot(crime_realty_data$price, crime_realty_data$발생)
abline(lm(발생 ~ price, crime_realty_data), col = 'red')

#ggplot 이용방법
ggplot(crime_realty_data, aes(x = price, y = 발생)) +
  geom_point(size = 3) +
  geom_abline(intercept = 2.422e+03, slope = 1.947e-06, col = 'red', size = 2)

###지도 시각화###
# 사용할 데이터 셋 만들기
crime_realty_data <- dplyr::rename(crime_realty_data, district = "자치구", cnt = 발생)
realty_1 <- merge(x = code_seoul_b, y = crime_realty_data, by = 'district')
realty_merge <- merge(seoul_map, realty_1, by= 'id')

ggplot() + geom_polygon(data = realty_merge, aes(x = long, y = lat, group = group, fill = price)) +
  scale_fill_gradient(low = "#E6E6FA", high = "#4169E1") +
  theme_bw() +
  labs(title = "2020년도 서울시 부동산 가격") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5))  +
  geom_point(data = b, aes(x = long, y = lat), size = b$cnt/ 280, color = "red", alpha = b$cnt/cntMax) +
  geom_text(data = b, aes(x = long, y = lat), label = b$district, color = "white", size = 5)


# 근로소득 연말정산 ---------------------------------------------------------------
str(tax_raw)

tax <- tax_raw
tax <- dplyr::rename(tax, salary="금액(백만원)")
tax <- dplyr::rename(tax, 자치구 = ...1)
tax <- tax%>%mutate(salary/인원)
summary(tax)
# 자치구               인원            salary          salary/인원   
# Length:25          Min.   : 48476   Min.   : 2293687   Min.   :29.53  
# Class :character   1st Qu.:117979   1st Qu.: 4560624   1st Qu.:35.41  
# Mode  :character   Median :157918   Median : 6381557   Median :41.60  
#                    Mean   :154876   Mean   : 6814857   Mean   :43.70  
#                    3rd Qu.:185630   3rd Qu.: 7824542   3rd Qu.:48.01  
#                    Max.   :280206   Max.   :15754700   Max.   :74.69  


#salary/인원 시각화
ggplot(tax, aes(x=자치구, y=salary, fill=자치구))+geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle=0, hjust=1)) +
  coord_flip()

#정렬 / fill=salary
ggplot(tax, aes(x=reorder(자치구,salary), y=salary, fill=salary)) + geom_bar(stat="identity") +
  scale_fill_gradient(low = "#B3A7DC", high = "#4B0082") +
  theme(axis.text.x = element_text(angle=90, hjust=1)) + 
  coord_flip() +
  labs(title="2020년도 서울시 근로소득 연말정산", x="자치구", y="연말정산 평균") +
  theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5))

crime_tax_data <- left_join(crime_data, tax, by = "자치구")

#상관관계 분석
cor.test(crime_tax_data$발생, crime_tax_data$salary)
# t = 6.0719, df = 23, p-value = 3.416e-06
# cor 0.784742 

crime_tax_data <- dplyr::rename(crime_tax_data, mean_salary = `salary/인원`)

lm(발생 ~ mean_salary, crime_tax_data)
# (Intercept)  mean_salary  
# 1796.52        43.72  
# y 절편 : 1796.52
# 기울기 : 43.72

#회귀분석
summary(lm(발생 ~ mean_salary, crime_tax_data))
#결정계수가 0.2093으로 설명력이 매우 낮은 회귀직선이다.
#p-value는 0.02147로 0.05에서 유의하다.
###그래프 시각화###

#plot, abline 이용 방법
plot(crime_tax_data$mean_salary, crime_tax_data$발생)
abline(lm(발생 ~ mean_salary, crime_tax_data), col = 'red')

#ggplot 이용방법
ggplot(crime_tax_data, aes(x = mean_salary, y = 발생)) +
  geom_point(size = 3) +
  geom_abline(intercept = 1796.52, slope = 43.72, col = 'red', size = 2)

###지도 시각화###
# 사용할 데이터 셋 만들기
crime_tax_data <- dplyr::rename(crime_tax_data, district = "자치구")
tax_1 <- merge(x = code_seoul_b, y = crime_tax_data, by = 'district')
tax_1 <- dplyr::rename(tax_1, cnt = 발생)
tax_merge <- merge(seoul_map, tax_1, by= 'id')

ggplot() + geom_polygon(data = tax_merge, aes(x = long, y = lat, group = group, fill = mean_salary)) +
  scale_fill_gradient(low = "#B3A7DC", high = "#4B0082") +
  theme_bw() +
  labs(title = "2020년도 서울시 근로소득 연말정산") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5))  +
  geom_point(data = b, aes(x = long, y = lat), size = b$cnt/ 280, color = "red", alpha = b$cnt/cntMax) +
  geom_text(data = b, aes(x = long, y = lat), label = b$district, color = "white", size = 5)

# 주요 공공기관 -----------------------------------------------------------------
#공공기관데이터 가져오기
gorvernment_1 <- gorvernment_data

#
ggplot(gorvernment_1, aes(x=자치구, y=합계, fill=자치구))+geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle=0, hjust=1)) +
  coord_flip()

#정렬 / fill = 공공기관 개수
ggplot(gorvernment_1, aes(x=reorder(자치구,합계), y=합계, fill=합계))+geom_bar(stat="identity")+
  scale_fill_gradient(low = "#D8BFD8", high = "#8B008B") +
  theme(axis.text.x = element_text(angle=90, hjust=1)) + 
  coord_flip() +
  labs(title="2020년도 서울시 자치구별 주요기관 개수", x="자치구", y="합계") +
  theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5))

##이상치 확인 
summary(gorvernment_1)
#결과 
#Min.   : 4301  
#Median : 7952  
#Mean   : 7930  
#Max.   :14326  

##범죄데이터 가져오기
crime_data_8 <- crime_data

##상관분석을 위해 범죄데이터 + 주요기관 데이터 구 기준 결합
#공공기관 변수 변경 
gorvernment_1<- dplyr::rename(gorvernment_1, sum= 합계)
#결합
crime_gorvernmnet <- left_join(crime_data_8, gorvernment_1, by = "자치구")

###상관분석###
cor.test(crime_gorvernmnet$cnt, crime_gorvernmnet$sum)
#t = 2.3977, df = 23, p-value = 0.02501
#cor 0.4471836

#결과 분석
#p-value이 0.05미만이므로 유의미하다.
#cor값이 0.447이므로 거의 관계가 없다고 판단된다.

##절편과 기울기 구하기
#주요기관 수를 독립변수, 범죄발생수를 종속변수
lm(발생 ~ sum, data = crime_gorvernmnet)
#(Intercept)      sum  
# 1136.6        109.9  

#y절편 : 1136.6
#기울기 : 109.9

#회귀직선
summary(lm(발생 ~ sum, data = crime_gorvernmnet))
#결정계수0.2로 설명력이 매우 낮은 회귀직선이다.
#p-value는 0.02501로 0.05에서 유의하다.

###그래프 시각화###
#plot, abline 이용 방법
plot(crime_gorvernmnet$sum, crime_gorvernmnet$발생)
abline(lm(발생 ~ sum, data = crime_gorvernmnet), col = 'red')

#ggplot 이용방법
ggplot(crime_gorvernmnet, aes(x = sum, y = 발생)) +
  geom_point(size = 3) +
  geom_abline(intercept = 1136.6, slope = 109.9, col = 'red', size = 2)

crime_gorvernmnet <- dplyr::rename(crime_gorvernmnet, district = "자치구", cnt = "발생")
gorvernment_1 <- merge(x = code_seoul_b, y = crime_gorvernmnet, by = 'district')
gorvern_merge <- merge(seoul_map, gorvernment_1, by= 'id')

ggplot() + geom_polygon(data = gorvern_merge, aes(x = long, y = lat, group = group, fill = sum)) +
  scale_fill_gradient(low = "#D8BFD8", high = "#8B008B") +
  theme_bw() +
  labs(title = "2020년도 서울시 주요기관 합계") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5))  +
  geom_point(data = b, aes(x = long, y = lat), size = b$cnt/ 280, color = "red", alpha = b$cnt/cntMax) +
  geom_text(data = b, aes(x = long, y = lat), label = b$district, color = "white", size = 5)


# 안심스카우트 시각화 -----------------------------------------------------------------
# 2020년도 서울시 자치구별 안심스카우트 이용 건수
#2020년만 추출
a_2020 <- scout_data %>% 
  dplyr::select('자치구', '2020', '...14')

# 불필요한 행 제거 및 컬럼명 수정
a_2020 <- a_2020[-c(1:2),]
names(a_2020) <- c("district", "이용실적", "스카우트 인원")

# 문자형에서 숫자형으로 변환
a_2020 <- as.data.frame(a_2020)
a_2020$이용실적 <- as.numeric(a_2020$이용실적)
a_2020$'스카우트 인원' <- as.numeric(a_2020$'스카우트 인원')

#안심스카우트와 범죄발생 합치기
a_2020 <- cbind(a_2020, crime_data$발생)
a_2020 <- dplyr::rename(a_2020, cnt = "crime_data$발생")

#상관관계
cor.test(a_2020$이용실적, a_2020$cnt)
#상관계수가 -0.1817688로 관계가 거의 없다고 볼 수 있다. 

lm(cnt ~ 이용실적, a_2020)

#회귀분석
summary(lm(cnt ~ 이용실적, a_2020))
#결정계수가 0.03304로 설명력이 아주 낮다.
#p-value값이 0.3845로 유의하지 않다. 


#ggplot 이용하여 산점도에 회귀직선 그리기
ggplot(a_2020, aes(x = 이용실적, y = cnt)) +
  geom_point(size = 3) +
  geom_abline(intercept = 4150.29889, slope = -0.05268, col = 'red', size = 2)


# 2020 이용실적 내림차순 정렬
a_2020 <- a_2020 %>% arrange(desc(a_2020$`이용실적`))

##
ggplot(data = a_2020, aes(x = district, y = 이용실적, fill = district)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#정렬 / fill = 스카우트 이용실적
ggplot(a_2020, aes(x=reorder(district,이용실적), y=이용실적, fill=이용실적))+geom_bar(stat="identity")+
  scale_fill_gradient(low = "#FFC0CB", high = "#FF1493") +
  theme(axis.text.x = element_text(angle=90, hjust=1)) + 
  coord_flip() +
  labs(title="2020년도 서울시자치구별 여성안심스카우트 이용실적", x="자치구", y="이용실적") +
  theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5))


# 시각화
# 사용할 데이터 셋 만들기
ansim <- merge(x = code_seoul_b, y = a_2020, by = 'district')

ansim_merge <- merge(seoul_map, ansim, by= 'id')

ggplot() + geom_polygon(data = ansim_merge, aes(x = long, y = lat, group = group, fill = 이용실적)) +
  scale_fill_gradient(low = "#FFC0CB", high = "#FF1493") +
  theme_bw() +
  labs(title = "2020년도 서울시 여성안심스카우트 이용현황") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5))  +
  geom_point(data = b, aes(x = long, y = lat), size = b$cnt/ 280, color = "red", alpha = b$cnt/cntMax) +
  geom_text(data = b, aes(x = long, y = lat), label = b$district, color = "black", size = 5)



#유동인구-----------------------------------
#상관관계 전체 유동인구(x)와 범죄율(y)
cor.test(crime_n_time$tot_mean, crime_n_time$crime)
#상관계수가 0.2421251로 관계가 매우 낮다


#상관관계 성별 유동인구(x)와 범죄율(y)
cor.test(crime_n_time$man_mean, crime_n_time$crime)
#상관계수가 0.2879762로 관계가 매우 낮다

cor.test(crime_n_time$woman_mean, crime_n_time$crime)
#상관계수가 0.2019825로 관계가 매우 낮다





#전체 다중회귀분석----------------
lm(범죄발생수~ccty_개수+가로등_개수+벨_개수+공공기관_개수+평균소득+평균부동산+안심스카우트_이용실적,data=gu_total)

summary(lm(범죄발생수~ccty_개수+가로등_개수+벨_개수+공공기관_개수+평균소득+평균부동산+안심스카우트_이용실적,data=gu_total))
#결정개수가 0.7631로 설명력이 높은 회귀직선이다.
#가로등 개수는 p 값이 0.000388, 평균소득은 p값이 0.092652로 유의하다.












##############연도별 범죄율 소득#############---------------------
options(scipen = 100)

##cctv
tax <- read.csv("./Project/USE_DATA/사용할꺼/연말정산.csv")

#범죄율 데이터 가져오기
crime <- read_excel("./Project/USE_DATA/사용할꺼/범죄율_total.xls")

########################################################################
tax_crime <- read_excel("./Project/USE_DATA/사용할꺼/tax_crime.xlsx")

only_text_1 <- tax_crime[1, 1:6]
only_text_2 <- tax_crime[5, c(1, 7:11)]


ggplot(tax_crime, aes(x = 연도)) + 
  
  geom_point(aes(y = 도봉구, colour = "bottom5"), size = 3) +
  geom_line(aes(y = 도봉구, colour = "bottom5"), size = 1.5) +
  geom_point(aes(y = 성동구, colour = "bottom5"), size = 3) +
  geom_line(aes(y = 성동구, colour = "bottom5"), size = 1.5) +
  geom_point(aes(y = 서대문구, colour = "bottom5"), size = 3) +
  geom_line(aes(y = 서대문구, colour = "bottom5"), size = 1.5) +
  geom_point(aes(y = 성북구, colour = "bottom5"), size = 3) +
  geom_line(aes(y = 성북구, colour = "bottom5"), size = 1.5) +
  geom_point(aes(y = 금천구, colour = "bottom5"), size = 3) +
  geom_line(aes(y = 금천구, colour = "bottom5"), size = 1.5) +
  
  geom_point(aes(y = 강남구, colour = "top5"), size = 3) +
  geom_line(aes(y = 강남구, colour = "top5"), size = 1.5) +
  geom_point(aes(y = 송파구, colour = "top5"), size = 3) +
  geom_line(aes(y = 송파구, colour = "top5"), size = 1.5) +
  geom_point(aes(y = 강서구, colour = "top5"), size = 3) +
  geom_line(aes(y = 강서구, colour = "top5"), size = 1.5) +
  geom_point(aes(y = 영등포구, colour = "top5"), size = 3) +
  geom_line(aes(y = 영등포구, colour = "top5"), size = 1.5) +
  geom_point(aes(y = 서초구, colour = "top5"), size = 3) +
  geom_line(aes(y = 서초구, colour = "top5"), size = 1.5) +
  labs(x = '연도', y = '소득', title = '범죄율 대비 소득')+
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(angle = 0, vjust = 0.5))







###cctv 비율로 파악하기####--------------

#상위 5 - 강남, 송파, 강서, 영등포, 서초구
#하위 5 - 도봉, 성동, 서대문, 성북, 금천

library(readxl)

cctv_crime_top5 <- read_excel("./Project/USE_DATA/사용할꺼/cctv_crime_top5.xlsx")
cctv_crime_bottom5 <- read_excel("./Project/USE_DATA/사용할꺼/cctv_crime_bottom5.xlsx")


##각 구에 대한 회귀분석
#######################
#강남#

total_cctv <- cbind(cctv_crime_top5, cctv_crime_bottom5)
total_cctv <- total_cctv[, -9]

ggplot(total_cctv, aes(x = 연도)) + 
  
  geom_point(aes(y = 도봉구, colour = "bottom5"), size = 3) +
  geom_line(aes(y = 도봉구, colour = "bottom5"), size = 1.5) +
  geom_point(aes(y = 성동구, colour = "bottom5"), size = 3) +
  geom_line(aes(y = 성동구, colour = "bottom5"), size = 1.5) +
  geom_point(aes(y = 서대문구, colour = "bottom5"), size = 3) +
  geom_line(aes(y = 서대문구, colour = "bottom5"), size = 1.5) +
  geom_point(aes(y = 성북구, colour = "bottom5"), size = 3) +
  geom_line(aes(y = 성북구, colour = "bottom5"), size = 1.5) +
  geom_point(aes(y = 금천구, colour = "bottom5"), size = 3) +
  geom_line(aes(y = 금천구, colour = "bottom5"), size = 1.5) +
  
  geom_point(aes(y = 강남구, colour = "top5"), size = 3) +
  geom_line(aes(y = 강남구, colour = "top5"), size = 1.5) +
  geom_point(aes(y = 송파구, colour = "top5"), size = 3) +
  geom_line(aes(y = 송파구, colour = "top5"), size = 1.5) +
  geom_point(aes(y = 강서구, colour = "top5"), size = 3) +
  geom_line(aes(y = 강서구, colour = "top5"), size = 1.5) +
  geom_point(aes(y = 영등포구, colour = "top5"), size = 3) +
  geom_line(aes(y = 영등포구, colour = "top5"), size = 1.5) +
  geom_point(aes(y = 서초구, colour = "top5"), size = 3) +
  geom_line(aes(y = 서초구, colour = "top5"), size = 1.5) +
  
  labs(x = '연도',
       y = '범죄율 대비 CCTV대수',
       title = '범죄율 대비 CCTV대수') +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 15, face = "bold"))


 








###가로등 비율로 파악하기####

#상위 5 - 강남, 송파, 강서, 영등포, 서초구
#하위 5 - 도봉, 성동, 서대문, 성북, 금천


light_crime_top5 <- read_excel("./Project/USE_DATA/사용할꺼/light_crime_top5.xlsx")
light_crime_bottom5 <- read_excel("./project/USE_DATA/사용할꺼/light_crime_bottom5.xlsx")


##각 구에 대한 회귀분석
#######################
#강남#

total_light <- cbind(light_crime_top5, light_crime_bottom5)
total_light <- total_light[, -9]

ggplot(total_light, aes(x = 연도)) + 
  
  geom_point(aes(y = 도봉구, colour = "bottom5"), size = 3) +
  geom_line(aes(y = 도봉구, colour = "bottom5"), size = 1.5) +
  geom_point(aes(y = 성동구, colour = "bottom5"), size = 3) +
  geom_line(aes(y = 성동구, colour = "bottom5"), size = 1.5) +
  geom_point(aes(y = 서대문구, colour = "bottom5"), size = 3) +
  geom_line(aes(y = 서대문구, colour = "bottom5"), size = 1.5) +
  geom_point(aes(y = 성북구, colour = "bottom5"), size = 3) +
  geom_line(aes(y = 성북구, colour = "bottom5"), size = 1.5) +
  geom_point(aes(y = 금천구, colour = "bottom5"), size = 3) +
  geom_line(aes(y = 금천구, colour = "bottom5"), size = 1.5) +
  
  geom_point(aes(y = 강남구, colour = "top5"), size = 3) +
  geom_line(aes(y = 강남구, colour = "top5"), size = 1.5) +
  geom_point(aes(y = 송파구, colour = "top5"), size = 3) +
  geom_line(aes(y = 송파구, colour = "top5"), size = 1.5) +
  geom_point(aes(y = 강서구, colour = "top5"), size = 3) +
  geom_line(aes(y = 강서구, colour = "top5"), size = 1.5) +
  geom_point(aes(y = 영등포구, colour = "top5"), size = 3) +
  geom_line(aes(y = 영등포구, colour = "top5"), size = 1.5) +
  geom_point(aes(y = 서초구, colour = "top5"), size = 3) +
  geom_line(aes(y = 서초구, colour = "top5"), size = 1.5) +
  
  labs(x = '연도',
       y = '범죄율 대비 가로등수',
       title = '범죄율 대비 가로등수') +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 15, face = "bold"))




##########부동산 #####################

realty_crime <- read_excel("./Project/USE_DATA/사용할꺼/realty_crime.xlsx")

ggplot(realty_crime, aes(x = 연도)) + 
  
  geom_point(aes(y = 도봉, colour = "bottom5"), size = 3) +
  geom_line(aes(y = 도봉, colour = "bottom5"), size = 1.5) +
  geom_point(aes(y = 성동, colour = "bottom5"), size = 3) +
  geom_line(aes(y = 성동, colour = "bottom5"), size = 1.5) +
  geom_point(aes(y = 서대문, colour = "bottom5"), size = 3) +
  geom_line(aes(y = 서대문, colour = "bottom5"), size = 1.5) +
  geom_point(aes(y = 성북, colour = "bottom5"), size = 3) +
  geom_line(aes(y = 성북, colour = "bottom5"), size = 1.5) +
  geom_point(aes(y = 금천, colour = "bottom5"), size = 3) +
  geom_line(aes(y = 금천, colour = "bottom5"), size = 1.5) +
  
  geom_point(aes(y =강남, colour = "top5"), size = 3) +
  geom_line(aes(y =강남, colour = "top5"), size = 1.5) +
  geom_point(aes(y = 송파, colour = "top5"), size = 3) +
  geom_line(aes(y = 송파, colour = "top5"), size = 1.5) +
  geom_point(aes(y = 강서, colour = "top5"), size = 3) +
  geom_line(aes(y = 강서, colour = "top5"), size = 1.5) +
  geom_point(aes(y = 영등포, colour = "top5"), size = 3) +
  geom_line(aes(y = 영등포, colour = "top5"), size = 1.5) +
  geom_point(aes(y = 서초, colour = "top5"), size = 3) +
  geom_line(aes(y = 서초, colour = "top5"), size = 1.5)+ 
  labs(x = '연도',
       y = '범죄율 대비 부동산가격',
       title = '범죄율 대비 부동산가격') +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 15, face = "bold"))




##########################################################
###안심스카우트/범죄율 통합그래프

ansim_crime <- read_excel("./Project/USE_DATA/사용할꺼/ansim_crime.xlsx")

ggplot(ansim_crime, aes(x = 연도)) + 
  
  geom_point(aes(y = 도봉, colour = "bottom5"), size = 3) +
  geom_line(aes(y = 도봉, colour = "bottom5"), size = 1.5) +
  geom_point(aes(y = 성동, colour = "bottom5"), size = 3) +
  geom_line(aes(y = 성동, colour = "bottom5"), size = 1.5) +
  geom_point(aes(y = 서대문, colour = "bottom5"), size = 3) +
  geom_line(aes(y = 서대문, colour = "bottom5"), size = 1.5) +
  geom_point(aes(y = 성북, colour = "bottom5"), size = 3) +
  geom_line(aes(y = 성북, colour = "bottom5"), size = 1.5) +
  geom_point(aes(y = 금천, colour = "bottom5"), size = 3) +
  geom_line(aes(y = 금천, colour = "bottom5"), size = 1.5) +
  
  geom_point(aes(y =강남, colour = "top5"), size = 3) +
  geom_line(aes(y =강남, colour = "top5"), size = 1.5) +
  geom_point(aes(y = 송파, colour = "top5"), size = 3) +
  geom_line(aes(y = 송파, colour = "top5"), size = 1.5) +
  geom_point(aes(y = 강서, colour = "top5"), size = 3) +
  geom_line(aes(y = 강서, colour = "top5"), size = 1.5) +
  geom_point(aes(y = 영등포, colour = "top5"), size = 3) +
  geom_line(aes(y = 영등포, colour = "top5"), size = 1.5) +
  geom_point(aes(y = 서초, colour = "top5"), size = 3) +
  geom_line(aes(y = 서초, colour = "top5"), size = 1.5)+ 
  labs(x = '연도',
       y = '범죄율 대비 안심스카우트 이용건수',
       title = '범죄율 대비 안심스카우트 이용건수') +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 15, face = "bold"))


###연도별 - 서울시 전체 - 요소 평균값 - 범죄발생수 선형회귀###

#cctv
#가로등
#소득
#부동산
#여성 스카우트


##cctv
cctv <- read_excel("./Project/USE_DATA/사용할꺼/CCTV(12~21)_fix.xlsx")
cctv_total <- cctv[,c(1,27)]
cctv_total <- dplyr::rename(cctv_total, cctv = 평균)

##가로등
light <- read_excel("./Project/USE_DATA/사용할꺼/가로등.xlsx")
light_total <- light[,c(1,27)]
light_total <- dplyr::rename(light_total, 가로등 = 평균)

##소득
tax <- read.csv("./Project/USE_DATA/사용할꺼/tax_add_mean.csv")
tax_total <- tax[,c(1,27)]
tax_total <- dplyr::rename(tax_total, 소득 = 평균)

##부동산
land <- read_excel("./Project/USE_DATA/사용할꺼/부동산_평균.xlsx")
land_total <- land
land_total <- dplyr::rename(land_total, 부동산 = 서울시)

##여성 스카우트 
scoute <- read_excel("./Project/USE_DATA/사용할꺼/안심스카우트_평균.xlsx")
scoute_total <- scoute
scoute_total <- dplyr::rename(scoute_total, 스카우트 = 서울시)

############

##범죄발생수
crime <- read_excel("./Project/USE_DATA/사용할꺼/범죄율_total.xls")
crime_total <- crime[,c(1,27)]
crime_total <- dplyr::rename(crime_total, 범죄발생수 = 평균)


### 16~19년도 통합
total <- left_join(crime_total, cctv_total, by = "연도")
total <- left_join(total, light_total, by = "연도")
total <- left_join(total, tax_total, by = "연도")
total <- left_join(total, land_total, by = "연도")
total <- left_join(total, scoute_total, by = "연도")


total_2 <- scale(total[2:7])
total_2 <- total_2[2:5, ] 

total_2 <- as.data.frame(total)

lm(범죄발생수 ~ cctv + 가로등 + 소득 + 부동산 + 스카우트, data = total)

library(psych)
total_2 <- total_2[,-1]
pairs.panels(total_2)