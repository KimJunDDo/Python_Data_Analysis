fire <- read.csv("/Users/junseok/Downloads/wildfire.csv", fileEncoding = "EUC-KR")
# 한글 폰트 설정
par(family = "NanumGothic")

class(fire)
str(fire)
head(fire)

t(summary(fire[sapply(fire, is.numeric)]))
t(summary(fire[sapply(fire, is.character)]))

colnames(fire) <- c('num', 'date', 'day', 'start_time', 'end_time', 'use_time', 'location', 'damage_area(ha)', 
                    'tree_species', 'damage_money(1000won)', 'fire_cause', 'official', 'professional', 'public',
                    'police', 'firefighting', 'military', 'other', 'mountainhelicopter', 'firehelicopter',
                    'evolutioncar', 'firetruck', 'otherequipment', 'note', 'criteria_date')

#결측값 확인
colSums(is.na(fire))
#결측값 채우기
fire[is.na(fire)] <- 0

# 새로운 열 생성: total_human
fire$total_human <- fire$official + fire$professional + fire$public +
  fire$police + fire$firefighting + fire$military + fire$other
# 생성된 열 확인
fire$total_human

# 새로운 열(total_machine) 생성
fire$total_machine <- fire$mountainhelicopter + fire$firehelicopter +
  fire$evolutioncar + fire$firetruck + fire$otherequipment

# total_machine 열 확인
fire$total_machine

# 'num' 열 삭제
fire <- fire[, !names(fire) %in% "num"]

# 'official' 열부터 'criteria_date' 열까지 삭제
fire <- fire[, !(names(fire) %in% names(fire[which(names(fire) == "official"):which(names(fire) == "criteria_date")]))]

# 데이터 확인
head(fire)

# 소요시간 데이터를 ":" 제거 후 숫자로 변환
fire$use_time_min <- gsub(":", "", fire$use_time)  # ":" 제거
fire$use_time_min <- as.numeric(fire$use_time_min)  # 정수형 변환

# 시간을 분 단위로 계산
fire$use_time_min <- (fire$use_time_min %/% 100 * 60) + (fire$use_time_min %% 100)

# 데이터 확인
head(fire)

# 헥타르 단위를 제곱미터로 변환하여 새로운 열 생성
fire$`damage_area(m2)` <- fire$`damage_area(ha)` * 10000

library(lubridate)

# 발생일자 데이터를 날짜 형식으로 변환 후 '월' 정보 추출
fire$month <- month(ymd(fire$date))

# 'location' 열에서 '광주광역시 ' 문자열 제거
fire$location <- gsub("광주광역시 ", "", fire$location)

# 데이터 확인
head(fire)

library(ggplot2)
library(dplyr)

# 'startyear' 열 생성
fire$startyear <- format(as.Date(fire$date), "%Y")

# 연도별 산불 발생 건수
year_count <- fire %>%
  group_by(startyear) %>%
  summarise(firecounts = n())

# 연도별 피해 면적 합계
year_sum <- fire %>%
  group_by(startyear) %>%
  summarise(`damage_area(m2)` = sum(`damage_area(m2)`, na.rm = TRUE))

# 데이터 병합
plot_data <- left_join(year_count, year_sum, by = "startyear")

# startyear를 숫자형으로 변환
plot_data$startyear <- as.numeric(plot_data$startyear)

# Damaged Area 데이터를 Number of Forest Fires 범위에 맞게 스케일링
scaling_factor <- max(plot_data$firecounts) / 14000

ggplot(data = plot_data, aes(x = startyear)) +
  geom_bar(aes(y = firecounts), stat = "identity", fill = "skyblue", alpha = 0.7) +
  geom_line(aes(y = (`damage_area(m2)` * scaling_factor), color = "Damaged Area (x10^4 m^2)"), size = 1) +
  geom_point(aes(y = (`damage_area(m2)` * scaling_factor), color = "Damaged Area (x10^4 m^2)"), size = 2) +
  scale_y_continuous(
    name = "산불 발생 횟수",
    sec.axis = sec_axis(~ . / scaling_factor, name = "총 피해 면적 (㎡)")
  ) +
  scale_x_continuous(
    breaks = seq(2010, 2022, by = 2),  # 2년 단위로 표시
    labels = seq(2010, 2022, by = 2)
  ) +
  labs(
    title = "2011년~2022년 광주광역시 연도별 산불 발생 횟수 & 총 피해 면적",
    x = "년도",
    y = "산불 발생 횟수"
  ) +
  theme_minimal() +
  theme(
    axis.title.y.right = element_text(color = "red"),
    axis.text.y.right = element_text(color = "red"),
    legend.position = "none"
  )

library(ggplot2)
library(dplyr)

# 월별 산불 발생 건수
month_count <- fire %>%
  group_by(month) %>%
  summarise(firecounts = n())

# 월별 피해 면적 합계
fire <- fire %>%
  mutate(`damage_area(m2)` = as.numeric(`damage_area(m2)`))  # 피해 면적을 숫자형으로 변환

month_sum <- fire %>%
  group_by(month) %>%
  summarise(`damage_area(m2)` = sum(`damage_area(m2)`, na.rm = TRUE))

# 데이터 병합 (선택 사항)
plot_data <- left_join(month_count, month_sum, by = "month")

# Scaling factor 계산: 오른쪽 축 최대값 25,000 기준으로 스케일 조정
scaling_factor <- max(plot_data$firecounts) / 25000  # 왼쪽 축의 최대값을 기준으로 스케일링

# 이중 축 그래프 생성
ggplot(data = plot_data, aes(x = month)) +
  geom_bar(aes(y = firecounts), stat = "identity", fill = "skyblue", alpha = 0.7) +  # 막대그래프
  geom_line(aes(y = (`damage_area(m2)` * scaling_factor), color = "Damaged Area (x10^4 m^2)"), size = 1) +  # 선그래프
  geom_point(aes(y = (`damage_area(m2)` * scaling_factor), color = "Damaged Area (x10^4 m^2)"), size = 2) +  # 포인트
  scale_y_continuous(
    name = "산불 발생 횟수",  # 왼쪽 축
    sec.axis = sec_axis(~ . / scaling_factor, name = " 총 피해 면적 (㎡)")  # 오른쪽 축 스케일 변환
  ) +
  scale_x_continuous(
    breaks = seq(1, 12, by = 1),  # 1개월 단위로 표시
    labels = seq(1, 12, by = 1)
  ) +
  labs(
    title = "2011년~2022년 광주광역시 월별 산불 발생 횟수 & 총 피해 면적",
    x = "월",
    color = "Metric"
  ) +
  theme_minimal() +
  theme(
    axis.title.y.right = element_text(color = "red"),
    axis.text.y.right = element_text(color = "red"),
    legend.position = "none"
  )


library(dplyr)
library(stringr)

# location 열에서 자치구 이름만 남기기
fire <- fire %>%
  mutate(location = str_replace(location, " .*$", ""))  # 공백 뒤 문자열 제거

# 결과 확인
fire

# location별 피해면적 합계 계산
area_by_location <- fire %>%
  group_by(location) %>%
  summarise(`damage_area(m2)` = sum(`damage_area(m2)`, na.rm = TRUE))

# 라벨 생성: location 이름과 damage_area 값 결합
labels <- paste0(area_by_location$location, "\n", 
                 round(area_by_location$`damage_area(m2)`, 2), " ㎡")

# 파이 차트 그리기
pie(
  area_by_location$`damage_area(m2)`,  # 데이터 값
  labels = labels,                     # 라벨 (location + damage_area)
  main = "지역(자치구)별 총 피해 면적",    # 차트 제목
  col = rainbow(nrow(area_by_location)), # 색상 지정
  cex = 0.8                            # 글자 크기 조정
)

library(showtext)

# showtext 활성화 및 한글 폰트 설정
font_add_google("Nanum Gothic", "nanumgothic")  # 나눔고딕 폰트 추가
showtext_auto()

# 피해 원인별 데이터 계산
cause_data <- fire %>%
  group_by(fire_cause) %>%
  summarise(firecounts = n()) %>%
  arrange(desc(firecounts))

# 막대그래프
ggplot(cause_data, aes(x = reorder(fire_cause, -firecounts), y = firecounts)) +
  geom_bar(stat = "identity", fill = "tomato") +
  labs(
    x = "Fire Cause",
    y = "Number of Fires",
    title = "화재 발생 원인"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(ggplot2)

# 산점도 및 회귀선
ggplot(data = fire, aes(x = `damage_area(m2)`, y = use_time_min)) +
  geom_point(alpha = 0.6, color = "blue") +  # 산점도: 투명도 설정
  geom_smooth(method = "lm", se = TRUE, color = "tomato") +  # 회귀선: 선형 회귀 (lm)
  labs(
    x = "피해 면적 (㎡)",
    y = "화재 진압까지 소요시간 (min)",
    title = "소요시간(분)과 피해 면적의 산포도"
  ) +
  theme_minimal()

library(ggplot2)

# 산점도
ggplot(data = fire, aes(x = total_human, y = total_machine)) +
  geom_point(color = "blue", alpha = 0.7) +
  labs(
    x = "총 동원 인력량",
    y = "총 동원 기계량",
    title = "총 동원 인력량과 총 동원 기계량의 산포도"
  ) +
  theme_minimal()

library(ggplot2)

# 산점도
ggplot(data = fire, aes(x = total_human, y = use_time_min, color = total_machine)) +
  geom_point(size = 3, alpha = 0.7) +  # 점의 크기와 투명도 설정
  scale_color_viridis_c(name = "총 동원 기계량") +  # 색상 맵과 범례 라벨 설정
  labs(
    x = "총 동원 인력량",
    y = "화재 진압까지 소요시간 (min)",
    title = "소요시간(분)과 총 동원 인력량의 산포도"
  ) +
  theme_minimal()
