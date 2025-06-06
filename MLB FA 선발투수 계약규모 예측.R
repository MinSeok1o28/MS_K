library(rvest)
library(dplyr)
library(stringr)
library(httr)
library(tidyverse)
library(XML)

#2018-19 ~ 2024-25 FA 선수 계약규모 크롤링 
years <- 2019:2025
base_url <- "https://www.spotrac.com/mlb/free-agents/_/year/%d/position/p/level/mlb/sort/contract_value"
all_data <- list()

for(year in years){
  url <- sprintf(base_url, year)
  html <- GET(url)
  html.parsed <- htmlParse(html)
  
  Player <- xpathSApply(html.parsed, "//*[@id='table']/tbody/tr/td[2]/a", xmlValue)
  AGE <- xpathSApply(html.parsed, '//*[@id="table"]/tbody/tr/td[4]', xmlValue)
  ARM <- xpathSApply(html.parsed, '//*[@id="table"]/tbody/tr/td[5]', xmlValue, trim = TRUE)
  YRS <- gsub("[^0-9.]", "", xpathSApply(html.parsed, '//*[@id="table"]/tbody/tr/td[8]', xmlValue, trim = TRUE))
  VALUE <- gsub("[^0-9.]", "", xpathSApply(html.parsed, '//*[@id="table"]/tbody/tr/td[9]', xmlValue, trim = TRUE))
  AAV <- gsub("[^0-9.]", "", xpathSApply(html.parsed, '//*[@id="table"]/tbody/tr/td[10]', xmlValue, trim = TRUE))
  
  df <- data.frame(
    Year = year,
    Player = Player,
    AGE = head(AGE, length(Player)),
    ARM = head(ARM, length(Player)),
    YRS = head(YRS, length(Player)),
    VALUE = head(VALUE, length(Player)),
    AAV = head(AAV, length(Player))
  )
  
  all_data[[as.character(year)]] <- df
  
  print(paste(year, "년도 크롤링 완료"))
  
  Sys.sleep(0.5)
}

final_data <- do.call(rbind, all_data)


write.csv(final_data, "VALUE 크롤링.csv", row.names = FALSE)



#크롤링 데이터 가공
# 1. Player와 Year를 기준으로 정렬
final_data <- final_data[order(final_data$Player, final_data$Year), ]

# 2. 각 선수별로 등장 순서대로 번호 부여(다른 년도에 FA 나온 같은 선수 구분)
final_data$Player <- ave(as.character(final_data$Player), 
                         final_data$Player, 
                         FUN = function(x) {
                           if(length(x) == 1) return(x)
                           paste0(x, c("", paste0("_", 2:length(x))))
                         })

write.csv(final_data, "VALUE 크롤링_전처리 완료.csv", row.names = FALSE)



# 2018-19 ~ 2023-24 FA 선발투수 명단 데이터
sp <- read.csv("./FA Starting Pitchers_Final.csv")

# 역대 토미존 수술 받은 투수 명단 데이터
tj <- read_csv("./Tommy John Surgery List.csv")

#sp와 tj 병합: 토미존 수술 여부 및 복귀일자 추가
df_merged <- sp %>%
  left_join(tj %>% 
              select(Player, `TJ Surgery Date`, `Return Date (same level)`),
            by = "Player")

#날짜, 수치 형식으로 데이터 변경
df_merged <- df_merged %>%
  mutate(
    `TJ Surgery Date` = as.Date(`TJ Surgery Date`, format = "%m/%d/%Y"),
    `Return Date (same level)` = as.Date(`Return Date (same level)`, format = "%m/%d/%Y"),
    Recovery_Time = as.numeric(`Return Date (same level)` - `TJ Surgery Date`),
    FA_Year = as.numeric(paste0("20", str_sub(FA.Season, -2))),
    Return_Year = year(`Return Date (same level)`),
    Surgery_Year = year(`TJ Surgery Date`),
    year_diff = FA_Year - Return_Year
  )

# 1. FA 시즌 이후 수술받은 경우 제외
# 2. year_diff가 3년 이상인 경우 제외
# 3. year_diff가 0 이하인 경우 제외
df_filtered <- df_merged %>%
  mutate(
    `TJ Surgery Date` = if_else(
      (FA_Year < Surgery_Year) | 
        (year_diff > 3) | 
        (year_diff <= 0),
      NA_Date_,
      `TJ Surgery Date`
    ),
    `Return Date (same.level)` = if_else(
      (FA_Year < Surgery_Year) | 
        (year_diff > 3) | 
        (year_diff <= 0),
      NA_Date_,
      `Return Date (same level)`
    ),
    Recovery_Time = if_else(
      (FA_Year < Surgery_Year) | 
        (year_diff > 3) | 
        (year_diff <= 0),
      NA_real_,
      Recovery_Time
    )
  )

# 필터링 후, TJ Surgery 여부 열 생성
df_filtered <- df_filtered %>%
  mutate(TJ_Surgery = as.numeric(!is.na(`TJ Surgery Date`)))

# 중복행 처리
df_final <- df_filtered %>%
  group_by(Player, FA.Season) %>%
  arrange(desc(TJ_Surgery), .by_group = TRUE) %>%
  filter(row_number() == 1) %>%
  ungroup()


write.csv(df_final, "Surgery_data_final.csv", row.names = FALSE)


# 2018-19 ~ 2023-24 FA 선발투수 명단에 속한 투수들 통산 시즌별 성적 크롤링

get_player_stats <- function(url, player_name) {
  result <- tryCatch({
    webpage <- read_html(url)
    
    rows <- webpage %>%
      html_node("#dashboard > div:nth-child(3) > div > div > div > div.table-scroll > table > tbody") %>%
      html_nodes("tr.row-mlb-season") %>%
      html_nodes("td:not([data-col-id='divider'])") %>%
      html_text(trim = TRUE)
    
    if(length(rows) == 0) {
      message("No data found for URL: ", url)
      return(NULL)
    }
    
    data_matrix <- matrix(rows, ncol = 23, byrow = TRUE)
    
    stats_df <- as.data.frame(data_matrix, stringsAsFactors = FALSE)
    
    # 열 이름 지정
    colnames(stats_df) <- c(
      "Season", "Team", "Level", "Age", "W", "L", "SV", "G", "GS", "IP", 
      "K/9", "BB/9", "HR/9", "BABIP", "LOB%", "GB%", "HR/FB", "vFA", 
      "ERA", "xERA", "FIP", "xFIP", "WAR"
    )
    
    stats_df <- stats_df[stats_df$Team != "- - -", ]
    
    # 선수 이름 추가
    stats_df$Player <- player_name
    
    # 숫자형 데이터 변환
    numeric_cols <- c("Age", "W", "L", "SV", "G", "GS", "WAR")
    stats_df[numeric_cols] <- lapply(stats_df[numeric_cols], as.numeric)
    
    # IP 열 처리
    stats_df$IP <- as.numeric(gsub("\\..*", "", stats_df$IP)) + 
      as.numeric(gsub(".*\\.", "", stats_df$IP))/3
    
    # 비율 스탯 변환
    rate_cols <- c("K/9", "BB/9", "HR/9", "BABIP", "ERA", "xERA", "FIP", "xFIP")
    stats_df[rate_cols] <- lapply(stats_df[rate_cols], as.numeric)
    
    # 퍼센트 스탯 변환
    percent_cols <- c("LOB%", "GB%", "HR/FB")
    stats_df[percent_cols] <- lapply(stats_df[percent_cols], 
                                     function(x) as.numeric(gsub("%", "", x))/100)
    
    # 직구구속 변환
    stats_df$vFA <- as.numeric(stats_df$vFA)
    
    return(stats_df)
  }, error = function(e) {
    message("Error processing URL: ", url)
    message("Error message: ", e$message)
    return(NULL)
  })
  
  return(result)
}

# 모든 선수의 데이터 수집
all_players_data <- list()
for(i in 1:nrow(df_final)) {
  message(sprintf("Processing player %d of %d: %s", i, nrow(df_final), df_final$Player[i]))
  
  url <- df_final$url[i]
  player_name <- df_final$Player[i]
  player_stats <- get_player_stats(url, player_name)
  
  # 데이터가 성공적으로 수집된 경우에만 리스트에 추가
  if(!is.null(player_stats)) {
    all_players_data[[i]] <- player_stats
  }
  
  Sys.sleep(0.7)
}

# 모든 데이터 합치기
final_data_stats <- bind_rows(all_players_data)

final_data_stats <- final_data_stats %>%
  select(Player, Season, Team, Age, W, L, SV, G, GS, IP, 
         `K/9`, `BB/9`, `HR/9`, BABIP, `LOB%`, `GB%`, 
         `HR/FB`, vFA, ERA, xERA, FIP, xFIP, WAR)


final_data_stats <- final_data_stats %>%
  mutate(Player = trimws(Player)) 

# 결과 저장
write.csv(final_data_stats, "all_players_stats_clean_final.csv", row.names = FALSE)



# 2018-19 ~ 2023-24 FA 선발투수 명단에 속한 투수들 계약 전 3개년치 성적만 추출

sp <- sp %>%
  mutate(
    Player = trimws(Player),  # 공백 제거
    FA_Season = as.numeric(substr(FA.Season, 1, 4)) + 1  # 2024-25 -> 2025
  ) %>%
  select(Player, FA_Season) 

# 각 FA 선수별로 3년치 데이터 추출 및 FA 시즌 정보 추가
filtered_data <- sp %>%
  inner_join(final_data_stats, by = "Player") %>%
  # 각 선수별로 FA 신청 전 3년 데이터만 필터링
  filter(Season >= FA_Season - 3 & Season < FA_Season) %>%
  group_by(Player, FA_Season) %>%
  ungroup()

write.csv(filtered_data, "./3 Year Stats.csv")


# 3개년치 성적 추출 후 3년 전, 2년 전, 1년 전 가중치 매겨서 3년 평균 성적 계산


calculate_weighted_stats <- function(df) {
  df <- df %>%
    mutate(
      FA_Season = as.numeric(as.character(FA_Season)),
      Season = as.numeric(as.character(Season))
    )
  
  # 코로나 시즌 보정: 단축시즌으로 60경기만 운영, 따라서 누적스탯은 162/60 배 변환
  df <- df %>%
    mutate(across(
      c(W, L, SV, G, GS, IP, WAR), #비율스탯은 같을 것이라고 가정
      ~ifelse(Season == 2020, . * (162/60), .)
    ))
  
  results <- df %>%
    group_by(Player, FA_Season) %>%
    group_split() %>%
    lapply(function(player_data) {
      
      player_data <- player_data %>% arrange(Season)
      
      fa_year <- unique(player_data$FA_Season)
      available_seasons <- sort(unique(player_data$Season))
      
      # 가중치 계산: 계약년도 1년 전은 0.5, 2년 전은 0.3, 3년 전은 0.2를 곱함
      # 최근 년도일수록 가치산정에 많이 반영될 것이라는 가정
      weights <- numeric()
      season_data <- list()
      
      for(season in available_seasons) {
        if(season >= fa_year-3 && season <= fa_year-1) {
          season_row <- player_data %>% 
            filter(Season == season) %>% 
            head(1)
          season_data[[length(season_data) + 1]] <- season_row
          
          if(length(available_seasons) == 3) {
            if(season == fa_year-3) weights <- c(weights, 0.2)
            else if(season == fa_year-2) weights <- c(weights, 0.3)
            else weights <- c(weights, 0.5)
          } else if(length(available_seasons) == 2) {
            if(season == fa_year-3) weights <- c(weights, 0.2)
            else if(season == fa_year-2) {
              if(fa_year %in% available_seasons) weights <- c(weights, 0.3)
              else weights <- c(weights, 0.2)
            }
            else weights <- c(weights, 0.5)
          } else {
            years_before_fa <- fa_year - season
            if(years_before_fa == 1) weights <- c(weights, 0.5)
            else if(years_before_fa == 2) weights <- c(weights, 0.3)
            else weights <- c(weights, 0.2)
          }
        }
      }
      
      if(length(season_data) == 0) return(NULL)
      
      season_df <- bind_rows(season_data)
      weighted_stats <- list()
      
      # 기본 통계: 가중치를 곱하고 더하기만 함
      basic_stats <- c('W', 'L', 'SV', 'G', 'GS', 'IP', 'WAR')
      for(stat in basic_stats) {
        values <- season_df[[stat]]
        weighted_stats[[paste0('weighted_', stat)]] <- sum(values * weights)
      }
      
      # 이닝 비례 통계: 가중치와 이닝을 곱한 후 평균
      ip_weighted_stats <- c('K/9', 'BB/9', 'HR/9', 'BABIP', 'LOB%', 'GB%', 
                             'HR/FB', 'ERA', 'FIP', 'vFA')
      for(stat in ip_weighted_stats) {
        if(stat %in% colnames(season_df)) {
          values <- season_df[[stat]]
          ip_weights <- season_df$IP
          combined_weights <- ip_weights * weights
          weighted_stats[[paste0('weighted_', stat)]] <- 
            weighted.mean(values, combined_weights)
        }
      }
      
      result <- data.frame(
        Player = unique(player_data$Player),
        FA_Season = unique(player_data$FA_Season),
        Age = tail(season_df$Age, 1),
        Years_of_Data = length(season_data),
        Seasons_Used = paste(available_seasons, collapse=","),
        Weights_Used = paste(weights, collapse=",")
      )
      
      for(stat_name in names(weighted_stats)) {
        result[[stat_name]] <- weighted_stats[[stat_name]]
      }
      
      return(result)
    }) %>%
    bind_rows()
  
  # 같은 선수가 두 번 이상 계약했을 경우: _2, _3... 붙임
  results <- results %>%
    group_by(Player) %>%
    mutate(
      Player = if(n() > 1) {
        paste0(Player, "_", row_number())
      } else {
        Player
      }
    ) %>%
    ungroup()
  
  return(results)
}

# 함수 실행
weighted_stats <- calculate_weighted_stats(filtered_data)
write.csv(weighted_stats, "./agg_data_new.csv")

# 3년 평균 성적, 계약 규모, 토미존 수술 경력 데이터 통합

fa <- final_data
TJ<- df_final
agg <- weighted_stats

fa$Player <- sub("_[0-9]+$", "", fa$Player) 

agg$Player <- sub("_[0-9]+$", "", agg$Player) 


agg_selected <- agg %>%
  select(Player, FA_Season, weighted_W, weighted_L, weighted_SV, 
         weighted_G, weighted_GS, weighted_WAR, weighted_IP, 
         'weighted_K/9', 'weighted_BB/9', 'weighted_HR/9', 
         weighted_vFA, weighted_BABIP, 'weighted_LOB%', 'weighted_GB%', 
         'weighted_HR/FB', weighted_ERA, weighted_FIP)

tj_selected <- TJ %>%
  select(Player, Recovery_Time, FA_Year, TJ_Surgery)

merged_data <- agg_selected %>%
  left_join(fa, by = c("Player" = "Player", "FA_Season" = "Year")) %>%
  left_join(tj_selected, by = c("Player" = "Player", "FA_Season" = "FA_Year"))

merged_data$ARM <- NULL
merged_data <- merged_data %>%
  mutate(
    AGE = as.numeric(AGE),
    YRS = as.numeric(YRS),
    VALUE = as.numeric(VALUE),
    AAV = as.numeric(AAV),
    Recovery_Time = as.numeric(Recovery_Time),
    TJ_Surgery = as.numeric(TJ_Surgery)
  ) #수치형으로 변환


merged_data <- merged_data %>%
  mutate(
    YRS = replace_na(YRS, 0),
    VALUE = replace_na(VALUE, 0),
    AAV = replace_na(AAV, 0),
    Recovery_Time = replace_na(Recovery_Time, 0),
    TJ_Surgery = replace_na(TJ_Surgery, 0) #결측치 처리
  )

merged_data$weighted_vFA[is.na(merged_data$weighted_vFA)] <- mean(merged_data$weighted_vFA, na.rm = TRUE) #관측되지 않은 직구구속은 평균구속으로 처리



merged_data <- merged_data %>%
  mutate(
    AGE = map2_dbl(Player, FA_Season, ~{
      matching_row <- filtered_data %>% 
        filter(Player == .x, 
               Season == .y - 1) %>% 
        pull(Age)
      if(length(matching_row) > 0) as.numeric(matching_row[1]) else NA_real_
    })
  ) #AGE같은 경우 filtered_data에서 가져옴. merged_data의 FA Season이 filtered data의 Season보다 1 클 때를 기준

merged_data$AGE[is.na(merged_data$AGE)] <- mean(merged_data$AGE, na.rm = TRUE) #그래도 AGE가 결측치이면 평균으로 대체

merged_data$weighted_SV <- NULL #선발투수는 세이브와 무관
merged_data$weighted_G <- NULL #경기수는 이닝, 승과 강한 상관관계
merged_data$weighted_GS <- NULL #G와 GS는 선발투수에게 거의 동일
merged_data$TJ_Surgery <- NULL #Recovery_Time과 강한 상관관계, Recovery_Time으로만 분석

merged_data <- merged_data %>%
  mutate(success = ifelse(YRS > 0, 1, 0)) #YRS가 1 이상이면 계약 성공





# 같은 방법으로, 예측을 위한 merge_data2025 형성 (24-25시즌 계약 예정자들 데이터)
sp25 <- read.csv("./FA Starting Pitchers 24-25.csv")

df_merged25 <- sp25 %>%
  left_join(tj %>% 
              select(Player, `TJ Surgery Date`, `Return Date (same level)`),
            by = "Player")

df_merged25 <- df_merged25 %>%
  mutate(
    `TJ Surgery Date` = as.Date(`TJ Surgery Date`, format = "%m/%d/%Y"),
    `Return Date (same level)` = as.Date(`Return Date (same level)`, format = "%m/%d/%Y"),
    Recovery_Time = as.numeric(`Return Date (same level)` - `TJ Surgery Date`),
    FA_Year = as.numeric(paste0("20", str_sub(FA.Season, -2))),
    Return_Year = year(`Return Date (same level)`),
    Surgery_Year = year(`TJ Surgery Date`),
    year_diff = FA_Year - Return_Year
  )

# 1. FA 시즌 이후 수술받은 경우 제외
# 2. year_diff가 3년 이상인 경우 제외
# 3. year_diff가 0 이하인 경우 제외
df_filtered25 <- df_merged25 %>%
  mutate(
    `TJ Surgery Date` = if_else(
      (FA_Year < Surgery_Year) | 
        (year_diff > 3) | 
        (year_diff <= 0),
      NA_Date_,
      `TJ Surgery Date`
    ),
    `Return Date (same.level)` = if_else(
      (FA_Year < Surgery_Year) | 
        (year_diff > 3) | 
        (year_diff <= 0),
      NA_Date_,
      `Return Date (same level)`
    ),
    Recovery_Time = if_else(
      (FA_Year < Surgery_Year) | 
        (year_diff > 3) | 
        (year_diff <= 0),
      NA_real_,
      Recovery_Time
    )
  )

df_filtered25 <- df_filtered25 %>%
  mutate(TJ_Surgery = as.numeric(!is.na(`TJ Surgery Date`)))

df_final25 <- df_filtered25 %>%
  group_by(Player, FA.Season) %>%
  arrange(desc(TJ_Surgery), .by_group = TRUE) %>%
  filter(row_number() == 1) %>%
  ungroup()


write.csv(df_final25, "Surgery_data_final_25.csv", row.names = FALSE)

#같은 방법으로 25년 계약대상자들의 통산 스탯 다 크롤링
all_players_data25 <- list()
for(i in 1:nrow(df_final25)) {
  message(sprintf("Processing player %d of %d: %s", i, nrow(df_final25), df_final25$Player[i]))
  
  url <- df_final25$url[i]
  player_name <- df_final25$Player[i]
  
  player_stats <- get_player_stats(url, player_name)
  
  if(!is.null(player_stats)) {
    all_players_data25[[i]] <- player_stats
  }
  
  Sys.sleep(0.7)
}

final_data_stats25 <- bind_rows(all_players_data25) %>%
  select(Player, Season, Team, Age, W, L, SV, G, GS, IP,
         `K/9`, `BB/9`, `HR/9`, BABIP, `LOB%`, `GB%`,
         `HR/FB`, vFA, ERA, xERA, FIP, xFIP, WAR) %>%
  mutate(Player = trimws(Player))


#3년치 데이터 추리기 및 위와 같은 방법으로 weighted stats 계산
filtered_data25 <- sp25 %>%
  inner_join(final_data_stats25, by = "Player") %>%
  filter(Season >= 2022 & Season <= 2024) %>%
  rename(FA_Season = FA.Season) %>%
  mutate(
    FA_Season = 2025,
    Season = as.numeric(Season)
  )

filtered_data25 <- filtered_data25 %>%
  group_by(Player) %>%
  mutate(
    vFA = case_when(
      is.na(vFA) ~ mean(vFA, na.rm = TRUE),
      TRUE ~ vFA
    )
  ) %>%
  ungroup()

weighted_stats25 <- calculate_weighted_stats(filtered_data25)

weighted_stats25 <- weighted_stats25 %>%
  left_join(filtered_data25 %>% 
              filter(Season == 2024) %>%
              select(Player, Age) %>%
              rename(AGE = Age),
            by = "Player")



#3년치 평균성적 데이터와 토미존이력 데이터 병합 (25년도 계약예정자들 한해)
agg25<- weighted_stats25
tj25 <-df_final25

agg25_selected <- agg25 %>%
  select(Player, FA_Season, AGE, weighted_W, weighted_L, weighted_SV, 
         weighted_G, weighted_GS, weighted_WAR, weighted_IP, 
         'weighted_K/9', 'weighted_BB/9', 'weighted_HR/9', 
         weighted_vFA, weighted_BABIP, 'weighted_LOB%', 'weighted_GB%', 
         'weighted_HR/FB', weighted_ERA, weighted_FIP)

tj25_selected <- tj25 %>%
  select(Player, Recovery_Time, TJ_Surgery)

merged25_data <- agg25_selected %>%
  left_join(tj25_selected, by = c("Player" = "Player"))


merged25_data <- merged25_data %>%
  mutate(
    Recovery_Time = as.numeric(Recovery_Time),
    Recovery_Time = replace_na(Recovery_Time, 0),
    TJ_Surgery = as.numeric(TJ_Surgery),
    TJ_Surgery = replace_na(TJ_Surgery, 0),
    weighted_vFA = replace(weighted_vFA, is.na(weighted_vFA), mean(weighted_vFA, na.rm = TRUE)),    
  ) #위와 같은 방법으로 결측치 대체

write.csv(merged25_data,file="merged_data2025.csv")



merged25_data$AGE[is.na(merged25_data$AGE)] <- mean(merged25_data$AGE, na.rm = TRUE)


# 1.계약 성공여부 예측

merged_data$weighted_SV <- NULL
merged_data$weighted_G <- NULL
merged_data$weighted_GS <- NULL
merged_data$TJ.Surgery <- NULL #위와 같은 이유

merged_data <- merged_data %>%
  mutate(success = factor(ifelse(YRS > 0, "Success", "Failure")))

#merged_data <- merged_data %>%
  #mutate(success = ifelse(YRS > 0, 1, 0))

cor_matrix <- cor(merged_data[, sapply(merged_data, is.numeric)]) #상관관계 분석

library(dplyr)
library(caret)
library(randomForest)
library(smotefamily)


x_vars <- c(grep("weighted_", names(merged_data), value = TRUE), "AGE", "Recovery_Time") #학습 위한 독립변수들
model_data <- merged_data[, c(x_vars, "success")] #success가 종속변수
model_data$success <- as.factor(model_data$success) 

model_data <- model_data %>%
  rename(
    weighted_K_9 = `weighted_K/9`,
    weighted_BB_9 = `weighted_BB/9`,
    weighted_HR_9 = `weighted_HR/9`,
    weighted_LOB_pct = `weighted_LOB%`,
    weighted_GB_pct = `weighted_GB%`,
    weighted_HR_FB = `weighted_HR/FB`
  ) #랜덤포레스트에서 %, / 안 돌아감

# Train-Test Split 
set.seed(123)
train_index <- createDataPartition(model_data$success, p = 0.7, list = FALSE)
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]


# cross validation (데이터셋이 많지 않음)
ctrl <- trainControl(
  method = "cv",         
  number = 5,               
  classProbs = TRUE,       
  summaryFunction = twoClassSummary 
)

rf_model <- train(
  success ~ .,
  data = train_data,
  method = "rf",
  trControl = ctrl,
  importance = TRUE,
  ntree = 500,
  nodesize = 5,
  maxnodes = 20,
)

# 변수 중요도 출력
importance_matrix <- varImp(rf_model)
varImpPlot(rf_model$finalModel)


# 모델 평가
# Training Accuracy
success_pred_train <- predict(rf_model, train_data)
conf_matrix_train <- confusionMatrix(success_pred_train, train_data$success)
train_accuracy_success <- conf_matrix_train$overall["Accuracy"]

# Test Accuracy
success_pred_test <- predict(rf_model, test_data)
conf_matrix_test <- confusionMatrix(success_pred_test, test_data$success)
test_accuracy_success <- conf_matrix_test$overall["Accuracy"]
precision <- conf_matrix_test$byClass['Pos Pred Value']
recall <- conf_matrix_test$byClass['Sensitivity']

#모든 선수들 계약성공여부 예측
merged25_data$weighted_SV <- NULL
merged25_data$weighted_G <- NULL
merged25_data$weighted_GS <- NULL
merged25_data$TJ.Surgery <- NULL

merged25_data <- merged25_data %>%
  rename(
    weighted_K_9 = `weighted_K/9`,
    weighted_BB_9 = `weighted_BB/9`,
    weighted_HR_9 = `weighted_HR/9`,
    weighted_LOB_pct = `weighted_LOB%`,
    weighted_GB_pct = `weighted_GB%`,
    weighted_HR_FB = `weighted_HR/FB`
  )

success_pred_merged25 <- predict(rf_model, merged25_data)
merged25_data$success_pred <- success_pred_merged25 #merged25_data에 성공여부 예측 칼럼 추가














# 2. 계약년수 예측
# Yrs 컬럼이 0이 아닌 행만 필터링
filtered_data <- merged_data %>%
  filter(YRS != 0)


filtered_data <- filtered_data %>%
  mutate(
    Yrs_category = case_when(
      YRS %in% c(1) ~ "category1",    #1년 계약
      YRS %in% c(2, 3, 4) ~ "category2", #단기~중기 계약
      YRS >=5 ~ "category3", #장기 계약
      TRUE ~ NA_character_
    )
  )

filtered_data$Yrs_category <- as.factor(filtered_data$Yrs_category)


filtered_data <- filtered_data %>%
  select(-YRS & -AAV & -success)  # AAV, YRS, succes 지금은 필요 없음

filtered_data <- na.omit(filtered_data)

filtered_data <- filtered_data %>%
  rename(
    weighted_K_9 = `weighted_K/9`,
    weighted_BB_9 = `weighted_BB/9`,
    weighted_HR_9 = `weighted_HR/9`,
    weighted_LOB_pct = `weighted_LOB%`,
    weighted_GB_pct = `weighted_GB%`,
    weighted_HR_FB = `weighted_HR/FB`
  )

table(filtered_data$Yrs_category) #클래스 불균형 확인 위해

# Train-Test Split  
set.seed(123)
train_index <- createDataPartition(filtered_data$Yrs_category, p = 0.8, list = FALSE)
train_data <- filtered_data[train_index, ]
test_data <- filtered_data[-train_index, ]

# train_data에서 X, Player, FA_Season, Yrs, Value 열 제거
train_data <- train_data %>% 
  select(-Player, -FA_Season, -VALUE)

# category 간 불균형 맞춰주기 위해 업샘플링
balanced_train <- upSample(
  x = train_data[, -which(names(train_data) == "Yrs_category")],
  y = train_data$Yrs_category,
  yname = "Yrs_category"
)


table(balanced_train$Yrs_category) #클래스 분포 재확인

# cross validation (데이터셋이 많지 않음)
ctrl <- trainControl(
  method = "cv",            
  number = 5,               
  classProbs = TRUE,        
  summaryFunction = multiClassSummary,
)

set.seed(123)
rf_model_yrs <- train(
  Yrs_category ~ .,
  data = balanced_train,
  method = "rf",
  trControl = ctrl,
  importance = TRUE,
  ntree = 500,
  nodesize = 3,
  maxnodes = 20,
)

# 변수 중요도 출력
importance_matrix_yrs <- varImp(rf_model_yrs)
varImpPlot(rf_model_yrs$finalModel)





# 모델 평가
# Training Accuracy
Yrs_category_pred_train <- predict(rf_model_yrs, balanced_train)
conf_matrix_train <- confusionMatrix(Yrs_category_pred_train, balanced_train$Yrs_category)
train_accuracy_Yrs_category <- conf_matrix_train$overall["Accuracy"]

# Test Accuracy
predictions <- predict(rf_model_yrs, test_data)
conf_matrix <- confusionMatrix(predictions, test_data$Yrs_category)
test_accuracy_Yrs_category <- conf_matrix$overall["Accuracy"]
precision_by_class <- conf_matrix$byClass[, "Pos Pred Value"]
recall_by_class <- conf_matrix$byClass[, "Sensitivity"]

#모든 선수들 년수 카테고리 예측
merged25_success <- merged25_data %>%
  filter(success_pred =='Success')

yrs_predict <- predict(rf_model_yrs, merged25_success)
merged25_success$predicted_years <- yrs_predict
cat3_predict <- predict(rf_model_yrs, merged25_success, type="prob")
merged25_success$predicted_catprob <- cat3_predict













# 3. AAV 예측

filtered_data_value <- merged_data
filtered_data_value <- filtered_data_value %>%
  filter(AAV != 0) %>% #AAV 0이면 계약실패이므로 회귀에서 제외
  select(-Player, -FA_Season, -YRS, -VALUE, -success)

filtered_data_value <- filtered_data_value %>%
  rename(
    weighted_K_9 = `weighted_K/9`,
    weighted_BB_9 = `weighted_BB/9`,
    weighted_HR_9 = `weighted_HR/9`,
    weighted_LOB_pct = `weighted_LOB%`,
    weighted_GB_pct = `weighted_GB%`,
    weighted_HR_FB = `weighted_HR/FB`
  )


filtered_data_value <- na.omit(as.data.frame(filtered_data_value))

hist(filtered_data_value$AAV) #AAV 분포 확인


#filtered_data_value$AAVroot <- sqrt(filtered_data_value$AAV)
filtered_data_value$AAVlog <- log(filtered_data_value$AAV)

#hist(filtered_data_value$AAVroot)
hist(filtered_data_value$AAVlog) #로그분포는 정규분포임을 확인함


# Train-Test Split
set.seed(123)
train_index <- createDataPartition(filtered_data_value$AAVlog, p = 0.8, list = FALSE)
train_data <- filtered_data_value[train_index, ]
test_data <- filtered_data_value[-train_index, ]

train_data$X.1 <-NULL
test_data$X.1 <-NULL
train_data$AAV <- NULL  
test_data$AAV <- NULL   



# cross validation (데이터셋이 많지 않음)
ctrl <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  savePredictions = TRUE
)

set.seed(123)
rf_model_value <- train(
  AAVlog ~ .,
  data = train_data,
  method = "rf",
  trControl = ctrl,
  importance = TRUE,
  ntree = 500,
  nodesize = 5,
  maxnodes = 10,
  metric = "RMSE"
)


predictions <- predict(rf_model_value, test_data)

# 성능 지표 계산
test_results <- data.frame(
  Actual = test_data$AAVlog,
  Predicted = predictions
)

r_squared <- cor(test_results$Actual, test_results$Predicted)^2
rmse <- sqrt(mean((test_results$Actual - test_results$Predicted)^2))
mae <- mean(abs(test_results$Actual - test_results$Predicted))
mape <- mean(abs((test_results$Actual - test_results$Predicted) / test_results$Actual)) * 100



# 변수 중요도 출력
importance_matrix_value <- varImp(rf_model_value)
varImpPlot(rf_model_value$finalModel)


#모든 선수들 AAV 예측
value_predict <- predict(rf_model_value, merged25_success)
merged25_success$predicted_AAV <- exp(value_predict)

