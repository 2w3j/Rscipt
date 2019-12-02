

#-------------------------------------------------------------------------------------
# Library 
rm(list = ls())
gc()

lib<-c("dplyr", "plyr", "ggplot2", "data.table", "pracma", "tidyverse",
       "lubridate", "ggridges", "reshape2","knitr","MASS","caret","corrplot",
       "kernlab","data.table","rvest","XML","jsonlite","httr","readxl","xlsx","sn","digest")

suppressMessages(sapply(lib,require,character.only = TRUE))



## profile 및 시계열 결제 내역 데이터 만들기 

#1) 기본 프로파일 데이터 
#2) 결제 시계열 데이터 


# Type 선택 (1:numerical, 2.categorical, 3.time_series)
  # 1 -1) 최대 최소 값 제어가 필요한가?
  # 1 -2) skwness가 이입되어 있어야 하는가?


D_GAN_NUM <- function(size, minmax, skeness,)
D_GAN_CAT()
D_GAN_TIME()


## 시간 
## n 명의 유니크 사용자에 대해서 st시작 시간, et 끝시간, 평균적으로 r 회 결제가 발생하는 결제 데이터 

 Time_Gen <- function(N, st="2019/11/01", et="2019/11/30") {
   # 이 함수는 1명에 대한 시계열 데이터 생성
   # 이 한명에 필요한 파라미터는 lamda(r)이다.
   if ( N > 0 ) {
          st <- as.POSIXct(as.Date(st))
          et <- as.POSIXct(as.Date(et))
          dt <- as.numeric(difftime(et,st,unit="sec"))
          ev <- sort(runif(N, 0, dt))
          rt <- st + ev
    return(rt) } else {
        return(print("N is not a positive value"))
        }
      }


 Payment_timeseries <- function(n,r,st,et){
                            #n : the number of client
                            #r : the mean count of payment per client (using poisson dist.)
                            #st: start time (day)
                            #et: end time (day)
    # unique user id 생성 
    user_id <- lapply(1:n, function(x){
        tmp <- sprintf("custom_%s", substr(digest(runif(1)),1,7)) 
    return(tmp)}); 
    user_id<-do.call(rbind,user_id);colnames(user_id) <- "user_id"
    
    # 우리의 유저들이 평균 빈도 r을 가지는 포아송 분포에서 생성된다는 가정이 있고
    # r 값은 참조 받는 것이 가장 나을 듯 
    
    r_set <- rpois(n,r)
    
    #
    data_set <- lapply(1:n, function(x){
             result_set<- data.frame( user_id=rep(user_id[x],r_set[x]),
                               pay_date_tiem=Time_Gen(r_set[x],st,et))       
                
           return(result_set) })
    
    return( rbindlist(data_set) )
    
} 

 
 
tmp <- Payment_timeseries(1000,10,st="2019/11/01", et="2019/11/30") # 유저 1000명, 평균 10회 결제, 시간은 11월




