library(dplyr) 
library(stringr)

covid_19_project = read.csv("C:/Users/lyj/Desktop/UCSB/PSTAT 120c/final project/daily.csv")
temperature = read.csv("C:/Users/lyj/Desktop/UCSB/PSTAT 120c/final project/Average Temperature of Cities.csv")
state_temp = read.csv("C:/Users/lyj/Desktop/UCSB/PSTAT 120c/final project/temp.csv")

base = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/'
confirm = 'time_series_covid19_confirmed_'

us_confirm = read.csv(paste0(base,confirm,"US.csv"))

View(covid_19_project)
View(state_temp)
View(us_confirm)
View(us_temp)

rate_temp=function(start, end){
  start_date=as.Date(start)
  end_date=as.Date(end)
  
  state_rate=array(dim = 51)
  for (i in c(1:51)) {
    col_names = colnames(us_confirm)
    all_dates_raw = as.Date(paste0(str_sub(col_names[13:dim(us_death)[2]-1], 2, -1), "20"), tryFormats = c("%m.%d.%Y"))
    a=substr(all_dates_raw,start=1, stop=2)
    b=substr(all_dates_raw,start=5, stop=10)
    all_dates=as.Date(paste0('20',a,b))
    
    intended_state=state_temp$name[i]
    state_confirmed = us_confirm %>%
      dplyr::filter(Province_State == intended_state)
    state_confirmed_sum = apply(state_confirmed[,12:dim(state_confirmed)[2]], 2, sum)
    state_confirmed_selected = state_confirmed_sum[which(all_dates %in% seq.Date(start_date, end_date, by=1))]
    state_confirmed_selected=as.numeric(state_confirmed_selected)
    
    state_cum_confirm=state_confirmed_selected[length(state_confirmed_selected)]-state_confirmed_selected[1]
    state_info= state_temp %>%
      dplyr::filter(name == intended_state)
    state_pop=state_info[[3]]
    state_comfirm_rate=state_cum_confirm/state_pop
    state_rate[i]=state_comfirm_rate
  }
  rate=data.frame(name=state_temp$name,rate=state_rate,temp=state_temp$temp)
  return(rate)
}
temp_rate=rate_temp("2020-03-31","2021-01-31")
temp_rate=rate_temp("2020-06-30","2021-01-31")
temp_rate=rate_temp("2020-10-31","2021-01-31")


plot(temp_rate$rate~temp_rate$temp, 
     xlab="state temperature", ylab = "confirmed rate", main="figure 4.3")

model1=lm(temp_rate$rate~temp_rate$temp)
summary(model1)


rate_temp_2=function(start,duration){
  start_date=as.Date(start)
  
  state_rate=array(dim = 51)
  for (i in c(1:51)) {
    col_names = colnames(us_confirm)
    all_dates_raw = as.Date(paste0(str_sub(col_names[13:dim(us_death)[2]-1], 2, -1), "20"), tryFormats = c("%m.%d.%Y"))
    a=substr(all_dates_raw,start=1, stop=2)
    b=substr(all_dates_raw,start=5, stop=10)
    all_dates=as.Date(paste0('20',a,b))
    
    intended_state=state_temp$name[i]
    state_confirmed = us_confirm %>%
      dplyr::filter(Province_State == intended_state)
    
    state_confirmed_sum = apply(state_confirmed[,12:dim(state_confirmed)[2]], 2, sum)
    
    n=12
    while (state_confirmed_sum[n]<=30){
      n=n+1
    }
    
    state_confirmed_selected = state_confirmed_sum[n:n+duration]
    state_confirmed_selected=as.numeric(state_confirmed_selected)
    
    state_cum_confirm=state_confirmed_selected[length(state_confirmed_selected)]
    state_info= state_temp %>%
      dplyr::filter(name == intended_state)
    state_pop=state_info[[3]]
    state_comfirm_rate=state_cum_confirm/state_pop
    state_rate[i]=state_comfirm_rate
  }
  rate=data.frame(name=state_temp$name,rate=state_rate,temp=state_temp$temp)
  return(rate)
}

temp_rate_2=rate_temp_2("2020-01-31",50)
plot(temp_rate_2$rate~temp_rate_2$temp, 
     xlab="state temperature", ylab = "confirmed rate", main="figure 4.4")
temp_rate_2=rate_temp_2("2020-01-31",75)
plot(temp_rate_2$rate~temp_rate_2$temp, 
     xlab="state temperature", ylab = "confirmed rate", main="figure 4.5")
temp_rate_2=rate_temp_2("2020-01-31",100)
plot(temp_rate_2$rate~temp_rate_2$temp, 
     xlab="state temperature", ylab = "confirmed rate", main="figure 4.6")


model2=lm(temp_rate_2$rate~temp_rate_2$temp)



model2=lm(temp_rate$rate~(exp(temp_rate$temp)))
summary(model2)

