library(vroom)
library(timetk)
library(tidyverse)
library(patchwork)

ID_train <- vroom("./train.csv") 
ID_test <- vroom("./test.csv") 



ID_train %>% 
  plot_time_series(date, sales, .interactive = FALSE)

store1_item3 <- ID_train %>% 
  filter(store ==1 & item ==3)

store5_item11 <- ID_train %>% 
  filter(store ==5 & item ==11)

store7_item13 <- ID_train %>% 
  filter(store ==7 & item ==13)

store10_item25 <- ID_train %>% 
  filter(store ==10 & item ==25)

plot1 <- store1_item3 %>%
  pull(sales) %>% 
  forecast::ggAcf(.,lag.max= 2*365)

plot2 <- store5_item11 %>%
  pull(sales) %>% 
  forecast::ggAcf(.,lag.max= 2*365)

plot3 <- store7_item13 %>%
  pull(sales) %>% 
  forecast::ggAcf(.,lag.max= 2*365)

plot4 <- store10_item25 %>%
  pull(sales) %>% 
  forecast::ggAcf(.,lag.max= 2*365)

(plot1 + plot2)/(plot3+plot4)
