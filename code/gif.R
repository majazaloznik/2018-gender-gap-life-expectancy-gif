# preliminaries ###############################################################
###############################################################################
# devtools::install_github('thomasp85/transformr')
# devtools::install_github('thomasp85/gganimate')

library(dplyr)
library(readr)
library(tidyr)


df <- read_csv("data/interim/life-expectancy-gender-gap-usa.csv")

# clean data ##################################################################

df %>%  
  mutate(Age = ifelse(Age == "100+", 100, Age)) %>% 
  mutate(Age = as.numeric(Age)) %>% 
  gather( key = year, value = life_expectancy, 5:34) %>% 
  separate(year, c("start", "end")) %>% 
  mutate(midyear = as.numeric( start) + 2.5) %>% 
  select(-start, -end, -1, -2) %>% 
  filter(midyear < 2050) -> df
  

# function for creating file name with leading zeros
# makes it easier to process them sequentially
rename <- function(x){
  if (x < 10) {
    return(name <- paste('000',i,'plot.png',sep=''))
  }
  if (x < 100 && i >= 10) {
    return(name <- paste('00',i,'plot.png', sep=''))
  }
  if (x >= 100) {
    return(name <- paste('0', i,'plot.png', sep=''))
  }
}

for (i in seq(length(unique(df$midyear)))) {
  name <- rename(i)
  midyear <- unique(df$midyear)[i]
  png(name)
  men <- df[df$midyear == midyear & df$Sex == "Male",]
  women <- df[df$midyear == midyear & df$Sex == "Female",]
  plot(women$Age,women$life_expectancy, type = "b", lwd = 2, col = "orange",
       xlab = "Age", ylab = "(Remaining) Life expectancy", ylim = c(0, 85))
  text(x = 60, y = 70, paste("USA - Year:", midyear), cex = 1.5)
  lines(men$Age,men$life_expectancy, type = "b", lwd = 2, col = "purple")
  legend(x = 80, y = 60, legend = c("ladies", "gents"),  pch = c(1, 1), lty = c(1, 1), lwd = 2,
         col = c("orange", "purple"))
  dev.off()
}

my_command <- '"C:\\Program Files\\ImageMagick-7.0.5-Q16\\convert.exe" *.png -delay 50 animation.gif'
system(my_command)


