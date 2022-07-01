#List of teams

Team = c("Arizona","Atlanta","Baltimore","Boston","Chicago (AL)","Chicago (NL)","Cincinnati","Cleveland","Colorado","Detroit","Houston","Kansas City","Anaheim","Los Angeles","Miami","Milwaukee","Minnesota","New York (AL)","New York (NL)","Oakland","Philadelphia","Pittsburgh","San Diego","San Francisco","Seattle","St. Louis","Tampa Bay","Texas","Toronto","Washington","Montreal") #SAME AS SPREADSHEET
titles = c(5, 20, 9, 10, 5, 8, 10, 10, 0, 7, 10, 7, 9, 19, 0, 4, 12, 19, 6, 17, 11, 9, 5, 8, 3, 14, 3, 7, 6, 5, 1)

div_ldrs = read.csv(file.choose())
cols = colnames(div_ldrs)

#Split by month
library(tidyverse)

may = select(div_ldrs, ends_with("May"))
may = cbind(Team, may)

june = select(div_ldrs, ends_with("June"))
june = cbind(Team, june)

july = select(div_ldrs, ends_with("July"))
july = cbind(july)

august = select(div_ldrs, ends_with("Aug"))
august = cbind(Team, august)

sept = select(div_ldrs, ends_with("Sept"))
sept = cbind(Team, sept)

install.packages("ggplot2")
library(ggplot2)

calc_cor <- function(df) {
  v= c()
  for (i in 1:nrow(df)) {
    team_leads = 0
    for (j in 2:ncol(df)) {
      leader = 0 + ifelse(df[i, j] == "TRUE", 1, 0)
      team_leads = team_leads + leader
    }
    v = c(v, team_leads)
  }
  div_plot = plot(v, titles, xlab="Leading in Month", ylab = "Division Titles", pch = 16)

  legend(x='bottomright', 
         legend=paste('Cor =', round(cor(titles, v), 2)))
}

calc_cor(may)
calc_cor(june)
calc_cor(july)
calc_cor(august)
calc_cor(sept)





