library(dplyr)
library(tidyr)

war2015 <- read.csv("WAR2015.csv")
war2016 <- read.csv("WAR2016.csv")
war2017 <- read.csv("WAR2017.csv")
war2018 <- read.csv("WAR2018.csv")
war2019 <- read.csv("WAR2019.csv")
dfs <- list(war2015, war2016, war2017, war2018, war2019)

n <- colnames(war2015)[2:length(colnames(war2015))]

fix_column <- function(col) {
  new_col <- data.frame(substr(col, 1, 3), as.numeric(substr(col, 4, 1000)))
  names(new_col) <- c("Team", "")
  return(new_col)
}

clean <- function(df, year) {
  new <- lapply(df[2:ncol(df)], fix_column)
  new_df <- combine(new)
  colnames(new_df) <- c("Team", n)
  return(new_df)
}

combine <- function(l) {
  if(length(l) == 2) {return(merge(l[[1]], l[[2]], by="Team"))}
  else {merge(l[[1]], combine(l[2:length(l)]))}
}

yearly <- lapply(dfs, clean)
new_yearly <- mapply(cbind, yearly, Year = c(2015:2019), SIMPLIFY = F)
final <- bind_rows(new_yearly)
final <- final[,c(ncol(final), 1:ncol(final)-1)]
final$All.P <- final$SP + final$RP
names(final)[16] <- "OF.All"
final2 <- gather(final, Position, WAA, Total:PH)
final2[final2$Position == "X1B","Position"] <- "1B"
final2[final2$Position == "X2B","Position"] <- "2B"
final2[final2$Position == "X3B","Position"] <- "3B"
final3 <- final3 %>% filter(Team %in% c("PIT", "STL", "CHC", "CIN", "MIL"))

write.csv(final2, "WAR.csv", row.names = F)
