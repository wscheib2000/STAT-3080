
## Write a function named Vfunc1 that calculates the average
## education level, the average vocabulary score, and correlation
## between the two from a random sample of 48 respondents in the
## Vocab data.

library(car)

Vfunc1 <- function(iter) {
  print(paste("Iteration", iter))
  Vsub <- Vocab[sample(nrow(Vocab), 48),c("education", "vocabulary")]
  c(
    meanE=mean(Vsub$education),
    meanv=mean(Vsub$vocabulary),
    corEV=cor(Vsub$education, Vsub$vocabulary)
  )
}

Vfunc1()

sapply(1:10, Vfunc1)

## Write a function named Vfunc2 that calculates the average
## education level, the average vocabulary score, and correlation
## between the two using all of the respondents from a given year
## in the Vocab data.

Vfunc2 <- function(year) {
  Vsub <- Vocab[Vocab$year==year,c("education", "vocabulary")]
  c(
    meanE=mean(Vsub$education),
    meanv=mean(Vsub$vocabulary),
    corEV=cor(Vsub$education, Vsub$vocabulary)
  )
}

Vfunc2(2017)

sapply(unique(Vocab$year), Vfunc2)

## Write a function named Vfunc3 that calculates the average
## education level, the average vocabulary score, and correlation
## between the two from a random sample of 48 respondents from a
## given year in the Vocab data.

Vfunc3 <- function(year) {
  Vyear <- Vocab[Vocab$year==year,]
  Vsub <- Vyear[sample(nrow(Vyear), 48),c("education", "vocabulary")]
  c(
    meanE=mean(Vsub$education),
    meanv=mean(Vsub$vocabulary),
    corEV=cor(Vsub$education, Vsub$vocabulary)
  )
}

Vfunc3(2017)

sapply(unique(Vocab$year), Vfunc3)
