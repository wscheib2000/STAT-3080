v1 <- data.frame(X1=rnorm(100))
ggplot(v1, aes(x=X1)) + geom_density()
ggplot(v1, aes(x=X1)) + geom_line(stat="density")
ggplot(v1, aes(x=X1)) + stat_function(fun = dnorm)


alpha <- 0.05
mu <- 99.5
sd <- 4.8
reps <- 10000
sizes <- c(9, 27, 51)

ztest <- function(n, mu, sd, alpha) {
  sample <- rnorm(n, mu, sd)
  samp_mean <- mean(sample)
  z_stat <- (samp_mean-mu)/(sd/sqrt(n))
  p_val <- 2*pnorm(-abs(z_stat))
  p_val < alpha
}

sum(replicate(reps, ztest(9, mu, sd, alpha)))/reps
sum(replicate(reps, ztest(27, mu, sd, alpha)))/reps
sum(replicate(reps, ztest(51, mu, sd, alpha)))/reps


ttest <- function(n, mu_0, sd, alpha) {
  sample <- rnorm(n, 2.30, sd)
  t.test(sample, alternative="two.sided", mu=2.31, conf.level=0.95)$p.val < alpha
  # samp_mean <- mean(sample)
  # samp_sd <- sd(sample)
  # t_stat <- (samp_mean-mu_0)/(samp_sd/sqrt(n))
  # p_val <- 2*pt(-abs(t_stat), n-1)
  # p_val < alpha
}

replicate(10, mean(replicate(10000, ttest(400, 2.31, 0.16, 0.05))))

sum(replicate(reps, ttest(9, mu, sd, alpha)))/reps
sum(replicate(reps, ttest(27, mu, sd, alpha)))/reps
sum(replicate(reps, ttest(51, mu, sd, alpha)))/reps


badztest_2side <- function(n, mu, sd, alpha) {
  sample <- rnorm(n, mu, sd)
  samp_mean <- mean(sample)
  samp_sd <- sd(sample)
  z_stat <- (samp_mean-mu)/(samp_sd/sqrt(n))
  p_val <- 2*pnorm(-abs(z_stat))
  p_val < alpha
}

sum(replicate(reps, badztest_2side(9, mu, sd, alpha)))/reps
sum(replicate(reps, badztest_2side(27, mu, sd, alpha)))/reps
sum(replicate(reps, badztest_2side(51, mu, sd, alpha)))/reps


badztest_leftside <- function(n, mu, sd, alpha) {
  sample <- rnorm(n, mu, sd)
  samp_mean <- mean(sample)
  samp_sd <- sd(sample)
  z_stat <- (samp_mean-mu)/(samp_sd/sqrt(n))
  p_val <- pnorm(z_stat)
  p_val < alpha
}

sum(replicate(reps, badztest_leftside(9, mu, sd, alpha)))/reps
sum(replicate(reps, badztest_leftside(27, mu, sd, alpha)))/reps
sum(replicate(reps, badztest_leftside(51, mu, sd, alpha)))/reps



set.seed(1000)

samp_data <- c(11.3, 21.6, 16.3, 12.9, 12.8, 18.8, 17.1, 14.7)
samp_sd <- sd(samp_data)
B<-10000

## Draw the bootstrap samples
boot_samp <- replicate(B, sample(samp_data, replace=T))

## Determine the sample mean from each bootstrap sample
boot_sds <- apply(boot_samp,2,sd)

boot_sds <- boot_samp <- replicate(B, sd(sample(samp_data, replace=T)))

sigma0 <- 2
boot_sds_null <- boot_sds - mean(boot_sds) + sigma0

## Determine the p-value (two-tailed)
sum(boot_sds_null >= samp_sd | boot_sds_null <= 2*mean(boot_sds_null) - samp_sd)/B
## Second or condition is equivalent to mean(boot_sds_null)-(samp_sd-mean(boot_sds_null))

set.seed(2000)

samp_data <- c(11.3, 21.6, 16.3, 12.9, 12.8, 18.8, 17.1, 14.7)
samp_data2 <- c(15.9, 14.4, 10.9, 19.7, 11.5, 16.4, 11.6, 14.5)
samp_sd <- sd(samp_data)
samp_sd2 <- sd(samp_data2)
samp_diff <- samp_sd - samp_sd2
B<-10000

rand.test <- function(x){
  rand_comb <- sample( c(samp_data, samp_data2) )
  bsd1 <- mean(rand_comb[1:x])
  bsd2 <- mean(rand_comb[(x+1):(length(samp_data)+length(samp_data2))])
  bsd1 - bsd2
}

boot_diffs_null <- replicate(B, rand.test(length(samp_data)))

## Determine the p-value (left-tailed)
sum(boot_diffs_null <= samp_diff)/B


sizes <- c(9, 27, 51)
sign.test <- function(n, df, dir, null.hyp) {
  v <- sum(rchisq(n, df)>null.hyp)[1]
  binom.test(v, n, alternative=dir)$p.value < 0.05
}
sign.rank.test <- function(n, df, dir, null.hyp) {
  v <- rchisq(n, df)
  wilcox.test(v, mu=null.hyp, alternative=dir)$p.value < 0.05
}
rank.sum.test <- function(n, df, dir, null.hyp, std.dev) {
  v1 <- rchisq(n, df)
  v2 <- rnorm(n, null.hyp, std.dev)
  wilcox.test(v1, v2, alternative=dir)$p.value < 0.05
}
monte_carlo <- function(n, df, dir, null.hyp, k, FUN, std.dev=FALSE) {
  if(std.dev) {
    results <- replicate(k, FUN(n, df, dir, null.hyp, std.dev))
  } else {
    results <- replicate(k, FUN(n, df, dir, null.hyp))
  }
  mean(results)
}

sapply(sizes, monte_carlo, 75, "greater", 74.3344, 10000, sign.test)
sapply(sizes, monte_carlo, 75, "greater", 74.3344, 10000, sign.rank.test)
sapply(sizes, monte_carlo, 2, "greater", 1.386294, 10000, sign.test)
sapply(sizes, monte_carlo, 2, "greater", 1.386294, 10000, sign.rank.test)
sapply(sizes, monte_carlo, 75, "greater", 74.3344, 10000, rank.sum.test, 12.25)
sapply(sizes, monte_carlo, 2, "greater", 1.386294, 10000, rank.sum.test, 2)


one.samp.z.test <- function(n, p, dir) {
  samp <- rbinom(1, n, p)
  prop.test(samp, n, p=p, alternative=dir, correct=FALSE)$p.value < 0.05
}
monte_carlo <- function(n, p, dir, k, FUN) {
  set.seed(3080)
  results <- replicate(k, FUN(n, p, dir))
  mean(results)
}
sizes <- c(9, 27, 51, 78, 103)

sapply(sizes, monte_carlo, 0.5, "greater", 10000, rt.1samp.z.test)
sapply(sizes, monte_carlo, 0.3, "greater", 10000, rt.1samp.z.test)
sapply(sizes, monte_carlo, 0.1, "greater", 10000, rt.1samp.z.test)
