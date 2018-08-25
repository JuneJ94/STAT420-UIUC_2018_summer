birthday = 19890310
set.seed(birthday)
beta_0_model1 = 3
beta_1_model1 = 1
beta_2_model1 = 1
beta_3_model1 = 1
sample_size = 25
sigma = c(1, 5, 10)
sim_data = read.csv('/Users/xinqu/Sandbox/STAT\ 420/week6/sim-proj/study_1.csv')
num_sim = 2500
sigma1 = 1
F_st = rep(0, 2500)
pval = rep(0, 2500)
R = rep(0, 2500)
for (i in 1: 2500) {
  eps = rnorm(25, mean = 0, sd = sigma1)
  sim_data$y = beta_0_model1 + beta_1_model1 * sim_data$x1+ beta_2_model1 * sim_data$x2 + beta_3_model1 * sim_data$x3 + eps
  model = lm(y ~ x1 + x2 + x3, data = sim_data)
  F_st[i] = summary(model)$fstatistic[1]
  pval[i] = pf(summary(model)$fstatistic[1], summary(model)$fstatistic[2], summary(model)$fstatistic[3], lower.tail = FALSE)
  R[i] = summary(model)$r.squared
}
hist(F_st, probability = TRUE, ylim = c(0, 0.08))
x = seq(1, 300, length = 100)
curve(df(x, df1 = 3, df2 = 21), add = T, col = 'red')












x <- rchisq(100, 5) 
hist(x, prob=TRUE) 
curve( dchisq(x, df=21), col='red', add=TRUE) 
x = seq(0, 5, length = 100)
plot(x, df(x = x, df1 = 3, df2 = 21), add = T)
curve(df(x, df1 = 3, df2 = 21), add = T)
hist(data_model1[[1]]$p_val, prob = TRUE, xlab = "", main = "sigma = 1", border = "dodgerblue", breaks = 30)
x = seq(min(data_model1[[1]]$p_val), max(data_model1[[1]]$p_val), length = 100)
curve(dt(x, df = 21), add = TRUE, col = 'red', lwd = 2)

sigma_values = c("`sigma = 1`", "`sigma = 5`", "`sigma = 10`")
result1 = data.frame(sigma_values, F_stat_mean_1, p_val_mean_1, R_2_mean_1)
colnames(result1) = c('sigma', 'F statistic', 'p-value')
knitr::kable(result1)

library(broom)

for (i in 1: 2500) {
  eps = rnorm(25, mean = 0, sd = sigma1)
  sim_data$y = beta_0_model1 + beta_1_model1 * sim_data$x1+ beta_2_model1 * sim_data$x2 + beta_3_model1 * sim_data$x3 + eps
  avo_res = aov(y ~ x1 + x2 + x3, data = sim_data)
  F_st[i] = glance(avo_res)$stat
}
hist(F_st, probability = TRUE)
x = seq(1, 300, length = 100)
curve(df(x, df1 = 3, df2 = 21), add = T, col = 'red', lwd = 2)


hist(data_model1[[1]]$F_stat, prob = TRUE, xlab = "", main = "sigma = 1, model1", 
     ylim = c(0, 0.1), border = "dodgerblue")
#x = seq(1, 300, length = 100)
curve(df(x, df1 = 4 - 1, df2 = 25 - 4), add = TRUE, col = 'orange', lwd = 2)
all.equal(F_st, data_model1[[1]]$F_stat)

hist(data_model1[[1]]$F_stat, prob = TRUE, xlab = "", main = "sigma = 1, model1", 
     ylim = c(0, 0.08), border = "dodgerblue", breaks = 100, xlim = c(0, 250))
x1 = seq(min(data_model1[[1]]$F_stat), max(data_model1[[1]]$F_stat), length = 100)
curve(df(x1, df1 = 4 - 1, df2 = 25 - 4), add = TRUE, col = 'orange', lwd = 2)


times = matrix(c(rep(0, num_model * num_sim)), 
               ncol = num_model, 
               nrow = num_sim)
times_data = list(sigma_1_t = times, 
                  sigma_2_t = times,
                  sigma_3_t = times)

###run for each sigma with 1000 simulation
for (s in 1: length(sigma)) {
  for (i in 1: num_sim) {
    row_test = all_data[[s]]$test_RMSE[i, ]
    minimum = min(row_test)
    times[i, ] = as.numeric(row_test == minimum)
  }
  times_data[[s]] = times
}
num_model = 9
num_sims = 1000
selection = matrix(c(rep(0, num_model * num_sims)),
                   ncol = num_model,
                   nrow = num_sims)

selection_data = list(
  sigma_1_s = selection,
  sigma_2_s = selection,
  sigma_3_s = selection
)

for(s in 1:length(sigma)) {
  for (i in 1:num_sims) {
    
    # Get a row (1 sim)
    test_row  = all_data[[s]]$test_rmse[i, ]
    
    # Find min
    test_min  = min(test_row)
    
    # Populate selection matrix with 0\1
    selection[i, ] = as.numeric(test_row == test_min)
  }
  
  # Populate Selection data
  selection_data[[s]] = selection
}

# Table showing sums of selections
s = data.frame(
  colSums(selection_data[[1]]),
  colSums(selection_data[[2]]),
  colSums(selection_data[[3]])
)

model_size = c("`1`", "`2`", "`3`", "`4`", "`5`", "`6`", "`7`", "`8`", "`9`")
results = data.frame(model_size, s[1], s[2], s[3])
colnames(results) = c("Model Size", 
                      "`Sigma = 1`", "`Sigma = 2`", "`Sigma = 4`"
)
knitr::kable(results)


row = all_data[[s]]$test_RMSE[1, ]

minimum = min(row)

times[1, ] = as.numeric(row == minimum)

x = seq(0, 1, length = 100)
curve(dbeta(x, shape1 = (3 - 1)/ 2, shape2 = (25 - 3) / 2), col = 'orange', lwd = 2, ylim = c(0, 10))
curve(runif(x, min = 0, max = 8), col = 'orange', lwd = 2)
