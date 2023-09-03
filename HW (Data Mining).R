#=====1=====
data1 = c(1, 5, 10, 4)
EM = function(data, N, k, m) {
  mu = vector("numeric")
  mu[1] = m
  for (i in 1:1000) {
    m = (sum(data) + (N - k) * m) / N
    p = mu[length(mu)] - m
    if (abs(p) < 0.001)
      break
    else
      mu[i] = m
  }
  
  return(mu)
}
EM(data1, 6, 4, 3)


#=====2=====
set.seed(1810052)
mis = 2
N = 10
lambda2 = 9
pois2 = rpois(N, lambda2)
pois2[9:10] = NA
m = 10

mu = vector("numeric")
EM = function(x, N) {
  mu[1] = m
  for (i in 1:N) {
    m = (sum(x, na.rm = T) + mis * m) / N
    p = mu[length(mu)] - m
    if (abs(p) < 0.001)
      break
    else
      mu[i] = m
  }
  
  return(list(
    All_mu = mu,
    mu = mu[length(mu)],
    No_of_Iterations = i
  ))
}
EM(pois2, 10)


#=====3=====
set.seed(1810052)
EM = function(N, lambda, mis) {
  data = rpois(N, lambda)
  data[19:20] = NA
  m = round(sum(data, na.rm = T) / N)
  m
  mu = vector("numeric")
  for (i in 1:N) {
    m = (sum(data, na.rm = T) + mis * m) / N
    mu[i] = m
  }
  data[19:20] = mu[length(mu)]
  sd = sd(data)
  return(list(
    Mu = mu[length(mu)],
    Sigma = sd,
    All_mu = mu,
    Number_of_Iteration = i,
    Data = data
  ))
}
EM(20, 15, 2)


#=====4=====
set.seed(1810052)
EM = function(N, lambda, mis) {
  data = rexp(N, rate = 1 / lambda)
  data[5:6] = NA
  m = round(sum(data, na.rm = T) / N)
  mu = vector("numeric")
  mu[1] = m
  for (i in 1:1000) {
    m = (sum(data, na.rm = T) + mis * m) / N
    p = mu[length(mu)] - m
    if (abs(p) < 0.001)
      break
    else
      mu[i] = m
  }
  
  mean1 = mean(data, na.rm = T)
  var1 = var(data, na.rm = T)
  data[5:6]  = mu[length(mu)]
  mean2 = mean(data)
  var2 = var(data) / N
  
  result = list(
    Mu = mu,
    Data = data,
    Mean_Estimate = mean2,
    Variance_Estimate = var2
  )
  return(result)
}
results = EM(10, 15, 2)

#a
MVUE = results$Mean_Estimate
MVUE

#b
UVSM = results$Variance_Estimate
UVSM



#======================THE END=====================#