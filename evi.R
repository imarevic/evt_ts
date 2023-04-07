
pickand_evi <- function(data) {
  
  data = sort(data)
  n = length(data)
  max_ks = floor(n/4)
  ks = 1:max_ks
  pick = log((data[ks] - data[2*ks])/(data[2*ks] - data[4*ks]))/log(2)
  res = data.frame(ks, pick)
  return(res)
}

moment_evi <- function(data) {
  
  data = sort(data)
  n = length(data)
  ks = 1:n
  hill_one = hill_evi(data, 1)
  hill_two = hill_evi(data, 2)
  mom = hill_one[ks] + (1-(0.5*(1/(1-((hill_one[ks]**2) / hill_two[ks])))))
  res = data.frame(ks, mom)
  return(res)
}

reduced_moment_evi <- function(data) {
  
  data = sort(data)
  n = length(data)
  ks = 1:n
  hill_one = hill_evi(data, 1)
  hill_two = hill_evi(data, 2)
  mom = 1-(0.5*(1/(1-((hill_one[ks]**2) / hill_two[ks]))))
  res = data.frame(ks, mom)
  return(res)
}

hill_evi <- function(data, r) {
  
  data = sort(data)
  n = length(data)
  m = numeric(n)
  for (k in 1:(n-1)) {
    summ = 0
    for (i in 0:(k-1)) {
      summ = summ + (log(data[(n-i)]) - log(data[(n-k)]))**r
    }
    s = (1/k) * summ
    m[k] = s
  }
  return(m)  
}

pwm_evi <- function(data, r) {
  
  data = sort(data)
  n = length(data)
  pwm = numeric(n)
  ks = 1:n
  
  for (k in ks) {
    summ = 0
    for (i in 1:r) {
      summ = summ + ((((n-k)*(n-k-1)*(n-k-r+1)) / ((n-1)*(n-2)*(n-r))) * data[k])
    }
    pwm[k] = (1/n) * summ
  }
  res = data.frame(ks, pwm)
  return(res)
}
