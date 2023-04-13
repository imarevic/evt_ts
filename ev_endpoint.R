source("evi.R")

calc_endpoint <- function(data, xi) {
  
  data = sort(data)
  n = length(data)
  ks = 1:(n-1)
  x_hat = numeric(n-1)
  hill_one = hill_evi(data, 1)
  
  for (k in ks) {
    m_n = data[(n-k)]
    a_hat = m_n*hill_one[k]*(1-xi)
    b_hat = m_n
    x_hat[k] = b_hat-(a_hat/xi)
  }
  res = data.frame(ks, x_hat)
  return(res)
}

calc_se_dekkers <- function(data, xi) {
  
  n = length(data)
  num = ((1-xi)**2) * (1-(3*xi)+(4*(xi**2)))
  denom = (xi**4) * (1-(2*xi)) * (1-(3*xi)) * (1-(4*xi))
  var = num / denom
  sd = sqrt(var)
  se = sd / sqrt(n)
  return(se)
}

calc_se <- function(data) {
  n = length(data)
  se = sd(data) / sqrt(n)
  return(se)
}

calc_endpoint_quality <- function(data, xi) {
  
  data = sort(data)
  n = length(data)
  q = numeric(n-1)
  ks = 1:(n-1)
  hill_one = hill_evi(data, 1)
  
  for (k in ks) {
    m_n = data[(n-k)]
    a_hat = m_n*hill_one[k]*(1-xi)
    b_hat = m_n
    d = 1+(xi*((max(data) - b_hat) / (a_hat)))
    max_d = max(c(0,d))
    q[k] = k*(max_d**(-1/xi))
  }
  res = data.frame(ks, q)
  return(res)
}