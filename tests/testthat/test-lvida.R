test_that("lv-ida-cov works", {
    set.seed(123)
    pvar <- 8
    sample <- 1000
    x <- 2
    y <- pvar
    
    rDAG <- pcalg::randomDAG(n = pvar, prob= 0.5, lB = 0.5, uB = 1.5)
    
    data <- pcalg::rmvDAG(sample,rDAG,errDist="normal")
    suffStat <- list(C=cor(data),n=nrow(data))
    rules <- c(TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE) # don't want any possible --- edges
    fci.est <- pcalg::fci(suffStat, indepTest = pcalg::gaussCItest, p = ncol(data), alpha=0.01, rules=rules)
    
    #plot(fci.est)
    
    if(is.cyclic(fci.est@amat)){
      cat("#### FOUND CYCLIC GRAPH #### \n LV-IDA won't work here! \n try again! \n")
    }
    
    lv.ida.est <- lv.ida.cov(x,y,cov(data),fci.est@amat,method="local")
    
    lv.ida.est # this is the multiset of causal effects of x = 2 on y = pvar
    print("non lm")
    
    # Set up to ensure no causal effect, only one MAG in PAG
    expect_equal(lv.ida.est[1], 0)
})

test_that("lv-ida-lm works", {
  set.seed(123)
  pvar <- 8
  sample <- 1000
  x <- 2
  y <- pvar
  
  rDAG <- pcalg::randomDAG(n = pvar, prob= 0.5, lB = 0.5, uB = 1.5)
  
  data <- pcalg::rmvDAG(sample,rDAG,errDist="normal")
  data <- as.data.frame(data)
  data[, 1] <- as.factor(data[, 1] > 0)

  # Mocked FCI adj matrix output  
  amat <- as.matrix(read.csv("test.csv", header = FALSE))
  
  if(is.cyclic(amat)){
    cat("#### FOUND CYCLIC GRAPH #### \n LV-IDA won't work here! \n try again! \n")
  }
  
  lv.ida.est <- lv.ida.lm(x.pos=x,y.pos=y,data=data,pag=amat,method="local")
  lv.ida.est # this is the multiset of causal effects of x = 2 on y = pvar
  print("lm")
  print(lv.ida.est)
  # Set up to ensure no causal effect, only one MAG in PAG
  expect_equal(lv.ida.est[1], 0)
})

test_that("lv-ida unified interface works for lm queries", {
  set.seed(123)
  n <- 4
  sample <- 1000
  a <- rnorm(sample, mean = 0)
  b <- rnorm(sample, mean= 0)
  c <- rnorm(sample, mean= 2 * a + b)
  d <- rnorm(sample, mean= c)
  data <- data.frame(a, b, c, d)
  suffStat <- list(C=cor(data),n=nrow(data))
  #rules <- c(TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE) # don't want any possible --- edges
  fci.est <- pcalg::fci(suffStat, indepTest = pcalg::gaussCItest, p = ncol(data), alpha=0.001)
  
  var.names <- letters[1:n]
  
  # Named dataframe 
  colnames(data) <- var.names
  
  # Named matrix representing PAG
  pag <-fci.est@amat
  dimnames(pag) <- list(var.names, var.names)
  
  # LVIDA call using names 
  lv.ida.est <- lv.ida(x.pos=var.names[1], y.pos=var.names[n], data=data,pag=pag,method="local")
  print(lv.ida.est)
  expect_gte(abs(lv.ida.est[1]), 0)
  
})