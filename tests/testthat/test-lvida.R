test_that("lv-ida works", {
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
    
    lv.ida.est <- lv.ida(x,y,cov(data),fci.est@amat,method="local")
    
    lv.ida.est # this is the multiset of causal effects of x = 2 on y = pvar
    print("non lm")
    print(lv.ida.est)
    
    # Set up to ensure no causal effect, only one MAG in PAG
    expect_equal(lv.ida.est[1], 0)
})

test_that("lv-ida lm works", {
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
  
  lv.ida.est <- lv.ida.lm(x.pos=x,y.pos=y,data=data,pag=fci.est@amat,method="local")
  lv.ida.est # this is the multiset of causal effects of x = 2 on y = pvar
  print("lm")
  print(lv.ida.est)
  # Set up to ensure no causal effect, only one MAG in PAG
  expect_equal(lv.ida.est[1], 0)
})
