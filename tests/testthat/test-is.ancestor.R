test_that("is.ancestors works with unnamed matrices", {
    gMag <- matrix(c(0, 3, 2, 0), nrow=2, dimnames = NULL)
    print(gMag)
    x.pos <- 1 
    y.pos <- 2
    result <- is.ancestor(x.pos, y.pos, gMag)
    expect_equal(result, TRUE)
})

test_that("is.ancestors works with unnamed matrices for the same variable", {
    gMag <- matrix(c(0, 2, 0, 0), nrow=2, dimnames = NULL)
    print(gMag)
    x.pos <- 1 
    y.pos <- 1
    result <- is.ancestor(x.pos, y.pos, gMag)
    expect_equal(result, TRUE)
})

test_that("is.ancestors works with unnamed matrices for the same variable", {
    gMag <- matrix(c(0, 2, 0, 0), nrow=2, dimnames = NULL)
    print(gMag)
    x.pos <- 1 
    y.pos <- 1
    result <- is.ancestor(x.pos, y.pos, gMag)
    expect_equal(result, TRUE)
})