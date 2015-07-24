library(iCacheR)

context("Basic tests")

test_that("Basic cache",
{
  f = function(n) rnorm(n)
  cf = ic_cache(f)
  expect_equal(cf(1), cf(1))
  expect_false(cf(1) == cf(2)[1])
})

test_that("Disable cache in memory",
          {
            options(iCacheR.cache = NULL)
            f = function(n) n
            cf = ic_cache(f)
            val = cf(1)
            val = cf(1)
            options(iCacheR.cache = new.env())
          })


test_that("Load from memory cache",
{
  f = function(n) n
  cf = ic_cache(f)
  params = list(n = 1)
  params$.FNC = "f"
  paramsHash = digest(params)
  val = cf(n = 1)

  expect_true(paramsHash %in% ls(getOption("iCacheR.cache")))

})


