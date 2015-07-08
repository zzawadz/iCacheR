Simple example:
---------------

    slow_function = function(n)
    {
      Sys.sleep(n) # sleep for n secs
      rnorm(1000)
    }

    system.time(slow_function(2))

    ##    user  system elapsed 
    ##       0       0       2

    library(iCacheR)

    cache_function = ic_cache(slow_function)

    system.time(t1 <- cache_function(2)) # first use - normal time

    ##    user  system elapsed 
    ##    0.00    0.00    2.02

    system.time(t2 <- cache_function(2)) # second - read cached value

    ##    user  system elapsed 
    ##       0       0       0

    all(t1 == t2)

    ## [1] TRUE
