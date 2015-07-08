library(iCacheR)

ff = function(i)
{
  rnorm(1000)
}

cf = ic_cache(ff)

xlist = as.list(as.numeric(1:100))

ic_lapply(xlist, cf, nodes = 2)

ic_progress(cf)

ic_join(cf)




