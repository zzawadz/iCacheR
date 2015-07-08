library(iCacheR)

ff = function(i)
{
  rnorm(100)
}

cf = ic_cache(ff, nodes = 4)

xlist = as.list(as.numeric(1:1000))

ic_lapply(xlist, cf)

ic_progress(cf)

ic_join(cf)




