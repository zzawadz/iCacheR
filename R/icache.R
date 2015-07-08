ic_cache = function(fun, path2cache = file.path(getwd(),".icacheR"))
{
  force(fun)
  fnName = as.character(substitute(fun))
  repoPath = file.path(path2cache, fnName)
  .ic_create_repo(repoPath)
  cluster = NULL
  function(...)
  {
    .ic_cache_fun(repoPath, fun, ...)
  }
}

.ic_create_repo = function(repoPath)
{
  if(!dir.exists(repoPath)) dir.create(repoPath, recursive = TRUE)
  return(invisible())
}

.ic_cache_fun = function(repoPath, fun, ...)
{
  params = list(...)
  paramsHash = digest(params)
  path = file.path(repoPath, paramsHash)
  if(file.exists(path)) return(readRDS(path))
  result = fun(...)
  saveRDS(result, file = path)
  return(result)
}

ic_lapply = function(x, fun, ..., nodes = 2)
{
  envfun = environment(fun)
  envfun$nodes = nodes
  envfun$cluster = makeCluster(envfun$nodes)

  clusterEvalQ(envfun$cluster, {library(clusterCacher)})

  ncl = length(envfun$cluster)
  args = list(...)

  splX = parallel:::splitList(x, ncl)
  xhash = digest(x)

  fnc_maker = function(fun, xl, args, xhash)
  {
    force(xl)
    force(args)
    force(fun)
    force(xhash)

    function()
    {
      timePath = file.path(environment(fun)$repoPath, xhash)

      for(i in seq_along(xl))
      {
        allArgs = c(list(xl[[i]]), args)
        pp = capture.output(print(lapply(allArgs, class)))

        t = Sys.time()
        do.call(fun, args = allArgs)
        t = round(as.numeric(Sys.time()) - as.numeric(t),2)



        cat(sprintf("%s\n",t), file = paste0(timePath,"_",t,"_",as.character(runif(1)*1e6)))

      }
    }
  }

  for(node in 1:ncl)
  {
    fnc = fnc_maker(fun, splX[[node]], args, xhash)
    parallel:::sendCall(con = envfun$cluster[[node]], fun = fnc, args = list(), return = FALSE)
  }


  xhash = file.path(environment(fun)$repoPath, xhash)
  attr(xhash, "n") = length(x)

  environment(fun)$xhash = xhash
  return(NULL)
}

ic_progress = function(fun)
{
  if(is.null(environment(fun)$xhash))
  {
    warning("There is no running cluster for this function.")
    return(NULL)
  }
  vals = dir(environment(fun)$repoPath)
  time = as.numeric(sapply(strsplit(vals[grep(vals,pattern = "_")],"_"),"[[",2))
  mtime = mean(time)/length(environment(fun)$cluster)
  mtime * (attr(environment(fun)$xhash, "n") - length(time))
}

ic_join = function(fun)
{
  if(is.null(environment(fun)$cluster))
  {
    warning("There is no running cluster for this function.")
    return(NULL)
  }
  parallel:::checkForRemoteErrors(lapply(environment(fun)$cluster, parallel:::recvResult))
  stopCluster(environment(fun)$cluster)
  environment(fun)$cluster = NULL


  timeFiles = dir(environment(fun)$repoPath, full.names = TRUE)
  timeFiles = timeFiles[grep(environment(fun)$xhash, timeFiles)]
  file.remove(timeFiles)

  environment(fun)$xhash = NULL
  return(TRUE)
}
