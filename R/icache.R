ic_cache_depr = function(fun, path2cache = getOption("iCacheR.repoPath"))#file.path(getwd(),".icacheR"))
{
  force(fun)
  fnName = as.character(substitute(fun))
  repoPath = NULL
  if(!is.null(path2cache))
  {
    repoPath = file.path(path2cache, fnName)
    .ic_create_repo(repoPath)
  }
  cluster = NULL
  .envCacher = environment()
  function(...)
  {
    .ic_cache_fun(..., .envCacher = .envCacher)
  }
}

ic_cache = function(fun, path2cache = getOption("iCacheR.repoPath"))#file.path(getwd(),".icacheR"))
{
  force(fun)
  fnName = as.character(substitute(fun))
  repoPath = NULL
  if(!is.null(path2cache))
  {
    repoPath = file.path(path2cache, fnName)
    .ic_create_repo(repoPath)
  }
  cluster = NULL
  .envCacher = environment()

  cf_function = function(...) .ic_cache_fun(..., .envCacher = .envCacher)
  paramsList = c(head(as.list(fun), -1))

  namesParams = names(paramsList)
  namesParams = sapply(namesParams, as.name)

  if(any(names(namesParams) == "..."))
  {
    k = which(names(namesParams) == "...")
    names(namesParams)[k] = ""
  }

  body = as.list(cf_function)[[2]]
  body = as.list(body)
  body = as.call(c(body[1], namesParams, body[3]))


  cf_function = as.function(c(paramsList, body))
  cf_function
}

.ic_create_repo = function(repoPath)
{
  if(!file.exists(repoPath)) dir.create(repoPath, recursive = TRUE)
  return(invisible())
}

.ic_cache_fun = function(..., .envCacher)
{
  fun = .envCacher$fun

  if(is.null(.envCacher$repoPath))
  {
    path = getOption("iCacheR.repoPath")
    if(is.null(path)) path = tempdir()
    .envCacher$repoPath = file.path(path, .envCacher$fnName)
    .ic_create_repo(.envCacher$repoPath)
  }
  repoPath = .envCacher$repoPath

  params = list(...)
  params$.FNC = .envCacher$fnName
  paramsHash = digest(params)

  cacheEnv = getOption("iCacheR.cache")

  if( is.environment(cacheEnv) &&
      !is.null(value <- get0(paramsHash, cacheEnv)))
  {
    return(value)
  }

  path = file.path(repoPath, paramsHash)
  if(file.exists(path))
  {
    result = readRDS(path)
  } else
  {
    result = fun(...)
    saveRDS(result, file = path)
  }

  if(is.environment(cacheEnv))
  {
    assign(paramsHash, result, envir = cacheEnv)
  }

  return(result)
}

ic_lapply = function(x, fun, ..., .cluster = NULL, .packages = NULL, .vars = NULL, .varsEnv = .GlobalEnv, .expr = NULL, .nodes = 2)
{
  envfun = environment(fun)
  envfun$nodes = .nodes

  if(is.null(.cluster))
  {
    envfun$cluster = makeCluster(envfun$nodes)
  } else
  {
    envfun$cluster = .cluster
  }

  # export packages
  if(!is.null(.packages))
  {
    evalExpr = paste(sprintf("library(%s)", .packages), collapse = "; ")
    evalExpr = sprintf('function() {%s}', evalExpr)
    evalExpr = parse(text = evalExpr)
    loadLib = eval(evalExpr)
    clusterCall(envfun$cluster, loadLib)
  }

  if(!is.null(.vars))
  {
    clusterExport(cl = envfun$cluster, varlist = .vars, envir = .varsEnv)
  }

  if(!is.null(.expr))
  {
    clusterCall(envfun$cluster, eval, substitute(.expr), env = .GlobalEnv)
  }

  clusterEvalQ(envfun$cluster, {library(iCacheR)})

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
      for(i in seq_along(xl))
      {
        allArgs = c(list(xl[[i]]), args)
        t = Sys.time()
        do.call(fun, args = allArgs)
        t = round(as.numeric(Sys.time()) - as.numeric(t),2)
        timePath = file.path(environment(fun)$repoPath, xhash)
        cat(sprintf("%s\n",t), file = paste0(timePath,"_",t,"_",as.character(runif(1)*1e6)))

      }
      q(save = "no")
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
