#!/usr/bin/env Rscript
local({
  env <- new.env(parent = parent.env(topenv()))
  for (p in dir("R", full.names = TRUE)) {
    sys.source(p, env)
  }
  attach(env, name = "gs2yt")
})

if (!interactive()) {
  main()
}
