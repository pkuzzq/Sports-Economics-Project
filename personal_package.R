############################################################################## #
### The Giancarlo Arcese personal package, trademark pending
### gtarcese@gmail.com -> 10/05/2022
############################################################################## #

theme_set(theme_bw())
options(scipen=999)

# General ----------------------------------------------------------------------

# `?` <- function(x, y) {
#   xs <- as.list(substitute(x))
#   if (xs[[1]] == as.name("<-")) x <- eval(xs[[3]])
#   r <- eval(sapply(strsplit(deparse(substitute(y)), ":"), function(e) parse(text = e))[[2 - as.logical(x)]])
#   if (xs[[1]] == as.name("<-")) {
#     xs[[3]] <- r
#     eval.parent(as.call(xs))
#   } else {
#     r
#   }
# }       

# assign to environment and update namespace
assign_to_namespace <- function(.name, .obj, .env) {
  envName = as.character(match.call()$.env)
  assign(.name, .obj, envir = .env)
  if (envName %in% search()) {
    detach(envName, character.only = T)
    attach(.env, name = envName)
  }
  else {
    attach(.env, name = envName)
  }
}
# attaches personal package
my_attach <- function(.env) {
  envName = as.character(match.call()$.env)
  if (envName %in% search()) {
    detach(envName, character.only = T)
    attach(.env, name = envName)
  }
  else {
    attach(.env, name = envName)
  }
  rm(list = envName, envir = .GlobalEnv)
}
# attaches any other .R file in one line instead of two
attach_source <- function(.src, .envName) {
  .env <- new.env()
  source(.src, local = .env)
  if (.envName %in% search()) {
    detach(.envName, character.only = T)
    attach(.env, name = .envName)
  }
  else {
    attach(.env, name = .envName)
  }
}
# returns vector of ordered weights of consecutive entries satisfy a condition
cond_weights <- function(.vec, .fun) {
  ids <- which(.fun(.vec))
  weights <- NULL
  i <- 0
  while (i < length(ids)) {
    if (i == length(ids) - 1) {
      weights <- c(weights, 1)
      i <- i + 1
    }
    else {
      j <- 1
      while (ids[i+j] == ids[i+j+1] - 1 & j < length(ids)) {
        j <- j + 1
      }
      weights <- c(weights, j)
      i <- i + j
    }
  }
  weights
}
# return day from date object as character with leading 0s.
day0 <- function(.date) {
  output <- day(.date) %>% as.character()
  if (str_length(output) == 1) paste0("0", output)
  else output
}
# fill numeric vector NA values with smooth averages
fill_average <- function(.vec) {
  if (all(is.na(.vec))) { # county 06003, is literally all 0s. so if you get c(NA, NA, ...) just return a vec of 0s.
    rep(0, length = length(.vec))
  }
  else {
    filledVec <- .vec
    minNumId <- which(!is.na(filledVec)) %>% min()
    maxNumId <- which(!is.na(filledVec)) %>% max()
    for (i in 1:length(filledVec)) {
      if (is.na(filledVec[i]) & i > minNumId & i < maxNumId) {
        lagDist <- length(head(filledVec, i-1)) - max(which(!is.na(head(filledVec, i-1)))) + 1
        lagValue <- head(filledVec, i-1)[max(which(!is.na(head(filledVec, i-1))))]
        leadDist <- min(which(!is.na(tail(filledVec, -i))))
        leadValue <- tail(filledVec, -i)[leadDist]
        filledVec[i] <- seq(lagValue, leadValue, length = (lagDist + leadDist + 1))[lagDist + 1]
      }
    }
    replace(filledVec, is.na(filledVec), 0) # replace any existing NAs on the boundaries with 0.
  }
}
# mass load all csvs in a directory
mass_load <- function(.src, .pos, .bind = F) {
  files <- list.files(path = .src, pattern = "*.csv")
  if (.bind) {
    dat.binded <- NULL
    for (file in files) {
      dat.binded <- bind_rows(dat.binded, read_csv(paste0(.src, file)))
    }
    dat.binded
  }
  else {
    for (file in files) {
      assign(str_sub(file, 1L, -5L), read_csv(paste0(.src, file)), pos = .pos)
    }
  }
}
# mass write data as csv
mass_write <- function(.path, .data_names) {
  for (.dat in .data_names) {
    if (exists(.dat )) {
      write_csv(get(.dat ), paste0(.path, .dat, ".csv"))
      rm(list = .dat, envir = .GlobalEnv)
    }
  }
}
# return month from date object as character with leading 0s.
month0 <- function(.date) {
  output <- month(.date) %>% as.character()
  if (str_length(output) == 1) paste0("0", output)
  else output
}
# compress a vector
my_compress <- function(.vec, .fun = function(x) T) {
  newVec <- as.list(.vec)
  for (i in length(newVec):2) {
    condition <- if (T) identical(newVec[i], newVec[i-1]) & .fun(newVec[i-1])
    if (condition) {
      newVec[i] <- "lmao broski"
    }
  } 
  newVec[which(newVec != "lmao broski")] %>%
    as_vector()
}
# return object name as c haracter
obj_name <- function(.obj) {
  deparse(match.call()$.obj)
}
# Population Standard Deviation
psd <- function(.vec) {
  sqrt(mean((.vec - mean(.vec))^2))
} 
# Population Variance
pvar <- function(.vec) {
  sum((.vec - mean(.vec))^2)/length(.vec)
}
# contains operator
`%includes%` <- function(.vec, .string) {
  str_detect(.vec, .string)
}
# negates "includes" operator
`%excludes%` <- Negate(`%includes%`) 
# negate %in% operator
`%notin%` <- Negate(`%in%`) 
# expands indices that satisfy a condition provided weights
weighted_expand <- function(.vec, .weights, .indices) {
  expandedVec <- .vec
  indexAdjust <- 0
  numNew <- 1
  for (i in .indices) {
    expandedVec <- append(expandedVec, rep(expandedVec[i + indexAdjust], .weights[numNew]-1),
                          after = i + indexAdjust)
    indexAdjust <- sum((.weights - 1)[1:numNew])
    numNew <- numNew + 1
  }
  expandedVec
}
# Get summary stats of var
summary_stats <- function(.data, ..., yes_median = F) {
  out <- NULL
  expr <- expr(c(...))
  pos <- eval_select(expr, data = .data)
  count <- 1
  for (var in names(pos)) {
    if (count == 1){
      out <- out %>% 
        bind_cols(
          .data %>% 
            summarize(
              mean = mean(.data[[var]]),
              median = median(.data[[var]]),
              min = min(.data[[var]]),
              max = max(.data[[var]]),
              psd = psd(.data[[var]]),
              pvar = pvar(.data[[var]])
            ) %>% 
            rename_with(~paste(var, .x, sep = "_"), contains(c("mean", "median", "min", "max", "psd", "pvar")))
        )
      count <- count + 1
    }
    else {
      out <- out %>% 
        bind_cols(
          .data %>% 
            summarize(
              mean = mean(.data[[var]]),
              median = median(.data[[var]]),
              min = min(.data[[var]]),
              max = max(.data[[var]]),
              psd = psd(.data[[var]]),
              pvar = pvar(.data[[var]])
            ) %>% 
            select(2:ncol(.)) %>% 
            rename_with(~paste(var, .x, sep = "_"), contains(c("mean", "median", "min", "max", "psd", "pvar")))
        )
    }
  }
  if (!yes_median) {
    out <- out %>% 
      select(!contains("median"))
  }
  out 
}

# Old Stuff --------------------------------------------------------------------

# fill NA values smoothly based on nearest non-NA values
old_fill_average <- function(.vec, .groupNAs = F) {
  newVec <- if (.groupNAs) my_compress(.vec, function(x) is.na(x)) else .vec
  filledVec <- newVec
  for (i in 1:length(newVec)) {
    if (is.na(newVec[i]) & i %notin% c(1, length(newVec))) {
      lagDist <- length(head(newVec, i-1)) - max(which(!is.na(head(newVec, i-1)))) + 1
      lagValue <- head(newVec, i-1)[max(which(!is.na(head(newVec, i-1))))]
      leadDist <- min(which(!is.na(tail(newVec, -i))))
      leadValue <- tail(newVec, -i)[leadDist]
      filledVec[i] <- seq(lagValue, leadValue, length = (lagDist + leadDist + 1))[lagDist + 1]
    }
  }
  if (.groupNAs) weighted_expand(filledVec, 
                                 cond_weights(.vec, function(x) is.na(x)), 
                                 which(is.na(my_compress(.vec, function(x) is.na(x)))))
  else filledVec
}


 






