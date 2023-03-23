# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#  skimReport function                                      #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Copy this entire code into the Console and then hit Enter
# to let R "learn" the function.

skimReport = function(data, round = FALSE){
  require(skimr)
  require(dplyr)
  require(purrr)
  x = skim(data)
  N = nrow(data)
  with(x, {
    skim_type == "factor" -> fac.mask
    vars = skim_variable[fac.mask]
    n.unique = factor.n_unique[fac.mask]
    strsplit(factor.top_counts, "\\, |\\:")[fac.mask] %>%
      purrr::map(function(x){
        as.numeric(x) -> x
        data.frame(category = x[seq(1, length(x), by = 2)],
                   count = x[seq(2, length(x), by = 2)]) %>%
          dplyr::mutate(percent = count/N)
      }) %>%
      {names(.) = vars;.} %>%
      map_df(~as.data.frame(.), .id = "variable")
  }) -> factors
  with(x, {
    skim_type == "numeric" -> num.mask
    data.frame(variable = skim_variable[num.mask],
               mean = numeric.mean[num.mask],
               sd = numeric.sd[num.mask],
               n = N-x$n_missing[num.mask],
               n.missing = n_missing[num.mask],
               perc.missing = n_missing[num.mask]/N)
  }) -> numerics
  if (round == TRUE){
    within(factors, {
      percent = round(percent*100, 2)
    }) -> factors
    within(numerics, {
      mean = round(mean, 2)
      sd = round(sd, 2)
      perc.missing = round(perc.missing*100, 2)
    }) -> numerics
  }
  dat = list(factors = factors,
             numerics = numerics)
  class(dat) = c("list", "skimReport")
  return(dat)
}

print.skimReport = function(x){
  cat("Categorial Variables \n ------------------ \n")
  cat("\n")
  print(x$factors)
  cat("\n")
  cat("Numeric Variables \n ------------------ \n")
  cat("\n")
  print(x$numerics)
}
