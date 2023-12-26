## custom functions
get_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

acc <-  function(pred, val){
  sum(round(pred)==val)/length(pred)
}
