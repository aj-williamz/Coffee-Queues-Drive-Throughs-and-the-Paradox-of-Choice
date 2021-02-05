Purchases <- function(n) {
  sample(x = Prices, size = n, 
         prob = Probs, replace=T)
}
