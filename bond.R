# Create a vector of interest rates; length n
  y <- c(0.03, 0.03, 0.035)

# Create a function that takes three arguments
  # F = Final payoff
  # C = Coupon payment
  # n = number of time periods til expiry
  
payoff <- function(F, C, n){
  
  # Create an empty vector, coupons:
  coupons <- c()

  # Populate the coupons vector - each element is the value of a single coupon payment at time j:
  for (j in 1:n){
    coupons[j] <- C*exp(-y[j] * j)
  }

  # In the loop above, y[j] is the interest rate at time j
  
  # Sum over all the coupon payments
  coupons_sum <- sum(coupons)

  # Calculate the value of the final payment
  final <- F*exp(-y[n] * y[n])

  # Add the coupon payments to the final payment and return the solution
  return(coupons_sum + final)
}
