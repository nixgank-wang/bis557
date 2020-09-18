context("Testing grab coefficients")

library(palmerpenguins)
data(penguins)
fit<- lm(bill_length_mm~.,data=penguins[,-8])
coefs <- round(as.vector(grab_coeffs(fit)))
expect_equal(coefs,c(15,10,6,-1,0,0,0,0,2))
