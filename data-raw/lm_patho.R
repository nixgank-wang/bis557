## code to prepare `lm_patho` dataset goes here

lm_patho<- read.csv("/Users/kangxinwang/Desktop/homework1/lm_patho.csv")
usethis::use_data(lm_patho, overwrite = TRUE)
