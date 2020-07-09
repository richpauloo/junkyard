library(imputeTS)
library(ggplot2)

x <- c(rep(NA, 2000), 50, 
       rep(NA, 1000), 65, 
       rep(NA, 10000), 68, 
       rep(NA, 700)
       )

df <- data.frame(t = rep(1:length(x),2), 
                 x = c(x, na_interpolation(x)), 
                 col = c(rep("dtw measurements", length(x)), 
                         rep("linear interp",   length(x)))
                 ) 

ggplot(df, aes(t,x,color=col)) + 
  geom_point() +
  facet_wrap(~col) +
  theme(legend.position = "bottom")
