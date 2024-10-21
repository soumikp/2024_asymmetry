affine_unif <- function(x){
  (x - min(x))/(max(x) - min(x))
}

affine_norm <- function(x){
  (x - mean(x))/sd(x)
}

op_unif <- replicate(250, 
          {
            x <- runif(250)
            y <- x^2
            rmi::lnn_entropy(affine_unif(x)) - rmi::lnn_entropy(affine_unif(y))
          })

summary(op_unif)
plot(density((op_unif - mean(op_unif))/sd(op_unif)))
qqnorm((op_unif - mean(op_unif))/sd(op_unif))
qqline((op_unif - mean(op_unif))/sd(op_unif))

op_norm <- replicate(250, 
                     {
                       x <- rnorm(250)
                       y <- x^2
                       rmi::lnn_entropy(affine_norm(x)) - rmi::lnn_entropy(affine_norm(y))
                     })

summary(op_norm)
plot(density((op_norm - mean(op_norm))/sd(op_norm)))
qqnorm((op_norm - mean(op_norm))/sd(op_norm))
qqline((op_norm - mean(op_norm))/sd(op_norm))

