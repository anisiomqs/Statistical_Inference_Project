# ```{r, echo=TRUE, message=FALSE, cache=FALSE}
# require(gdata)
require(ggplot2)
# require(lattice)
require(knitr)
# require(plyr)
# require(reshape2)
require(xtable)

set.seed(987654)
opts_chunk$set(echo=TRUE, fig.align='center', message=FALSE, cache=TRUE)
# ```

# ```{r}
lambda <- 0.2
s <- 40
n <- 1000

dist <- matrix(rexp(s*n, rate=lambda), ncol=s, nrow=n)
distMean <- rowMeans(dist)
summary(distMean)
# ```

centerCalculated <- mean(distMean)
centerTheoretical <- 1/lambda

sdTheoretical <- (1/lambda)*(1/sqrt(s)); sdCalculated <- sd(distMean);
varTheoretical <- sdTheoretical^2; varCalculated <- sdCalculated^2;

# pl <- ggplot(data=data.frame(distMean), aes(x=distMean)) +
#   geom_histogram(binwidth=0.25, colour='white') + 
#   ggtitle('Frequency of Distribution Means') + 
#   xlab('Distribution Mean') + 
#   scale_x_continuous(breaks = round(seq(min(distMean), max(distMean), by = 0.5),1)) + 
#   geom_vline(xintercept=centerCalculated, colour='green', size=1) + 
#   ylab('Frequency') +
#   theme(axis.text=element_text(size=12),
#         axis.title=element_text(size=14))
# print(pl)

calcCover <- function(x) {
  offset <-  (1.96 * sqrt(1/lambda**2/s));
  ll <- distMean - offset; ul <- distMean + offset;
  mean(ll < x & ul > x)
}
lambdas <- seq(4.25, 5.75, by=0.01)
cover <- sapply(lambdas, calcCover)
vBounds <- centerCalculated + (c(-1,1) * 1.96 * sdCalculated/sqrt(s))

pl <- ggplot(data.frame(lambdas, cover), aes(x = lambdas, y = cover)) + 
  geom_line(size = 1) + 
  geom_vline(xintercept=vBounds, col="red") +
  geom_hline(yintercept = 0.95, col="blue") + 
  ylim(.85, 1.0)

print(pl)
  