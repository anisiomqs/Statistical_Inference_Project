# ```{r, echo=TRUE, message=FALSE, cache=FALSE}
# require(gdata)
require(ggplot2)
# require(lattice)
require(knitr)
# require(plyr)
# require(reshape2)
require(xtable)

set.seed(54321)
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

plot <- ggplot(data=data.frame(distMean), aes(x=distMean))
plot <- plot + geom_histogram(binwidth=0.25, colour='white')
plot <- plot + ggtitle('Frequency of Distribution Means')
plot <- plot + xlab('Distribution Mean')
plot <- plot + scale_x_continuous(breaks = round(seq(min(distMean), max(distMean), by = 0.5),1))
plot <- plot + ylab('Frequency')
plot <- plot + geom_vline(xintercept=centerCalculated, colour='green')        
print(plot)
