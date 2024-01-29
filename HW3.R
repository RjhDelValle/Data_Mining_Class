#Homework 3
#Roberto J. Herrera del Valle
#Problem 1
#We get the dataset
library(bnlearn)
library(Rgraphviz)
data(asia)
#Problem 1
#Part A
First<-subset(asia,select=c(A,T))
#We extract the probabilities
summary(First)
table(First)
#Part B
#We create our logistic regression
mylogit <- glm(T ~ A, data = First, family = "binomial")
summary(mylogit)
#The significance level is below 0.05, therefore they are not associated

#Part C
#We create a Bayesian network
x<-hc(asia)
graphviz.plot(x)
#From my initial plot, I ascertain that T does not depend on A
#In fact A doesnt seem depend on anything.

#Part D: S is indepedent of X when not given the value of E
#Part E: L is independent of T when not given the value of E

#Problem 2
library(splines2)
x0 <- seq.int(0,1,0.001)
knots <- c(0.3, 0.5, 0.6)

density.func <- function (x) {
  x<- c(0.5,x)
  bsMat <-bSpline(x,knots = knots, degree =3, intercept = TRUE,Boundary.knots = c(0,1))
  gamma <- c(1.0077328,1.3602215,1.1510585, 0.5026708,1.3840294, 0.9864255, 0.9046191)
  bsMat[-1,] %*% gamma
}

plot(x0, density.func(x0),type="l")

#Rejection Sampling
sampled <- data.frame(proposal = runif(5000,0,1))
sampled$targetDensity <- density.func(sampled$proposal)
maxDens = max(sampled$targetDensity, na.rm = T)
sampled$accepted = ifelse(runif(5000,0,1) < sampled$targetDensity / maxDens, TRUE, FALSE)
hist(sampled$proposal[sampled$accepted], freq = F, col = "grey", breaks = 50)

#Monte Carlo Markov Chain: Gibbs Sampling
library(MCMCpack)
x<-density.func(x0)
## integration approximation
library(splines2)
bsMat <- bSpline(x, knots = knots, degree = 3, intercept = TRUE,
                 Boundary.knots = c(0, 1))
int.b <- colSums(bsMat) * grid

gamma <- runif(7)
gamma <- gamma/sum(int.b * gamma)


dens <- bsMat %*% gamma
plot(x, bsMat %*% gamma, type = "l")

init <- runif(1)
X.all <- init
current <- init
step.size <- 10000
for(iter in 1:step.size) {
  # transit <- rnorm(1, mean = current, sd = 0.5)
  transit <- runif(1)
  basis.update <- bSpline(c(current, transit), knots = knots, degree = 3, intercept = TRUE,
                          Boundary.knots = c(0, 1))
  
  p.current <- sum(basis.update[1, ] * gamma)
  p.transit <- sum(basis.update[2, ] * gamma)
  
  Q.current.transit <- 1
  Q.transit.current <- 1
  
  Z <- runif(1)
  accept.prob <- min(1, (p.transit * Q.current.transit)/(p.current * Q.transit.current))
  X.all <- c(X.all, current)
  if (Z < accept.prob) {
    current <- transit
  } 
}

length(X.all)/step.size
hist(X.all[5001:10000], freq = FALSE, nclass = 20)
points(x, bsMat %*% gamma, type = "l", col = "blue")
length(X.all)

#Importance Sampling
set.seed(123)
sampled <- data.frame(proposal = runif(5000,0,1))
w <- density.func(sampled$proposal) / dexp(sampled$proposal)
mean(x)
mean(w * x)
mean(w)


#Problem 3 Latent Dirichilet Allocation
#We find our books 
#library(tm) #to process text
#library(topicmodels)
#library(dplyr)
#library(tidytext)
#library(tidyverse)
#library(SnowballC) # for stemming
#library(stringr)
#library(ldatuning)
library(tidyr)
library(stringr)
library(tidytext)
library(gutenbergr)
library(tidyverse)
#devtools::install_github("ropensci/gutenbergr")
#gutenberg_metadata
#FBrown <- gutenberg_works(title == "The Innocence of Father Brown") %>%
#  gutenberg_download(meta_fields = "title")
#BC <- gutenberg_works(title == "The Ball and the Cross") %>%
#  gutenberg_download(meta_fields = "title")
#Ulysses <- gutenberg_works(title == "Ulysses") %>%
#  gutenberg_download(meta_fields = "title")

#titles <- "A Connecticut Yankee in King Arthur's Court"
#titles <- "Don Quixote"
titles <- "The Jungle"
#titles <- "Around the World in Eighty Days"



library(gutenbergr)

books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title")
books[1:4, ]

by_chapter <- books %>%
  group_by(title) %>%
  mutate(chapter = cumsum(str_detect(
    text, regex("^chapter ", ignore_case = TRUE)
  ))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(document, title, chapter)

by_chapter[1:4, ]

library(tm)

by_chapter_word <- by_chapter %>%
  unnest_tokens(word, text)

by_chapter_word[1:4, ]
stop_words[1:4, ]

word_counts <- by_chapter_word %>%
  anti_join(stop_words) %>%
  count(document, word, sort = TRUE) %>%
  ungroup()
word_counts

chapters_dtm <- word_counts %>%
  cast_dtm(document, word, n)

inspect(chapters_dtm[1:5,1:10])

chapters_lda <- topicmodels::LDA(chapters_dtm, k = 10, control = list(seed = 1234))
chapters_lda

chapter_topics <- tidy(chapters_lda, matrix = "beta")
chapter_topics

top_terms <- chapter_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 5) %>% 
  ungroup() %>%
  arrange(topic, -beta)

top_terms

library(ggplot2)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

chapters_gamma <- tidy(chapters_lda, matrix = "gamma")
chapters_gamma[chapters_gamma$document == "Great Expectations_57", ]

chapters_gamma <- chapters_gamma %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE)

chapters_gamma

chapters_gamma %>%
  mutate(title = reorder(title, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ title) +
  labs(x = "topic", y = expression(gamma))

