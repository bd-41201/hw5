## actors network example
## Be sure to install the arules package before trying to source the script.

library(igraph)

### GRAPH
## read in a graph in the `graphml' formal: xml for graphs.
## it warns about pre-specified ids, but we want this here
## (these ids match up with the castlists in movies.txt)
actnet <- read.graph("actors.graphml",format="graphml")

### TRANSACTION
## read in the table of actor ids for movies
## this is a bit complex, because the movie names
## contain all sorts of special characters.
movies <- read.table("movies.txt", sep="\t",
	row.names=1, as.is=TRUE, comment.char="", quote="")
## it's a 1 column matrix.  treat it like a vector
movies <- drop(as.matrix(movies))
## each element is a comma-separated set of actor ids.
## use `strsplit' to break these out
movies <- strsplit(movies,",")
## and finally, match ids to names from actnet
casts <- lapply(movies,
	function(m) V(actnet)$name[match(m,V(actnet)$id)])
## check it
casts['True Romance']
## format as arules transaction baskets
library(arules)
casttrans <- as(casts, "transactions")


## Set up STM information
castsize <- unlist(lapply(casts, function(m) length(m)))
## see ?rep.int: we're just repeating movie names for each cast member
acti <- factor(rep.int(names(casts),times=castsize))
## actors
actj <- factor(unlist(casts), levels=V(actnet)$name)
## format as STM (if you specify without `x', its binary 0/1)
actmat <- sparseMatrix(i=as.numeric(acti),j=as.numeric(actj),
		dimnames=list(movie=levels(acti),actor=levels(actj)))

## count the number of appearences by actor
nroles <- colSums(actmat)
names(nroles) <- colnames(actmat)

## Q2 - Plot neighborhoods for Kevin Bacon
# First degree neighbors
kbacon1 <- graph.neighborhood(actnet, 1, V(actnet)["Bacon, Kevin"])[[1]]
plot(kbacon1,  edge.arrow.width=0, edge.curved=FALSE,
     vertex.label=NA, vertex.frame.color=0, vertex.size=6)
length(V(kbacon1))
# ~> [1] 97

# Second degree neighbors
kbacon2 <- graph.neighborhood(actnet, 2, V(actnet)["Bacon, Kevin"])[[1]]
#plot(kbacon1,  edge.arrow.width=0, edge.curved=FALSE,
#     vertex.label=NA, vertex.frame.color=0, vertex.size=6)
length(V(kbacon2))
# ~> [1] 2129

# Third degree neighbors
kbacon3 <- graph.neighborhood(actnet, 3, V(actnet)["Bacon, Kevin"])[[1]]
#plot(kbacon1,  edge.arrow.width=0, edge.curved=FALSE,
#     vertex.label=NA, vertex.frame.color=0, vertex.size=6)
length(V(kbacon3))
V(kbacon3)$color <- "blue"
V(kbacon2)$color <- "green"
V(kbacon1)$color <- "red"

## Q3 - Who are the most common actors
nroles[which.max(nroles)]
nroles.ordered <- nroles[order(-nroles)]
head(nroles.ordered,10)

# Who are the most connected actors
actnet.degree <- degree(actnet)
actnet.degree.ordered <- actnet.degree[order(-actnet.degree)]
head(actnet.degree.ordered,10)

## Q4 - Actor / Cast association rules
castrules <- apriori(casttrans, parameter=list(support=.0001, confidence=.1, maxlen=2))
inspect(castrules)
## Choose any subset you want.
inspect(subset(castrules, subset=support > .001 & confidence > 0.1))

## Q+
y <- actmat[,"Austin, Steve (IV)"]*1
x <- actmat[,"Foley, Mick"]*1
# Run the regression without an intercept to force all of the signal into the beta
regr.equiv <- glm(y~x-1,family="binomial")
exp(coef(regr.equiv))
# ~>        x
# ~> 7.499789
# We need to transform this number a bit in order to compare to the confidence and lift figures

# First, we calculate confidence. We know that exp(beta) is interpreted as the 'odds multiplier'
# and therefore can find the probability multiplier by taking exp(beta)/(1+exp(beta))
regr.equiv.conf <- exp(coef(regr.equiv))/(1+exp(coef(regr.equiv)))
# ~>       x
# ~> 0.88235
# We see that this value matches nicely with the confidence calculated by apriori

# Next, we find the support for Steve Austin which is the base rate at which he appears in the set
# This is easliy found by taking the number of roles over the total number of movies in the set
p.a <- nroles["Austin, Steve (IV)"]/length(movies)
# ~> Austin, Steve (IV)
# ~>         0.00132626

# Last, we calculate the lift which is the confidence in the rule divided by the support for
# Steve Austin

regr.equiv.lift <- regr.equiv.conf/p.a
# ~> x
# ~> 665.2919
# Again, this matches very nicely with the figure from the apriori function
