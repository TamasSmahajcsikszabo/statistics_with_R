# Jones 2018
# network psychometrics
# constructs such as mental disorders as emerging from the interactions of their symptoms
# the concepct extended to personality:
# traits and attitutes are better represented as properties of complex networks than as latent variables

# misunderstandings/misinterpretation of networks:
# 1. spatial positions are not necessarily reflecting statistical associations
# 2. spatial rendering not directly relates to X and Y pane positions in a meaningful way
# 3. central position is not necessarily signifies centrality
# 4. different networks from previous study do not necessary mean failed research:
# visually different networks might have similar statistical structure

library(tibble)
library(ggplot2)
library(qgraph)
library(MPsychoR)
data(Rogers)
data(Rogers_Adolescent)

# colnames are set to preserve space in network visualization
colnames(Rogers) <- colnames(Rogers_Adolescent) <- 1:26

# Fruchterman and Reingold - FR
# network ~ physical system of balls connected by elastic strings
#The aim of force-directed algorithms is to provide
#aesthetically pleasing graphs by minimizing the number of
#crossing edges and by positioning nodes so that edges have
#approximately equal length

# FR spatial interpretation is misleading! and not meant to be so
adult_zeroorder <- cor(Rogers)
qgraph(adult_zeroorder, layout = "spring",
       groups = list(
                     Depression = 1:16,
                     OCD = 17:26),
                     color = c("lightblue", "lightsalmon")
                     )
# the positioning of nodes in a force-directed algorithm cannot be
# interpreted

#multidimensional scaling of networks (MDS)
# proximity is an umbrella term for similarities and dissimilarities between variables,
# such as correlation or Euclidean distance
# MDS represents proximities between variables as distances between points in a low-dimensional space
# MDS helps understanding networks as it represents proxemities in Euclidean distances
# i.e. related nodes will be closer to each other
# MDS considers a matrix of proximities between objects
# proximities can be observed or derived
# psychometric networks provide edges - derived proxemities: normally zero order or partial correlations

#an example with zero order correlations as weight matrix
adult_zeroorder <- cor(Rogers)
library(smacof) # it needs dissimilarities, so correlations need to be transformed
dissimilarity_matrix <- sim2diss(adult_zeroorder)
adult_mds <- mds(dissimilarity_matrix)
plot_data <- tibble(data.frame(adult_mds$conf))

ggplot(plot_data)+
    geom_point(aes(D1, D2), size = 10)

# multiple transformations options are available
# ordinal, interval, ratio or spline, among others
# in psuchometric networks, a parsimonous transformation is needed with good GoF
# A value known as stress indicates how well one’s data can be
# represented in two-dimensions

adult_mds_spline  <- mds(dissimilarity_matrix, type= "mspline")
plot(adult_mds_spline, plot.type="Shepard", main = "Spline")
adult_mds_spline$stress

qgraph(adult_zeroorder,
        layout= adult_mds_spline$conf,
        groups=list(Depression=1:16,
                    OCD=17:26),
       color = c("lightblue", "lightsalmon",
       vsize = 4)
)

# to get rid of the overlapping nodes:
library(wordcloud)
qgraph(adult_zeroorder,
        layout= adult_mds_spline$conf,
        groups=list(Depression=1:16,
                    OCD=17:26),
       color = c("lightblue", "lightsalmon"),
       vsize = 0, rescale = FALSE, labels = FALSE)
points(adult_mds_spline$conf, pch=16)
textplot(
         adult_mds_spline$conf[,1]+.03,
         adult_mds_spline$conf[,2]+.03,
         colnames(adult_zeroorder),
         new=FALSE)

#graohical Lasso
adult_glasso <- EBICglasso(cor(Rogers), n = 408)

#it's a mixed network, where edge thickness is related to LASSO partial regularized correlations, but spacial position relates to zero order correlation
qgraph(adult_glasso,
        layout= adult_mds_spline$conf,
        groups=list(Depression=1:16,
                    OCD=17:26),
       color = c("lightblue", "lightsalmon",
       vsize = 4)
)

#Procrustes
# accurate visual comparison between two networks
# it brings the two networks in one space
# it removes statistically meaningless differences

adolescent_zeroorder <- cor(Rogers_Adolescent)
dissimilarity_adolescent <- sim2diss(adolescent_zeroorder)
adolescent_MDS <- mds(dissimilarity_adolescent, type="mspline")
fit_procrustes <- Procrustes(adult_mds_spline$conf, adolescent_MDS$conf)
adolescent_glasso <- EBICglasso(cor(Rogers_Adolescent), n=87, gamma=0)
qgraph(adult_glasso, layout=fit_procrustes$X,groups = list(
                                                           Depression = 1:16,
                                                           "OCD" = 17:26),
       color = c("lightblue", "lightsalmon"), 
       title="Adults, n=408", vsize=4)
text(-1,-1, paste("Stress=",
round(adult_mds_spline$stress,2)))
qgraph(adolescent_glasso,
       layout=fit_procrustes$Yhat,
       groups = list(Depression = 1:16,
                     "OCD" = 17:26),
       color = c("lightblue", "lightsalmon"),
       title="Adolescents, n=87", vsize=4)
text(-1,-1, paste("Stress=",
round(adolescent_MDS$stress,2)))

# the congruence coefficient can be used to quantify the degree MDS replicates between two networks
# congruence = measure of similarity of two configurations; measured around the centroids
# it's ideal for similarity measures of geometric similarity

## PCA and eigenmodels
# plotting nodes in a coordinate system based upon extracted dimensions (by loadings on these features)
# while MDS helps meaningful plotting, PCA helps visualize nodes on two features
# X or Y distances are meaningful, but not the Euclidean distances
# features represent agregations of variance in the data
# even if the features are not interpretable, it's useful to plot py them, as they are aggregations of variance
# information loss

# PCA:
# method 1: singular value decomposition
# method 2: eigenvalue decomposition of covariance (or correlation) matrix
# % variance accounted for components can help to qualify how well a two dimensional solution is appropriate for the network, just like stress for MDS

library(psych)
PCA_adult <- principal(cor(Rogers), nfactors = 2)

qgraph(adult_glasso,
        layout=PCA_adult$loadings,
        groups=list(Depression=1:16,
                    OCD=17:26),
       color = c("lightblue", "lightsalmon",
       vsize = 4)
)

# eigenmodel networks:
# also useful for meaningful plotting in a space
# solely based on the weight matrix (i.e. edges)
library(eigenmodel)

# need NA diagonals
diag(adult_glasso) <- NA
p <- 2
#estimates with Markov chain Monte Carlo (MCMC)
fitEM <- eigenmodel_mcmc(
                         Y = adult_glasso,
                         R = p,
                         S = 1000,
                         burn = 100,
                         seed = 123
)

EVD <- eigen(fitEM$ULU_postmean)
evecs <- EVD$vec[, 1:p] # eigenvectors, taken as a 2-dimensional solution

qgraph(adult_glasso,
        layout=evecs,
        groups=list(Depression=1:16,
                    OCD=17:26),
       color = c("lightblue", "lightsalmon",
       vsize = 4)
)


## comparison
# FR networks are aesthetic, less interpretable
# MDS interpretable layout, interpretability can be quantified with the stress value: 
# higher stress ~ less interpretability
# PCA: interpretable placement


