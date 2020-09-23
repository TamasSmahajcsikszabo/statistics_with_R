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


