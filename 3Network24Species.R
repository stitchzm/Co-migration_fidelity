# import data
load("/P24Species.rds")
p <- p24species
p$Locality <- as.integer(p$Locality)
jj = table(p$YEAR,p$SPS)
quantile(rowSums(t(jj)), 0.025)   
# species with fewer than ~697 individuals total are among the rarest 2.5% 

library(dplyr)
options(scipen = 999)
# ABUNDANCE ####
# function to calculate abundance 
ringtomatrix=function(Dbase) {
  library(netdiffuseR)
  BCPD <- aggregate(Dbase$Locality,list(Dbase$SCIENTIFIC_NAME,Dbase$JDays),sum)
  #Assign names to the columns
  colnames(BCPD)[1]<-'Species'
  colnames(BCPD)[2]<-'JDays'
  colnames(BCPD)[3]<-'Individuals'
  #Matching EURING CODE with the species
  BCPD=na.omit(BCPD)#Check for NA
  r<-length(unique(BCPD$JDays))
  c<-length(unique(BCPD$Species))
  names<-(as.data.frame(unique(BCPD$Species)))
  elist<-data.frame(BCPD$JDays,BCPD$Species,BCPD$Individuals)
  raw_matrix <- edgelist_to_adjmat(elist[,1:2],w=elist$BCPD.Individuals)
  M<-t(as.matrix(raw_matrix)[1:r,r+1:c])
  return(M)
}
# Define the range of years you want to process
years <- 2007:2024
# Initialize lists to store results for each year
ponza_abundance_list <- list()
# Calculate for ponza data
for (year in years) {
  # Subset `ponza` for the current year and calculate abundance
  ponza_year <- p %>%
    dplyr::filter(YEAR == year) %>% 
    dplyr::group_by(JDays)
  
  # Calculate abundance matrix for `ponza_year`
  ponza_abundance <- ringtomatrix(ponza_year)
  
  # Save the processed data for the current year
  ponza_abundance_list[[as.character(year)]] <- ponza_abundance
}


# save the list to use then for GAM
save(ponza_abundance_list,
     file = "/ponza_abundance_list.rds")

# SET UP PACKAGES FOR MODULARITY ####
#First check'n'load required packages. In case of missing packages, this________
#function is meant to automatically download and install them___________________
lib <- .libPaths()[1]
required.packages <- c("igraph","vegan","bipartite","smooth","data.table"
                       ,"psych","Hmisc","dplyr","readxl","netdiffuseR","tidyverse"
                       ,"trend","remotes")
i1 <- !(required.packages %in% row.names(installed.packages()))
if(any(i1)) {
  install.packages(required.packages[i1], dependencies = TRUE, lib = lib) 
}
lapply(required.packages, require, character.only = TRUE)

# BUILD THE NETWORK AND CALCULATE MODULARITY ####
# identify those values exceeding the 25th per year, i.e., species with lower numbers
# lag=how the abundance of the previous day may influence the one of the following day 
# cor.matrix with 'Pearson' correlation instead of Spearman correlation
# net.mat=account only for links of correlation > 0.5 
tempNet <- function(x) {
  keep <- rowSums(x) >= quantile(rowSums(x), 0.025)#values exceeding the 2.5th percentile
  purged <- x[keep,]
  data=t(apply(purged,1,function(x) Lag(x,shift=1)))
  cor.matrix=rcorr(as.matrix(t((empty(data)))),type="spearman")
  padj=round(p.adjust(cor.matrix$P,"BH"))
  m <- matrix(padj, nrow = nrow(cor.matrix$r), ncol(cor.matrix$r)) 
  net.mat=(cor.matrix$r>0.5)*(m<=0.05)
  net.mat[is.na(net.mat)]=0
  G=graph_from_adjacency_matrix(as.matrix(net.mat),"undirected",weighted=TRUE)
  Isolated = which(igraph::degree(G)==0)
  g=igraph::delete_vertices(G,Isolated)
  g
}

# network inference from time series abundance data 
set.seed(1234)
Networks=lapply(ponza_abundance_list,tempNet) #apply tempNet function to a list

# set up Modularity function
Q=function(x) {
  Q=cluster_louvain(x, weights = NULL)
  Q.values=Q$modularity[1]
  Q.mod=(data.frame(species=Q$names,Mod=Q$membership))
  return(list(Q=Q.values,Mod=Q.mod))
}
#Calculate Q values for each temporal co-occurrence graph
set.seed(1234)
Modularity_values=data.frame(do.call(rbind.data.frame,(lapply(Networks,function(x) Q(x)[[1]]))))
colnames(Modularity_values)=c("Q")
#Apply a simple moving average on the Q values time series
set.seed(1234)
Modularity.sma=as.data.frame(apply(Modularity_values,2,function(x) sma(x)$fitted))
#Mann Kendall test for monotonic trend analysis
mktest.fun=function(x){
  mk=mk.test(x)
  return(list(z=mk$statistic,p=mk$p.value))
}
set.seed(1234)
MK.test=round(data.frame(do.call(rbind.data.frame,(apply(Modularity.sma,2,mktest.fun)))),4)
MK.test # p = 0.039

# Average and standard deviation of modularity (Q)
mean_Q <- mean(Modularity.sma$Q)
sd_Q <- sd(Modularity.sma$Q)

png("Modularity24.png", width = 3000, height = 2000, units = "px", res = 300)
par(mar = c(6, 6, 4, 2) + 0.1)
years <- 2007:2024
modularity <- Modularity.sma$Q
plot(years, modularity, type = "b",
     xlab = "Years", ylab = expression("Modularity " * italic(Q)),
     cex.axis = 1.5,     
     cex.lab = 1.9,     
     cex.main = 2.5,
     pch = 19,
     xlim = c(2007, 2024),
     xaxt = "n")    
axis(1, at = years, labels = years, cex.axis = 1.3)
dev.off()

# save and export 

#*******************************************************************************
# Build networks of avoided relationships btw species
# net.mat < -0.5 -> avoided/negative relationships, ie., when species are not associated  
# net.mat=ps(net.mat)
tempNet1 <- function(x) {
  keep <- rowSums(x) >= quantile(rowSums(x), 0.025)#values exceeding the 2.5th percentile
  purged <- x[keep,]
  data=t(apply(purged,1,function(x) Lag(x,shift=1)))
  cor.matrix=rcorr(as.matrix(t((empty(data)))),type="spearman")
  padj=round(p.adjust(cor.matrix$P,"BH"))
  m <- matrix(padj, nrow = nrow(cor.matrix$r), ncol(cor.matrix$r)) 
  net.mat=(cor.matrix$r<-0.5)*(m<=0.05)
  net.mat[is.na(net.mat)]=0
  G=graph_from_adjacency_matrix(as.matrix(net.mat),"undirected",weighted=TRUE)
  Isolated = which(igraph::degree(G)==0)
  g=igraph::delete_vertices(G,Isolated)
  g
}
# network inference of avoided relationships (degree of non-association btw species)
set.seed(1234)
Networks1=lapply(ponza_abundance_list,tempNet1) 

# set up Modularity function for avoided relationships 
Q1=function(x) {
  if (is.null(x) || igraph::vcount(x) == 0 || igraph::ecount(x) == 0) {
    return(list(
      Q = NA,
      Mod = data.frame(species = character(0),
                       Mod = numeric(0))))}
  Qc = cluster_louvain(x, weights = NULL)
  Q.values = modularity(Qc)
  # FIX: extract names from graph, not membership
  species_names = igraph::V(x)$name
  # if names missing, create artificial names
  if (is.null(species_names)) {
    species_names = paste0("sp_", seq_along(Qc$membership))
  }
  Q.mod = data.frame(
    species = species_names,
    Mod = Qc$membership)
  return(list(Q = Q.values, Mod = Q.mod))}

# Modularity from network of avoided relationships
#Calculate Q values for each temporal co-occurrence graph
set.seed(1234)
Modularity_values1=data.frame(do.call(rbind.data.frame,lapply(Networks1,function(x) Q1(x)[[1]])))
colnames(Modularity_values1)=c("Q")
#Apply a simple moving average on the Q values time series
set.seed(1234)
Modularity.sma1=as.data.frame(apply(Modularity_values1,2,function(x) sma(x)$fitted))
#Mann Kendall test for monotonic trend analysis
set.seed(1234)
MK.test1=round(data.frame(do.call(rbind.data.frame,(apply(Modularity.sma1,2,mktest.fun)))),4)
MK.test1 # p = 0 -> no monotonic trend in modularity
# modularity of avoided relationships is decreasing over time 
sum(is.na(Modularity_values1$Q)) # 0 
# the trend is not caused by missing netwroks 
# the community is becoming less structured into distinct group
plot(Modularity_values1$Q, type="b")

png("Modularity24_avoidance.png", width = 3000, height = 2000, units = "px", res = 300)
par(mar = c(6, 6, 4, 2) + 0.1)
years <- 2007:2024
modularity1 <- Modularity.sma1$Q
plot(years, modularity1, type = "b",
     xlab = "Years", ylab = expression(Modularity ~ italic(Q) ),
     cex.axis = 1.3,     
     cex.lab = 1.83,     
     cex.main = 2.5,
     pch = 19,
     xlim = c(2007, 2024),
     xaxt = "n")    
axis(1, at = years, labels = years, cex.axis = 1.3)
title(main = "Modularity of species temporal segregation networks", adj = 0.5, line = 0.5, cex.main = 1.8)
dev.off()

# CO-MIGRATION FIDELITY ####
# uses the normalized mutual information (nmi) on modular partition
# First create a species x module-in-years matrix
set.seed(1234)
PF=lapply(Networks, function(x) Q(x)[[2]])
res_PF <- Reduce(function(x, y) merge(x, y, by="species", all=TRUE), PF)
rownames(res_PF)=res_PF$species
res_PF=as.matrix(res_PF[,2:19])
colnames(res_PF)=c(2007:2024)
dist_fun=function(mat){
  mat[is.na(mat)]=-1
  res <-matrix(nrow = nrow(mat), ncol = nrow(mat))
  for (i in (1:nrow(mat))) {
    for (j in (i:nrow(mat))){
      res[i,j] <- igraph::compare(mat[i,],mat[j,],"nmi")
    }
  }
  res[lower.tri(res)] <- t(res)[lower.tri(res)]
  rownames(res)=rownames(mat)
  colnames(res)=rownames(mat)
  res_nmi=(res/max(res))
  diag(res_nmi)=0
  attr(res_nmi, "method") <- "nmi"
  return(res_nmi)
}
#This function measure the pairwise nmi between rows in the res_PF matrix
set.seed(1234)
nmi_mat24=dist_fun((res_PF))
heatmap(nmi_mat24)

# scale color graduation in pheatmap 
pheatmap::pheatmap(
  nmi_mat24,
  clustering_distance_rows = "euclidean",  # Hierarchical clustering for rows
  clustering_distance_cols = "euclidean",  # Hierarchical clustering for columns
  clustering_method = "ward.D2",  # Clustering method for a balanced dendrogram
  border_color = NA,              # No cell borders
  display_numbers = FALSE,        # Hide numbers for a cleaner view
  fontsize_row = 7,              
  fontsize_col = 7,              
  main = "Species Clustering by NMI",  
  angle_col = 45                  
)

# save the matrix as an R object 

# check the similarity btw species 
nmi_mat24["Hippolais icterina", "Muscicapa striata"] # 1
nmi_mat24["Hippolais icterina", "Sylvia borin"] # 1
nmi_mat24["Hippolais icterina", "Oriolus oriolus"] # 1
nmi_mat24["Hippolais icterina", "Curruca communis"] # 1
nmi_mat24["Hippolais icterina", "Acrocephalus schoenobaenus"] # 1
nmi_mat24["Hippolais icterina", "Jynx torquilla"] # 0.8038579
nmi_mat24["Acrocephalus schoenobaenus", "Jynx torquilla"] # 0.8038579
nmi_mat24["Acrocephalus schoenobaenus", "Sylvia borin"] # 1
nmi_mat24["Phoenicurus phoenicurus", "Phylloscopus trochilus"] # 0.7390663
nmi_mat24["Oenanthe oenanthe", "Phylloscopus trochilus"] # 0.657023
nmi_mat24["Phoenicurus phoenicurus", "Oenanthe oenanthe"] # 0.4310304
nmi_mat24["Merops apiaster", "Sylvia borin"] # 0.5119982
nmi_mat24["Lanius senator", "Sylvia borin"] # 0.5879505
nmi_mat24["Ficedula albicollis", "Ficedula hypoleuca"] # 0.5437335
nmi_mat24["Ficedula albicollis", "Phylloscopus sibilatrix"] # 0.4141377
nmi_mat24["Ficedula hypoleuca", "Jynx torquilla"] # 0.4392999
nmi_mat24["Ficedula hypoleuca", "Phylloscopus sibilatrix"] # 0.643538
nmi_mat24["Anthus trivialis", "Oriolus oriolus"] # 0.4033544
nmi_mat24["Anthus trivialis", "Ficedula hypoleuca"] # 0.4729075

nmi_df <- as.data.frame(nmi_mat24)
nmi_df <- tibble::rownames_to_column(nmi_df, var = "Species")
nmi_df[,-1] <- format(round(nmi_df[,-1], 3), nsmall = 3)

# Export as CSV
write.csv(nmi_df, 
          "/nmi_species_similarity.csv", 
          row.names = FALSE)

#****************************************************************************************#
#*                                     CO-MIGRATION FIDELITY
library(dendextend)                                    
# Colorblind-friendly palette
cb_palette1 <- c("#E65700","#7F6F5A","#8CCC9B","#E28B8B","#F9F37E","#8A126C", "#89BDC7","#A9A9F6") 
# Create dissimilarity matrix and hierarchical clustering,
diss_nmi <- 1 - nmi_mat24
hc_nmi <- hclust(as.dist(diss_nmi), method = "average")
dend_nmi <- as.dendrogram(hc_nmi)

# Style the dendrogram
dend_nmi <- color_branches(dend_nmi, k = 8, col = cb_palette1)
dend_nmi <- dendextend::set(dend_nmi, "branches_lwd", 3)
labels_cex(dend_nmi) <- 1.6
cut_height <- 0.52  

png("Dendrogram_CoMigration_Fidelity.png", width = 3000, height = 2800, res = 300)

layout(matrix(c(1, 2), nrow = 1), widths = c(0.8, 0.2))

par(mar = c(5, 10, 4, 18))  # bottom, left, top, right
plot(
  dend_nmi,
  horiz = TRUE, 
  ylab = "Dissimilarity",
  cex.main = 2.5,   
  cex.lab = 2,      
  cex.axis = 1.8)
abline(v = cut_height, lty = 2, lwd = 2, col = "black")
title(main = "Co-migration fidelity", adj = 0, line = 0.5, cex.main = 2.5)
# Legend panel
par(mar = c(0, 0, 0, 0))
plot.new()
legend(
  "left",
  legend = paste("Cluster", 1:8),
  fill = cb_palette1,
  border = "black",
  bty = "n",
  cex = 1.5  # reduce if still cropped
)

dev.off()
