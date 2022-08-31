# Fast adaptive spectral clustering in R (brain cancer RNA-seq)

## load libraries required for analysis
library(Spectrum)


library(plot3D)
library(survival)
library(survminer)
library(scales)

#### Single-view clustering: Gaussian blobs
## run Spectrum
test1 <- Spectrum(blobs, showpca=TRUE, fontsize=8,dotsize=2)
names(test1)

#### Single-view clustering: Brain cancer RNA-seq
RNAseq <- brain[[1]]
test2 <- Spectrum(RNAseq, fontsize = 8, dotsize = 2)

pca(test2$similarity_matrix,labels=test2$assignments,
    axistextsize=2,legendtextsize = 8,dotsize = 2)

#### Single-view clustering: 
####   Brain cancer RNA-seq clustering a range of K
test3 <- Spectrum(RNAseq,showres=FALSE,runrange=TRUE,krangemax=10)
head(test3[[2]]$assignments)

####  Multi-view clustering: Brain cancer multi-omics
test4 <- Spectrum(brain, fontsize = 8, dotsize = 2)
kernel_pca(test4$similarity_matrix,label=test4$assignments,
           axistextsize=8,legendtextsize=8,dotsize =1.5)



#### Multi-view clustering: 
####   Brain cancer multi-omics with missing data
brain1 <- brain[[1]]
brain2 <- brain[[2]]
brain3 <- brain[[3]]

brain1 <- brain1[,-5:-10]
brain_m <- list(brain1,brain2,brain3)

test5 <- Spectrum(brain_m,missing=TRUE,fontsize =8,dotsize = 2)


#### Single-view clustering: Non-Gaussian data, 3 circles
test6 <- Spectrum(circles,showpca=TRUE,method=2,fontsize=8,
                  dotsize = 2)

#### Single-view clustering: Non-Gaussian data, spirals
test7 <- Spectrum(spirals,showpca=TRUE,method=2,
                  tunekernel=TRUE,fontsize=8,dotsize=2)

#### Ultra-fast single-view clustering: Gaussian blobs II
test8 <- Spectrum(blobs,FASP=TRUE,FASPk=300,fontsize=8,dotsize=2)

names(test8)
head(test8[[1]])

#### Advanced operation: Ng spectral clustering
s <- sigma_finder(blobs)
s1 <- ng_kernel(blobs,sigma = s)
e1 <- estimate_k(s1,showplots = FALSE)
r <- cluster_similarity(s1,k=8, clusteralg = 'GMM')

#### Advanced operation: Customised data integration
s2 <- CNN_kernel(blobs)
s3 <- CNN_kernel(blobs)
klist <- list(s2,s3)
x <- integrate_similarity_matrices(klist)
e2 <- estimate_k(x,showplots = FALSE)
r <- cluster_similarity(x,k=8,clusteralg = 'GMM')


