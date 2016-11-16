# REndo 
R Package implementing state of the art methods to controll for endogeneity when no instrumental variables are available.

The second version of the pacakge, REndo 1.1, implements four instrument-free methods. These are: the latent instrumental variables method proposed by Ebbes et al. (2005), the higher moments approach proposed by Lewbel (1997) , the joint estimation method using Gaussian copula (Park and Gupta, 2012) and the mixed generalized method of moments proposed by Kim and Frees (2007). The later method can be used to treat endogeneity in multilevel models.

The names of the two functions implemented in the first version of the package have been changed to the following: 

liv()       ->  latentIV()
hmlewbel()  ->  higherMomentsIV()


