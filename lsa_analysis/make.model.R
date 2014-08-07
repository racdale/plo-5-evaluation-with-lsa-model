library(Matrix)
library(irlba)
library(tm)

cosine_similarity_matrix = function(ix) {
  A = X[ix[1],]
  B = X[ix[2],]
  return(sum(A*B)/sqrt(sum(A^2)*sum(B^2)))
}   
# beautiful code to build dissimilarity matrix, using cos.sim above:
# http://stats.stackexchange.com/questions/31565/is-there-an-r-function-that-will-compute-the-cosine-dissimilarity-matrix

setwd('/Users/rickdale/Dropbox/duties/ucmservice/CISgradchair/SelfAssessments/2014/lsa_analysis')
a = read.table('abstracts.txt',sep='\n',quote = "")
colnames(a) = list('abs')
a$abs = as.character(a$abs)

abs = a$abs
studs = c(483,620,260,670)
# kerster: 483
# szary: 620
# st. clair: 260
# winter: 670

ts = Corpus(VectorSource(abs)) # using tm to strip / clean
ts = tm_map(ts, removeWords, stopwords("english"))
removepunct = function(x) { return(gsub("[[:punct:]]","",x)) }
ts = tm_map(ts, removepunct)
removenum = function(x) { return(gsub("[0-9]","",x)) }
ts = tm_map(ts, removenum)
doublespace = function(x) { return(gsub("  "," ",x)) }
ts = tm_map(ts, doublespace)

abs = PlainTextDocument(ts) # make it plain

chunk = paste(abs,collapse=" ") # cheap plural removal
words = unique(unlist(strsplit(doublespace(chunk),split=" ")))
pukeplural = function(x) { 
  return(gsub(search_term,allstring_minus_last,x))
}
for (w in words) {
  search_term <- w
  lastletter <- substr(w,nchar(w),nchar(w))
  allstring_minus_last <- substr(w,1,nchar(w)-1)
  if (lastletter=="s" & allstring_minus_last %in% words & nchar(w)>3) {    
    abs = lapply(abs, pukeplural)
  }  
}

ts = Corpus(VectorSource(abs)) 
tXd = DocumentTermMatrix(ts) 

# convert to sparse matrix
tXd.mat = sparseMatrix(tXd$i,tXd$j,x=tXd$v) # removes col's with 0's... need to fix
save(tXd.mat,tXd,file="tXd.Rdata")
#load('tXd.Rdata')

# tXd.mat too big for non-sparse SVD; use irlba
S = irlba(tXd.mat,nu=15,nv=15) # very slow beyond 30 or so
X = S$u

# let's compare ALL PAIRS of abstracts
n = nrow(X)
cmb = expand.grid(i=1:n, j=1:n) 
allpairs = matrix(apply(cmb,1,cosine_similarity_matrix),n,n) # get all pairwise cosine similarity measures
hist(allpairs[allpairs<1],100,main='Distribution of topical similarity at CogSci 2013',ylab='Density',xlab='Similarity score between a pair of abstracts (cosine)')

# compare just the students
cmb = expand.grid(i=studs, j=studs) 
compare_studs = matrix(apply(cmb,1,cosine_similarity_matrix),length(studs),length(studs))
compare_studs = compare_studs[compare_studs<1]
points(mean(compare_studs),20000,pch=15)
points(c(mean(compare_studs)-sd(compare_studs),mean(compare_studs)+sd(compare_studs)),c(20000,20000),type='l',lwd=2)

# let's plot the range of the *maximal* fit
for (i in studs) {
  maxi = which.max(allpairs[i,])
  ts[maxi]
}




