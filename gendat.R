#######################################
# Generate df and matrices for gen.dat
#######################################

library(ape)
library(Matrix)

#objects needed to run Henrik's code
#load("gen.dat") ## R object for genetic data called gen.dat that consists of a list of four sublists, one for each serotype. Each sublist has the following components:
# $N 		- How many sequences of that serotype there are
# $dmat 	- An N x N matrix  - each cell represents the spatial distance between case i and case j (where there are n sequences available for that serotype) (diagonal is NA)
# $tmat  	- An N x N matrix - each cell represents the time (in days) between case i and case j (diagonal is NA)
# $gmat  	- An N x N matrix - each cell represents the total evolutionary time (in days) between that pair of viruses (diagonal is NA)
# $ttotmrca  	- An N x N matrix - each cell represents the time (in days) to the most recent common ancestor for the earlier of the two cases (diagonal is NA)
# dat 		- A dataframe with N rows comprising:
# $date 		- Date (in decimal years - e.g., 2000.09 for 1 Feb 2000)
# $serotype 	- The serotype of each case
# $X 			- X coordinate of each case (UTM)
# $Y 			- Y coordinate of each case (UTM)

#trees
d1t<-read.nexus("C:/Users/rache/OneDrive/Desktop/BEAST v1.10.4/cluster output/D1/d1_10.nexus")
d2t<-read.nexus("C:/Users/rache/OneDrive/Desktop/BEAST v1.10.4/cluster output/D2/d2_10c.nexus")
d3t<-read.nexus("C:/Users/rache/OneDrive/Desktop/BEAST v1.10.4/cluster output/d3_nolr4pc.tree.nexus")
d4t<-read.nexus("C:/Users/rache/OneDrive/Desktop/BEAST v1.10.4/cluster output/D4/d4_150.nexus")

################################################################################calculate N
N1<-length(d1t$tip.label)
N2<-length(d2t$tip.label)
N3<-length(d3t$tip.label)
N4<-length(d4t$tip.label)

################################################################spatial distance as 0 (dmat)
dmat1<-matrix(rep(0,N1*N1),ncol=N1)
dmat2<-matrix(rep(0,N2*N2),ncol=N2)
dmat3<-matrix(rep(0,N3*N3),ncol=N3)
dmat4<-matrix(rep(0,N4*N4),ncol=N4)

#set diagonals to NA
diag(dmat1)<-NA
diag(dmat2)<-NA
diag(dmat3)<-NA
diag(dmat4)<-NA

############################################################################get dates for tmat
dd1<-as.data.frame(dist1,row.names=rownames(dist1))
rd1<-rownames(dd1)
r1<-sub("_.*","",rd1)
r1n<-as.numeric(substr(r1,2,nchar(r1)))
r1n[54]<-2013

#matrix of differences (row minus column)
c1<-outer(r1n,r1n, '-')
rownames(c1)<-rownames(dist1)
colnames(c1)<-colnames(dist1)

dd2<-as.data.frame(dist2,row.names=rownames(dist2))
rd2<-rownames(dd2)
r2<-sub("_.*","",rd2)
r2n<-as.numeric(substr(r2,2,nchar(r2)))
r2n[230]<-2004

#matrix of differences (row minus column)
c2<-outer(r2n,r2n, '-')
rownames(c2)<-rownames(dist2)
colnames(c2)<-colnames(dist2)

dd3<-as.data.frame(dist3,row.names=rownames(dist3))
rd3<-rownames(dd3)
r3<-sub("_.*","",rd3)
r3n<-as.numeric(substr(r3,2,nchar(r3)))
r3n[137]<-2013

#matrix of differences (row minus column)
c3<-outer(r3n,r3n, '-')
rownames(c3)<-rownames(dist3)
colnames(c3)<-colnames(dist3)

dd4<-as.data.frame(dist4,row.names=rownames(dist4))
rd4<-rownames(dd4)
r4<-sub("_.*","",rd4)
r4n<-as.numeric(substr(r4,2,nchar(r4)))
r4n[127]<-2004

#matrix of differences (row minus column)
c4<-outer(r4n,r4n, '-')
rownames(c4)<-rownames(dist4)
colnames(c4)<-colnames(dist4)

#convert to days
tmat1<-c1*365
tmat2<-c2*365
tmat3<-c3*365
tmat4<-c4*365

#set diagonals to NA
diag(tmat1)<-NA
diag(tmat2)<-NA
diag(tmat3)<-NA
diag(tmat4)<-NA

################################################pairwise distances between tips of tree (gmat)
dist1<-cophenetic.phylo(d1t)
dist2<-cophenetic.phylo(d2t)
dist3<-cophenetic.phylo(d3t)
dist4<-cophenetic.phylo(d4t)

#convert to days
gmat1<-dist1*365
gmat2<-dist2*365
gmat3<-dist3*365
gmat4<-dist4*365

#set diagonals to NA
diag(gmat1)<-NA
diag(gmat2)<-NA
diag(gmat3)<-NA
diag(gmat4)<-NA

####################################################################ttomrca matrix
#mrca for all tips
anc1<-mrca(d1t)

#matrix of time difference between pairs; keep negatives as indicator of row being
#earlier of pair
c1r<-c1
c1r[c1r>0]<-NA
diag(c1r)<-NA 
c1r[!is.na(c1r)]<-0

a1<-anc1+c1r

dn1<-dist.nodes(d1t)

m1<-matrix(mapply(function(x, y) {
    dn1[x,y]}, 
    rep(1:N1,N1), c(a1)),nrow=N1)

m1m<-t(m1)

#flag matrices
m1i<-m1
m1i[m1i>=0]<-0

m1mi<-m1m
m1mi[m1mi>=0]<-0

m1dups<-m1i+m1mi
m1dups[is.na(m1dups)]<-1
m1dups[m1dups==0]<-NA
m1dups[m1dups==1]<-0

m1mf<-m1m+m1dups

m1z<-m1
m1z[is.na(m1z)]<-0
m1mfz<-m1mf
m1mfz[is.na(m1mfz)]<-0
m1f<-m1z+m1mfz

ttotmrca1<-m1f*365

#set diag to NA
diag(ttotmrca1)<-NA

##########################################mrca for all tips D2
anc2<-mrca(d2t)

#matrix of time difference between pairs; keep negatives as indicator of row being
#earlier of pair
c2r<-c2
c2r[c2r>0]<-NA
diag(c2r)<-NA #set self pair to zero
c2r[!is.na(c2r)]<-0

a2<-anc2+c2r

dn2<-dist.nodes(d2t)

m2<-matrix(mapply(function(x, y) {
    dn2[x,y]}, 
    rep(1:N2,N2), c(a2)),nrow=N2)

m2m<-t(m2)

#flag matrices
m2i<-m2
m2i[m2i>=0]<-0

m2mi<-m2m
m2mi[m2mi>=0]<-0

m2dups<-m2i+m2mi
m2dups[is.na(m2dups)]<-1
m2dups[m2dups==0]<-NA
m2dups[m2dups==1]<-0

m2mf<-m2m+m2dups

m2z<-m2
m2z[is.na(m2z)]<-0
m2mfz<-m2mf
m2mfz[is.na(m2mfz)]<-0
m2f<-m2z+m2mfz

ttotmrca2<-m2f*365

#set diag to NA
diag(ttotmrca2)<-NA

##########################################mrca for all tips D3
anc3<-mrca(d3t)

#matrix of time difference between pairs; keep negatives as indicator of row being
#earlier of pair
c3r<-c3
c3r[c3r>0]<-NA
diag(c3r)<-NA #set self pair to zero

a3<-anc3+c3r

dn3<-dist.nodes(d3t)

m3<-matrix(mapply(function(x, y) {
    dn3[x,y]}, 
    rep(1:N3,N3), c(a3)),nrow=N3)

m3m<-t(m3)

#flag matrices
m3i<-m3
m3i[m3i>=0]<-0

m3mi<-m3m
m3mi[m3mi>=0]<-0

m3dups<-m3i+m3mi
m3dups[is.na(m3dups)]<-1
m3dups[m3dups==0]<-NA
m3dups[m3dups==1]<-0

m3mf<-m3m+m3dups

m3z<-m3
m3z[is.na(m3z)]<-0
m3mfz<-m3mf
m3mfz[is.na(m3mfz)]<-0
m3f<-m3z+m3mfz

ttotmrca3<-m3f*365

#set diag to NA
diag(ttotmrca3)<-NA

##########################################mrca for all tips D4
anc4<-mrca(d4t)

#matrix of time difference between pairs; keep negatives as indicator of row being
#earlier of pair
c4r<-c4
c4r[c4r>0]<-NA
diag(c4r)<-NA 
c4r[!is.na(c4r)]<-0

a4<-anc4+c4r

#convert matrix to df (values go down columns)
#c4v<-data.frame(node=c(c4r),tipno=rep(seq(1:N4),N4),earlier=rep(rownames(c4r),N4),
#                pair=rep(colnames(c4r),each=N4))
#c4c<-c4v[complete.cases(c4v),]#eliminate rows with NA
#a4v<-data.frame(node=c(a4),tipno=rep(seq(1:N4),N4),earlier=rep(rownames(a4),N4),
#                pair=rep(colnames(a4),each=N4))

#steps to eliminate duplicates from same-time pairings
#c4c$ear<-gsub("'","",c4c$earlier)
#c4c$mate<-gsub("'","",c4c$pair)
#c4c$eary<-as.numeric(sub("_.*","",c4c$ear))
#c4c$maty<-as.numeric(sub("_.*","",c4c$mate))
#c4c2<-subset(c4c,c4c$eary!=c4c$maty)#158685

#values with duplicated pairs (pairs with same date)
#c4m<-subset(c4c,c4c$eary==c4c$maty)#162
#restrict to unique pairs only
#c4m3<-c4m[,5:6]
#c4m4<-c4m3[!duplicated(t(apply(c4m3, 1, sort))),]#71
#c4m5<-c4m[rownames(c4m4),c(1,2,5:8)]

#match back up with nondup pairings
#c4l<-rbind(c4m5,c4c2[,c(1,2,5:8)])
#c4l$tn<-paste(c4l$node,c4l$tipno,sep="_")

#dcalc<-function(x,y){
#    a<-dist.nodes(d4t)[x,y]
#    return(a)}

#d4calc<-mapply(dcalc,c4l$node,c4l$tipno)

#dcalc<-function(x,y){
#    a<-dist.nodes(d4t)[x,y]
#    return(a)}

dn4<-dist.nodes(d4t)

#tst<-a4[1:10,1:10]
#rownames(tst)<-1:10
#colnames(tst)<-1:10

#tsts<-data.frame(node=c(tst),tipno=rep(seq(1:10),10),earlier=rep(rownames(tst),10),
#               pair=rep(colnames(tst),each=10))

#tsts$dist<-dn4[tsts$node,tsts$tipno]#pastes matrix with desired values on diagonal

#a4v$dist<-diag(dn4[a4v$node,a4v$tipno])

m4<-matrix(mapply(function(x, y) {
    dn4[x,y]}, 
    rep(1:N4,N4), c(a4)),nrow=N4)

m4m<-t(m4)

#flag matrices
m4i<-m4
m4i[m4i>=0]<-0

m4mi<-m4m
m4mi[m4mi>=0]<-0

m4dups<-m4i+m4mi
m4dups[is.na(m4dups)]<-1
m4dups[m4dups==0]<-NA
m4dups[m4dups==1]<-0

m4mf<-m4m+m4dups

m4z<-m4
m4z[is.na(m4z)]<-0
m4mfz<-m4mf
m4mfz[is.na(m4mfz)]<-0
m4f<-m4z+m4mfz

#uhh<-mapply(function(a,b){ 
#    is.na(m4[a,b])<-m4m[a,b]
#    !is.na(m4[a,b])<-m4[a,b]
#    },rep(1:N4,N4),rep(1:N4,each=N4))

#merge distance information with pairs
#c4l$dist<-d4calc

ttotmrca4<-m4f*365

#set diag to NA
diag(ttotmrca4)<-NA



################################################################################## dat df
dat1<-data.frame(date=r1n,serotype=rep(1,N1),X=rep(1,N1),Y=rep(1,N1))
dat2<-data.frame(date=r2n,serotype=rep(2,N2),X=rep(1,N2),Y=rep(1,N2))
dat3<-data.frame(date=r3n,serotype=rep(3,N3),X=rep(1,N3),Y=rep(1,N3))
dat4<-data.frame(date=r4n,serotype=rep(4,N4),X=rep(1,N4),Y=rep(1,N4))

##############################################################################ser.dat
s1<-list(N=N1,dmat=dmat1,tmat=tmat1,gmat=gmat1,ttotmrca=ttotmrca1,dat=dat1)
s2<-list(N=N2,dmat=dmat2,tmat=tmat2,gmat=gmat2,ttotmrca=ttotmrca2,dat=dat2)
s3<-list(N=N3,dmat=dmat3,tmat=tmat3,gmat=gmat3,ttotmrca=ttotmrca3,dat=dat3)
s4<-list(N=N4,dmat=dmat4,tmat=tmat4,gmat=gmat4,ttotmrca=ttotmrca4,dat=dat4)

# $N 		- How many sequences of that serotype there are
# $dmat 	- An N x N matrix  - each cell represents the spatial distance between case i and case j (where there are n sequences available for that serotype) (diagonal is NA)
# $tmat  	- An N x N matrix - each cell represents the time (in days) between case i and case j (diagonal is NA)
# $gmat  	- An N x N matrix - each cell represents the total evolutionary time (in days) between that pair of viruses (diagonal is NA)
# $ttotmrca  	- An N x N matrix - each cell represents the time (in days) to the most recent common ancestor for the earlier of the two cases (diagonal is NA)
# dat
gen.dat<-list(D1=s1,D2=s2,D3=s3,D4=s4)

saveRDS(gen.dat,"gen.dat.RData")
