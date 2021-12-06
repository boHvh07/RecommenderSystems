options("scipen"= 200, "digits"=3, width = 100)

rm(list=ls())

row_names<-c("AS", "JR", "surprise")
col_names<-c("PW", "TR", "EB", "T2", "P")

item <- matrix(c(0,1,0,1,1,
                 1,0,1,0,0,
                 .1,.4,.1,0,.1), byrow = TRUE, nrow = 3, ncol = 5, dimnames=list(row_names,col_names))

item


rating <-matrix(c(3,1,5,2,4),nrow=1,ncol=5)
rating_m <-rating-mean(rating)
rating_m

# t() means taking the transpose of a matrix, M'
user<-item %*% t(rating_m) / rowSums(item)

user


row_names<-c("AS", "JR", "surprise")
col_names<-c("TL", "NH")

new_item <- matrix(c(1,0,
                     0,1,
                     .1,0), byrow = TRUE, nrow = 3, ncol = 2, dimnames=list(row_names,col_names))
new_item


CS = t(new_item) %*% user / (sqrt(colSums(new_item^2))*sqrt(sum(user^2)))

CS


row_names<-c("Funny", "Romant", "Suspense", "Dark")
col_names<-c("Sharp Obj", "Arrested Dev", "Arbitrage", "Margin C", "Bojack", "Orphan B", "Hinterland")

item <- matrix(c(0,1,0,1,1,1,0,
                 1,1,0,0,1,0,0,
                 1,1,1,0,1,0,1,
                 1,0,1,1,0,1,1), 
               byrow = TRUE, nrow = 4, ncol = 7, dimnames=list(row_names,col_names))

item

#### Collaborative FIltering ####
row_names<-c("A", "B", "C", "D", "E", "F", "G")
col_names<-c("PW", "TR", "EB", "T2", "P", "NH")

util <- matrix(c(2,5,4,2,NA, NA,
                 5,1,2,NA,1,NA,
                 5,5,5,5,5,5,
                 2,5,NA,3,NA,NA,
                 5,4,5,3,NA,5,
                 1,5,NA,NA,NA,1,
                 2,NA,5,NA,5,NA),byrow = TRUE, nrow = 7, ncol = 6, dimnames=list(row_names,col_names))
util


cor(t(util), use="pairwise.complete.obs")


m<-cor(t(util), use="pairwise.complete.obs")

m[row=c("B","F","G"),col=c("A")]


util_n <-util-rowMeans(util, na.rm=TRUE)

predm<-m[row=c("B","F","G"),col=c("A")]*util_n[row=c("B","F","G"),col=c("P","NH")]

pred<-colMeans(predm, na.rm=TRUE)

pred

### Item Based ###
cor(util, use="pairwise.complete.obs")

m<-cor(util, use="pairwise.complete.obs")

m<-m[row=c("PW", "TR","EB"), col=c("P", "NH")]

# make NA anything less than 1
m[abs(m)<1]<-NA

m

predm<-m*util_n[row=c("A"),col=c("PW", "TR","EB")]
predm

pred<-colMeans(predm, na.rm = TRUE)
pred


row_names<-c("George", "Adam", "Ben", "Cam", "Dan")
col_names<-c("Sharp Obj", "Arrested Dev", "Arbitrage", "Margin C", "Bojack", "Orphan B", "Hinterland")

util <- matrix(c(4,3,4,5,3,NA,NA,
                 4,3,4,4,3,NA,NA,
                 3,4,3,1,3,5,NA,
                 4,4,4,4,4,2,4,
                 2,1,2,3,1,NA,3),byrow = TRUE, nrow = 5, ncol = 7, dimnames=list(row_names,col_names))
util