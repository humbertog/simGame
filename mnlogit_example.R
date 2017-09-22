library("mlogit")

data("Heating", package = "mlogit")
H <- mlogit.data(Heating, shape="wide", choice="depvar", varying=c(3:12))
m <- mlogit(depvar~ic+oc|0, H)
summary(m)


mi <- mlogit(depvar~oc+I(ic/income), H)
summary(mi)

mi2 <- mlogit(depvar~oc+ic|income, H, reflevel="hp")
summary(mi2)


mc <- mlogit(depvar~ic+oc, H, reflevel = 'hp')
summary(mc)


X <- model.matrix(mc)
alt <- index(H)$alt
chid <- index(H)$chid
eXb <- as.numeric(exp(X %*% coef(mc)))
SeXb <- tapply(eXb, chid, sum)
P <- eXb / SeXb[chid]
P <- matrix(P, ncol = 5, byrow = TRUE)
head(P)
apply(P, 2, mean)
