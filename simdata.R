set.seed(11202018)


#covariates
zip.prop = c(0.1061873598,
              0.1852409501,
              0.1444739371,
              0.1489908929,
              0.1578600522,
              0.1696215276,
              0.02982472198,
              0.03760926273,
              0.008539654936,
              0.01165164066)

zip.codes = c(28401,
              28403,
              28405,
              28409,
              28411,
              28412,
              28428,
              28429,
              28449,
              28480)

zip.prop = zip.prop*10000
zip.prop = round(zip.prop,0)
zip.prop[1] = 1062
sum(zip.prop)



zip = NULL
for (i in 1:length(zip.codes)){
  zip = c(zip, rep(zip.codes[i],zip.prop[i]))
}

##############age variable
set.seed(11262020)


alpha = 3
beta = 5
med = (alpha-(1/3))/(alpha+beta-(2/3))
zip.med = c(38,
            27.7,
            40.3,
            43.4,
            39.7,
            40.3,
            44.7,
            38.8,
            57.4,
            43.3)
zip.med.par = zip.med/med



for (i in 1:length(zip.codes)){
  agevar <- paste("age.", i, sep = "")
  assign(agevar, rbeta(zip.prop[i],alpha,beta)*zip.med.par[i])
}
alpha = 5
beta = 8
med = (alpha-(1/3))/(alpha+beta-(2/3))
age.9 = rbeta(zip.prop[9],alpha,beta)*(zip.med[9]/med)
summary(age.9)

age.est = NULL
for (i in 1:length(zip.codes)){
  age.est = rbind(c(age.est,get(paste('age.',i,sep = ''))))
}
hist(age.est)
summary(t(age.est))
summary(age.2)


###########gender variable
zip.gender.prob = c(0.5018747576,
                    0.5311658473,
                    0.5232031423,
                    0.5209485195,
                    0.5080593726,
                    0.5299212174,
                    0.5155746509,
                    0.4997566318,
                    0.5321543408,
                    0.4505106049)
zip.gender.prob = zip.gender.prob*1.15
zip.gender = NULL
for (i in 1:length(zip.prop)){
  zip.gender = c(zip.gender, rbinom(n=zip.prop[i], size = 1, prob = zip.gender.prob[i]))
}
mean(zip.gender)

#############income variable
set.seed(11262020)

zip.income = c(29467,
               34590,
               54449,
               76512,
               69707,
               51523,
               61660,
               48419,
               68125,
               77232)
summary(abs(rnorm(1000,zip.income[1],zip.income[1]/5)))
inc.alpha = 3
inc.beta = 5
inc.avg = inc.alpha/(inc.alpha+inc.beta)
inc.par = zip.income/inc.avg

for (i in 1:length(zip.codes)){
  incvar <- paste("inc.", i, sep = "")
  assign(incvar, rbeta(zip.prop[i],inc.alpha,inc.beta)*inc.par[i])
}


inc.est = NULL
for (i in 1:length(zip.codes)){
  inc.est = rbind(c(inc.est,get(paste('inc.',i,sep = ''))))
}
summary(t(inc.est))
hist(t(inc.est))
