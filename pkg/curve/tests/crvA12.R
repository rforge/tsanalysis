# Tests of DSE curvature functions from dsecurvature.function.testsA
if(!require("dse"))  stop("this test requires dse.")
if(!require("curve"))stop("this test requires curve.")
 Sys.info()
 DSEversion()
 
fuzz.small <- 1e-12
fuzz.large <- 1e-6
fuzz.very.large <- 1e-2
digits <- 18
all.ok <- TRUE
test.rng <- list(kind="Wichmann-Hill",seed=c(979,1479,1542),normal.kind="Box-Muller")


# comparison values come only from a previous run of the 
#  code (theoretical values would be nice)...
# Test values have been changed with change to RNG when R 1.0.0 was released
#   (Feb. 29, 2000) and also previously.
  

# from user guide

  ARMAmodel1<-ARMA(A=array(c(1,.5,.3,0,.2,.1,0,.2,.05,1,.5,.3),c(3,2,2)),
             B=array(c(1,.2,0,.1,0,0,1,.3),c(2,2,2)), C=NULL) 


  ARMAmodel1<-l(ARMAmodel1,simulate(ARMAmodel1, rng=test.rng))
  SSmodel  <- l(toSS(ARMAmodel1),  ARMAmodel1$data)
  ARMAmodel<- l(toARMA(SSmodel), ARMAmodel1$data)


cat("DSE curvature test A 12a..\n")
  curvatureVAR <- curvature(ARMAmodel1,warn=FALSE)$stats
#  good <- c(11, 200, 0.05, 0.8490449316698463, 0.712275843318316,
#        1.151573691713139,  0.9660715137831059, 1.000000000576390) #, NaN)


  good <- c(11, 200, 0.05, 0.807396116175452816,  0.681455079712046774,
         1.0950847140096498,  0.924268802049481475,  1.00000000053258109)

  good <- c(11, 200, 0.05, 0.807400747659583362,  0.681458988757835171,
         1.09509099576822266,  0.924274103952975268,  1.00000000053258109)

   tst  <- curvatureVAR[-9]
   error <- max(abs(good-tst))
   print(good, digits=18)
   print(tst,  digits=18)
   
   cat("max. error ", max(error), "\n")
     
   if (any(is.na(error)) || any(is.nan(error)) || fuzz.large < error) 
     {all.ok <- F
     cat("test failed!\n") 
     }

cat("DSE curvature test A 12b..\n")
  func.residual <- function(coefficients,Shape,data)
   {c(l(setArrays(Shape,coefficients=coefficients),data,result="pred")
       - outputData(data))}
   
  curvatureVAR.def <- curvature(func.residual, coef(ARMAmodel1), 
        method="Richardson", method.args=list(d=0.01, eps=1e-4, r=6, v=2),
	show.details=FALSE,
	Shape=TSmodel(ARMAmodel1), data=TSdata(ARMAmodel1))$stats

  curvatureVAR.def2 <- curvature(ARMAmodel1, compiled=FALSE,
        method="Richardson", method.args=list(d=0.01, eps=1e-4, r=6, v=2),
        show.details=FALSE)$stats

  print(curvatureVAR.def,  digits=18)
  print(curvatureVAR.def2, digits=18)
      
  if (! testEqual(curvatureVAR.def2, curvatureVAR.def, fuzz=1e-15)) {
     all.ok <- F
     cat("test failed!\n")
     }
 
  good <- curvatureVAR[-9]
  tst  <- curvatureVAR.def[-9]
  error <- max(abs(good-tst))
  
  print(good, digits=18)
  print(tst,  digits=18)
  cat("max. error ",error, "\n")
    
  if (any(is.na(error)) || any(is.nan(error)) || fuzz.small < error) { 
    all.ok <- F
    cat("test failed!\n")
    }

  if (! all.ok) stop("some tests FAILED")
