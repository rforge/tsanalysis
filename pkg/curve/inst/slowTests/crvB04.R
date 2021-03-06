# Tests of DSE curvature functions (previously dsecurvature.function.testsB )

# comparison values come only from a previous run of the 
#  code (theoretical values would be nice)...
# Test values have been changed with change to toARMA in 2001.2 which
# eliminates near zero parameter values using fixConstants. The result is
# much more stable and believable curvature results. The span results do not
# change much (as would be hoped) but do change more than the tolerance of 
# these tests. Old values in comments are  strictly for historical reference.

if(!require("dse"))  stop("this test requires dse.")
if(!require("curve"))stop("this test requires curve.")
 Sys.info()
 DSEversion()

fuzz.small <- 1e-12
fuzz.large <- 1e-6
digits <- 18
all.ok <- T  

 data("eg1.DSE.data.diff", package="dse")

# data size affects memory constraints
  data <- eg1.DSE.data.diff
   inputData(data) <- NULL
  outputData(data) <- outputData(data)[1:50,1:2]

  VARmodel <- estVARXls(data, re.add.means=FALSE)
  SSmodel  <- l(toSS(VARmodel),  data)
  ARMAmodel<- l(toARMA(SSmodel), data)


cat("DSE curvature test B 4 ...")

  curvatureVAR <- curvature(VARmodel)$stats

   good <- c(24, 100, 0.05, 0, 0, 0, 0, 1, 1 )
   tst  <- curvatureVAR
   error <- max(abs(good - tst))

   print(good, digits=18)
   print(tst,  digits=18)
   cat("max. error ",error, "\n")

   if (any(is.na(error)) || any(is.nan(error)) || 1e-5 < error) 
     {all.ok <- F  
      cat("test failed!\n")
     }

cat("DSE curvature test B 5 ...")

  curvatureSS <- curvature(SSmodel, warn=FALSE)$stats

# old R value   tst <- c(48, 100, 0.05, 323.99227471499745, 124.74717834454975,
#        409.3130651286905,  157.59835625487983, 1.0000000021695887, NaN)[-9]
# old Splus value tst <- c(48, 100, 0.05, 255.387386434704,    98.33696083718515,
#        322.6416248003061, 124.23321787877502, 1, 1.0000000004520695  ) [-9]

# Solaris with svd:
#               c(48, 100, 0.05, 323.992363637504695, 124.759997096571936,  
#	        409.313177468233278, 157.614550723361958, 1.0000000014689332) else 
# Solaris with La.svd:
#               c(48, 100, 0.05, 323.992363637501512, 124.759997096569762,
#	        409.313177468229242, 157.614550723359201, 1.00000000146893298)
#with La.svd
# not BLAS    c(48, 100, 0.05, 323.989506507851672, 124.76868587630355,  
#	        409.309567936195094, 157.625527624176414, 1.00000000058753002) else 
#   ATLAS     c(48, 100, 0.05, 323.992424992640395, 124.77395817674784,
#               409.313254980756369,   157.63218834303774,  1.00000000148918633)

# R 2.0.0 on amd opteron 64 linux  (much closer to Solaris )
#              c(48, 100, 0.05, 323.992363637501512, 124.759997096569762, 
#                409.313177468229242, 157.614550723359201, 1.00000000146893853)

# values with Splus 3.3 
#                c(48, 100, 0.05, 323.992363637504695, 124.759997096571936,  
# 	        409.313177468233278, 157.614550723361958, 1.0000000014689332) else 
# values with R 1.2.2 ( note Linux is different !)
   good <-   if ((Sys.info()[["sysname"]] == "Linux") &
	         (.Machine$sizeof.pointer <=4) ) 
               c(48, 100, 0.05, 323.989506507851672, 124.76868587630355,  
	        409.309567936195094, 157.625527624176414, 1.00000000058753002) else 
	     if (Sys.info()[["sysname"]] == "SunOS" ) 
               c(48, 100, 0.05, 323.992363637501512, 124.759997096569762,
	        409.313177468229242, 157.614550723359201, 1.00000000146893298) else 
               c(48, 100, 0.05, 323.992363637501512, 124.759997096569762,
	        409.313177468229242, 157.614550723359201, 1.00000000146893298)#defaulat Solaris
	      
# R 1.3.0 in Linux sometimes gives
# c(48, 100, 0.05, 323.991985730258421,  124.768491521140618,  
#    409.312700042117513,  157.625282087126067,  1.00000000079737927)

   tst  <- curvatureSS[-9]
   error <- max(abs(good - tst))

   print(good, digits=18)
   print(tst,  digits=18)
   cat("max. error ",error, "\n")

ok <- T
# relaxed  for R 1.4.1 in Linux
tol <- if (Sys.info()[["sysname"]] == "Linux") 1e-1 else 100*fuzz.small
warning("Using relaxed tolerance for Linux.")  
# this is a pretty relaxed tolerance!!! 
if (any(is.na(error)) || any(is.nan(error)) || tol < error) ok <- F  
       
if (! ok) 
     {all.ok <- F
      cat("test failed!\n")
     }
   
   
cat("DSE curvature test B 6 ...")

  curvatureARMA <- curvature(ARMAmodel, warn=FALSE)$stats

# previous test values were suspicious
#  if (print.values) printTestValue(curvatureARMA, digits=digits)
# R   tst <- c(71,100,0.05, 8.083857768891898e+23, 354675866.7879350,
#          1.0653907038344889e+24, 467435699.82689363, 1, NaN )[-9]
# Splus tst <- c(72,100,0.05, 31947799733885313024, 1708051.5249938569,
#       42274456907383595008,  2260154.101077503, 1, 1.0000267463352852 )[-9] 
#   tst <- c(71, 100, 0.0500000000000000028, 1.48023299583791679e+23, 
#     7.16541930125915901e+21,  1.95083401806432887e+23, 9.443475294697275e+21,   1 )

# values with R 1.2.2 and Splus 3.3 ( note Linux is different !)
# R1.2.3 Linux  c(48, 100, 0.05, 149.094838773755185, 73.5157177804486111,
#	        188.357779539762589, 92.875497745497043, 1.00001977123294594) else 
# value below is R1.3.0 Linux
# but really seems just good to c(48, 100, 0.05, 149.0, 73.5, 188.3, 92., 1.0000) else 
# Solaris with svd:
#               c(48, 100, 0.05, 149.118676845175685, 73.5365097052148116,  
#                188.387895177823538, 92.9017650583934795, 1.00005841302234355) 
# Solaris with La.svd:
#               c(48, 100, 0.05, 149.118676845179181, 73.5365097052158205,
#	        188.387895177827971, 92.9017650583947585, 1.00005841302234355)
#  Splus
#               c(48, 100, 0.05, 149.118676845175685, 73.5365097052148116,  
#                188.387895177823538, 92.9017650583934795, 1.00005841302234355) else 
   good <-  if (Sys.info()[["sysname"]] == "Linux") 
               c(48, 100, 0.05, 149.059221869575282,     73.5866620776083522,
	        188.312783206954663,     92.9651246594393541,
		1.00005807705159655) else 
	     if (Sys.info()[["sysname"]] == "Windows" ) 
   	       c(48, 100, 0.05, 149.1188123779424472559, 73.5597433788594798898, 
                188.3880664020672099923, 92.9311171353282361451,
		1.0000583685204342821 )else 
	     if (Sys.info()[["sysname"]] == "SunOS" ) 
               c(48, 100, 0.05, 149.118676845179181,     73.5365097052158205,
	        188.387895177827971,     92.9017650583947585, 
		1.00005841302234355) else 
               c(48, 100, 0.05, 149.118676845179181,     73.5365097052158205,
	        188.387895177827971,     92.9017650583947585, 
		1.00005841302234355) #defaulat Solaris
	     	      
  
 # relaxed from fuzz.small to 10*fuzz.small for R 1.4.1 in Linux and then to 1.0
  tst  <- curvatureARMA[-9]
   error <- max(abs(good - tst))

   print(good, digits=18)
   cat("max. error ",error, " platform:", Sys.info()[["sysname"]],"\n")
   print(tst,  digits=18)
   if (any(is.na(error)) || any(is.nan(error)) || (if (Sys.info()[["sysname"]] == 
	"Linux")1.0 else fuzz.small) < error) {all.ok <- F ; cat("test failed!\n") }
  if (! all.ok) stop("some tests FAILED")
