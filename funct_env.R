funct_env <- function(SO4_out, PMF, z, Temp, R) {

# calculate H_in_out, nCalc, G_A0, and A3 for each environment
upSO4 <- c(0.102, 1.104, 10.186, 31.204, 99.665, 315.677, 999.920, 3167.125, 10032.496, 31511.710, 99809.548, 123005.771)*1e-6
upAcc <- c(5176.297, 1065.852, 245.214, 117.075, 54.368, 25.482, 11.833, 5.546, 2.552, 1.207, 0.566, 0.486)
fAcc_fun <- approxfun(log10(upSO4), upAcc)
fAcc <- fAcc_fun(log10(SO4_out))

upSO4 <- c(0.102, 1.106, 10.181, 31.147, 100.179, 316.892, 1002.357, 3170.702, 10029.220, 31723.334, 100348.752, 117591.020)*1e-6
upn <- c(2.985, 2.645, 2.323, 2.161, 1.992, 1.824, 1.659, 1.490, 1.325, 1.159, 0.991, 0.967) # [] number of positive ions taken up
nCalc_fun <- approxfun(log10(upSO4), upn)
nCalc <- nCalc_fun(log10(SO4_out))

phi <- nCalc*PMF/2+log10(fAcc)*(z/2)
H_in_out <- 10^((PMF-phi)/z)
G_A0 <- phi*(nCalc-2)*R*Temp/z # J/mol, choose depending on SO4_out
#A3 <- exp(G_A0/(R*Temp))*(H_in_out^nCalc)

#return(c(fAcc, nCalc, phi, H_in_out, G_A0, A3))
return(c(nCalc, H_in_out, G_A0))
}