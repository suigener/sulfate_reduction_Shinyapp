# This file contains a script that calculates metabolite concentrations over a range of
# metabolic parameter values and then calculates fractionation. A value for a dummy
# parameter called "var" is identified by user input and controls what metabolic
# parameter is varied in the calculation.

rm(list = ls())

#***********************************************************************************
varx <- 1 # change this value, this will be user-chosen for x axis (ranges from 1 to 21)
vary <- 5 # change this value, this will be user-chosen for y axis (ranges from 1 to 5)
#***********************************************************************************

numdum <- 1 # if positive, broadens range, lower end--if negative, narrows range
dumdum <- 1 # if negative, broadens range, upper end--if positive, narrows range
int <- 4 # sets resolution of plot


# setting environment
SO4_out <- 1e-3
H2S <- 1e-3

s_day <- 60*60*24

J0 = 10
J0v = c(0.1,100)


# set possible varied parameters
m <- 2.0
mv <- c(1,100)

b <- 70
bv <- c(1,100)

V_A <- 3.98e-20*(1e15)*s_day # fmol/(cell*day)
V_Ai <- seq(-2-numdum, 2+dumdum, int)
V_Av <- 10^V_Ai

K_As1 <- 0.01*1e-3 # mM
K_As1i <- seq(-7-numdum, -3+dumdum, int)
K_As1v <- 10^K_As1i

K_Ap1 <- 0.01*1e-3 # mM
K_Ap1i <- seq(-7-numdum, -3+dumdum, int)
K_Ap1v <- 10^K_Ap1i

V_B <- 3.24e-19*(1e15)*s_day # fmol/(cell*day)
V_Bi <- seq(-1-numdum, 3+dumdum, int)
V_Bv <- 10^V_Bi

K_Bs1 <- 10.00*1e-3 # mM
K_Bs1i <- seq(-4-numdum, 0+dumdum, int)
K_Bs1v <- 10^K_Bs1i

K_Bs2 <- 0.10*1e-3 # mM
K_Bs2i <- seq(-6-numdum, -2+dumdum, int)
K_Bs2v <- 10^K_Bs2i

K_Bp1 <- 0.17*1e-3 # mM
K_Bp1i <- seq(-6-numdum, -2+dumdum, int)
K_Bp1v <- 10^K_Bp1i

K_Bp2 <- 0.13*1e-3 # mM
K_Bp2i <- seq(-6-numdum, -2+dumdum, int)
K_Bp2v <- 10^K_Bp2i

V_C <- 3.49e-19*(1e15)*s_day # fmol/(cell*day)
V_Ci <- seq(-2-numdum, 2+dumdum, int)
V_Cv <- 10^V_Ci

K_Cs1 <- 0.10*1e-3 # mM, for MK_red
K_Cs1i <- seq(-6-numdum, -2+dumdum, int)
K_Cs1v <- 10^K_Cs1i

K_Cs2 <- 0.02*1e-3 # mM, for APS
K_Cs2i <- seq(-7-numdum, -3+dumdum, int)
K_Cs2v <- 10^K_Cs2i

K_Cp1 <- 0.10*1e-3 # mM, for MK_ox
K_Cp1i <- seq(-6-numdum, -2+dumdum, int)
K_Cp1v <- 10^K_Cp1i

K_Cp2 <- 0.40*1e-3 # mM
K_Cp2i <- seq(-6-numdum, -2+dumdum, int)
K_Cp2v <- 10^K_Cp2i

K_Cp3 <- 0.30*1e-3 # mM
K_Cp3i <- seq(-6-numdum, -2+dumdum, int)
K_Cp3v <- 10^K_Cp3i

V_D <- 4.28e-19*(1e15)*s_day # fmol/(cell*day)
V_Di <- seq(-1-numdum, 3+dumdum, int)
V_Dv <- 10^V_Di

K_Ds1 <- 0.05*1e-3 # mM
K_Ds1i <- seq(-7-numdum, -3+dumdum, int)
K_Ds1v <- 10^K_Ds1i

K_Ds2 <- 0.02*1e-3 # mM, for MK_red
K_Ds2i <- seq(-7-numdum, -3+dumdum, int)
K_Ds2v <- 10^K_Ds2i

K_Dp1 <- 0.01*1e-3 # mM
K_Dp1i <- seq(-7-numdum, -3+dumdum, int)
K_Dp1v <- 10^K_Dp1i

K_Dp2 <- 0.02*1e-3 # mM, for MK_ox
K_Dp2i <- seq(-7-numdum, -3+dumdum, int)
K_Dp2v <- 10^K_Dp2i


# set constants
R <- 8.314 # J/(mol*K)
Temp <- 298.15 # K
Fara <- 96485 # J/(V*mol)
PMF <- -132e-3 # V
z <- 2.3*R*Temp/Fara # V

ATP <- 2.6*1e-3 # M
G_B0 <- 55.9e3 # J/mol

AMP <- 0.3*1e-3 # M
MK_ox <- 0.6/101*1e-3 # M
MK_red <- MK_ox*100 # M
G_C0 <- 5.4e3 # J/mol

G_D0 <- 31.2e3 # J/mol


# define arrays of equilibrium and kinetic fractionation factors
a34eqA <- 1.000
a34eqB <- 1.000
a34eqC <- 1.006
a34eqD <- 1.065

a34kinA <- 0.995
a34kinB <- 1.000
a34kinC <- 1.022
a34kinD <- 1.025


# data.frame(J0_ref, J0, SO4_out, H2S, env, m, b,
# V_A, V_Av, K_As1, K_As1v, K_Ap1, K_Ap1v, V_B, V_Bv, K_Bs1, K_Bs1v, K_Bs2, K_Bs2v,
# K_Bp1, K_Bp1v, K_Bp2, K_Bp2v, V_C, V_Cv, K_Cs1, K_Cs1v, K_Cs2, K_Cs2v,
# K_Cp1, K_Cp1v, K_Cp2, K_Cp2v, K_Cp3, K_Cp3v, V_D, V_Dv, K_Ds1, K_Ds1v,
# K_Ds2, K_Ds2v, K_Dp1, K_Dp1v, K_Dp2, K_Dp2v, Temp, R, Fara, PMF, z, ATP,
# G_A0, A3, G_B0, B3, AMP, MK_ox, MK_red, G_C0, C3, G_D0, D3,
# fAcc, nCalc, phv, H_in_out,
# a34eqA, a34eqB, a34eqC, a34eqD, a34kinA, a34kinB, a34kinC, a34kinD,
# Temp, R, Fara, PMF, z, ATP)


sing_vals <- data.frame(J0, m, b, V_A, K_As1, K_Ap1, V_B, K_Bs1, K_Bs2, K_Bp1, K_Bp2,
                        V_C, K_Cs1, K_Cs2, K_Cp1, K_Cp2, K_Cp3, V_D, K_Ds1, K_Ds2, K_Dp1, K_Dp2,
                        Temp, R, SO4_out, H2S, Fara, PMF, z, ATP,
                        G_B0, AMP, MK_ox, MK_red, G_C0, G_D0,
                        a34eqA, a34eqB, a34eqC, a34eqD, a34kinA, a34kinB, a34kinC, a34kinD)

var_vals <- data.frame(J0v, mv, bv, V_Av, K_As1v, K_Ap1v, V_Bv, K_Bs1v, K_Bs2v, K_Bp1v, K_Bp2v,
                       V_Cv, K_Cs1v, K_Cs2v, K_Cp1v, K_Cp2v, K_Cp3v, 
                       V_Dv, K_Ds1v, K_Ds2v, K_Dp1v, K_Dp2v)

# should be 22 of these names for J0, m, b, V_max, and K_m
ana_char <- c("csSRR (J0)", "m", "b", "V<sub>max</sub>: sulfate uptake",
              "K<sub>M</sub>: external sulfate", "K<sub>M</sub>: internal sulfate",
              "V<sub>max</sub>: sulfate activation", "K<sub>M</sub>: internal sulfate",
              "K<sub>M</sub>: ATP", "K<sub>M</sub>: APS", "K<sub>M</sub>: PPi",
              "V<sub>max</sub>: APS reduction", "K<sub>M</sub>: menaquinol",
              "K<sub>M</sub>: APS", "K<sub>M</sub>: menaquinone", "K<sub>M</sub>: sulfite",
              "K<sub>M</sub>: menaquinol", "V<sub>max</sub>: sulfite reduction",
              "K<sub>M</sub>: sulfite", "K<sub>M</sub>: menaquinol", "K<sub>M</sub>: sulfate",
              "K<sub>M</sub>: menaquinone")

saveRDS(sing_vals, file = "const_input.rds")
saveRDS(var_vals, file = "var_input.rds")
saveRDS(ana_char, file = "char_input.rds")

