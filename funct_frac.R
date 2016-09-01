#funct_frac <- function(SO4_in, SO4_out, H2S, SO3, H_in_out, nCalc, PPi, 
#                     APS, ATP, AMP, MK_ox, MK_red, G_A0, G_B0, G_C0, G_D0, 
#                     a34eqA, a34eqB, a34eqC, a34eqD, a34kinA, a34kinB, a34kinC, a34kinD, R, Temp){

funct_frac <- function(SO4_in, APS, SO3, PPi, SO4_out, H2S,
                       H_in_out, nCalc, ATP, AMP, MK_ox, MK_red,
                       G_A0, G_B0, G_C0, G_D0,
                       a34eqA, a34eqB, a34eqC, a34eqD,
                       a34kinA, a34kinB, a34kinC, a34kinD, R, Temp) {

# calculate fractionation
G_1 <- G_A0+R*Temp*log(SO4_in/SO4_out*H_in_out^nCalc)
f_1 <- exp(G_1/(R*Temp))

G_2 <- G_B0+R*Temp*log(PPi*APS/(ATP*SO4_in))
f_2 <- exp(G_2/(R*Temp))

G_3 <- G_C0+R*Temp*log(AMP*MK_ox*SO3/(MK_red*APS))
f_3 <- exp(G_3/(R*Temp))

G_4 <- G_D0+R*Temp*log((MK_ox^3)*H2S/((MK_red^3)*SO3))
f_4 <- exp(G_4/(R*Temp))


a34_4 <- f_4*(a34eqD-a34kinD)+a34kinD
a34_3 <- f_3*(a34eqC*a34_4-a34kinC)+a34kinC
a34_2 <- f_2*(a34eqB*a34_3-a34kinB)+a34kinB
a34_1 <- f_1*(a34eqA*a34_2-a34kinA)+a34kinA

net.frac = (a34_1 - 1)*1000

return(net.frac)
}
