nls(gdpgrowth ~ a + b*linigdp + c*yr.sch.secM + 
      d*yr.sch.secF + e*llexpec + f*lfert +
      g*linigdp*(c*yr.sch.secM + d*yr.sch.secF + e*llexpec),
    start=list(a=0.193,b=-0.014,c=-0.008,d=0.005,
               e=-0.011, f=-0.001, g=-1.28),
    data = IQM_pro_data)

summary(pooled.time1 <- plm(gdpgrowth ~ linigdp + yr.sch.secM + 
                              yr.sch.secF + llexpec + lfert + kg + ToT +
                              ki + 
                              south.asia + east.asia.pacific +
                              latin.america.caribbean + middle.east.north.africa + 
                              sub.saharan.africa + llaw.order,  
                            data = IQM_pro_data, 
                            model = "pooling",
                            effect="time"))

#### Extract coefficients ####
coef.llexpec1 <- coefficients(pooled1)["llexpec"]
coef.llexpec1

