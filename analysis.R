ls(IQM_pro_data)
# Make human capital variable (many codes so done in other file)
source("~/Dropbox/Studieophold/College_of_Europe/Master_Thesis/CoE_thesis_repository/human.capital.R",
       local = FALSE, echo = TRUE)

# First we make an object with all the variable names to avoid repetition
var1 <- c("linigdp + yr.sch.secM + yr.sch.secF + llexpec + lfert + kg + ToT + ki + south.asia + east.asia.pacific + europe.central.asia + latin.america.caribbean + middle.east.north.africa + sub.saharan.africa + llaw.order")
model1 <- paste("gdpgrowth ~ ",var1,sep = "")
model1 <- as.formula(model1)

var1 <- c("linigdp", "yr.sch.secM", "yr.sch.secF", "llexpec",
          "lfert", "kg", "ToT", "ki", "south.asia",
          "east.asia.pacific",
          "latin.america.caribbean", "middle.east.north.africa",
          "sub.saharan.africa", "llaw.order")
model1 <- paste(var1, sep="")
model1 <- as.formula(paste("gdpgrowth ~", paste(model1, collapse = "+")))
model1

summary(test1 <- plm(model1, data = IQM_pro_data, effect="time"))

# pooled.time regression (time effects)
summary(pooled.time1 <- plm(gdpgrowth ~ linigdp + yr.sch.secM + 
                              yr.sch.secF + llexpec + lfert + kg + ToT +
                              ki + 
                              south.asia + east.asia.pacific +
                              latin.america.caribbean + middle.east.north.africa + 
                              sub.saharan.africa + llaw.order,  
                            data = IQM_pro_data, 
                            model = "pooling",
                            effect="time"))
summary(pooled.time2 <- plm(gdpgrowth ~ linigdp + yr.sch.secM + 
                              yr.sch.secF + llexpec + lfert + kg + ToT +
                              ki +  
                              south.asia + east.asia.pacific +
                              latin.america.caribbean + middle.east.north.africa + 
                              sub.saharan.africa + 
                              democ + democ2, 
                            data = IQM_pro_data, 
                            model = "pooling",
                            effect="time"))
summary(pooled.time3 <- plm(gdpgrowth ~ linigdp + yr.sch.secM + 
                              yr.sch.secF + llexpec + lfert + kg + ToT +
                              ki + POLCONIII + 
                              south.asia + east.asia.pacific +
                              latin.america.caribbean + middle.east.north.africa + sub.saharan.africa, 
                            data = IQM_pro_data, 
                            model = "pooling",
                            effect="time"))
summary(pooled.time4 <- plm(gdpgrowth ~ linigdp + yr.sch.secM + 
                              yr.sch.secF + llexpec + lfert + kg + ToT +
                              ki + POLCONV + 
                              south.asia + east.asia.pacific +
                              latin.america.caribbean + middle.east.north.africa + sub.saharan.africa, 
                            data = IQM_pro_data, 
                            model = "pooling",
                            effect="time"))
summary(pooled.time5 <- plm(gdpgrowth ~ linigdp + yr.sch.secM + 
                              yr.sch.secF + llexpec + lfert + kg + ToT +
                              ki + POLCONVJ + 
                              south.asia + east.asia.pacific +
                              latin.america.caribbean + middle.east.north.africa + sub.saharan.africa, 
                            data = IQM_pro_data, 
                            model = "pooling",
                            effect="time"))
summary(pooled.time6 <- plm(gdpgrowth ~ linigdp + yr.sch.secM + 
                              yr.sch.secF + llexpec + lfert + kg + ToT +
                              ki + l.icrgQoG + 
                              south.asia + east.asia.pacific +
                              latin.america.caribbean + middle.east.north.africa + sub.saharan.africa, 
                            data = IQM_pro_data, 
                            model = "pooling",
                            effect="time"))

# fixed.time effects (time)
summary(fixed.time1 <- plm(gdpgrowth ~ linigdp + yr.sch.secM + 
                             yr.sch.secF + llexpec + lfert + kg + ToT +
                             ki + 
                             south.asia + east.asia.pacific +
                             latin.america.caribbean + middle.east.north.africa + 
                             sub.saharan.africa + llaw.order, 
                           data = IQM_pro_data, 
                           model = "within",
                           effect="time"))
summary(fixed.time2 <- plm(gdpgrowth ~ linigdp + yr.sch.secM + 
                             yr.sch.secF + llexpec + lfert + kg + ToT +
                             ki + 
                             south.asia + east.asia.pacific  +
                             latin.america.caribbean + middle.east.north.africa + 
                             sub.saharan.africa +  democ + democ2, 
                           data = IQM_pro_data, 
                           model = "within",
                           effect="time"))
summary(fixed.time3 <- plm(gdpgrowth ~ linigdp + yr.sch.secM + 
                             yr.sch.secF + llexpec + lfert + kg + ToT +
                             ki + POLCONIII + 
                             south.asia + east.asia.pacific  +
                             latin.america.caribbean + middle.east.north.africa + sub.saharan.africa, 
                           data = IQM_pro_data, 
                           model = "within",
                           effect="time"))
summary(fixed.time4 <- plm(gdpgrowth ~ linigdp + yr.sch.secM + 
                             yr.sch.secF + llexpec + lfert + kg + ToT +
                             ki + POLCONV + 
                             south.asia + east.asia.pacific  +
                             latin.america.caribbean + middle.east.north.africa + sub.saharan.africa, 
                           data = IQM_pro_data, 
                           model = "within",
                           effect="time"))
summary(fixed.time5 <- plm(gdpgrowth ~ linigdp + yr.sch.secM + 
                             yr.sch.secF + llexpec + lfert + kg + ToT +
                             ki + POLCONVJ + 
                             south.asia + east.asia.pacific + 
                             latin.america.caribbean + middle.east.north.africa + sub.saharan.africa, 
                           data = IQM_pro_data, 
                           model = "within",
                           effect="time"))
summary(fixed.time6 <- plm(gdpgrowth ~ linigdp + yr.sch.secM + 
                             yr.sch.secF + llexpec + lfert + kg + ToT +
                             ki + l.icrgQoG + 
                             south.asia + east.asia.pacific  +
                             latin.america.caribbean + middle.east.north.africa + sub.saharan.africa, 
                           data = IQM_pro_data, 
                           model = "within",
                           effect="time"))

# fixed effects (twoways)
# summary(fixed.twoways1 <- plm(gdpgrowth ~ linigdp + yr.sch.secM + 
#                              yr.sch.secF + llexpec + lfert + kg +
#                              ki + 
#                              south.asia + east.asia.pacific +
#                              latin.america.caribbean + middle.east.north.africa + 
#                              sub.saharan.africa + llaw.order, 
#                            data = IQM_pro_data, 
#                            model = "within",
#                            effect="twoways"))
# summary(fixed.twoways2 <- plm(gdpgrowth ~ linigdp + yr.sch.secM + 
#                              yr.sch.secF + llexpec + lfert + kg +
#                              ki + democ + democ2 + 
#                              south.asia + east.asia.pacific  +
#                              latin.america.caribbean + middle.east.north.africa + sub.saharan.africa, 
#                            data = IQM_pro_data, 
#                            model = "within",
#                            effect="twoways"))
# summary(fixed.twoways3 <- plm(gdpgrowth ~ linigdp + yr.sch.secM + 
#                              yr.sch.secF + llexpec + lfert + kg +
#                              ki + POLCONIII + 
#                              south.asia + east.asia.pacific  +
#                              latin.america.caribbean + middle.east.north.africa + sub.saharan.africa, 
#                            data = IQM_pro_data, 
#                            model = "within",
#                            effect="twoways"))
# summary(fixed.twoways4 <- plm(gdpgrowth ~ linigdp + yr.sch.secM + 
#                              yr.sch.secF + llexpec + lfert + kg +
#                              ki + POLCONV + 
#                              south.asia + east.asia.pacific  +
#                              latin.america.caribbean + middle.east.north.africa + sub.saharan.africa, 
#                            data = IQM_pro_data, 
#                            model = "within",
#                            effect="twoways"))
# summary(fixed.twoways5 <- plm(gdpgrowth ~ linigdp + yr.sch.secM + 
#                              yr.sch.secF + llexpec + lfert + kg +
#                              ki + POLCONVJ + 
#                              south.asia + east.asia.pacific + 
#                              latin.america.caribbean + middle.east.north.africa + sub.saharan.africa, 
#                            data = IQM_pro_data, 
#                            model = "within",
#                            effect="twoways"))
# summary(fixed.twoways6 <- plm(gdpgrowth ~ linigdp + yr.sch.secM + 
#                              yr.sch.secF + llexpec + lfert + kg + ToT +
#                              ki + l.icrgQoG + 
#                              south.asia + east.asia.pacific  +
#                              latin.america.caribbean + middle.east.north.africa + sub.saharan.africa, 
#                            data = IQM_pro_data, 
#                            model = "within",
#                            effect="twoways"))



# Test of region dummies - virker ikke
# summary(pooled.time1r <- plm(gdpgrowth ~ inigdp + yr.sch.secM + 
#                          yr.sch.secF + llexpec + lfert + kg + ToT +
#                          ki + llaw.order, 
#                        data = IQM_pro_data, 
#                        model = "pooling"))
# summary(pooled.time2r <- plm(gdpgrowth ~ inigdp + yr.sch.secM + 
#                          yr.sch.secF + llexpec + lfert + kg + ToT +
#                          ki + democ + democ2, 
#                        data = IQM_pro_data, 
#                        model = "pooling"))
# summary(pooled.time3r <- plm(gdpgrowth ~ inigdp + yr.sch.secM + 
#                          yr.sch.secF + llexpec + lfert + kg + ToT +
#                          ki + POLCONIII, 
#                        data = IQM_pro_data, 
#                        model = "pooling"))
# summary(pooled.time4r <- plm(gdpgrowth ~ inigdp + yr.sch.secM + 
#                          yr.sch.secF + llexpec + lfert + kg + ToT +
#                          ki + POLCONV, 
#                        data = IQM_pro_data, 
#                        model = "pooling"))
# summary(pooled.time5r <- plm(gdpgrowth ~ inigdp + yr.sch.secM + 
#                          yr.sch.secF + llexpec + lfert + kg + ToT +
#                          ki + POLCONVJ, 
#                        data = IQM_pro_data, 
#                        model = "pooling"))
# summary(pooled.time6r <- plm(gdpgrowth ~ inigdp + yr.sch.secM + 
#                          yr.sch.secF + llexpec + lfert + kg + ToT +
#                          ki + l.icrgQoG, 
#                        data = IQM_pro_data, 
#                        model = "pooling"))
# 
# anova(pooled.time1,pooled.time1r)
# anova(pooled.time2,pooled.time2r)
# anova(pooled.time3,pooled.time3r)
# anova(pooled.time4,pooled.time4r)
# anova(pooled.time5,pooled.time5r)
# anova(pooled.time6,pooled.time6r)

# GLS
summary(GLS1 <- pggls(gdpgrowth ~ inigdp + yr.sch.secM + 
                        yr.sch.secF + llexpec + lfert + kg + ToT +
                        ki + llaw.order, 
                      data = IQM_pro_data, 
                      model = "pooling"))
summary(GLS2 <- pggls(gdpgrowth ~ inigdp + yr.sch.secM + 
                        yr.sch.secF + llexpec + lfert + kg + ToT +
                        ki + democ + democ2, 
                      data = IQM_pro_data, 
                      model = "pooling"))
summary(GLS3 <- pggls(gdpgrowth ~ inigdp + yr.sch.secM + 
                        yr.sch.secF + llexpec + lfert + kg + ToT +
                        ki + POLCONIII, 
                      data = IQM_pro_data, 
                      model = "pooling"))
summary(GLS4 <- pggls(gdpgrowth ~ inigdp + yr.sch.secM + 
                        yr.sch.secF + llexpec + lfert + kg + ToT +
                        ki + POLCONV, 
                      data = IQM_pro_data, 
                      model = "pooling"))
summary(GLS5 <- pggls(gdpgrowth ~ inigdp + yr.sch.secM + 
                        yr.sch.secF + llexpec + lfert + kg + ToT +
                        ki + POLCONVJ, 
                      data = IQM_pro_data, 
                      model = "pooling"))
summary(GLS6 <- pggls(gdpgrowth ~ inigdp + yr.sch.secM + 
                        yr.sch.secF + llexpec + lfert + kg + ToT +
                        ki + l.icrgQoG, 
                      data = IQM_pro_data, 
                      model = "pooling"))

library(estout)
## @knitr analysis2
# eststo(fixed.time_effects1)
# eststo(fixed.time_effects2)
# eststo(fixed.time_effects3)
# eststo(fixed.time_effects4)
# eststo(fixed.time_effects5)
# eststo(fixed.time_effects6)
# esttab(label = "fe3", colnumber=TRUE, var.rename=NULL, table="sidewaystable", caption = "Estimation results from fixed.time effects estimation (individual effects)", caption.top=FALSE, table.pos="p", texfontsize="\\small")
# estclear()
# # fixed.time effects, twoways
# eststo(fixed.time_effects1a)
# eststo(fixed.time_effects2a)
# eststo(fixed.time_effects3a)
# eststo(fixed.time_effects4a)
# eststo(fixed.time_effects5a)
# eststo(fixed.time_effects6a)
# esttab(label = "fe4", colnumber=TRUE, var.rename=NULL, table="sidewaystable", caption = "Estimation results from fixed.time effects estimation (individual and time effects)", caption.top=FALSE, table.pos="p", texfontsize="\\small")
# estclear()
# # FE, Time effects
# eststo(fixed.time_effects1b)
# eststo(fixed.time_effects2b)
# eststo(fixed.time_effects3b)
# eststo(fixed.time_effects4b)
# eststo(fixed.time_effects5b)
# eststo(fixed.time_effects6b)
# esttab(label = "fe5", colnumber=TRUE, var.rename=NULL, table="sidewaystable", caption = "Estimation results from fixed.time effects estimation (time effects)", caption.top=FALSE, table.pos="p", texfontsize="\\small")
# estclear()

# fixed.time effects
eststo(fixed.time2)
eststo(fixed.time3)
eststo(fixed.time4)
eststo(fixed.time5)
eststo(fixed.time1)
eststo(fixed.time6)
esttab(label = "fe9", 
       colnumber=TRUE, 
       var.rename=NULL, 
       table="table", 
       caption = "Estimation results from OLS fixed effects model with time effects", 
       caption.top=FALSE, 
       table.pos="p", 
       texfontsize="\\small",
       file = "CoE_thesis_repository/fixed.time.tab")
estclear()

# pooled.time
eststo(pooled.time2)
eststo(pooled.time3)
eststo(pooled.time4)
eststo(pooled.time5)
eststo(pooled.time1)
eststo(pooled.time6)
esttab(label = "fe6", 
       colnumber=TRUE, 
       var.rename=NULL, 
       table="table", 
       caption = "Estimation results from OLS pooling model with time effects", 
       caption.top=FALSE, 
       table.pos="p", 
       texfontsize="\\small",
       round.dec=3,
       file = "CoE_thesis_repository/pooled.time.tab")
estclear()

# GLS
eststo(GLS1)
eststo(GLS2)
eststo(GLS3)
eststo(GLS4)
eststo(GLS5)
eststo(GLS6)
esttab(label = "GLS", 
       colnumber=TRUE, 
       var.rename=NULL, 
       table="sidewaystable", 
       caption = "Estimation results from GLS regression", 
       caption.top=FALSE, 
       table.pos="p", 
       texfontsize="\\small")
estclear()
library(texreg)
texreg(GLS1)
