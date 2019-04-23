ILanClinDGeo <- read.csv("ClinDGeo.csv", fileEncoding = "UTF-8")
ILanClinWGeo <- read.csv("ClinWGeo.csv", fileEncoding = "UTF-8")

df1 <- ILanClinWGeo
df2 <- ILanClinDGeo
df <- df1
df$ClinND_villarea <- df2$ClinND_villarea
df$ClinND <- df2$ClinND

df <- df[df$vill_area < 2,]
var1 <- df$popDens
var2 <- df$ClinNW_villarea
var3 <- df$ClinND_villarea
var4 <- df$vill_area
var5 <- df$popN
var6 <- df$ClinNW
var7 <- df$ClinND

lm1 <- lm(var1 ~ var2 + var3 + var4 + var5 + var6 + var7, data=df)

library(effects)
plot(allEffects(lm1))

summary(ILanClinWGeo$ClinNW100_popN)
a <- ILanClinWGeo[ILanClinWGeo$ClinNW100_popN > 0.00099,]
summary(a$ClinNW100_popN)


df$var1 <- df$popDens
df$var2 <- df$ClinNW200_villarea
df$var3 <- df$vill_area
df$var4 <- df$popN
plot(df$var2, df$var1)

sat.mod <- lm(var2 ~ var1 + var3 + var4, data=df)
summary(sat.mod)
plot(sat.mod)
coef(summary(sat.mod))

library(effects)
plot(allEffects(sat.mod))

install.packages("lme4")
library(lme4)
