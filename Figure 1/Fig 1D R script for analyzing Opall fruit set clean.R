rm(list=ls())

Prop1 = 0.794 # unbagged
Prop2 = 0.675 # hand pollination
Prop3 = 0.114 # pollinator exclusion
Prop4 = 0.477 # nocturnal pollinator exclusion
Prop5 = 0.657 # diurnal pollinator exclusion

Prop=c(Prop1, Prop2, Prop3, Prop4, Prop5)
SampleSize = c(63, 40, 44, 44, 67) 
Up=NULL
Do=NULL
for (i in 1:5){
  sd = sqrt(Prop[i]*(1-Prop[i])/SampleSize[i])
  Up[i] = Prop[i]+sd
  Do[i] = Prop[i]-sd
}

LEGENDS = c("+ control", "+ pollen", "- control", 
            "night exclusion", "day exclusion")

par(mar=c(5,6,4,1)+.1)
b=barplot(Prop, ylim=c(0,0.9), col=c('olivedrab4', 'mediumturquoise', 
                                     'darkgrey', 'orange2', 'navy'), 
          ylab='Proportion of fruit set', cex.lab=1, cex.axis=1)
for (i in 1:5){
  arrows(b[i], Up[i], b[i], Do[i], code=3, angle=90)
  mtext(side=1, line=1, at = b[i], LEGENDS[i], cex=1)
  SigLabels = c("a", "a,b", "c", "b", "a,b") # significance labels
  text(x=Prop, label=SigLabels, pos=3, cex=1)
  }

#Multiple simple proportions test

# 5-sample test for equality of proportions without continuity correction 
# (do all samples have equal proportions?)
prop.test(c(5, 27, 21, 50, 44), c(44, 40, 44, 63, 67)) 

# Pairwise comparisons using Pairwise comparison of proportions (Applying correction, 
# is each pair different from each other?)
pairwise.prop.test(c(5, 27, 21, 50, 44), c(44, 40, 44, 63, 67)) 

