# Look here for plotting power calcs: http://www.statmethods.net/stats/power.html

# power calcs

# see http://genome.sph.umich.edu/wiki/Power_Calculations:_Quantitative_Traits
# and http://www.statmethods.net/stats/power.html

# -- Set up parameters to run loop


# range of MAF
maf <- seq(0, 0.5, 0.01); maf
n.maf <- length(maf)

# sd of trait (HDL, LDL or TG, which range from 0.3 to 0.8)
sd <- seq(0.3, 0.8, 0.1); sd
n.sd <- length(sd)

# range of effect size
beta.g = seq(0.05, 0.2, 0.025); length(beta.g); beta.g
n.beta = length(beta.g)

# --------------- obtain power in loop ------------------------------------

power.a <- array(numeric(n.maf*n.sd*n.beta), dim=c(n.beta, n.sd, n.maf))
dim(power.a) #check

N = 600
n.tests = 1
alpha = 0.05/n.tests # setting up a Bonferroni FWER control

for (i in 1:n.maf){
  for (j in 1:n.sd){
    for(k in 1:n.beta){

      allele.freq = maf[i]
      var.x = 2*allele.freq*(1-allele.freq)
      sd.trait = sd[j]
      b = beta.g[k]
      
      h2 = (b^2)*var.x/(sd.trait^2)
      
      threshold = qchisq(alpha, df=1, lower.tail=F)
      pwr = pchisq(threshold, df=1, lower.tail=F, ncp = N*h2)

      power.a[k,j,i] = round(pwr,4)
    }
  }
}

power.a # check


# ---------- set up graph ------------------



anno.f = function(b.num){
  

  xrange <- range(maf)
  yrange <- round(range(power.a),4); yrange
  sd.range = range(sd)
  beta.g.range = range(beta.g)
  
  colors = rainbow(length(sd))
  
  plot(xrange, yrange, type="n",
       xlab="Minor Allele Frequency (MAF)",
       ylab="Power" )
  
  
  # add annotation (grid lines, title, legend)
  abline(v=0, h=seq(0,yrange[2],50), lty=2, col="grey89")
  abline(h=0, v=seq(xrange[1],xrange[2],.02), lty=2,
         col="grey89")
  abline(h=0.8, col="black", lty=3, lwd=1)
  title(paste("Power calculations for beta coefficient = ", beta.g[b.num], " \n Sig=0.05 (Two-tailed)", sep=""))
  
  legend("bottomright", title="SD (trait)", as.character(sd),
         fill=colors)
  
  for (j in 1:n.sd){
    lines(maf, power.a[b.num,j,], type="l", lwd=2, col=colors[j])
  }
}

# write 2 x 2 plot to png file
# ---------------------------------------------


png("power-plot.png", width = 8, height = 8, units = 'in', res = 300)
  par(mfrow=c(3,2))
  
  # Plot for power vs maf (stratified by sd) for first value for beta.g, 0.05 ------------
  anno.f(1)

  anno.f(2)
  
  # Plot for power vs maf (stratified by sd) for first value for beta.g, 0.1 ------------
  anno.f(3)
  
  anno.f(4)

  # Plot for power vs maf (stratified by sd) for first value for beta.g, 0.15 ------------
  anno.f(5)
  
  # Plot for power vs maf (stratified by sd) for first value for beta.g, 0.2 ------------
  anno.f(7)
dev.off()

anno.f(1)

