# Position of A
xA=c(-0.7,-0.6)
yA=c(0.4,0.5)

# Position of B
xB=c(0.1,0.5)
yB=c(0.3,0.4)

# Position of C
xC=c(-0.1,0.1)
yC=c(-0.3,-0.1)

##
# Rectangles
##
plot_rectangles = function() {
  # Plot rectangles
  rect(xA[1],yA[1],xA[2],yA[2], lwd=0.5, border="black")
  rect(xB[1],yB[1],xB[2],yB[2], lwd=0.5, border="black")
  rect(xC[1],yC[1],xC[2],yC[2], lwd=0.5, border="black")
  
  # Text
  text(mean(xA), mean(yA), "A", cex = .8)
  text(mean(xB)-0.1, mean(yB), "B", cex = .8)
  text(mean(xC), mean(yC), "C", cex = .8)
}

##
# Arrows
##
write_arrow = function(x, y, curve = curve) {
  iArrows(x[1], x[2], y[1], y[2],
          h.lwd=1, sh.lwd=1, sh.col="black",
          curve=curve, width=1, size=0.7)
}

plot_arrows = function() {
  shift=0.01
  
  xFlecheCA=c(xC[1]-shift,mean(yC))
  yFlecheCA=c(mean(xA),yA[1]-shift)
  
  xFlecheAB=c(xA[2]+shift,mean(yA))
  yFlecheAB=c(xB[1]-shift,mean(yB))
  
  xFlecheBC=c(mean(xB), yB[1]-shift)
  yFlecheBC=c(xC[2]+shift, yC[2]-shift)
  
  xFlecheC=c(xC[2]-5*shift, yC[1]-shift)
  yFlecheC=c(xC[1]+5*shift, yC[1]-shift)
  
  xFlecheB=c(xB[1]+15*shift, yB[2]+shift)
  yFlecheB=c(xB[2]-15*shift, yB[2]+shift)
  
  xFlecheA=c(xA[1]-shift, yA[1]+shift)
  yFlecheA=c(xA[1]-shift, yA[2]-shift)
  
  write_arrow(xFlecheCA, yFlecheCA, curve = 0)
  write_arrow(xFlecheAB, yFlecheAB, curve = 0)
  write_arrow(xFlecheBC, yFlecheBC, curve = 0)
  write_arrow(xFlecheC, yFlecheC, curve = 2)
  write_arrow(xFlecheB, yFlecheB, curve = 2)
  write_arrow(xFlecheA, yFlecheA, curve = 2)
  
  text(-0.4, 0.05, "p", cex = 1.2)
  text(-0.2, 0.45, "p", cex = 1.2)
  text(0.25, 0.05, "p", cex = 1.2)
  
  text(0, -0.425, "1-p", cex = 1.2)
  text(-0.8, 0.5, "1-p", cex = 1.2)
  text(0.3, 0.525, "1-p", cex = 1.2)
}