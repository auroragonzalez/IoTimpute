# args <- commandArgs(TRUE)
# args
# eval(parse(text=args[1]))
# eval(parse(text=args[2]))
# eval(parse(text=args[3]))
# eval(parse(text=args[4]))

library("mnormt")

# lowerinput = c(-1.57, -0.9295, -0.0708)
# upperinput = c(0.63,1.2705,2.1292)
# meaninput = c(0,0,0)
# matrixinput = matrix(c(2.7746,-0.0757,0.0358,-0.0757,3.5240,1.3156,0.0358,1.3156,3.7585),3,3)

lowerinput <- as.matrix(read.csv("./lowerinput.csv", header = FALSE, sep = ","))
# lowerinput
upperinput <- as.matrix(read.csv("./upperinput.csv", header = FALSE, sep = ","))
# upperinput
meaninput <- rep(0, len = nrow(lowerinput))
matrixinput <- as.matrix(read.csv("./matrixinput.csv", header = FALSE, sep = ","))
# matrixinput

out <- sadmvn(lower=lowerinput, upper=upperinput, 
             mean = meaninput, 
             varcov= matrixinput ) 
 # print(out)

write.table(out, file = "salida.csv", row.names = FALSE, col.names = FALSE)
# Rscript myscript3.R lowerinput="c(-1.57, -0.9295, -0.0708)" upperinput="c(0.63,1.2705,2.1292)" meaninput="c(0,0,0)" matrixinput="matrix(c(2.7746,-0.0757,0.0358,-0.0757,3.5240,1.3156,0.0358,1.3156,3.7585),3,3)"