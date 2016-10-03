#documentation start
#=============================================================================
# File data
# creator: Christiane Hassenrück
# acknowledgements: ALDEx2 authors
#=============================================================================
# File contents
# modified aldex.glm function to run more complicated glms
#
# input: 
# data.clr - output from aldex.clr
# formulaGLM - glm formula to be used, of the form e.g. "~ var1 + var2" or "~ var1 * var2"
# env - data.frame containig the input variables for formulaGLM
#
# output:
# list with 2 data.frame containing p-values and BH-adjusted p-values for each term in the model formula 
#  (p.adjust run separately for each term)
# 
# dependencies:
# ALDEx2
#=============================================================================
#documentation end


aldex.glm2 <- function(data.clr, formulaGLM, env) {
  
  mc.instances <- numMCInstances(data.clr)
  feature.number <- numFeatures(data.clr)
  vars <- attributes(terms(as.formula(formulaGLM)))$term.labels
  nvars <- length(vars)
  feature.names <- getFeatureNames(data.clr)
  
  output <- list(p.glm = data.frame(matrix(NA, feature.number, nvars)),
                 BH.glm = data.frame(matrix(NA, feature.number, nvars)))
  colnames(output$p.glm) <- vars
  colnames(output$BH.glm) <- vars
  rownames(output$p.glm) <- feature.names
  rownames(output$BH.glm) <- feature.names
  
  output.mci <- vector("list", length = nvars)
  names(output.mci) <- vars
  for (i in 1:nvars) {
    output.mci[[i]] <- matrix(NA, feature.number, mc.instances)
  }
  
  for (mc.i in 1:mc.instances) {
    print(mc.i)
    t.input <- sapply(getMonteCarloInstances(data.clr), function(y) { y[, mc.i] })
    for (i in 1:nrow(t.input)) {
      Y <- t.input[i, ]
      F1 <- as.formula(paste("Y", formulaGLM))
      GLM <- glm(F1, data = env)
      AOV <- anova(GLM,test = "F")
      for (j in 2:nrow(AOV)) {
        output.mci[names(output.mci) == rownames(AOV)[j]][[1]][i, mc.i] <- AOV$"Pr(>F)"[j]
      }
    }
  }
  
  for(i in names(output.mci)) {
    output$p.glm[, colnames(output$p.glm) == i] <- apply(output.mci[[i]],1,mean)
    output$BH.glm[, colnames(output$p.glm) == i] <- p.adjust(output$p.glm[, colnames(output$p.glm) == i], method = "BH")
  }
  
  return(output)
  
}