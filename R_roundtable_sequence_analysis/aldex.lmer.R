#documentation start
#=============================================================================
# File data
# creator: Christiane Hassenrück
# acknowledgements: ALDEx2 authors
#=============================================================================
# File contents
# modified aldex.lmer function to run more complicated glmms
#
# input: 
# data.clr - output from aldex.clr
# formulaLMER - glm formula to be used, of the form e.g. "~ var1 + var2" or "~ var1 * var2"
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


aldex.lmer <- function(data.clr, formulaLMER, env) {
  
  if (!"lme4" %in% installed.packages()) {
    install.packages("lme4")
  }
  require(lme4)
  
  if (!"lmerTest" %in% installed.packages()) {
    install.packages("lmerTest")
  }
  require(lmerTest)
  
  mc.instances <- numMCInstances(data.clr)
  feature.number <- numFeatures(data.clr)
  vars_all <- attributes(terms(as.formula(formulaLMER)))$term.labels
  vars <- vars_all[ - grep("1 |", vars_all, fixed = T)]
  nvars <- length(vars)
  feature.names <- getFeatureNames(data.clr)
  
  output <- list(p.lmer = data.frame(matrix(NA, feature.number, nvars)),
                 BH.lmer = data.frame(matrix(NA, feature.number, nvars)))
  colnames(output$p.lmer) <- vars
  colnames(output$BH.lmer) <- vars
  rownames(output$p.lmer) <- feature.names
  rownames(output$BH.lmer) <- feature.names
  
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
      F1 <- as.formula(paste("Y", formulaLMER))
      LMER <- lmer(F1, data = env)
      AOV <- anova(LMER)
      for (j in 1:nrow(AOV)) {
        output.mci[names(output.mci) == rownames(AOV)[j]][[1]][i, mc.i] <- AOV$"Pr(>F)"[j]
      }
    }
  }
  
  for(i in names(output.mci)) {
    output$p.lmer[, colnames(output$p.lmer) == i] <- apply(output.mci[[i]],1,mean)
    output$BH.lmer[, colnames(output$p.lmer) == i] <- p.adjust(output$p.lmer[, colnames(output$p.lmer) == i], method = "BH")
  }
  
  return(output)
  
}