#' Check if synthetic variable is in convex hull
#'
#' Check if synthetic variable is in convex hull.
#'
#' @param y.actual The actual (target) data. Default is SCUL.output$y.actual.
#' @param y.scul Synthetic data created by SCUL procedure. Default is SCUL.output$y.scul.
#' @param TreatmentBeginsAt  Default is  SCUL.output$TreatmentBeginsAt.
#' @param coef.exact The sparse matrix of coefficients used to construc the synthetic estimate. 
#'                   Default is SCUL.output$coef.exact.
#' @param x.DonorPool The (T by K) donor pool matrix. Default is SCUL.input$x.DonorPool.
#'         
#' @return indicator A binary indicator if scul prediction is in convex hull
#' @export
ConvexHullCheck <- function(
                              coef.exact=SCUL.output$coef.exact,
                              x.DonorPool = SCUL.input$x.DonorPool,
                              y.scul = SCUL.output$y.scul,
                              y.actual = SCUL.output$y.actual,
                              TreatmentBeginsAt = SCUL.output$TreatmentBeginsAt
                            ) {

  # Remove any columns if any elements are missing# Check for membership in the convex hull #
  ## Is the synthetic prediction within the convex hull of the variables composing it? ##
  #This is pretty easy to do in 1-D space. Essentially the synthetic unit needs to lie within the min and max of the surviving donor variables in each time period. 
  
  # Get the set of variables that survived the lasso
  
  # Create a copy of the coefficient matrix
  SurvivingVariables<-as.matrix(coef.exact[-1,])
  
  # If it's non-zero, just replace it with one
  SurvivingVariables[abs(SurvivingVariables)>0]<-1
  
  SurvivingValue <- sweep(t(x.DonorPool),MARGIN=1,FUN="*",STATS=SurvivingVariables)
  SurvivingValue <- t(SurvivingValue)
  
  ## Remove zero contributions
  SurvivingValue<-SurvivingValue[ , !apply(SurvivingValue==0,2,all)]
  
  
  ## Create a matrix of zeros
  ConvexHullChecker <- matrix(0L, nrow = (nrow(y.actual)-TreatmentBeginsAt+1), ncol = 3)
  
  
  ## For each time period is the value of the synthetic unit in between the minimum and maximum
  for (i in 1:(nrow(y.actual)-TreatmentBeginsAt+1)){
    j <- i + TreatmentBeginsAt-1
    if (y.scul[j]>min(SurvivingValue[j,])) {
      ConvexHullChecker[i,1]<-1
    }
    
    if (y.scul[j]<max(SurvivingValue[j,])) {
      ConvexHullChecker[i,2]<-1
    }
    if (ConvexHullChecker[i,1]==1 & ConvexHullChecker[i,2]==1)  {
      ConvexHullChecker[i,3] <-1
    }
  }
  
  ## Below will equal one if the synthetic control unit is in the convex hull
  InConvexHull <- min(ConvexHullChecker[,3])
  
  return(InConvexHull)
}
