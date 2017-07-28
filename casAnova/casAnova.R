##################################################################
#
#
#   Function to fit CAS-ANOVA method of Bondell and Reich (2009)
#
#
#
#####################################################################


CasANOVA=function(formula, lambdas = seq(0.1,5,0.1), eps = 10^{-6})
{
    require(MASS)
    lambdas = sort(lambdas)
    if (min(lambdas) <=0) {return(cat("ERROR: All values for lambda must be > 0. \n"))}
    if (eps <=0) {return(cat("ERROR: Eps must be > 0. \n"))}
    init.fit = lm(formula, x=T, y=T)
    n.terms = length(attr(terms(init.fit),"dataClasses")[-1])
    n.levels = as.vector(2*(attr(terms(init.fit),"dataClasses")[-1]=="numeric"))
    if (min(n.levels)==0) {n.levels[n.levels==0] = as.vector(sapply(init.fit$xlevels,length))}
    n.factors = length(n.levels)
    total.p = sum(n.levels-1)
    full.x = init.fit$x
    if (ncol(full.x)>nrow(full.x)) {return(cat("ERROR: Number of predictors (including dummy variables created for factors) \n cannot exceed the number of observations. \n"))}
    
    n = length(y)
    
    
    # Create Difference Matrix
    
    for (j in 1:n.factors)
    {
        p = n.levels[j]-1
        qp = p*(p-1)/2
        
        if (qp==0) temp.dm=matrix(0,nrow=1,ncol=1)
        if (qp>0)
        {
            v1=rep(0,qp)
            c1=qp
            c2=0
            for(w in 1:(p-1))
            {
                c1 = c1-w
                c2 = c2+(w-1)
                v1 = cbind(c(rep(0,c1),rep(1,w),rep(0,c2)),v1)
            }
            v2 = matrix(0,qp,p)
            c1 = 1
            c2 = p-1
            for (w in 1:(p-1))
            {
                v2[c1:c2,(w+1):p] = diag(p-w)
                c1 = c2+1
                c2 = c2+p-w-1
            }
            temp.dm = v1 - v2
            
        }
        if (j==1) dm = temp.dm
        else 
        {	 
            topleft = dm 
            topright = matrix(0, nrow(dm), ncol(temp.dm)) 
            lowleft = matrix(0, nrow(temp.dm), ncol(dm)) 
            lowright = temp.dm 
            dm = rbind(cbind(topleft, topright), cbind(lowleft, lowright)) 
        } 
        
    }		
    dm = dm[apply(abs(dm),1,sum)>0,]
    if (length(dm) == 0) {dm = matrix(0,nrow=1,ncol=total.p)}
    if (length(dm) == total.p) {dm = matrix(dm,nrow=1)}
    
    beta.mle = init.fit$coeff
    
    beta.wt = beta.mle[-1]
    abs.wt.beta = abs(beta.wt)
    abs.wt.diff = abs(dm %*% beta.mle[-1])		
    
    x.sc.mat = full.x[,2:(total.p+1)] %*% t(ginv(cbind(diag(total.p), t(dm))))
    sc.vec = sqrt(apply(x.sc.mat^2,2,sum))
    
    a.sc.vec = as.vector(sc.vec / c(abs.wt.beta, abs.wt.diff))
    
    full.mat = NULL
    beta.t = beta.mle
    
    for(lambda in lambdas)
    {		
        beta.eps = rep(1,length(beta.wt)) 
        while(max(beta.eps)>eps)
        {
            beta.tilde = beta.t[-1]
            abs.tilde = abs(beta.tilde)
            abs.diff = as.vector(abs(dm %*% beta.tilde))
            if (length(abs.diff)<2) {quad.mat.2 = a.sc.vec[-(1:total.p)]/(abs.diff+eps)} else {quad.mat.2 = diag(a.sc.vec[-(1:total.p)]/(abs.diff+eps))}
            if (length(abs.tilde)<2) {quad.mat.1 = a.sc.vec[1:total.p]/(abs.tilde+eps)} else {quad.mat.1 = diag(a.sc.vec[1:total.p]/(abs.tilde+eps))}
            
            if (length(abs.diff)==1) {quad.mat.2 = 0}
            penalty.mat = lambda*(quad.mat.1 + t(dm) %*% quad.mat.2 %*% dm)
            Sigma.Mat = t(full.x) %*% full.x + rbind(0,cbind(0,penalty.mat))
            beta.t = solve(Sigma.Mat) %*% t(full.x) %*% y
            beta.eps = abs(beta.tilde-beta.t[-1])/(abs(beta.tilde)+eps)
        }     
        df = sum((unique(abs(round(beta.t,6)))!=0))
        bic = n*log(sum((y-full.x %*% beta.t)^2))+log(n)*df
        full.mat = rbind(full.mat,c(beta.t,bic,df))
    }
    
    BIC.loc = which.min(full.mat[,(total.p+2)])
    if (lambdas[BIC.loc] == max(lambdas)) {cat("Note: The chosen lambda is the largest of the input grid. \n You should try larger values of lambda \n to continue the search for the minimum BIC. \n")}
    if (lambdas[BIC.loc] == min(lambdas)) {cat("Note: The chosen lambda is the smallest of the input grid. \n You should try smaller values of lambda \n to continue the search for the minimum BIC. \n")}
    BIC.coefs = round(full.mat[BIC.loc, 1:(total.p+1)],6)
    names(BIC.coefs) = colnames(full.x)
    fit = NULL
    fit$coefficients = BIC.coefs
    fit$lambda.grid = lambdas
    fit$BIC = full.mat[, total.p+2]
    fit$df = full.mat[, total.p+3]
    fit$lambda = lambdas[BIC.loc]
    fit$fitted = full.x %*% BIC.coefs
    fit$init.coefs = init.fit$coef
    
    
    return(fit)
    
}
