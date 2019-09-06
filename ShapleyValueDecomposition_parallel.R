### LOAD LIBRARIES ---
require("foreach")
require("doParallel")

### FUNCTION: ShapleyValue.Decomposition() ---
ShapleyValue.Decomposition.parallel <- function(dat, n_cores){
    # SORT GOBAL OBSERVATIONS AND ASSIGN GLOBAL RANK ---
    dat <- dat[order(dat$Observation, decreasing=FALSE),]
    dat$RankGlobal <- c(1:nrow(dat))

    # GLOBAL OBS. AND MEAN --- 
    n <- length(dat$Observation)
    m <- mean(dat$Observation)

    # GET GINI INDEX: GLOBAL ---
    G <- getGiniIndex(df=dat, analysisGlobal=TRUE, n_cores=n_cores)

    # GET GINI INDEX: BY GROUP ---
    list.group <- getGiniIndexByGroup(df=dat, n_cores=n_cores)

    # SHAPLEY DECOMPOSITION: WITHIN GROUP INEQUALITY DECOMPOSITION ---
    list.W_ineq <- getWithinGrpIneq(df=dat, list.group, n, m)

    # SHAPLEY DECOMPOSITION: BETWEEN GROUP INEQUALITY DECOMPOSITION ---
    B <- getBetweenGrpIneq(df=dat, list.group, n, m, n_cores)

    # COMPUTE OVERLAP EFFECT ---
    O <- getOverlapEffect(G=G, W=list.W_ineq$W, B=B)

    # GENERATE OUTPUT ---
    list.output <- generateOutput(G, list.group, list.W_ineq, B, O)

    return(list.output)
}

### FUNCTION: getGiniIndex() ---
getGiniIndex <- function(df, analysisGlobal, n_cores){
    # G = 2/(n^2 * m) * sum( r_i * (y_i - m) )

    if(analysisGlobal == TRUE){
        df$Rank <- df$RankGlobal
    }else {
        df$Rank <- df$RankLocal
    }
    
    n <- length(df$Observation)
    m <- mean(df$Observation)

    # var_1 ---
    var_1 <- 2/(n^2 * m)

    # PARALLELIZE HERE ---
    # Declate Cluster 
    #n_cores <- 4
    cl <- makeCluster(n_cores)
    registerDoParallel(cl)

        # var_2 --
        list.var_2 <- list()
        lpar <- foreach(i = 1:nrow(df), .combine='+') %dopar% { 
        #for(i in 1:nrow(df)){
            lpar <- df$Rank[i] * (df$Observation[i] - m) 
        }

    stopCluster(cl)

    #var_2 <- sum(unlist(list.var_2))
    var_2 <- lpar

    # Gini Index ---
    G <- var_1 * var_2

    return(G)
}

### FUNCTION: getGiniIndexByGroup() ---
getGiniIndexByGroup <- function(df, n_cores){
    # GET GROUPS ---
    k <- unique(df$Group)

    # GET GINI INDEX: LOCAL (GROUPS) ---
    list.df_k <- list()
    list.G_k <- list()
    for(ctr in 1:length(k)){
        df.temp <- subset(df, df$Group == k[ctr])
        df.temp$RankLocal <- c(1:nrow(df.temp))
        list.df_k[[k[ctr]]] <- df.temp
        list.G_k[[k[ctr]]] <- getGiniIndex(df=list.df_k[[k[ctr]]], analysisGlobal=FALSE, n_cores) 
    }

    # GROUP MEAN ---
    list.m_k <- lapply(list.df_k, function(x) mean(x$Observation))

    return(list(data_k=list.df_k, Gini_k=list.G_k, mean_k=list.m_k))
}

### FUNCTION: computeWithinGrpIneq() ---
computeWithinGrpIneq <- function(n_k, n, m_k, m, G_k){
    # W_k = V_k^2 * b_k * G_k
    # W_k = (n_k/n)^2 * (m_k/m) * G_k 

    W_k <- (n_k/n)^2 * (m_k/m) * G_k
    return(W_k)
}

### FUNCTION: getWithinGrpIneq() ---
getWithinGrpIneq <- function(df, list.group, n, m){
    # GET GROUPS ---
    k <- unique(df$Group)

    list.W_k <- list()
    for(ctr in 1:length(k)){
        n_k <- nrow(list.group$data_k[[k[ctr]]])
        m_k <- list.group$mean_k[[ctr]]
        G_k <- list.group$Gini_k[[k[ctr]]]

        list.W_k[[k[ctr]]] <- computeWithinGrpIneq(n_k, n, m_k, m, G_k)
    }

    # GET WITHIN GROUP INEQUALITY ---
    W <- sum(unlist(list.W_k))

    return(list(W=W, W_k=list.W_k))
}

### FUNCTION: getBetweenGrpIneq() ---
getBetweenGrpIneq <- function(df, list.group, n, m, n_cores){
    # B = sum_k{1,m}( b_k * V_k * [ sum_j{1,k}(V_j) - sum_j{k,m}(V_j) ] ) 

    # GET GROUPS ---
    k <- unique(df$Group)

    list.B <- list()

    # PARALLELIZE HERE ---
    # Declate Cluster 
    #n_cores <- 4
    cl <- makeCluster(n_cores)
    registerDoParallel(cl)

        lpar <- foreach(ctr1 = 1:length(k), .combine='+') %dopar% { 
            #for(ctr1 in 1:length(k)){
            m_k <- list.group$mean_k[[ctr1]]
            n_k <- nrow(list.group$data_k[[k[ctr1]]])

            # OPERATION-1
            list.n_kx <- list()
            for(ctr2 in 1:ctr1){
                list.n_kx[[ctr2]]  <- (nrow(list.group$data_k[[k[ctr2]]]) / n)
            }
            V_x <- sum(unlist(list.n_kx))

            # OPERATION-2
            list.n_ky <- list()
            for(ctr3 in ctr1:length(k)){
                list.n_ky[[ctr3]]  <- (nrow(list.group$data_k[[k[ctr3]]]) / n)
            }
            V_y <- sum(unlist(list.n_ky))

            # COMPUTE BETWEEN GROUP INEQUALITY ---
            lpar <- (m_k/m) * (n_k/n) * (V_x - V_y)
        }

    stopCluster(cl)

    list.B <- lpar

    # COMPUTE BETWEEN GROUP INEQUALITY DECOMPOSITION ---
    B <- list.B

    return(B)
}

### OVERLAP EFFECT ---
getOverlapEffect <- function(G, W, B){
    O <- G - W - B
    return(O)
}

generateOutput <- function(G, list.group, list.W_ineq, B, O){
    ### OUTPUT ---------
    # G
    # list.group$Gini_k
    # list.W_ineq$W
    # list.W_ineq$W_k
    # B
    # O
    ### OUTPUT ---------

    df.G <- data.frame(Description="Gini Index", Item="G", Value=G)
    df.W <- data.frame(Description="Within Group Inequality Decomposition", Item="W", Value=list.W_ineq$W)
    df.B <- data.frame(Description="Between Group Inequality Decomposition", Item="B", Value=B)
    df.O <- data.frame(Description="Overlap Effect", Item="O", Value=O)

    df.G_k <- data.frame(Description="Gini Index per Group", Item="G_k", 
                        Group=names(list.group$Gini_k), 
                        Value=unlist(list.group$Gini_k))

    df.W_k <- data.frame(Description="Within Group Inequality per Group", Item="W_k", 
                        Group=names(list.W_ineq$W_k), 
                        Value=unlist(list.W_ineq$W_k))

    list.output <- list(G=df.G, G_k=df.G_k, W=df.W, W_k=df.W_k, B=df.B, O=df.O)
    return(list.output)
}
