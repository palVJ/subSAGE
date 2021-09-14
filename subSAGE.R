xgboost.trees <- function(xgb_model, data, recalculate = FALSE) {
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    stop("Package \"xgboost\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  xgbtree <- xgboost::xgb.model.dt.tree(model = xgb_model)
  stopifnot(c("Tree", "Node", "ID", "Feature", "Split", "Yes", "No", "Missing", "Quality", "Cover") %in% colnames(xgbtree))
  xgbtree$Yes <- match(xgbtree$Yes, xgbtree$ID)
  xgbtree$No <- match(xgbtree$No, xgbtree$ID)
  xgbtree$Missing <- match(xgbtree$Missing, xgbtree$ID)
  xgbtree$Decision.type <- factor(x = rep("<=", times = nrow(xgbtree)), levels = c("<=", "<"))
  xgbtree$Decision.type[is.na(xgbtree$Feature)] <- NA
  xgbtree <- xgbtree[, c("Tree", "Node", "Feature", "Decision.type", "Split", "Yes", "No", "Missing", "Quality", "Cover")]
  colnames(xgbtree) <- c("Tree", "Node", "Feature", "Decision.type", "Split", "Yes", "No", "Missing", "Prediction", "Cover")
  
  # Here we lose "Quality" information
  xgbtree$Prediction[!(xgbtree$Feature == "Leaf")] <- NA
  
  empcumdist = apply(data,2, function(x) ecdf(x))
  #Update Cover column to represent probability of ending up at the present node given being at the parent node:
  max.depth = xgb_model$params$max_depth
  for(i in 1:nrow(xgbtree)){
    
    if(xgbtree$Node[i] == 0){
      xgbtree$Cover[i] = 1
      RootNode = i
      #Record the ancestors of each node
      gens = vector(mode = "list",length = 2^{max.depth}-1)
    }else{
      #Find parent node:
      parent = RootNode + which(xgbtree$Yes[RootNode:(i-1)] == i) -1
      
      if(length(parent) == 1){
        
        #Add parent as an ancestor of node i:
        gens[[xgbtree$Node[i]+1]] = c(gens[[xgbtree$Node[parent]+1]],parent)
        
        if(xgbtree$Cover[parent] == 0){
          #If the cover of the parent node is zero, the cover of node i will also be zero (relevant in a bootstrap setting). 
          xgbtree$Cover[i] = 0
        }else{
          #Check whether any of the features of the ancestor nodes of the parent node are equal to the feature at the parent node:
          check = which(xgbtree$Feature[gens[[xgbtree$Node[parent]+1]]] == xgbtree$Feature[parent])
          if(length(check) == 0){
            xgbtree$Cover[i] = empcumdist[[xgbtree$Feature[parent]]](xgbtree$Split[parent])
          }else{
            #Find all nodes including same feature as the feature of the parent node:
            nwsf = gens[[xgbtree$Node[parent]+1]][check]
            #collect all intervals that must hold for this specific feature down to the parent node:
            ints = t(sapply(nwsf,function(x) if(xgbtree$Yes[x] %in% c(gens[[xgbtree$Node[parent]+1]],parent))
              return(c(-Inf,xgbtree$Split[x])) else return(c(xgbtree$Split[x],Inf))))
            #The condition (interval) of the feature down to the parent node that must be met:
            cond_met = c(max(ints[,1]),min(ints[,2]))
            #The condition that must be met of feature in parent node to reach node i. We already know that node i is a "Yes"-node of parent node:
            cond_met_par =  c(-Inf,xgbtree$Split[parent])
            #Compute empirical probability that an individual goes from the parent node to node i, given previous conditions of the feature at the parent node:
            cond_met_all = c(max(cond_met_par[1],cond_met[1]),min(cond_met_par[2],cond_met[2]))
            xgbtree$Cover[i] = diff(empcumdist[[xgbtree$Feature[parent]]](cond_met_all))/
              (diff(empcumdist[[xgbtree$Feature[parent]]](cond_met)))
          }
        }
      }else{
        parent = RootNode + which(xgbtree$No[RootNode:(i-1)] == i) -1
        
        #Add parent as an ancestor of node i:
        gens[[xgbtree$Node[i]+1]] = c(gens[[xgbtree$Node[parent]+1]],parent)
        
        if(xgbtree$Cover[parent] == 0){
          #If the cover of the parent node is zero, the cover of node i will also be zero (relevant in a bootstrap setting).
          xgbtree$Cover[i] = 0
          
        }else{
          #Check whether any of the features of the ancestor nodes of the parent node are equal to the feature at the parent node:
          check = which(xgbtree$Feature[gens[[xgbtree$Node[parent]+1]]] == xgbtree$Feature[parent])
          
          if(length(check) == 0){
            xgbtree$Cover[i] = 1-empcumdist[[xgbtree$Feature[parent]]](xgbtree$Split[parent])
          }else{
            #Find all nodes including same feature as the feature of the parent node:
            nwsf = gens[[xgbtree$Node[parent]+1]][check]
            #collect all intervals that must hold for this specific feature down to the parent node:
            ints = t(sapply(nwsf,function(x) if(xgbtree$Yes[x] %in% c(gens[[xgbtree$Node[parent]+1]],parent))
                                                    return(c(-Inf,xgbtree$Split[x])) else return(c(xgbtree$Split[x],Inf))))
            #The condition (interval) of the feature down to the parent node that must be met:
            cond_met = c(max(ints[,1]),min(ints[,2]))
            #The condition that needs to be met of feature in parent node to reach node i. We already know that node i is a "No"-node of parent node:
            cond_met_par =  c(xgbtree$Split[parent],Inf)
            #Compute empirical probability that an individual goes from the parent node to node i, given previous conditions of the feature at the parent node:
            cond_met_all = c(max(cond_met_par[1],cond_met[1]),min(cond_met_par[2],cond_met[2]))
            xgbtree$Cover[i] = diff(empcumdist[[xgbtree$Feature[parent]]](cond_met_all))/
                            (diff(empcumdist[[xgbtree$Feature[parent]]](cond_met)))
            
          }  
          
        }
      }
      
    }
    
  }
  
  return(xgbtree)
}

compute_statistic = function(data,other_feats,n_inds,trees,k_set,feature_trees,roots_at_bar_trees){


  f_trees = function(trees,S,node){
    feat_names = names(S)
    feat = trees$Feature[node]
    if(feat == "Leaf"){
      return(trees$Prediction[node])
    }else{
      
      if(feat %in% feat_names){
        feat_val = S[which(feat == feat_names)]
        NextNode = ifelse(feat_val <= trees$Split[node],trees$Yes[node],trees$No[node])
        return(f_trees(trees,S,NextNode))
      }else{
        return(f_trees(trees,S,trees$Yes[node])*trees$Cover[trees$Yes[node]] + f_trees(trees,S,trees$No[node])*trees$Cover[trees$No[node]])
      }
    }
  }
  
  f_trees_vec = Vectorize(FUN = f_trees,vectorize.args = "node")
  
  
  statistic = 0
  for(i in 1:1){
    feat_i = other_feats[i]
    ret = rep(0,n_inds)
    for(j in 1:n_inds){
      
      S = unlist(data[j,..feat_i])
      SuK = c(S,k_set[j,])
      EfSuK = sum(f_trees_vec(trees,SuK,feature_trees))
      EfS = sum(f_trees_vec(trees,S,feature_trees))
      Ef_bartrees_S = sum(f_trees_vec(trees,S,roots_at_bar_trees))
      
      
      ret[j] = 2*data$response[j]*(EfSuK-EfS) + EfS^2 - EfSuK^2 + 2*Ef_bartrees_S*(EfSuK-EfS)
    }
    statistic  = statistic + mean(ret)
  }

  return(statistic)
}

compute_statistic_cpp = function(data,other_feats,n_inds,Features,Prediction,Yes,No,Split,Cover,
                                 k_set,feature_trees,roots_at_bar_trees,loss){
  
  
  library(Rcpp)
  sourceCpp("~/subSAGE/f_trees_fast.cpp")
  
  feat_k_name = as.integer(rownames(k_set))
  statistic = 0
  M = length(other_feats) + 1
  for(i in 1:(M+1)){
    if(i <= M -1){
      feat_names_S = as.integer(other_feats[i])
      feat_names_SuK = as.integer(c(feat_names_S,feat_k_name))
      #The Shapley weight:
      weight = 1/(3*(M-1))
      
      temp = feat_names_S + 1
      Feat_vals_S = as.data.frame(t(data[,..temp]))
      Feat_vals_SuK = rbind(Feat_vals_S,k_set)
    }else if(i == M){
      #S = phi, the empty set symbolized as S = {-1}:
      feat_names_S = -1
      feat_names_SuK = feat_k_name
      #The Shapley weight:
      weight = 1/3
      
      Feat_vals_S = as.data.frame(t(rep(NA,nrow(data))))
      Feat_vals_SuK = as.data.frame(k_set)
    }else{
      #S = {M-1}, the set of all features in M \ {k}:
      feat_names_S = other_feats
      feat_names_SuK = c(other_feats,feat_k_name)
      #The Shapley weight:
      weight = 1/3
      
      temp = feat_names_S + 1
      Feat_vals_S = as.data.frame(t(data[,..temp]))
      Feat_vals_SuK = rbind(Feat_vals_S,k_set)
    }
  
    response = data$response
    if(loss == "RMSE"){
    	MeanRet = subSAGE_per_S_linreg(Features = Features,Feat_vals_S = Feat_vals_S, Feat_vals_SuK = Feat_vals_SuK ,feat_names_S = feat_names_S,
                            	feat_names_SuK = feat_names_SuK,Prediction = Prediction,Yes = Yes,No = No,Split = Split,Cover = Cover,
                            	feature_trees = feature_trees,roots_at_bar_trees = roots_at_bar_trees,response = response,n_inds = n_inds)

    }else if(loss == "binary:logistic"){

	MeanRet = subSAGE_per_S_logreg(Features = Features,Feat_vals_S = Feat_vals_S, Feat_vals_SuK = Feat_vals_SuK ,feat_names_S = feat_names_S,
                                feat_names_SuK = feat_names_SuK,Prediction = Prediction,Yes = Yes,No = No,Split = Split,Cover = Cover,
                                feature_trees = feature_trees,roots_at_bar_trees = roots_at_bar_trees,response = response,n_inds = n_inds)

    }
    
    statistic  = statistic + weight*MeanRet
  }
  
  return(statistic)
}


subSage = function(data,trees,feature){
  
  library(data.table)
  #Unique feature in the trees:
  unique_feats = unique(trees$Feature)
  #Number of features in trees (subtract the Leaf feature):
  M = length(unique_feats)-1
  #All features included in the trees except feature of interest:
  other_feats = unique_feats[-c(which(unique_feats == "Leaf"),which(unique_feats == feature))]
  #Find all trees including feature, and find the root node of these trees:
  feat_in_trees = unique(trees$Tree[which(trees$Feature == feature)])
  feature_trees = which(trees$Tree %in% feat_in_trees & trees$Node == 0)
  #All trees not including feat_in_trees:
  all_trees = 0:trees$Tree[nrow(trees)]
  bar_trees = all_trees[!(all_trees %in% feat_in_trees)]
  roots_at_bar_trees = which(trees$Tree %in% bar_trees & trees$Node == 0)

  
  #k_set:
  k_set = as.matrix(data[,..feature])

  n_inds = nrow(data)

  b = compute_statistic(data,other_feats,n_inds,trees,k_set,feature_trees,roots_at_bar_trees)

  
  return(b)
}



subSage_cpp = function(data,trees,feature,loss){
  
  library(data.table)
  #Unique feature in the trees:
  unique_feats = unique(trees$Feature)
  
  #Number of features in trees (subtract the Leaf feature):
  M = length(unique_feats)-1
  
  #All features included in the trees except feature of interest:
  other_feats = unique_feats[-c(which(unique_feats == "Leaf"),which(unique_feats == feature))]
  #Adjust for c++ code beginning at 0 in vectors:
  other_feats = match(other_feats, colnames(data)) - 1
  
  #Find all trees including feature, and find the root node of these trees:
  feat_in_trees = unique(trees$Tree[which(trees$Feature == feature)])
  feature_trees = which(trees$Tree %in% feat_in_trees & trees$Node == 0)
  #Adjust for c++ code beginning at 0 in vectors:
  feature_trees = as.integer(feature_trees -1)
  #All trees not including feat_in_trees:
  all_trees = 0:trees$Tree[nrow(trees)]
  bar_trees = all_trees[!(all_trees %in% feat_in_trees)]
  roots_at_bar_trees = which(trees$Tree %in% bar_trees & trees$Node == 0)
  #Adjust for c++ code:
  roots_at_bar_trees = roots_at_bar_trees - 1
  
  #k_set:
  k_set = as.data.frame(t(data[,..feature]))
  rownames(k_set) = match(feature,colnames(data))-1
  
  
  n_inds = nrow(data)
  
  Features = trees$Feature
  Features = as.integer(match(Features, colnames(data)) - 1)
  Features[is.na(Features)] = -1
  
  Prediction = trees$Prediction
  Yes = as.integer(trees$Yes -1)
  No = as.integer(trees$No - 1)
  Split = trees$Split
  Cover = trees$Cover
  
  if(loss == "RMSE"){
  	b = compute_statistic_cpp(data,other_feats,n_inds,Features,Prediction,Yes,No,Split,Cover,k_set,feature_trees,roots_at_bar_trees,loss = "RMSE")
  }else if(loss == "binary:logistic"){
	b = compute_statistic_cpp(data,other_feats,n_inds,Features,Prediction,Yes,No,Split,Cover,k_set,feature_trees,roots_at_bar_trees,loss = "binary:logistic")
  }
    
  return(b)
}


