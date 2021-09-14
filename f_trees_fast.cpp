#include <Rcpp.h>
using namespace Rcpp;
#if !defined(WIN32) && !defined(__WIN32) && !defined(__WIN32__)
#include <unistd.h>
#include <Rinterface.h>
#endif
// [[Rcpp::plugins(cpp11)]]



// [[Rcpp::export]]
int findElement(IntegerVector x,int feat_name){
  
  const int n = x.size();
  for (int i=0; i < n;i++){
    if(x[i] == feat_name){
      return i;
    }
  }
  
  return -1;
  
}


// [[Rcpp::export]]
double f_trees_cpp(const IntegerVector &Features,const NumericVector &Feat_vals,const IntegerVector &feat_names,
                   const NumericVector &Prediction,const IntegerVector &Yes,const IntegerVector &No,
                    const NumericVector &Split,const NumericVector &Cover,int node){
  
  int feat = Features[node];
  
  if (feat == -1) {
   
    double pred = Prediction[node];
    return pred;
    
  } else { 
      
      int pos = findElement(feat_names,feat);
      if (pos >= 0) {
        double feat_val = Feat_vals[pos];
        double split_val = Split[node];
        int NextNode = Yes[node];
        if (feat_val > split_val) {
          NextNode = No[node];
        }

        return f_trees_cpp(Features,Feat_vals,feat_names,Prediction,Yes,No,Split,Cover,NextNode);
      } else {
        
        int YesNode = Yes[node];
        int NoNode = No[node];
        return f_trees_cpp(Features,Feat_vals,feat_names,Prediction,Yes,No,Split,Cover,YesNode)*Cover[YesNode] + f_trees_cpp(Features,Feat_vals,feat_names,Prediction,Yes,No,Split,Cover,NoNode)*Cover[NoNode];
        
      }
    
  }
  
}

// [[Rcpp::export]]
NumericVector f_trees_cpp_vec(const IntegerVector &Features,const NumericVector &Feat_vals,const IntegerVector &feat_names,
                              const NumericVector &Prediction,const IntegerVector &Yes,const IntegerVector &No,
                              const NumericVector &Split,const NumericVector &Cover,const IntegerVector &nodes){
  
  int tot = nodes.size();
  NumericVector all_trees (tot);
  for (int i=0; i < tot;i++){
  
    int node = nodes[i];
    double calc = f_trees_cpp(Features,Feat_vals,feat_names,Prediction,Yes,No,Split,Cover,node);
    all_trees[i] = calc;
  
  }
  
  return all_trees; 
}

// [[Rcpp::export]]
double subSAGE_per_S_linreg(const IntegerVector &Features,const DataFrame &Feat_vals_S, const DataFrame &Feat_vals_SuK, const IntegerVector &feat_names_S,
                     const IntegerVector &feat_names_SuK,const NumericVector &Prediction,const IntegerVector &Yes,const IntegerVector &No,
                     const NumericVector &Split,const NumericVector &Cover,const IntegerVector &feature_trees,const IntegerVector &roots_at_bar_trees,
                     const NumericVector &response,int &n_inds){
  
  NumericVector ret (n_inds);
  for (int j=0; j < n_inds;j++){
    
    NumericVector feat_vals_S_ind = Feat_vals_S[j];
    NumericVector feat_vals_SuK_ind = Feat_vals_SuK[j];
    
    
    double EfSuK = sum(f_trees_cpp_vec(Features,feat_vals_SuK_ind,feat_names_SuK ,Prediction,
                                       Yes,No,Split,Cover,feature_trees));
    double EfS = sum(f_trees_cpp_vec(Features,feat_vals_S_ind,feat_names_S ,Prediction,
                                     Yes,No,Split,Cover,feature_trees));
    double Ef_bartrees_S = sum(f_trees_cpp_vec(Features,feat_vals_S_ind,feat_names_S ,Prediction,
                                               Yes,No,Split,Cover,roots_at_bar_trees));
    double response_j = response[j];
    ret[j] = 2*response_j*(EfSuK-EfS) + EfS*EfS - EfSuK*EfSuK + 2*Ef_bartrees_S*(EfSuK-EfS);
    
  }
  double MeanRet = mean(ret);
  return MeanRet; 
  
}

// [[Rcpp::export]]
double subSAGE_per_S_logreg(const IntegerVector &Features,const DataFrame &Feat_vals_S, const DataFrame &Feat_vals_SuK, const IntegerVector &feat_names_S,
                     const IntegerVector &feat_names_SuK,const NumericVector &Prediction,const IntegerVector &Yes,const IntegerVector &No,
                     const NumericVector &Split,const NumericVector &Cover,const IntegerVector &feature_trees,const IntegerVector &roots_at_bar_trees,
                     const NumericVector &response,int &n_inds){

  NumericVector ret (n_inds);
  for (int j=0; j < n_inds;j++){

    NumericVector feat_vals_S_ind = Feat_vals_S[j];
    NumericVector feat_vals_SuK_ind = Feat_vals_SuK[j];


    double EfSuK = sum(f_trees_cpp_vec(Features,feat_vals_SuK_ind,feat_names_SuK ,Prediction,
                                       Yes,No,Split,Cover,feature_trees));
    double EfS = sum(f_trees_cpp_vec(Features,feat_vals_S_ind,feat_names_S ,Prediction,
                                     Yes,No,Split,Cover,feature_trees));
    double Ef_bartrees_S = sum(f_trees_cpp_vec(Features,feat_vals_S_ind,feat_names_S ,Prediction,
                                               Yes,No,Split,Cover,roots_at_bar_trees));
    double response_j = response[j];
    ret[j] = (1-response_j)*(EfS-EfSuK) + log((1+exp(-EfS-Ef_bartrees_S))/(1+exp(-EfSuK-Ef_bartrees_S)));

  }
  double MeanRet = mean(ret);
  return MeanRet;

}




