#include <Rcpp.h>
using namespace Rcpp;

//' @title
//' list2array
//' @description
//' If a given list (of length l >= 0) of numeric vectors has all vectors of the
//' same length k >= 0 this function returns a numeric matrix (k, l):
//' each column of the matrix corresponds to one element from the list.
//' If lengths of vectors in given list are not the same, a function returns
//' this list.
//' @details
//' ARGUMENTS:
//' x - a list (of length l >= 0) of numeric vectors
//'
//' RETURN VALUE:
//' a matrix or a list
//'
//' @family fungroup
//' @export
// [[Rcpp::export]]

  RObject list2array(List x) {
    int n = x.size();
    IntegerVector size(n);

    for (int i = 0; i < n; i++) {
      if (!Rf_isReal(x[i]) && !Rf_isInteger(x[i]) && !Rf_isLogical(x[i])){
        stop("Given argument is not a list of numeric vectors.");
      }
      else{
        NumericVector y = x[i];
        size[i] = y.size();
      }
    }

    for (int i = 1; i < n; i++) {
      if (size[i] != size[0]){
        return x;
        break;
      }
    }

    NumericMatrix out(size[0], n);

    for (int j = 0; j < n; j++) {
      for (int i = 0; i < size[0]; i++) {
        NumericVector y = x[j];
        out(i, j) = y[i];
      }
    }

    return out;
  }
