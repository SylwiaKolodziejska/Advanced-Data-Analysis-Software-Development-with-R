#include <Rcpp.h>
using namespace Rcpp;

//' @title
//' freq_value
//' @description
//' This function determines the most frequently occurring value in an integer
//' vector (mode). If the mode is ambiguous, a function returns any mode.
//' @details
//' ARGUMENTS:
//' x - a integer vector
//'
//' RETURN VALUE:
//' a integer value
//'
//' @family fungroup
//' @export
// [[Rcpp::export]]

 String freq_value(IntegerVector x) {
    Function R_factor("factor");
    x = R_factor(x);
    CharacterVector levels = x.attr("levels");

    int nl = levels.size(), nx = x.size();
    IntegerVector out(nl);

    for (int i=0; i<nx; ++i) {
      out[x[i]-1]++;
    }

    int index = 0;
    int temp = out[0];

    for (int i=0; i<nl; ++i) {
      if(temp < out[i]) {
         temp = out[i];
         index = i;
      }
    }

    return levels[index];
 }
