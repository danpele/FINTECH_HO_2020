#include <Rcpp.h>
#include <cmath>
#include <algorithm>

using namespace Rcpp;

// [[Rcpp::export]]
DataFrame getEqL3(DataFrame x, 
                  float DFL = 1.0, 
                  double X  = 0.01, 
                  double KT = 0.0125) {
  
  unsigned int n = x.nrow();
  NumericVector strike = (x["strike"]);
  NumericVector mid = (x["mid"]);
  NumericVector declaredExpDate = (x["declaredExpDate"]);
  NumericVector expiration_price = (x["expiration.price"]);
  NumericVector quote_datetime = (x["quote_datetime"]);
  NumericVector active_underlying_price = (x["active_underlying_price"]);
  NumericVector rfr = (x["L3M"]);
  
  NumericVector KTs(n);
  NumericVector portfolio_val(n);
  IntegerVector opt_qty(n);
  NumericVector opt_val(n);
  NumericVector S_opt_val(n);
  NumericVector opt_val_delta(n);
  NumericVector eqL_before_rfr(n);
  NumericVector cash(n);
  NumericVector rfr_interests(n);
  NumericVector eqL(n);
  NumericVector eqL_delta(n);
  
  portfolio_val[0]	 = 100000;
  opt_qty[0]	       = (-1) * int(X * portfolio_val[0] / (mid[0] + KT) * DFL);
  KTs[0]             = KT * abs(opt_qty[0]);
  opt_val[0]	       = opt_qty[0] * mid[0];
  S_opt_val[0]       = opt_qty[0] * strike[0];
  opt_val_delta[0]	 = 0;
  eqL_before_rfr[0]  = portfolio_val[0] - KTs[0];
  cash[0]	           = eqL_before_rfr[0] + opt_val[0];
  rfr_interests[0]	 = 0;
  eqL[0]	           = eqL_before_rfr[0];
  eqL_delta[0]       = 0;
  
  
  for (unsigned int i = 1; i < n; i++) {
    portfolio_val[i] = eqL[i - 1] + opt_qty[i - 1] * (mid[i] - mid[i - 1]);
    opt_qty[i] = (declaredExpDate[i] == declaredExpDate[i - 1]) ? opt_qty[i - 1] : (-1) * int(X * portfolio_val[i] / (mid[i] + KT) * DFL);
    opt_val[i] = opt_qty[i] * mid[i];
    S_opt_val[i]       = opt_qty[i] * strike[i];
    opt_val_delta[i]	 = 
      (declaredExpDate[i] == declaredExpDate[i - 1]) ? 
      opt_val[i] - opt_val[i - 1] : 
      opt_qty[i - 1] * 
        ( ((strike[i - 1] > expiration_price[i - 1]) ? strike[i - 1] - expiration_price[i - 1] : 0) - mid[i - 1]) ;
    KTs[i]             = (declaredExpDate[i] == declaredExpDate[i - 1]) ? 0 : KT * abs(opt_qty[i]);
    eqL_before_rfr[i]  = opt_val_delta[i] + eqL[i - 1] - KTs[i];
    cash[i]	           = eqL_before_rfr[i] + opt_val[i];
    rfr_interests[i]	 = cash[i - 1] * pow(1 + rfr[i], 
                                          (quote_datetime[i] - quote_datetime[i - 1]) / double(365 * 1440 * 60)) - cash[i - 1];
    eqL[i]	           = eqL_before_rfr[i] + rfr_interests[i];
    eqL_delta[i]       = eqL[i] - eqL[i - 1];
    
  }
  
  return DataFrame::create(_["quote_datetime"]   = quote_datetime, 
                           _["strike"]           = strike,
                           _["mid"]              = mid,
                           _["declaredExpDate"]  = declaredExpDate,
                           _["expiration_price"] = expiration_price,
                           _["portfolio_val"]    = portfolio_val,
                           _["opt_qty"]          = opt_qty,
                           _["opt_val"]          = opt_val,
                           _["S_opt_val"]        = S_opt_val,
                           _["opt_val_delta"]    = opt_val_delta,
                           _["eqL_before_rfr"]   = eqL_before_rfr,
                           _["cash"]             = cash,
                           _["rfr"]              = rfr,
                           _["rfr_interests"]    = rfr_interests,
                           _["eqL"]              = eqL,
                           _["eqL_delta"]        = eqL_delta,
                           _["active_underlying_price"] = active_underlying_price
  );
  
}


