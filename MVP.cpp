// Agron Velovic

# include<iostream>
# include<vector>
# include<fstream>
# include <cmath>
# include <map>
# include "matrix.h"
# include "DataExtraction.h"
using namespace std;


int main(){
  int size, column, i, j;
  vector<vector<double>> returns, covariance, invcovariance, transreturns;
  vector<string> tickers;
  vector<double> invCovOnes, minWeights;
  double money, sum, sum1, sum2, sum3;
  int transaction, shortsell, desired;

  // some user input
  cout<<"How many stocks would you like to sample from (100 maximum): "<<endl;
  cin>>size;
  cout<<"How many days would you like to sample from: "<<endl;
  cin>>column;
  cout<<"How much money would you like to invest: ";
  cin>>money; cout<<endl;
  cout<<"Is there a transaction fee (enter 1 for yes or 0 for no): ";
  cin>>transaction; cout<<endl;
  cout<<"Would you like to have a desired return (enter 1 for yes and 0 for no): ";
  cin>>desired; cout<<endl;
  cout<<"Would you like to short sell (enter 1 for yes and 0 for no): ";
  cin>>shortsell; cout<<endl;


  tickers.reserve(size);
  tickerName(tickers, size);

  returns.reserve(size);
  getVectorOfReturn(returns,tickers, size, column);

  // we must transpose the returns matrix because we want to correlate the stocks
  // not the days
  transreturns = Transpose(returns);

  covariance = Covariance(transreturns);

  invcovariance = Inverse(covariance);

  // we will allocate space for the column sum of the inverse of covariance matrix
  // vector
  for(i = 0; i<size; ++i){
    invCovOnes.push_back(0);
  }

  // this is the total sum of the inverse of the covariance matrix
  sum = 0;
  for(i = 0; i < size; i++){
    for(j = 0; j<size; j++){
      sum += invcovariance[i][j];
    }
  }

  // this is the column sum of the invers of the covariance matrix
  for(i = 0; i < size; i++){
    sum1 = 0;
    for(j = 0; j < size; j++){
      sum1 += invcovariance[j][i];
    }
    invCovOnes[i] = sum1;
  }

  sum2 = 1/sum;
  minWeights = Scalar_Multiple_Vector(sum2, invCovOnes);


  //////////////////////////////////////////////////////////////////////////////
  //minimum variance with shortselling and no desired return
  if((shortsell ==0) && (desired ==0)){
    int negative = 0, negative_index, indicator, fast, deleted = 1;
    vector<double> posInvCovOnes;
    vector< vector<double> > posCovariance, posInvCovariance, posReturns, posTransReturns;
    double smallest;

    cout<<"Would you lke to dump the negative weights the fast way or the slow way"<<endl;
    cout<<"Enter 1 for fast way and 0 for slow way: ";
    cin>>fast;cout<<endl;

    // first we will see if there are any minimum weights
    for(i = 0; i<size; ++i){
      if(minWeights[i] < 0){
        negative = 1;
      }
    }

    if(fast == 1 && negative == 1){
      sum = 0;
      for(i =0; i<size; ++i){
        if(minWeights[i] < 0){
        minWeights[i] = 0;
        }
        sum += minWeights[i];
      }

      for (i = 0; i<size; ++i){
        minWeights[i] = minWeights[i]/sum;
      }
    }

    else if(negative == 1 && fast == 0){
    // this way is very computationally slow
    // this loop will stop when all the weights are positive or there is only
    // one stock left, we will essentially delete the lowest weight and recalculate
    // the minweights until all the weights are positive, at the end we are usually
    // left with only a handful of stocks
    while((size>1) && (negative==1)){
      smallest = 0; //or should i say small negative

      // first we will find the most negative weight delete it and store its index
      for(i = 0; i<size; ++i){
        if(minWeights[i] < smallest){
          smallest = minWeights[i];
          negative_index = i;
        }
      }
      // we will delete the ticker corresponding to the smallest ticker essentially
      // deleting it from our matrix of returns
      tickers.erase(tickers.begin() + negative_index);
      --size;
      if(deleted==1){cout<<deleted<<" stock was deleted"<<endl;}
      else{cout<<deleted<<" stocks were deleted"<<endl;}
      ++deleted;
      // now we will find the returns of whats left
      posReturns.reserve(size);
      getVectorOfReturn(posReturns,tickers, size, column);

      posTransReturns = Transpose(posReturns);

      posCovariance = Covariance(posTransReturns);
      posInvCovariance = Inverse(posCovariance);

      // we will allocate space for the column sum of the inverse of covariance matrix
      for(i = 0; i<size; ++i){
        posInvCovOnes.push_back(0);
      }

      // this is the total sum of the inverse of the covariance matrix
      sum = 0;
      for(i = 0; i < size; i++){
        for(j = 0; j< size; j++){
          sum += posInvCovariance[i][j];
        }
      }

      // this is the column sum of the invers of the covariance matrix
      for(i = 0; i < size; i++){
        sum1 = 0;
        for(j = 0; j < size; j++){
          sum1 += posInvCovariance[j][i];
        }
        posInvCovOnes[i] = sum1;
      }


      sum2 = 1/sum;
      vector<double>().swap(minWeights);
      minWeights = Scalar_Multiple_Vector(sum2, posInvCovOnes);

      negative = 0;
      for(i = 0; i<size; ++i){
        if(minWeights[i] < 0){
          negative = 1;
        }
      }

      // lastly we will free up memory from all the vectors so that we can use
      // them again
      vector<double>().swap(posInvCovOnes);
      vector< vector<double> >().swap(posCovariance);
      vector< vector<double> >().swap(posInvCovariance);
      vector< vector<double> >().swap(posReturns);
      vector< vector<double> >().swap(posTransReturns);

    }
  }

    // in these loops we will put a 0 where our index was deleted and the weight where
    // it was not

    if(size == 1){
      minWeights[0] = 1;
    }

  }


  //////////////////////////////////////////////////////////////////////////////
  //minimum variance with desired return and short selling
  else if((desired == 1) && (shortsell == 1)){
  vector<vector<double> > mcminus1, M, invM;
  vector< double> M1;
  double mu, a, b;
  cout<<"please enter your desired return in decimals (in decimals): ";
  cin>>mu;
  cout<<endl;


  vector<double> v;
  v.push_back(mu);
  v.push_back(1);

  // allocate space for
  vector<double> mean;
  for(i =0; i<size; i++){
    mean.push_back(0);
  }

  // we will find the mean return of every stock
  for(i = 0; i<size; i++){
    for(j = 0; j<column; j++){
      mean[i]+= returns[i][j] / column;
    }
  }

  // allocate space for M vector
  for(i =0; i<2; i++){
    vector<double> temp;
    for(j = 0; j<2; ++j){
      temp.push_back(0);
    }
    M.push_back(temp);
  }

  // this will multiply every column in the inverse covariance matrix with its
  //corresponding stock mean
  vector<double> mc;
  for(i = 0; i<size; ++i){
    mc.push_back(0);
    for(j = 0; j<size; ++j){
      mc[i] += invcovariance[j][i]*mean[j];
    }
  }

  // this will multiply every entry in the inverse covariance matrix with the mean
  // vector
  double mcmt = 0;
  for(i =0; i< size; ++i){
    mcmt += mc[i]*mean[i];
  }

  // this will multiply every column in the inverse covariance matrix with the mean
  // vector and then add up the rows
  double mcut = 0;
  for(i = 0; i<size; ++i){
    mcut += mc[i];
  }

  // this will add up every entry in the inverse covariance matrix
  double ucut = 0;
  for(i =0; i<size; ++i){
    for(j = 0; j<size; ++j){
      ucut += invcovariance[i][j];
    }
  }

  // we are putting these values in the correspong spot in the M matrix
  M[0][0] = mcmt;
  M[0][1] = mcut;
  M[1][0] = mcut;
  M[1][1] = ucut;

  // we will find the inverse of the covariance matrix
  invM = Inverse(M);

  M1.push_back(   (v[0]*invM[0][0]) +  (v[1]*invM[1][0])      );
  M1.push_back(   (v[0]*invM[0][1]) +  (v[1]*invM[1][1])      );


// this will add up every column in the inverse covariance matrix
  vector<double> uc;
  for(i = 0; i<size; ++i){
    uc.push_back(0);
    for(j = 0; j<size; ++j){
      uc[i] += invcovariance[j][i];
    }
  }
  // we will put the mc and uc vector in a matrix
  mcminus1.push_back(mc);
  mcminus1.push_back(uc);

  // this will calculate the minimum variance portfolio with the desired return
  for(i = 0; i < size; ++i){
    a = M1[0]*mcminus1[0][i];
    b = M1[1]*mcminus1[1][i];
      minWeights[i] = a+b;
    }

  }

  //////////////////////////////////////////////////////////////////////////////
  //minimum variance with no short selling and desired return
  else if((shortsell == 0) && (desired == 1)){
    vector<vector<double> > mcminus1, M, invM;
    vector< double> M1;
    double mu, a, b;
    cout<<"Please enter your desired return (in decimals): ";
    cin>>mu;
    cout<<endl;

    vector<double> v;
    v.push_back(mu);
    v.push_back(1);

    // allocate space for
    vector<double> mean;
    for(i =0; i<size; i++){
      mean.push_back(0);
    }

    // we will find the mean return of every stock
    for(i = 0; i<size; i++){
      for(j = 0; j<column; j++){
        mean[i]+= returns[i][j] / column;
      }
    }

    // allocate space for M vector
    for(i =0; i<2; i++){
      vector<double> temp;
      for(j = 0; j<2; ++j){
        temp.push_back(0);
      }
      M.push_back(temp);
    }

    // this will multiply every column in the inverse covariance matrix with its
    //corresponding stock mean
    vector<double> mc;
    for(i = 0; i<size; ++i){
      mc.push_back(0);
      for(j = 0; j<size; ++j){
        mc[i] += invcovariance[j][i]*mean[j];
      }
    }

    // this will multiply every entry in the inverse covariance matrix with the mean
    // vector
    double mcmt = 0;
    for(i =0; i< size; ++i){
      mcmt += mc[i]*mean[i];
    }

    // this will multiply every column in the inverse covariance matrix with the mean
    // vector and then add up the rows
    double mcut = 0;
    for(i = 0; i<size; ++i){
      mcut += mc[i];
    }

    // this will add up every entry in the inverse covariance matrix
    double ucut = 0;
    for(i =0; i<size; ++i){
      for(j = 0; j<size; ++j){
        ucut += invcovariance[i][j];
      }
    }

    // we are putting these values in the correspong spot in the M matrix
    M[0][0] = mcmt;
    M[0][1] = mcut;
    M[1][0] = mcut;
    M[1][1] = ucut;

    // we will find the inverse of the covariance matrix
    invM = Inverse(M);

    M1.push_back(   (v[0]*invM[0][0]) +  (v[1]*invM[1][0])      );
    M1.push_back(   (v[0]*invM[0][1]) +  (v[1]*invM[1][1])      );


  // this will add up every column in the inverse covariance matrix
    vector<double> uc;
    for(i = 0; i<size; ++i){
      uc.push_back(0);
      for(j = 0; j<size; ++j){
        uc[i] += invcovariance[j][i];
      }
    }
    // we will put the mc and uc vector in a matrix
    mcminus1.push_back(mc);
    mcminus1.push_back(uc);

    // this will calculate the minimum variance portfolio with the desired return
    for(i = 0; i < size; ++i){
      a = M1[0]*mcminus1[0][i];
      b = M1[1]*mcminus1[1][i];
        minWeights[i] = a+b;
      }
    sum = 0;
    for(i =0; i<size; ++i){
      if(minWeights[i] < 0){
      minWeights[i] = 0;
      }
      sum += minWeights[i];
    }

    for (i = 0; i<size; ++i){
      minWeights[i] = minWeights[i]/sum;
    }
  }


  //////////////////////////////////////////////////////////////////////////////
  //minimum variance with transacation fee:

  if(transaction == 1){
    sum = 0;
    double fee;
    cout<<"What is the transacation fee in dollars: ";
    cin>>fee;
    cout<<endl;
    // the transacation fee will only account for 2% of our investment
    double small = fee/(money*.02);

    for(i = 0; i<size; ++i){
      if(minWeights[i] < small){
        minWeights[i] = 0;
      }
      sum += minWeights[i];
    }

    for(i =0; i<size; ++i){
      minWeights[i] = minWeights[i]/sum;
    }
  }

////////////////////////////////////////////////////////////////////////////////////
  for(i = 0; i<size; ++i){
    if(minWeights[i] == 0){
      tickers[i] = "0";
    }
  }
  map<string, double> portfolio;
  int number = 0;
  for(i =0; i<size; ++i){
    if(tickers[i] != "0"){
    portfolio[tickers[i]] = minWeights[i];
    ++number;
  }
}
  cout<<"Number of stocks in portfolio is: "<<number<<endl;
  for(auto it = portfolio.begin(); it != portfolio.end(); ++it){
    cout<<it->first<<": "<<it->second<<endl;
  }

  // sanity check
  sum = 0;
  for(i=0; i<size;++i){
    sum += minWeights[i];
  }
  cout<<"Sum of weights: "<<sum<<endl;

  return 0;
}
