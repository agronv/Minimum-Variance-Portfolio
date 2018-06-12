// this file contains functions that will be used for matrices

# include <vector>
# include <iostream>
# include <cmath>
# include <stdio.h>
using namespace std;


////////////////////////////////////////////////////////////////////////////////
// Scalar mutliplication of a matrix

vector < vector<double> >Scalar_Multiple(const double& number, vector < vector<double> >& A){
  // the input is a matrix number that we will scale by and the matrix that we are
  // multiplying
  vector < vector<double> > B;
  int rowA = A.size();
  int columnA = A[0].size();

  // allocating space for B
  for (int i = 0; i < rowA; i++) {
    vector <double> temp;
     for (int j = 0; j < columnA; j++) {
        temp.push_back(0);
     }
     B.push_back(temp);
  }
// this wll multiply every entry in the matrix by the int number
  for (int i = 0; i < rowA; i++) {
     for (int j = 0; j < columnA; j++) {
        B[i][j] = A[i][j] * number;
     }
  }

  return B;
}



////////////////////////////////////////////////////////////////////////////////
// multiplication of vectors

vector< vector<double> >Multiply (const vector< vector<double> >& A,const vector< vector<double> >& B){

   unsigned int i, j, k, columnA, rowA, columnB, rowB;
   vector< vector<double> > AB;

// this is the dimensions of the first matrix
   rowA = A.size();
   columnA = A[0].size();

// this is the dimensions of the second matrix
   rowB = B.size();
   columnB = B[0].size();

// we will check that the column of the first matrix is equal to the row of
// the second matrix so that we can multiply the matrices, we will exit the code
// if unsuccessful
  if (columnA != rowB) {
      cout<<"Dimensions don't match in matrix multiplication."<<endl;
      exit(1);
   }

   // Allocate space for the matrix AB
   for (i = 0; i < rowA; i++) {
     vector <double> temp;
      for (j = 0; j < columnB; j++) {
         temp.push_back(0);
      }
      AB.push_back(temp);
   }


   //Compute (AB)_ij.
  for (i = 0; i < rowA; i++){
    for (j = 0; j < columnB; j++){
        for (k = 0; k < columnA; k++){
            AB[i][j] += A[i][k]*B[k][j];
          }
        }
      }

   return AB;
}



////////////////////////////////////////////////////////////////////////////////
// transpose of a vectors

vector < vector<double> > Transpose (const vector < vector<double> >& A){
// the input is a matrix and the ouput is a matrix
   unsigned int i, j, n, m;
   vector < vector<double> > At;

   m = A.size();
   n = A[0].size();

// we are allocating space for the At matrix the transpose matrix
   for (i = 0; i < n; i++) {
     vector <double> temp;
      for (j = 0; j < m; j++) {
         temp.push_back(0);
      }
      At.push_back(temp);
   }

// we are simply transposing the matrix
   for (i = 0; i < n; i++) {
      for (j = 0; j < m; j++) {
         At[i][j] = A[j][i];
      }
   }
   return At;
}



////////////////////////////////////////////////////////////////////////////////
// this will subtract every column by its column mean

vector< vector<double> > MeanZero(const vector< vector<double> >& A){
// this will set the mean of each rows equal to zero, the input is a matrix, and
// the output is a matrix
   vector< vector<double> >B;
   vector<double> mean;

   // We first find the size of our matrix
   int row = A.size();
   int column = A[0].size();


   for(int i =0; i < row; ++i){
     vector<double> temp;
   for(int j = 0; j < column; ++j){
       temp.push_back(0);
     }
     B.push_back(temp);

   }

   mean.reserve(row);
   // this will find the means of all our COLUMNS
   for(int i =0; i < column; ++i){
     for(int j = 0; j < row; ++j){
       mean[i] += A[j][i] / row;
     }
   }
   // this will subtract every entry by its column mean
   for(int i =0; i < row; ++i){
     for(int j = 0; j < column; ++j){
       B[i][j] = A[i][j] - mean[j];
     }
   }

   return B;
}



////////////////////////////////////////////////////////////////////////////////
// this will find the covariance of a matrix

vector< vector<double> >Covariance (const vector< vector<double> >& matrix){

   int n;
   vector< vector<double> > X;
   vector< vector<double> > Xt;
   vector< vector<double> > C;
   vector< vector<double> > Cov;
// this will give the size of the covariacnce matrix
   n = matrix.size();
// this will find the meanZero of the covariance matrix
   X = MeanZero(matrix);
// this will find the Transpose of X
   Xt = Transpose(X);
// we will multiply the meanzero function with its transpose
   C = Multiply (Xt, X);
// we will divide the product with n-1 to get the covariance matrix
   double divide = 1/((double)n - 1);
   Cov = Scalar_Multiple (divide, C);

   return Cov;
}





////////////////////////////////////////////////////////////////////////////////
// this will take the diagonal of our lower triangular matrix

vector<double> variance(const vector< vector<double> >& covariance){
  // this code will give us the variance of the covariance matrix
  // the input is a matrix and the output is a vector
  vector<double> variance;
  int n = covariance.size();

// reserve space for the variance matrix
  variance.reserve(n);

// we are taking the diagonal of the matrix
  for(int i = 0; i < n; i++){
    variance[i] = covariance[i][i];
  }

  return variance;
}



////////////////////////////////////////////////////////////////////////////////
// this will make our matrix lower triangular

vector< vector<double> > Cholesky (const vector< vector<double> >& V){
// we will ask for a matrix and we will ouput a matrix
   unsigned int n, i, j, k, l;
   vector< vector<double> > L;
   int sum;

   // Check that the matrix V is square and symmetric, if not we will exit
   n = V.size();
    if (n != V[0].size()) {
      cout<<"In Cholesky function V is not square."<<endl;
      exit (1);
   }

   // allocating space for L
   for (i = 0; i < n; i++) {
     vector <double> temp;
      for (j = 0; j < n; j++) {
         temp.push_back(0);
      }
      L.push_back(temp);
   }

// this will check whether or not the matric is symmetric or not, if not it
// will exit
   for (i = 0; i < n; i++) {
      for (j = 0; j < i; j++) {
         if (V[i][j] != V[j][i]) {
            cout<<"In Cholesky function V is not symmetric."<<endl;
            exit (1);
         }
      }
   }

// Work on row k of L.
   for (k = 0; k < n; k++) {
      // Work on L[k][j]
      for (j = 0; j <= k; j++) {
         sum = 0;

// this will calculate the diagonal of the lower matrix
         if(j == k){
           for(l = 0; l < j; l++){
             sum += L[j][l]*L[j][l];
           }
// At this point should have sum < V[k][k].
           if (sum >= V[k][k]) {
              cout<<"In Cholesky function V is not positive definite"<<endl;
              exit(0);
           }
           L[j][j] = sqrt(V[j][j] - sum);
        }

// this part will work on the lower part of the matrix
        else{
          for(l = 0; l < j; l++){
            sum += (L[k][l] * L[j][l]);
          }
          L[k][j] = (V[k][j] - sum) / L[j][j];
        }
      }
    }
// notice that the rest of the matrix will be zero because we will have a
// lower triangular matrix
    return L;
  }



  ////////////////////////////////////////////////////////////////////////////////
  // this will find the cofactor of the matrix

  void Cofactor(const vector< vector<double> >& matrix, vector< vector<double> >& temp, int p, int q, int n){
      int i = 0, j = 0;

      // Looping for each element of the matrix
      for (int row = 0; row < n; row++){
          for (int col = 0; col < n; col++){
              //  Copying into temporary matrix only those element
              //  which are not in given row and column
              if (row != p && col != q){
                  temp[i][j++] = matrix[row][col];
                  // Row is filled, so increase row index and
                  // reset column index
                  if (j == n - 1){
                      j = 0;
                      i++;
                  }
              }
          }
      }
  }



  ////////////////////////////////////////////////////////////////////////////////
  // this will find the determinant of the matrix

  double Determinant(const vector< vector<double> >& matrix, int n){
    // this function will get the determinant of the function, the input is a matrix
    // and the size of the matrix, it is a recursive function
      double D = 0; // Initialize result
      //  Base case : if matrix contains single element
      if (n == 1){
          return matrix[0][0];
        }
      vector< vector<double> > temp; // To store cofactors
      // we will allocate space for temp
          for (int i = 0; i < n; i++){
            vector <double> tem;
             for (int j = 0; j < n; j++){
                tem.push_back(0);
             }
             temp.push_back(tem);
           }

      int sign = 1;  // To store sign multiplier

       // Iterate for each element of first row
      for (int f = 0; f < n; f++){
          // Getting Cofactor of mat[0][f]
          Cofactor(matrix, temp, 0, f, n);
          D += sign * matrix[0][f] * Determinant(temp, n - 1);

          // terms are to be added with alternate sign
          sign = -sign;
      }
      return D;
  }


  ////////////////////////////////////////////////////////////////////////////////
  // this will find the row mean of the matrix, pretty self explanatory
  vector<double> RowMean(const vector< vector<double> >& A){
    int row = A.size();
    int column = A[0].size();
    vector<double> B;
    B.reserve(row);
    double sum;

  // we will sum up every entry in a rw and divide by the length of the row
  // i.e the numbers of columns
    for(int i = 0; i<row; i++){
      for(int j = 0; j<column; j++){
        B[i] += A[i][j] / column;
      }
    }
    return B;
  }



  ////////////////////////////////////////////////////////////////////////////////
  // this function will put nxm 0's in the matrix
  void MatrixReserve(vector< vector<double> >& A, int row, int column){
    // this function will fill the given matrix with nxn 0's
    for(int i = 0; i < row; i++){
      vector<double> temp;
      for(int j = 0; j < column; j++){
        temp.push_back(0);
      }
      A.push_back(temp);
    }
  }

    ////////////////////////////////////////////////////////////////////////////////
    // this function is the new and improved inverse
    vector< vector<double> > Inverse(const vector< vector<double> >& A0){

       int n, rmax;
       vector <vector< double> > A, Ainv;
       double c, value;

       // Make a copy so that the original matrix is not changed during G.E.
       n = A0.size();
       for(int i=0; i<n; ++i){
         vector<double> temp1, temp2;
         for(int j=0; j<n; ++j){
           if(i == j){
             temp1.push_back(1);
             temp2.push_back(A0[i][j]);
           }
           else{
             temp1.push_back(0);
             temp2.push_back(A0[i][j]);
           }
         }
         A.push_back(temp2);
         Ainv.push_back(temp1);
       }

       if (n != A0[0].size()) {
          cout<<"Trying to invert a non-square matrix."<<endl;
          exit(1);
       }

       // Start with the n x n identity matrix.  This matrix will eventually hold
       //    the inverse of A.

       // Work on column j of A.
       for (int j = 0; j < n; j++) {
          // Find the largest non-zero entry in the column on or below the diagonal.
          c = 0;
          rmax = 0;
          for (int i = j; i < n; i++){
             if (abs(A[i][j]) > c) {
                c = abs(A[i][j]);
                rmax = i;
             }
          }

          // If they are all 0 the matrix is singular.
          if (c < 0) {
             cout<<"Trying to invert a singular matrix."<<endl;
             exit(1);
          }
          // Swap rows j and rmax in both A and Ainv.
          int i = j;
          for (int k = 0; k < n; k++) {
             c = A[i][k];
             A[i][k] = A[rmax][k];
             A[rmax][k] = c;
             c = Ainv[i][k];
             Ainv[i][k] = Ainv[rmax][k];
             Ainv[rmax][k] = c;
          }

          // Scale so the pivot is 1.
          c = A[i][i];
          for (int k = 0; k < n; k++) {
             A[i][k] /= c;
             Ainv[i][k] /= c;
          }

          // Make rest of column j equal to 0. Apply same row operations to Ainv.
          for (int i = 0; i < n; i++) if (i != j) {
             c = A[i][j];
             for (int k = 0; k < n; k++) {
                A[i][k] += -c * A[j][k];
                Ainv[i][k] += -c * Ainv[j][k];
             }
          }
       }

       return Ainv;
     }


////////////////////////////////////////////////////////////////////////////////
// Variance of portfolio with given weights
    double Var(vector< vector<double>> W, vector< vector<double> >V){
      double v;
      vector< vector<double> >var;
      vector< vector<double> >W_V;
      W_V=Multiply(W,V);
      var = Multiply(W_V,Transpose(W));
      v=var[0][0];
      return v;
     }



//////////////////////////////////////////////////////////////////////////////////
// multply a vector by a scalar
     vector<double> Scalar_Multiple_Vector(const double& number, vector<double>& A){
       // the input is a matrix number that we will scale by and the matrix that we are
       // multiplying
       vector<double> B;
       int rowA = A.size();

     // this wll multiply every entry in the matrix by the int number
       for (int i = 0; i < rowA; ++i){
         B.push_back(A[i]*number);
       }

       return B;
     }
