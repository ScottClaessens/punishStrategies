data {
  int N;           // number of participants
  int cheat1[N];   // cheat1 game
  int cheat2[N];   // cheat2 game
  int nocheat1[N]; // nocheat1 game
  int nocheat2[N]; // nocheat2 game
  int tppcheat[N]; // tppcheat game
}
parameters {
  simplex[5] p; // probability of different strategies
}
model {
  // vector to hold terms of sum
  vector[5] theta_j;
  
  // prior on the probabilities
  p ~ dirichlet( rep_vector(4,5) );
  
  // probability of data
  for (i in 1:N ) {
    theta_j = rep_vector(0,5); // clear out the vector
    if ( cheat1[i]==0 && cheat2[i]==1 && nocheat1[i]==0 && nocheat2[i]==1 && tppcheat[i]==0 ) theta_j[1]=1; // avoid di
    if ( cheat1[i]==1 && cheat2[i]==1 && nocheat1[i]==1 && nocheat2[i]==1 && tppcheat[i]==1 ) theta_j[2]=1; // competitive
    if ( cheat1[i]==0 && cheat2[i]==1 && nocheat1[i]==0 && nocheat2[i]==1 && tppcheat[i]==1 ) theta_j[3]=1; // egalitarian
    if ( cheat1[i]==1 && cheat2[i]==1 && nocheat1[i]==0 && nocheat2[i]==0 && tppcheat[i]==1 ) theta_j[4]=1; // retributive
    theta_j[5]=1.0/32.0; // choose randomly
    
    // compute log( p_S * Pr(y_i|S) )
    for (S in 1:5)
        theta_j[S] = log(p[S]) + log(theta_j[S]);
        
    // compute average log-probability of data
    target += log_sum_exp( theta_j );
  }
}
