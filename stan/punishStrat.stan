data {
  int N;           // number of participants
  int cheat1[N];   // cheat1 game
  int cheat2[N];   // cheat2 game
  int nocheat1[N]; // nocheat1 game
  int nocheat2[N]; // nocheat2 game
  int tppcheat[N]; // tppcheat game
}
parameters {
  simplex[4] p; // probability of different strategies
  real<lower=0,upper=1> alpha; // error rate
}
model {
  // vector to hold terms of sum
  vector[4] theta_j;
  
  // prior on the probabilities
  p ~ dirichlet( rep_vector(4,4) );
  
  // prior on the error rate
  alpha ~ beta( 0.1, 10 );
  
  // loop over five behavioural tasks
  for (T in 1:5) {
    // probability of data
    for (i in 1:N ) {
      theta_j = rep_vector(0,4); // clear out the vector
      // task 1: cheat1
      if ( T==1 && cheat1[i]==0 ) theta_j[1]=1; // avoid di
      if ( T==1 && cheat1[i]==1 ) theta_j[2]=1; // competitive
      if ( T==1 && cheat1[i]==0 ) theta_j[3]=1; // egalitarian
      if ( T==1 && cheat1[i]==1 ) theta_j[4]=1; // retributive
      // task 2: cheat2
      if ( T==2 && cheat2[i]==1 ) theta_j[1]=1; // avoid di
      if ( T==2 && cheat2[i]==1 ) theta_j[2]=1; // competitive
      if ( T==2 && cheat2[i]==1 ) theta_j[3]=1; // egalitarian
      if ( T==2 && cheat2[i]==1 ) theta_j[4]=1; // retributive
      // task 3: nocheat1
      if ( T==3 && nocheat1[i]==0 ) theta_j[1]=1; // avoid di
      if ( T==3 && nocheat1[i]==1 ) theta_j[2]=1; // competitive
      if ( T==3 && nocheat1[i]==0 ) theta_j[3]=1; // egalitarian
      if ( T==3 && nocheat1[i]==0 ) theta_j[4]=1; // retributive
      // task 4: nocheat2
      if ( T==4 && nocheat2[i]==1 ) theta_j[1]=1; // avoid di
      if ( T==4 && nocheat2[i]==1 ) theta_j[2]=1; // competitive
      if ( T==4 && nocheat2[i]==1 ) theta_j[3]=1; // egalitarian
      if ( T==4 && nocheat2[i]==0 ) theta_j[4]=1; // retributive
      // task 5: tppcheat
      if ( T==5 && tppcheat[i]==0 ) theta_j[1]=1; // avoid di
      if ( T==5 && tppcheat[i]==1 ) theta_j[2]=1; // competitive
      if ( T==5 && tppcheat[i]==1 ) theta_j[3]=1; // egalitarian
      if ( T==5 && tppcheat[i]==1 ) theta_j[4]=1; // retributive
      
      for (S in 1:4) {
          // add error
          if ( theta_j[S]==0 ) theta_j[S] = 0 + alpha;
          if ( theta_j[S]==1 ) theta_j[S] = 1 - alpha;
          // compute log( p_S * Pr(y_i|S) )
          theta_j[S] = log(p[S]) + log(theta_j[S]);
      }
          
      // compute average log-probability of data
      target += log_sum_exp( theta_j );
    }
  }
}
