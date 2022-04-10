data {
  int N;           // number of participants
  int id[N];       // participant id vector
  int cheat1[N];   // cheat1 game
  int cheat2[N];   // cheat2 game
  int nocheat1[N]; // nocheat1 game
  int nocheat2[N]; // nocheat2 game
  int tppcheat[N]; // tppcheat game
  real<lower=0,upper=1> error; // error rate (assumed)
}
parameters {
  vector[5] alpha;           // population-level intercepts for probabilities of different strategies
  matrix[5,N] z_id;          // participant varying intercepts
  vector<lower=0>[5] sigma;  // sd for varying intercepts
  cholesky_factor_corr[5] L; // Cholesky correlation matrix
}
transformed parameters {
  // non-centered version of varying intercepts
  matrix[5,N] z;
  z = diag_pre_multiply(sigma, L) * z_id; 
}
model {
  // vectors to hold terms of sum and probabilities
  vector[5] theta_j;
  vector[5] p;
  
  // priors
  alpha ~ normal(0, 1);
  L ~ lkj_corr_cholesky(2);
  sigma ~ exponential(2);
  to_vector(z_id) ~ normal(0, 1);
  
  // loop over five behavioural tasks
  for (T in 1:5) {
    // probability of data
    for (i in 1:N ) {
      theta_j = rep_vector(0,5); // clear out the vector
      // task 1: cheat1
      if ( T==1 && cheat1[i]==0 ) theta_j[1]=1; // avoid di
      if ( T==1 && cheat1[i]==1 ) theta_j[2]=1; // competitive
      if ( T==1 && cheat1[i]==0 ) theta_j[3]=1; // egalitarian
      if ( T==1 && cheat1[i]==1 ) theta_j[4]=1; // retributive
      if ( T==1 && cheat1[i]==0 ) theta_j[5]=1; // never punish
      // task 2: cheat2
      if ( T==2 && cheat2[i]==1 ) theta_j[1]=1; // avoid di
      if ( T==2 && cheat2[i]==1 ) theta_j[2]=1; // competitive
      if ( T==2 && cheat2[i]==1 ) theta_j[3]=1; // egalitarian
      if ( T==2 && cheat2[i]==1 ) theta_j[4]=1; // retributive
      if ( T==2 && cheat2[i]==0 ) theta_j[5]=1; // never punish
      // task 3: nocheat1
      if ( T==3 && nocheat1[i]==0 ) theta_j[1]=1; // avoid di
      if ( T==3 && nocheat1[i]==1 ) theta_j[2]=1; // competitive
      if ( T==3 && nocheat1[i]==0 ) theta_j[3]=1; // egalitarian
      if ( T==3 && nocheat1[i]==0 ) theta_j[4]=1; // retributive
      if ( T==3 && nocheat1[i]==0 ) theta_j[5]=1; // never punish
      // task 4: nocheat2
      if ( T==4 && nocheat2[i]==1 ) theta_j[1]=1; // avoid di
      if ( T==4 && nocheat2[i]==1 ) theta_j[2]=1; // competitive
      if ( T==4 && nocheat2[i]==1 ) theta_j[3]=1; // egalitarian
      if ( T==4 && nocheat2[i]==0 ) theta_j[4]=1; // retributive
      if ( T==4 && nocheat2[i]==0 ) theta_j[5]=1; // never punish
      // task 5: tppcheat
      if ( T==5 && tppcheat[i]==0 ) theta_j[1]=1; // avoid di
      if ( T==5 && tppcheat[i]==1 ) theta_j[2]=1; // competitive
      if ( T==5 && tppcheat[i]==1 ) theta_j[3]=1; // egalitarian
      if ( T==5 && tppcheat[i]==1 ) theta_j[4]=1; // retributive
      if ( T==5 && tppcheat[i]==0 ) theta_j[5]=1; // never punish
      
      // calculate p vector for this case
      p = softmax( alpha + col(z, id[i]) );
      
      for (S in 1:5) {
          // add error
          if ( theta_j[S]==0 ) theta_j[S] = 0 + error;
          if ( theta_j[S]==1 ) theta_j[S] = 1 - error;
          // compute log( p_S * Pr(y_i|S) )
          theta_j[S] = log(p[S]) + log(theta_j[S]);
      }
      
      // compute average log-probability of data
      target += log_sum_exp( theta_j );
    }
  }
}
generated quantities {
  vector[5] p;
  matrix[5, 5] Rho;
  p = softmax( alpha );
  Rho = multiply_lower_tri_self_transpose(L);
}
