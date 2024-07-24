data {
  int N;            // number of participants
  int Nc;           // number of countries
  array[N] int countryID; // country IDs (1 = UK, 2 = US)
  array[N] int pun1_1;    // No DI 1 - Take
  array[N] int pun1_2;    // No DI 1 - No take
  array[N] int pun2_1;    // No DI 2 - Take
  array[N] int pun2_2;    // No DI 2 - No take
  array[N] int pun3_1;    // No DI 3 - Take
  array[N] int pun3_2;    // No DI 3 - No take
  array[N] int pun4_1;    // No DI 4 - Take
  array[N] int pun4_2;    // No DI 4 - No take
  array[N] int pun5_1;    // DI - Take
  array[N] int pun5_2;    // DI - No take
  array[N] int pun6_1;    // 3PP - Take
  array[N] int pun6_2;    // 3PP - No take
  real<lower=0,upper=1> error; // error rate (assumed)
}
parameters {
  vector[10] alpha[Nc]; // intercepts for probabilities of different strategies
}
transformed parameters {
  // arrays to hold terms of sum and probabilities
  array[12,N] vector[10] theta_j;
  vector[10] p;
  
  // missing data? (0 = observed, 1 = missing)
  array[12,N] real missing;
  
  // loop over twelve punishment behaviours
  for (B in 1:12) {
    // probability of data
    for (i in 1:N ) {
      theta_j[B,i] = rep_vector(0,10); // clear out the vector
      missing[B,i] = 0;                // missingness
      // No DI 1 - Take
      if ( B==1 ) {
        if ( pun1_1[i]==-999 ) missing[B,i]=1  ;
        if ( pun1_1[i]==1 ) theta_j[B,i, 1]=1  ; // competitive
        if ( pun1_1[i]==0 ) theta_j[B,i, 2]=1  ; // avoid DI
        if ( pun1_1[i]==0 ) theta_j[B,i, 3]=1  ; // egalitarian
        if ( pun1_1[i]==0 ) theta_j[B,i, 4]=1  ; // seek AI
        if ( pun1_1[i]==1 ) theta_j[B,i, 5]=1  ; // retributive
        if ( pun1_1[i]==1 ) theta_j[B,i, 6]=1  ; // deterrent
        if ( pun1_1[i]==1 ) theta_j[B,i, 7]=1  ; // norm-enforcing
        if ( pun1_1[i]==0 ) theta_j[B,i, 8]=1  ; // antisocial
                            theta_j[B,i, 9]=0.5; // random choice
        if ( pun1_1[i]==0 ) theta_j[B,i,10]=1  ; // never punish
      }
      // No DI 1 - No take
      if ( B==2 ) {
        if ( pun1_2[i]==-999 ) missing[B,i]=1  ;
        if ( pun1_2[i]==1 ) theta_j[B,i, 1]=1  ; // competitive
        if ( pun1_2[i]==0 ) theta_j[B,i, 2]=1  ; // avoid DI
        if ( pun1_2[i]==0 ) theta_j[B,i, 3]=1  ; // egalitarian
        if ( pun1_2[i]==0 ) theta_j[B,i, 4]=1  ; // seek AI
        if ( pun1_2[i]==0 ) theta_j[B,i, 5]=1  ; // retributive
        if ( pun1_2[i]==0 ) theta_j[B,i, 6]=1  ; // deterrent
        if ( pun1_2[i]==0 ) theta_j[B,i, 7]=1  ; // norm-enforcing
        if ( pun1_2[i]==1 ) theta_j[B,i, 8]=1  ; // antisocial
                            theta_j[B,i, 9]=0.5; // random choice
        if ( pun1_2[i]==0 ) theta_j[B,i,10]=1  ; // never punish
      }
      // No DI 2 - Take
      if ( B==3 ) {
        if ( pun2_1[i]==-999 ) missing[B,i]=1  ;
        if ( pun2_1[i]==1 ) theta_j[B,i, 1]=1  ; // competitive
        if ( pun2_1[i]==0 ) theta_j[B,i, 2]=1  ; // avoid DI
        if ( pun2_1[i]==0 ) theta_j[B,i, 3]=1  ; // egalitarian
        if ( pun2_1[i]==1 ) theta_j[B,i, 4]=1  ; // seek AI
        if ( pun2_1[i]==1 ) theta_j[B,i, 5]=1  ; // retributive
        if ( pun2_1[i]==1 ) theta_j[B,i, 6]=1  ; // deterrent
        if ( pun2_1[i]==1 ) theta_j[B,i, 7]=1  ; // norm-enforcing
        if ( pun2_1[i]==0 ) theta_j[B,i, 8]=1  ; // antisocial
                            theta_j[B,i, 9]=0.5; // random choice
        if ( pun2_1[i]==0 ) theta_j[B,i,10]=1  ; // never punish
      }
      // No DI 2 - No take
      if ( B==4 ) {
        if ( pun2_2[i]==-999 ) missing[B,i]=1  ;
        if ( pun2_2[i]==1 ) theta_j[B,i, 1]=1  ; // competitive
        if ( pun2_2[i]==0 ) theta_j[B,i, 2]=1  ; // avoid DI
        if ( pun2_2[i]==0 ) theta_j[B,i, 3]=1  ; // egalitarian
        if ( pun2_2[i]==0 ) theta_j[B,i, 4]=1  ; // seek AI
        if ( pun2_2[i]==0 ) theta_j[B,i, 5]=1  ; // retributive
        if ( pun2_2[i]==0 ) theta_j[B,i, 6]=1  ; // deterrent
        if ( pun2_2[i]==0 ) theta_j[B,i, 7]=1  ; // norm-enforcing
        if ( pun2_2[i]==1 ) theta_j[B,i, 8]=1  ; // antisocial
                            theta_j[B,i, 9]=0.5; // random choice
        if ( pun2_2[i]==0 ) theta_j[B,i,10]=1  ; // never punish
      }
      // No DI 3 - Take
      if ( B==5 ) {
        if ( pun3_1[i]==-999 ) missing[B,i]=1  ;
        if ( pun3_1[i]==1 ) theta_j[B,i, 1]=1  ; // competitive
        if ( pun3_1[i]==0 ) theta_j[B,i, 2]=1  ; // avoid DI
        if ( pun3_1[i]==0 ) theta_j[B,i, 3]=1  ; // egalitarian
        if ( pun3_1[i]==1 ) theta_j[B,i, 4]=1  ; // seek AI
        if ( pun3_1[i]==1 ) theta_j[B,i, 5]=1  ; // retributive
        if ( pun3_1[i]==0 ) theta_j[B,i, 6]=1  ; // deterrent
        if ( pun3_1[i]==0 ) theta_j[B,i, 7]=1  ; // norm-enforcing
        if ( pun3_1[i]==0 ) theta_j[B,i, 8]=1  ; // antisocial
                            theta_j[B,i, 9]=0.5; // random choice
        if ( pun3_1[i]==0 ) theta_j[B,i,10]=1  ; // never punish
      }
      // No DI 3 - No take
      if ( B==6 ) {
        if ( pun3_2[i]==-999 ) missing[B,i]=1  ;
        if ( pun3_2[i]==1 ) theta_j[B,i, 1]=1  ; // competitive
        if ( pun3_2[i]==0 ) theta_j[B,i, 2]=1  ; // avoid DI
        if ( pun3_2[i]==0 ) theta_j[B,i, 3]=1  ; // egalitarian
        if ( pun3_2[i]==0 ) theta_j[B,i, 4]=1  ; // seek AI
        if ( pun3_2[i]==0 ) theta_j[B,i, 5]=1  ; // retributive
        if ( pun3_2[i]==0 ) theta_j[B,i, 6]=1  ; // deterrent
        if ( pun3_2[i]==0 ) theta_j[B,i, 7]=1  ; // norm-enforcing
        if ( pun3_2[i]==1 ) theta_j[B,i, 8]=1  ; // antisocial
                            theta_j[B,i, 9]=0.5; // random choice
        if ( pun3_2[i]==0 ) theta_j[B,i,10]=1  ; // never punish
      }
      // No DI 4 - Take
      if ( B==7 ) {
        if ( pun4_1[i]==-999 ) missing[B,i]=1  ;
        if ( pun4_1[i]==0 ) theta_j[B,i, 1]=1  ; // competitive
        if ( pun4_1[i]==0 ) theta_j[B,i, 2]=1  ; // avoid DI
        if ( pun4_1[i]==0 ) theta_j[B,i, 3]=1  ; // egalitarian
        if ( pun4_1[i]==0 ) theta_j[B,i, 4]=1  ; // seek AI
        if ( pun4_1[i]==1 ) theta_j[B,i, 5]=1  ; // retributive
        if ( pun4_1[i]==1 ) theta_j[B,i, 6]=1  ; // deterrent
        if ( pun4_1[i]==1 ) theta_j[B,i, 7]=1  ; // norm-enforcing
        if ( pun4_1[i]==0 ) theta_j[B,i, 8]=1  ; // antisocial
                            theta_j[B,i, 9]=0.5; // random choice
        if ( pun4_1[i]==0 ) theta_j[B,i,10]=1  ; // never punish
      }
      // No DI 4 - No take
      if ( B==8 ) {
        if ( pun4_2[i]==-999 ) missing[B,i]=1  ;
        if ( pun4_2[i]==0 ) theta_j[B,i, 1]=1  ; // competitive
        if ( pun4_2[i]==0 ) theta_j[B,i, 2]=1  ; // avoid DI
        if ( pun4_2[i]==0 ) theta_j[B,i, 3]=1  ; // egalitarian
        if ( pun4_2[i]==0 ) theta_j[B,i, 4]=1  ; // seek AI
        if ( pun4_2[i]==0 ) theta_j[B,i, 5]=1  ; // retributive
        if ( pun4_2[i]==0 ) theta_j[B,i, 6]=1  ; // deterrent
        if ( pun4_2[i]==0 ) theta_j[B,i, 7]=1  ; // norm-enforcing
        if ( pun4_2[i]==1 ) theta_j[B,i, 8]=1  ; // antisocial
                            theta_j[B,i, 9]=0.5; // random choice
        if ( pun4_2[i]==0 ) theta_j[B,i,10]=1  ; // never punish
      }
      // DI - Take
      if ( B==9 ) {
        if ( pun5_1[i]==-999 ) missing[B,i]=1  ;
        if ( pun5_1[i]==1 ) theta_j[B,i, 1]=1  ; // competitive
        if ( pun5_1[i]==1 ) theta_j[B,i, 2]=1  ; // avoid DI
        if ( pun5_1[i]==1 ) theta_j[B,i, 3]=1  ; // egalitarian
        if ( pun5_1[i]==0 ) theta_j[B,i, 4]=1  ; // seek AI
        if ( pun5_1[i]==1 ) theta_j[B,i, 5]=1  ; // retributive
        if ( pun5_1[i]==1 ) theta_j[B,i, 6]=1  ; // deterrent
        if ( pun5_1[i]==1 ) theta_j[B,i, 7]=1  ; // norm-enforcing
        if ( pun5_1[i]==0 ) theta_j[B,i, 8]=1  ; // antisocial
                            theta_j[B,i, 9]=0.5; // random choice
        if ( pun5_1[i]==0 ) theta_j[B,i,10]=1  ; // never punish
      }
      // DI - No take
      if ( B==10 ) {
        if ( pun5_2[i]==-999 ) missing[B,i]=1  ;
        if ( pun5_2[i]==1 ) theta_j[B,i, 1]=1  ; // competitive
        if ( pun5_2[i]==0 ) theta_j[B,i, 2]=1  ; // avoid DI
        if ( pun5_2[i]==0 ) theta_j[B,i, 3]=1  ; // egalitarian
        if ( pun5_2[i]==0 ) theta_j[B,i, 4]=1  ; // seek AI
        if ( pun5_2[i]==0 ) theta_j[B,i, 5]=1  ; // retributive
        if ( pun5_2[i]==0 ) theta_j[B,i, 6]=1  ; // deterrent
        if ( pun5_2[i]==0 ) theta_j[B,i, 7]=1  ; // norm-enforcing
        if ( pun5_2[i]==1 ) theta_j[B,i, 8]=1  ; // antisocial
                            theta_j[B,i, 9]=0.5; // random choice
        if ( pun5_2[i]==0 ) theta_j[B,i,10]=1  ; // never punish
      }
      // 3PP - Take
      if ( B==11 ) {
        if ( pun6_1[i]==-999 ) missing[B,i]=1  ;
        if ( pun6_1[i]==1 ) theta_j[B,i, 1]=1  ; // competitive
        if ( pun6_1[i]==0 ) theta_j[B,i, 2]=1  ; // avoid DI
        if ( pun6_1[i]==1 ) theta_j[B,i, 3]=1  ; // egalitarian
        if ( pun6_1[i]==0 ) theta_j[B,i, 4]=1  ; // seek AI
        if ( pun6_1[i]==0 ) theta_j[B,i, 5]=1  ; // retributive
        if ( pun6_1[i]==0 ) theta_j[B,i, 6]=1  ; // deterrent
        if ( pun6_1[i]==1 ) theta_j[B,i, 7]=1  ; // norm-enforcing
        if ( pun6_1[i]==0 ) theta_j[B,i, 8]=1  ; // antisocial
                            theta_j[B,i, 9]=0.5; // random choice
        if ( pun6_1[i]==0 ) theta_j[B,i,10]=1  ; // never punish
      }
      // 3PP - No take
      if ( B==12 ) {
        if ( pun6_2[i]==-999 ) missing[B,i]=1  ;
        if ( pun6_2[i]==1 ) theta_j[B,i, 1]=1  ; // competitive
        if ( pun6_2[i]==0 ) theta_j[B,i, 2]=1  ; // avoid DI
        if ( pun6_2[i]==0 ) theta_j[B,i, 3]=1  ; // egalitarian
        if ( pun6_2[i]==0 ) theta_j[B,i, 4]=1  ; // seek AI
        if ( pun6_2[i]==0 ) theta_j[B,i, 5]=1  ; // retributive
        if ( pun6_2[i]==0 ) theta_j[B,i, 6]=1  ; // deterrent
        if ( pun6_2[i]==0 ) theta_j[B,i, 7]=1  ; // norm-enforcing
        if ( pun6_2[i]==1 ) theta_j[B,i, 8]=1  ; // antisocial
                            theta_j[B,i, 9]=0.5; // random choice
        if ( pun6_2[i]==0 ) theta_j[B,i,10]=1  ; // never punish
      }
      
      // only if case observed
      if ( missing[B,i]==0 ) {
        // calculate p vector for this case
        p = softmax( alpha[countryID[i]] );
        // iterate over strategies
        for (S in 1:10) {
            // add error
            if ( theta_j[B,i,S]==0 ) theta_j[B,i,S] = 0 + error;
            if ( theta_j[B,i,S]==1 ) theta_j[B,i,S] = 1 - error;
            // compute log( p_S * Pr(y_i|S) )
            theta_j[B,i,S] = log(p[S]) + log(theta_j[B,i,S]);
        }
      }
    }
  }
}
model {
  // priors
  for (i in 1:Nc) alpha[i] ~ normal(0, 1);
  // compute average log-probability of data
  for (B in 1:12) {
    for (i in 1:N) {
      if ( missing[B,i]==0 ) {
        target += log_sum_exp( theta_j[B,i] );
      }
    }
  }
}
generated quantities {
  // log-likelihood
  vector[12*N] log_lik;
  { // "local" environment so temp doesn't get saved to file
    matrix[12,N] temp;
    for (B in 1:12) {
      for (i in 1:N) {
        if ( missing[B,i]==0 ) {
          temp[B,i] = log_sum_exp( theta_j[B,i] );
        } else {
          temp[B,i] = 0;
        }
      }
    }
    log_lik = to_vector(temp);
  }
}
