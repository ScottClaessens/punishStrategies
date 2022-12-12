data {
  int N;         // number of participants
  int pun1_1[N]; // No DI 1 - Take
  int pun1_2[N]; // No DI 1 - No take
  int pun2_1[N]; // No DI 2 - Take
  int pun2_2[N]; // No DI 2 - No take
  int pun3_1[N]; // No DI 3 - Take
  int pun3_2[N]; // No DI 3 - No take
  int pun4_1[N]; // No DI 4 - Take
  int pun4_2[N]; // No DI 4 - No take
  int pun5_1[N]; // DI - Take
  int pun5_2[N]; // DI - No take
  int pun6_1[N]; // 3PP - Take
  int pun6_2[N]; // 3PP - No take
  real<lower=0,upper=1> error; // error rate (assumed)
}
parameters {
  vector[10] alpha; // intercepts for probabilities of different strategies
}
model {
  // vectors to hold terms of sum and probabilities
  vector[10] theta_j;
  vector[10] p;
  
  // missing data? (0 = observed, 1 = missing)
  int missing;
  
  // priors
  alpha ~ normal(0, 1);
  
  // loop over twelve punishment behaviours
  for (B in 1:12) {
    // probability of data
    for (i in 1:N ) {
      theta_j = rep_vector(0,10); // clear out the vector
      missing = 0;                // clear out missingness
      // No DI 1 - Take
      if ( B==1 ) {
        if ( pun1_1[i]==-999 ) missing=1   ;
        if ( pun1_1[i]==1 ) theta_j[ 1]=1  ; // competitive
        if ( pun1_1[i]==0 ) theta_j[ 2]=1  ; // avoid DI
        if ( pun1_1[i]==0 ) theta_j[ 3]=1  ; // egalitarian
        if ( pun1_1[i]==0 ) theta_j[ 4]=1  ; // seek AI
        if ( pun1_1[i]==1 ) theta_j[ 5]=1  ; // retributive
        if ( pun1_1[i]==1 ) theta_j[ 6]=1  ; // deterrent
        if ( pun1_1[i]==1 ) theta_j[ 7]=1  ; // norm-enforcing
        if ( pun1_1[i]==0 ) theta_j[ 8]=1  ; // antisocial
                            theta_j[ 9]=0.5; // random choice
        if ( pun1_1[i]==0 ) theta_j[10]=1  ; // never punish
      }
      // No DI 1 - No take
      if ( B==2 ) {
        if ( pun1_2[i]==-999 ) missing=1   ;
        if ( pun1_2[i]==1 ) theta_j[ 1]=1  ; // competitive
        if ( pun1_2[i]==0 ) theta_j[ 2]=1  ; // avoid DI
        if ( pun1_2[i]==0 ) theta_j[ 3]=1  ; // egalitarian
        if ( pun1_2[i]==0 ) theta_j[ 4]=1  ; // seek AI
        if ( pun1_2[i]==0 ) theta_j[ 5]=1  ; // retributive
        if ( pun1_2[i]==0 ) theta_j[ 6]=1  ; // deterrent
        if ( pun1_2[i]==0 ) theta_j[ 7]=1  ; // norm-enforcing
        if ( pun1_2[i]==1 ) theta_j[ 8]=1  ; // antisocial
                            theta_j[ 9]=0.5; // random choice
        if ( pun1_2[i]==0 ) theta_j[10]=1  ; // never punish
      }
      // No DI 2 - Take
      if ( B==3 ) {
        if ( pun2_1[i]==-999 ) missing=1   ;
        if ( pun2_1[i]==1 ) theta_j[ 1]=1  ; // competitive
        if ( pun2_1[i]==0 ) theta_j[ 2]=1  ; // avoid DI
        if ( pun2_1[i]==0 ) theta_j[ 3]=1  ; // egalitarian
        if ( pun2_1[i]==1 ) theta_j[ 4]=1  ; // seek AI
        if ( pun2_1[i]==1 ) theta_j[ 5]=1  ; // retributive
        if ( pun2_1[i]==1 ) theta_j[ 6]=1  ; // deterrent
        if ( pun2_1[i]==1 ) theta_j[ 7]=1  ; // norm-enforcing
        if ( pun2_1[i]==0 ) theta_j[ 8]=1  ; // antisocial
                            theta_j[ 9]=0.5; // random choice
        if ( pun2_1[i]==0 ) theta_j[10]=1  ; // never punish
      }
      // No DI 2 - No take
      if ( B==4 ) {
        if ( pun2_2[i]==-999 ) missing=1   ;
        if ( pun2_2[i]==1 ) theta_j[ 1]=1  ; // competitive
        if ( pun2_2[i]==0 ) theta_j[ 2]=1  ; // avoid DI
        if ( pun2_2[i]==0 ) theta_j[ 3]=1  ; // egalitarian
        if ( pun2_2[i]==0 ) theta_j[ 4]=1  ; // seek AI
        if ( pun2_2[i]==0 ) theta_j[ 5]=1  ; // retributive
        if ( pun2_2[i]==0 ) theta_j[ 6]=1  ; // deterrent
        if ( pun2_2[i]==0 ) theta_j[ 7]=1  ; // norm-enforcing
        if ( pun2_2[i]==1 ) theta_j[ 8]=1  ; // antisocial
                            theta_j[ 9]=0.5; // random choice
        if ( pun2_2[i]==0 ) theta_j[10]=1  ; // never punish
      }
      // No DI 3 - Take
      if ( B==5 ) {
        if ( pun3_1[i]==-999 ) missing=1   ;
        if ( pun3_1[i]==1 ) theta_j[ 1]=1  ; // competitive
        if ( pun3_1[i]==0 ) theta_j[ 2]=1  ; // avoid DI
        if ( pun3_1[i]==0 ) theta_j[ 3]=1  ; // egalitarian
        if ( pun3_1[i]==1 ) theta_j[ 4]=1  ; // seek AI
        if ( pun3_1[i]==1 ) theta_j[ 5]=1  ; // retributive
        if ( pun3_1[i]==0 ) theta_j[ 6]=1  ; // deterrent
        if ( pun3_1[i]==0 ) theta_j[ 7]=1  ; // norm-enforcing
        if ( pun3_1[i]==0 ) theta_j[ 8]=1  ; // antisocial
                            theta_j[ 9]=0.5; // random choice
        if ( pun3_1[i]==0 ) theta_j[10]=1  ; // never punish
      }
      // No DI 3 - No take
      if ( B==6 ) {
        if ( pun3_2[i]==-999 ) missing=1   ;
        if ( pun3_2[i]==1 ) theta_j[ 1]=1  ; // competitive
        if ( pun3_2[i]==0 ) theta_j[ 2]=1  ; // avoid DI
        if ( pun3_2[i]==0 ) theta_j[ 3]=1  ; // egalitarian
        if ( pun3_2[i]==0 ) theta_j[ 4]=1  ; // seek AI
        if ( pun3_2[i]==0 ) theta_j[ 5]=1  ; // retributive
        if ( pun3_2[i]==0 ) theta_j[ 6]=1  ; // deterrent
        if ( pun3_2[i]==0 ) theta_j[ 7]=1  ; // norm-enforcing
        if ( pun3_2[i]==1 ) theta_j[ 8]=1  ; // antisocial
                            theta_j[ 9]=0.5; // random choice
        if ( pun3_2[i]==0 ) theta_j[10]=1  ; // never punish
      }
      // No DI 4 - Take
      if ( B==7 ) {
        if ( pun4_1[i]==-999 ) missing=1   ;
        if ( pun4_1[i]==0 ) theta_j[ 1]=1  ; // competitive
        if ( pun4_1[i]==0 ) theta_j[ 2]=1  ; // avoid DI
        if ( pun4_1[i]==0 ) theta_j[ 3]=1  ; // egalitarian
        if ( pun4_1[i]==0 ) theta_j[ 4]=1  ; // seek AI
        if ( pun4_1[i]==1 ) theta_j[ 5]=1  ; // retributive
        if ( pun4_1[i]==1 ) theta_j[ 6]=1  ; // deterrent
        if ( pun4_1[i]==1 ) theta_j[ 7]=1  ; // norm-enforcing
        if ( pun4_1[i]==0 ) theta_j[ 8]=1  ; // antisocial
                            theta_j[ 9]=0.5; // random choice
        if ( pun4_1[i]==0 ) theta_j[10]=1  ; // never punish
      }
      // No DI 4 - No take
      if ( B==8 ) {
        if ( pun4_2[i]==-999 ) missing=1   ;
        if ( pun4_2[i]==0 ) theta_j[ 1]=1  ; // competitive
        if ( pun4_2[i]==0 ) theta_j[ 2]=1  ; // avoid DI
        if ( pun4_2[i]==0 ) theta_j[ 3]=1  ; // egalitarian
        if ( pun4_2[i]==0 ) theta_j[ 4]=1  ; // seek AI
        if ( pun4_2[i]==0 ) theta_j[ 5]=1  ; // retributive
        if ( pun4_2[i]==0 ) theta_j[ 6]=1  ; // deterrent
        if ( pun4_2[i]==0 ) theta_j[ 7]=1  ; // norm-enforcing
        if ( pun4_2[i]==1 ) theta_j[ 8]=1  ; // antisocial
                            theta_j[ 9]=0.5; // random choice
        if ( pun4_2[i]==0 ) theta_j[10]=1  ; // never punish
      }
      // DI - Take
      if ( B==9 ) {
        if ( pun5_1[i]==-999 ) missing=1   ;
        if ( pun5_1[i]==1 ) theta_j[ 1]=1  ; // competitive
        if ( pun5_1[i]==1 ) theta_j[ 2]=1  ; // avoid DI
        if ( pun5_1[i]==1 ) theta_j[ 3]=1  ; // egalitarian
        if ( pun5_1[i]==0 ) theta_j[ 4]=1  ; // seek AI
        if ( pun5_1[i]==1 ) theta_j[ 5]=1  ; // retributive
        if ( pun5_1[i]==1 ) theta_j[ 6]=1  ; // deterrent
        if ( pun5_1[i]==1 ) theta_j[ 7]=1  ; // norm-enforcing
        if ( pun5_1[i]==0 ) theta_j[ 8]=1  ; // antisocial
                            theta_j[ 9]=0.5; // random choice
        if ( pun5_1[i]==0 ) theta_j[10]=1  ; // never punish
      }
      // DI - No take
      if ( B==10 ) {
        if ( pun5_2[i]==-999 ) missing=1   ;
        if ( pun5_2[i]==1 ) theta_j[ 1]=1  ; // competitive
        if ( pun5_2[i]==0 ) theta_j[ 2]=1  ; // avoid DI
        if ( pun5_2[i]==0 ) theta_j[ 3]=1  ; // egalitarian
        if ( pun5_2[i]==0 ) theta_j[ 4]=1  ; // seek AI
        if ( pun5_2[i]==0 ) theta_j[ 5]=1  ; // retributive
        if ( pun5_2[i]==0 ) theta_j[ 6]=1  ; // deterrent
        if ( pun5_2[i]==0 ) theta_j[ 7]=1  ; // norm-enforcing
        if ( pun5_2[i]==1 ) theta_j[ 8]=1  ; // antisocial
                            theta_j[ 9]=0.5; // random choice
        if ( pun5_2[i]==0 ) theta_j[10]=1  ; // never punish
      }
      // 3PP - Take
      if ( B==11 ) {
        if ( pun6_1[i]==-999 ) missing=1   ;
        if ( pun6_1[i]==1 ) theta_j[ 1]=1  ; // competitive
        if ( pun6_1[i]==0 ) theta_j[ 2]=1  ; // avoid DI
        if ( pun6_1[i]==1 ) theta_j[ 3]=1  ; // egalitarian
        if ( pun6_1[i]==0 ) theta_j[ 4]=1  ; // seek AI
        if ( pun6_1[i]==0 ) theta_j[ 5]=1  ; // retributive
        if ( pun6_1[i]==0 ) theta_j[ 6]=1  ; // deterrent
        if ( pun6_1[i]==1 ) theta_j[ 7]=1  ; // norm-enforcing
        if ( pun6_1[i]==0 ) theta_j[ 8]=1  ; // antisocial
                            theta_j[ 9]=0.5; // random choice
        if ( pun6_1[i]==0 ) theta_j[10]=1  ; // never punish
      }
      // 3PP - No take
      if ( B==12 ) {
        if ( pun6_2[i]==-999 ) missing=1   ;
        if ( pun6_2[i]==1 ) theta_j[ 1]=1  ; // competitive
        if ( pun6_2[i]==0 ) theta_j[ 2]=1  ; // avoid DI
        if ( pun6_2[i]==0 ) theta_j[ 3]=1  ; // egalitarian
        if ( pun6_2[i]==0 ) theta_j[ 4]=1  ; // seek AI
        if ( pun6_2[i]==0 ) theta_j[ 5]=1  ; // retributive
        if ( pun6_2[i]==0 ) theta_j[ 6]=1  ; // deterrent
        if ( pun6_2[i]==0 ) theta_j[ 7]=1  ; // norm-enforcing
        if ( pun6_2[i]==1 ) theta_j[ 8]=1  ; // antisocial
                            theta_j[ 9]=0.5; // random choice
        if ( pun6_2[i]==0 ) theta_j[10]=1  ; // never punish
      }
      
      // only if case observed
      if ( missing==0 ) {
        // calculate p vector for this case
        p = softmax( alpha );
        // iterate over strategies
        for (S in 1:10) {
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
}
