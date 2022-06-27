data {
  int N;         // number of participants
  real x[N];     // individual difference measure
  int pun1_1[N]; // No DI 1 - Take
  int pun1_2[N]; // No DI 1 - No take
  int pun2_1[N]; // No DI 2 - Take
  int pun2_2[N]; // No DI 2 - No take
  int pun3_1[N]; // No DI 3 - Take
  int pun3_2[N]; // No DI 3 - No take
  int pun4_1[N]; // DI - Take
  int pun4_2[N]; // DI - No take
  int pun5_1[N]; // 3PP - Take
  int pun5_2[N]; // 3PP - No take
  real<lower=0,upper=1> error; // error rate (assumed)
}
parameters {
  vector[10] alpha; // intercepts for probabilities of different strategies
  vector[10] beta;  // slopes for probabilities of different strategies
}
model {
  // vectors to hold terms of sum and probabilities
  vector[10] theta_j;
  vector[10] p;
  
  // priors
  alpha ~ normal(0, 1);
  beta ~ normal(0, 0.5);
  
  // loop over ten punishment behaviours
  for (B in 1:10) {
    // probability of data
    for (i in 1:N ) {
      theta_j = rep_vector(0,10); // clear out the vector
      // No DI 1 - Take
      if ( B==1  && pun1_1[i]==1 ) theta_j[ 1]=1  ; // competitive
      if ( B==1  && pun1_1[i]==0 ) theta_j[ 2]=1  ; // avoid DI
      if ( B==1  && pun1_1[i]==0 ) theta_j[ 3]=1  ; // egalitarian
      if ( B==1  && pun1_1[i]==0 ) theta_j[ 4]=1  ; // seek AI
      if ( B==1  && pun1_1[i]==1 ) theta_j[ 5]=1  ; // retributive
      if ( B==1  && pun1_1[i]==1 ) theta_j[ 6]=1  ; // deterrent
      if ( B==1  && pun1_1[i]==1 ) theta_j[ 7]=1  ; // norm-enforcing
      if ( B==1  && pun1_1[i]==0 ) theta_j[ 8]=1  ; // antisocial
      if ( B==1                  ) theta_j[ 9]=0.5; // random choice
      if ( B==1  && pun1_1[i]==0 ) theta_j[10]=1  ; // never punish
      // No DI 1 - No take
      if ( B==2  && pun1_2[i]==1 ) theta_j[ 1]=1  ; // competitive
      if ( B==2  && pun1_2[i]==0 ) theta_j[ 2]=1  ; // avoid DI
      if ( B==2  && pun1_2[i]==0 ) theta_j[ 3]=1  ; // egalitarian
      if ( B==2  && pun1_2[i]==0 ) theta_j[ 4]=1  ; // seek AI
      if ( B==2  && pun1_2[i]==0 ) theta_j[ 5]=1  ; // retributive
      if ( B==2  && pun1_2[i]==0 ) theta_j[ 6]=1  ; // deterrent
      if ( B==2  && pun1_2[i]==0 ) theta_j[ 7]=1  ; // norm-enforcing
      if ( B==2  && pun1_2[i]==1 ) theta_j[ 8]=1  ; // antisocial
      if ( B==2                  ) theta_j[ 9]=0.5; // random choice
      if ( B==2  && pun1_2[i]==0 ) theta_j[10]=1  ; // never punish
      // No DI 2 - Take
      if ( B==3  && pun2_1[i]==1 ) theta_j[ 1]=1  ; // competitive
      if ( B==3  && pun2_1[i]==0 ) theta_j[ 2]=1  ; // avoid DI
      if ( B==3  && pun2_1[i]==0 ) theta_j[ 3]=1  ; // egalitarian
      if ( B==3  && pun2_1[i]==1 ) theta_j[ 4]=1  ; // seek AI
      if ( B==3  && pun2_1[i]==1 ) theta_j[ 5]=1  ; // retributive
      if ( B==3  && pun2_1[i]==1 ) theta_j[ 6]=1  ; // deterrent
      if ( B==3  && pun2_1[i]==1 ) theta_j[ 7]=1  ; // norm-enforcing
      if ( B==3  && pun2_1[i]==0 ) theta_j[ 8]=1  ; // antisocial
      if ( B==3                  ) theta_j[ 9]=0.5; // random choice
      if ( B==3  && pun2_1[i]==0 ) theta_j[10]=1  ; // never punish
      // No DI 2 - No take
      if ( B==4  && pun2_2[i]==1 ) theta_j[ 1]=1  ; // competitive
      if ( B==4  && pun2_2[i]==0 ) theta_j[ 2]=1  ; // avoid DI
      if ( B==4  && pun2_2[i]==0 ) theta_j[ 3]=1  ; // egalitarian
      if ( B==4  && pun2_2[i]==0 ) theta_j[ 4]=1  ; // seek AI
      if ( B==4  && pun2_2[i]==0 ) theta_j[ 5]=1  ; // retributive
      if ( B==4  && pun2_2[i]==0 ) theta_j[ 6]=1  ; // deterrent
      if ( B==4  && pun2_2[i]==0 ) theta_j[ 7]=1  ; // norm-enforcing
      if ( B==4  && pun2_2[i]==1 ) theta_j[ 8]=1  ; // antisocial
      if ( B==4                  ) theta_j[ 9]=0.5; // random choice
      if ( B==4  && pun2_2[i]==0 ) theta_j[10]=1  ; // never punish
      // No DI 3 - Take
      if ( B==5  && pun3_1[i]==1 ) theta_j[ 1]=1  ; // competitive
      if ( B==5  && pun3_1[i]==0 ) theta_j[ 2]=1  ; // avoid DI
      if ( B==5  && pun3_1[i]==0 ) theta_j[ 3]=1  ; // egalitarian
      if ( B==5  && pun3_1[i]==1 ) theta_j[ 4]=1  ; // seek AI
      if ( B==5  && pun3_1[i]==1 ) theta_j[ 5]=1  ; // retributive
      if ( B==5  && pun3_1[i]==0 ) theta_j[ 6]=1  ; // deterrent
      if ( B==5  && pun3_1[i]==0 ) theta_j[ 7]=1  ; // norm-enforcing
      if ( B==5  && pun3_1[i]==0 ) theta_j[ 8]=1  ; // antisocial
      if ( B==5                  ) theta_j[ 9]=0.5; // random choice
      if ( B==5  && pun3_1[i]==0 ) theta_j[10]=1  ; // never punish
      // No DI 3 - No take
      if ( B==6  && pun3_2[i]==1 ) theta_j[ 1]=1  ; // competitive
      if ( B==6  && pun3_2[i]==0 ) theta_j[ 2]=1  ; // avoid DI
      if ( B==6  && pun3_2[i]==0 ) theta_j[ 3]=1  ; // egalitarian
      if ( B==6  && pun3_2[i]==0 ) theta_j[ 4]=1  ; // seek AI
      if ( B==6  && pun3_2[i]==0 ) theta_j[ 5]=1  ; // retributive
      if ( B==6  && pun3_2[i]==0 ) theta_j[ 6]=1  ; // deterrent
      if ( B==6  && pun3_2[i]==0 ) theta_j[ 7]=1  ; // norm-enforcing
      if ( B==6  && pun3_2[i]==1 ) theta_j[ 8]=1  ; // antisocial
      if ( B==6                  ) theta_j[ 9]=0.5; // random choice
      if ( B==6  && pun3_2[i]==0 ) theta_j[10]=1  ; // never punish
      // DI - Take
      if ( B==7  && pun4_1[i]==1 ) theta_j[ 1]=1  ; // competitive
      if ( B==7  && pun4_1[i]==1 ) theta_j[ 2]=1  ; // avoid DI
      if ( B==7  && pun4_1[i]==1 ) theta_j[ 3]=1  ; // egalitarian
      if ( B==7  && pun4_1[i]==1 ) theta_j[ 4]=1  ; // seek AI
      if ( B==7  && pun4_1[i]==1 ) theta_j[ 5]=1  ; // retributive
      if ( B==7  && pun4_1[i]==1 ) theta_j[ 6]=1  ; // deterrent
      if ( B==7  && pun4_1[i]==1 ) theta_j[ 7]=1  ; // norm-enforcing
      if ( B==7  && pun4_1[i]==0 ) theta_j[ 8]=1  ; // antisocial
      if ( B==7                  ) theta_j[ 9]=0.5; // random choice
      if ( B==7  && pun4_1[i]==0 ) theta_j[10]=1  ; // never punish
      // DI - No take
      if ( B==8  && pun4_2[i]==1 ) theta_j[ 1]=1  ; // competitive
      if ( B==8  && pun4_2[i]==0 ) theta_j[ 2]=1  ; // avoid DI
      if ( B==8  && pun4_2[i]==0 ) theta_j[ 3]=1  ; // egalitarian
      if ( B==8  && pun4_2[i]==0 ) theta_j[ 4]=1  ; // seek AI
      if ( B==8  && pun4_2[i]==0 ) theta_j[ 5]=1  ; // retributive
      if ( B==8  && pun4_2[i]==0 ) theta_j[ 6]=1  ; // deterrent
      if ( B==8  && pun4_2[i]==0 ) theta_j[ 7]=1  ; // norm-enforcing
      if ( B==8  && pun4_2[i]==1 ) theta_j[ 8]=1  ; // antisocial
      if ( B==8                  ) theta_j[ 9]=0.5; // random choice
      if ( B==8  && pun4_2[i]==0 ) theta_j[10]=1  ; // never punish
      // 3PP - Take
      if ( B==9  && pun5_1[i]==1 ) theta_j[ 1]=1  ; // competitive
      if ( B==9  && pun5_1[i]==0 ) theta_j[ 2]=1  ; // avoid DI
      if ( B==9  && pun5_1[i]==1 ) theta_j[ 3]=1  ; // egalitarian
      if ( B==9  && pun5_1[i]==0 ) theta_j[ 4]=1  ; // seek AI
      if ( B==9  && pun5_1[i]==0 ) theta_j[ 5]=1  ; // retributive
      if ( B==9  && pun5_1[i]==0 ) theta_j[ 6]=1  ; // deterrent
      if ( B==9  && pun5_1[i]==1 ) theta_j[ 7]=1  ; // norm-enforcing
      if ( B==9  && pun5_1[i]==0 ) theta_j[ 8]=1  ; // antisocial
      if ( B==9                  ) theta_j[ 9]=0.5; // random choice
      if ( B==9  && pun5_1[i]==0 ) theta_j[10]=1  ; // never punish
      // 3PP - No take
      if ( B==10 && pun5_2[i]==1 ) theta_j[ 1]=1  ; // competitive
      if ( B==10 && pun5_2[i]==0 ) theta_j[ 2]=1  ; // avoid DI
      if ( B==10 && pun5_2[i]==0 ) theta_j[ 3]=1  ; // egalitarian
      if ( B==10 && pun5_2[i]==0 ) theta_j[ 4]=1  ; // seek AI
      if ( B==10 && pun5_2[i]==0 ) theta_j[ 5]=1  ; // retributive
      if ( B==10 && pun5_2[i]==0 ) theta_j[ 6]=1  ; // deterrent
      if ( B==10 && pun5_2[i]==0 ) theta_j[ 7]=1  ; // norm-enforcing
      if ( B==10 && pun5_2[i]==1 ) theta_j[ 8]=1  ; // antisocial
      if ( B==10                 ) theta_j[ 9]=0.5; // random choice
      if ( B==10 && pun5_2[i]==0 ) theta_j[10]=1  ; // never punish
      
      // calculate p vector for this case
      p = softmax( alpha + beta*x[i] );
      
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
