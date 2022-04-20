data {
  int N;         // number of participants
  real x[N];     // individual difference measure
  int pun1_1[N]; // punishment behaviour 1.1
  int pun1_2[N]; // punishment behaviour 1.2
  int pun2_1[N]; // punishment behaviour 2.1
  int pun2_2[N]; // punishment behaviour 2.2
  int pun3_1[N]; // punishment behaviour 3.1
  int pun3_2[N]; // punishment behaviour 3.2
  int pun4_1[N]; // punishment behaviour 4.1
  int pun4_2[N]; // punishment behaviour 4.2
  real<lower=0,upper=1> error; // error rate (assumed)
}
parameters {
  vector[9] alpha; // intercepts for probabilities of different strategies
  vector[9] beta;  // slopes for probabilities of different strategies
}
model {
  // vectors to hold terms of sum and probabilities
  vector[9] theta_j;
  vector[9] p;
  
  // priors
  alpha ~ normal(0, 1);
  beta ~ normal(0, 0.5);
  
  // loop over eight punishment behaviours
  for (B in 1:8) {
    // probability of data
    for (i in 1:N ) {
      theta_j = rep_vector(0,9); // clear out the vector
      // behaviour 1.1
      if ( B==1 && pun1_1[i]==1 ) theta_j[1]=1  ; // competitive
      if ( B==1 && pun1_1[i]==0 ) theta_j[2]=1  ; // avoid-di
      if ( B==1 && pun1_1[i]==0 ) theta_j[3]=1  ; // egalitarian
      if ( B==1 && pun1_1[i]==0 ) theta_j[4]=1  ; // ai-seeking
      if ( B==1 && pun1_1[i]==1 ) theta_j[5]=1  ; // retributive
      if ( B==1 && pun1_1[i]==1 ) theta_j[6]=1  ; // norm-enforcing
      if ( B==1 && pun1_1[i]==0 ) theta_j[7]=1  ; // antisocial
      if ( B==1                 ) theta_j[8]=0.5; // random
      if ( B==1 && pun1_1[i]==0 ) theta_j[9]=1  ; // never punish
      // behaviour 1.2
      if ( B==2 && pun1_2[i]==1 ) theta_j[1]=1  ; // competitive
      if ( B==2 && pun1_2[i]==0 ) theta_j[2]=1  ; // avoid-di
      if ( B==2 && pun1_2[i]==0 ) theta_j[3]=1  ; // egalitarian
      if ( B==2 && pun1_2[i]==0 ) theta_j[4]=1  ; // ai-seeking
      if ( B==2 && pun1_2[i]==0 ) theta_j[5]=1  ; // retributive
      if ( B==2 && pun1_2[i]==0 ) theta_j[6]=1  ; // norm-enforcing
      if ( B==2 && pun1_2[i]==1 ) theta_j[7]=1  ; // antisocial
      if ( B==2                 ) theta_j[8]=0.5; // random
      if ( B==2 && pun1_2[i]==0 ) theta_j[9]=1  ; // never punish
      // behaviour 2.1
      if ( B==3 && pun2_1[i]==1 ) theta_j[1]=1  ; // competitive
      if ( B==3 && pun2_1[i]==0 ) theta_j[2]=1  ; // avoid-di
      if ( B==3 && pun2_1[i]==0 ) theta_j[3]=1  ; // egalitarian
      if ( B==3 && pun2_1[i]==1 ) theta_j[4]=1  ; // ai-seeking
      if ( B==3 && pun2_1[i]==1 ) theta_j[5]=1  ; // retributive
      if ( B==3 && pun2_1[i]==1 ) theta_j[6]=1  ; // norm-enforcing
      if ( B==3 && pun2_1[i]==0 ) theta_j[7]=1  ; // antisocial
      if ( B==3                 ) theta_j[8]=0.5; // random
      if ( B==3 && pun2_1[i]==0 ) theta_j[9]=1  ; // never punish
      // behaviour 2.2
      if ( B==4 && pun2_2[i]==1 ) theta_j[1]=1  ; // competitive
      if ( B==4 && pun2_2[i]==0 ) theta_j[2]=1  ; // avoid-di
      if ( B==4 && pun2_2[i]==0 ) theta_j[3]=1  ; // egalitarian
      if ( B==4 && pun2_2[i]==0 ) theta_j[4]=1  ; // ai-seeking
      if ( B==4 && pun2_2[i]==0 ) theta_j[5]=1  ; // retributive
      if ( B==4 && pun2_2[i]==0 ) theta_j[6]=1  ; // norm-enforcing
      if ( B==4 && pun2_2[i]==1 ) theta_j[7]=1  ; // antisocial
      if ( B==4                 ) theta_j[8]=0.5; // random
      if ( B==4 && pun2_2[i]==0 ) theta_j[9]=1  ; // never punish
      // behaviour 3.1
      if ( B==5 && pun3_1[i]==1 ) theta_j[1]=1  ; // competitive
      if ( B==5 && pun3_1[i]==1 ) theta_j[2]=1  ; // avoid-di
      if ( B==5 && pun3_1[i]==1 ) theta_j[3]=1  ; // egalitarian
      if ( B==5 && pun3_1[i]==1 ) theta_j[4]=1  ; // ai-seeking
      if ( B==5 && pun3_1[i]==1 ) theta_j[5]=1  ; // retributive
      if ( B==5 && pun3_1[i]==1 ) theta_j[6]=1  ; // norm-enforcing
      if ( B==5 && pun3_1[i]==0 ) theta_j[7]=1  ; // antisocial
      if ( B==5                 ) theta_j[8]=0.5; // random
      if ( B==5 && pun3_1[i]==0 ) theta_j[9]=1  ; // never punish
      // behaviour 3.2
      if ( B==6 && pun3_2[i]==1 ) theta_j[1]=1  ; // competitive
      if ( B==6 && pun3_2[i]==0 ) theta_j[2]=1  ; // avoid-di
      if ( B==6 && pun3_2[i]==0 ) theta_j[3]=1  ; // egalitarian
      if ( B==6 && pun3_2[i]==0 ) theta_j[4]=1  ; // ai-seeking
      if ( B==6 && pun3_2[i]==0 ) theta_j[5]=1  ; // retributive
      if ( B==6 && pun3_2[i]==0 ) theta_j[6]=1  ; // norm-enforcing
      if ( B==6 && pun3_2[i]==1 ) theta_j[7]=1  ; // antisocial
      if ( B==6                 ) theta_j[8]=0.5; // random
      if ( B==6 && pun3_2[i]==0 ) theta_j[9]=1  ; // never punish
      // behaviour 4.1
      if ( B==7 && pun4_1[i]==1 ) theta_j[1]=1  ; // competitive
      if ( B==7 && pun4_1[i]==0 ) theta_j[2]=1  ; // avoid-di
      if ( B==7 && pun4_1[i]==1 ) theta_j[3]=1  ; // egalitarian
      if ( B==7 && pun4_1[i]==0 ) theta_j[4]=1  ; // ai-seeking
      if ( B==7 && pun4_1[i]==0 ) theta_j[5]=1  ; // retributive
      if ( B==7 && pun4_1[i]==1 ) theta_j[6]=1  ; // norm-enforcing
      if ( B==7 && pun4_1[i]==0 ) theta_j[7]=1  ; // antisocial
      if ( B==7                 ) theta_j[8]=0.5; // random
      if ( B==7 && pun4_1[i]==0 ) theta_j[9]=1  ; // never punish
      // behaviour 4.1
      if ( B==8 && pun4_2[i]==1 ) theta_j[1]=1  ; // competitive
      if ( B==8 && pun4_2[i]==0 ) theta_j[2]=1  ; // avoid-di
      if ( B==8 && pun4_2[i]==0 ) theta_j[3]=1  ; // egalitarian
      if ( B==8 && pun4_2[i]==0 ) theta_j[4]=1  ; // ai-seeking
      if ( B==8 && pun4_2[i]==0 ) theta_j[5]=1  ; // retributive
      if ( B==8 && pun4_2[i]==0 ) theta_j[6]=1  ; // norm-enforcing
      if ( B==8 && pun4_2[i]==1 ) theta_j[7]=1  ; // antisocial
      if ( B==8                 ) theta_j[8]=0.5; // random
      if ( B==8 && pun4_2[i]==0 ) theta_j[9]=1  ; // never punish
      
      // calculate p vector for this case
      p = softmax( alpha + beta*x[i] );
      
      for (S in 1:9) {
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
