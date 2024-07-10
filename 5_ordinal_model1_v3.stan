// Model 3 - School Random effects. NO Centering or QR decomposition! 
// DIRICHLET PRIOR PARAMETERISATION
// ONLY random intercept (school)
// generated with brms 2.19.0
data {
  int<lower=1> N;                 // total number of observations
  int Y[N];                       // response variable
  int<lower=2> nthres;            // number of thresholds
  int<lower=1> K;                 // number of population-level effects
  matrix[N, K] X;                 // population-level design matrix
  // School-Level Random Effects
  // data for group-level effects of ID 1
  int<lower=1> N_1;               // number of grouping levels
  int<lower=1> J_1[N];            // grouping indicator per observation
  int prior_only;                 // should the likelihood be ignored?
  int ppc;                        // Posterior Predictive Check
  real<lower = 0> conc;           // concentration prior for dirichlet_lpdf(pi | rep_vector(conc, k))
}
transformed data {

}
parameters {
  vector[K] b;                    // population-level effects
  simplex[nthres+1] pi;  					// category probabilities for a person w/ average predictors
  real<lower = 0> sd_1;      // group-level standard deviations            // Could change this to a constant!! 
  vector[N_1] z_1;           // standardized group-level effects
}
transformed parameters {
  vector[N_1] r_1_1;              // unstandardized actual group-level effects
  real lprior = 0;                // prior contributions to the log posterior
  ordered[nthres] b_Intercept;      // temporary thresholds for centered predictors
  for (j in 1:nthres) b_Intercept[j] = logit(sum(pi[1:j])); 			// predictors are CENTERED
  // compute actual group-level effects (school)
  r_1_1 = (sd_1 * z_1);
  lprior += dirichlet_lpdf(pi | rep_vector(conc, nthres+1));
  lprior += student_t_lpdf(sd_1 | 3, 0, 2.5) - 1 * student_t_lccdf(0 | 3, 0, 2.5);        
  lprior += student_t_lpdf(b | 3, 0, 10);
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu = rep_vector(0.0, N);
    mu += X * b;
    for (n in 1:N) {
      // add more terms to the linear predictor
      mu[n] += r_1_1[J_1[n]]; //* Z_1_1[n] 
    }
    for (n in 1:N) {
      target += ordered_logistic_lpmf(Y[n] | mu[n], b_Intercept);
    }
  }
  // priors including constants
  target += lprior;
  target += std_normal_lpdf(z_1);
}
// generated quantities {
//   vector[N] Y_rep;                                                            // only variables declared outside of the brackets will be exported
// 
//   if (ppc){
//     vector[N] mu_ppc = rep_vector(0.0, N);
// 
//     // Recalculate mu
//     mu_ppc += X * b;
//     for (n in 1:N) {
//       mu_ppc[n] += r_1_1[J_1[n]];
//     }
// 
//     // Generate new responses
//     for (n in 1:N) {
//       Y_rep[n] = ordered_logistic_rng(mu_ppc[n], b_Intercept);
//     }
//   } else {
//     Y_rep = rep_vector(0.0, N); // Assign a default zero vector to Y_rep
//   }
// }
