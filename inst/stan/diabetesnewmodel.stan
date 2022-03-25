functions {

  real[] weightRedux_rng(real[] update){
    real temp[size(update)];
    real male_weightredux[7];
    real female_weightredux[7];
    real current_weight;
    real current_height;
    int age_group_index;

    male_weightredux[1]=2.351;
    male_weightredux[2]=1.227;
    male_weightredux[3]=1.652;
    male_weightredux[4]=1.412;
    male_weightredux[5]=1.102;
    male_weightredux[6]=0.638;
    male_weightredux[7]=0.795;

    female_weightredux[1]=1.562;
    female_weightredux[2]=1.320;
    female_weightredux[3]=1.123;
    female_weightredux[4]=0.784;
    female_weightredux[5]=0.647;
    female_weightredux[6]=0.472;
    female_weightredux[7]=0.567;

    temp=update;
    age_group_index = 0;
    while(temp[1]/10>=age_group_index+2){
      age_group_index+=1;
    }
    if (age_group_index>7){
      age_group_index=7;
    }
    current_height = temp[6];
    if (temp[52]==0){
      current_weight=temp[56]*current_height^2;
    if (temp[3]==0){
      current_weight=current_weight-normal_rng(female_weightredux[age_group_index],
      female_weightredux[age_group_index]*0.2);
    }
    else {
      current_weight=current_weight-normal_rng(male_weightredux[age_group_index],
      male_weightredux[age_group_index]*0.2);
    }
    } else {
    if (temp[3]==0){
      current_weight=temp[5]-normal_rng(female_weightredux[age_group_index],
      female_weightredux[age_group_index]*0.2);
    }
    else {
      current_weight=temp[5]-normal_rng(male_weightredux[age_group_index],
      male_weightredux[age_group_index]*0.2);
    }
    }
    temp[5] =current_weight;

    if (temp[52]==1){
      temp[13]=current_weight/current_height^2;
    }
    else if (temp[52]==0){
      temp[56]=current_weight/current_height^2;
    }

    return temp;
  }

  real[] simDiabetes(real[] update,real intercept, real age, real bmi,
  real sbp, real dbp, real bgl, real hdl, real trig, real female, real ethni,
  real rand){
    real predictor;
    real H1;
    real prob;
    real current_age;
    real temp[size(update)];

    temp=update;
    current_age = temp[1];

    predictor=intercept+age*temp[1]+hdl*temp[58]+trig*temp[55]+bgl*temp[54]+
    bmi*temp[56]+dbp*temp[53]+sbp*temp[57]+female*(1-temp[3])+
    ethni*(temp[4]==0?1:0);

    H1=1/(1+exp(-predictor));
    prob=1 - (1 - H1)^(1.0/10);
    if (prob>rand){
      temp[52]=1;
      temp[2]=current_age;
      temp[7]=1;
    }
    return temp;
  }

  real[] simDeath_not2d(real[] update, real rand){
    real temp[size(update)];
    int age_group_index;
    real male_death_prob[18];
    real female_death_prob[18];
    real prob;

    temp=update;

    male_death_prob[1]=0.013474053;
    male_death_prob[2]=0.002120751;
    male_death_prob[3]=0.001161851;
    male_death_prob[4]=0.001658403;
    male_death_prob[5]=0.005024379;
    male_death_prob[6]=0.008528301;
    male_death_prob[7]=0.010602457;
    male_death_prob[8]=0.01215091;
    male_death_prob[9]=0.015116213;
    male_death_prob[10]=0.020255891;
    male_death_prob[11]=0.025589345;
    male_death_prob[12]=0.036561306;
    male_death_prob[13]=0.051702029;
    male_death_prob[14]=0.071972068;
    male_death_prob[15]=0.109105713;
    male_death_prob[16]=0.150325933;
    male_death_prob[17]=0.22667667;
    male_death_prob[18]=0.360242006;

    female_death_prob[1]=0.011028114;
    female_death_prob[2]=0.00186436;
    female_death_prob[3]=0.001021302;
    female_death_prob[4]=0.001215887;
    female_death_prob[5]=0.002009777;
    female_death_prob[6]=0.002782015;
    female_death_prob[7]=0.003305858;
    female_death_prob[8]=0.003894631;
    female_death_prob[9]=0.005597997;
    female_death_prob[10]=0.008382667;
    female_death_prob[11]=0.012202781;
    female_death_prob[12]=0.019929291;
    female_death_prob[13]=0.031109977;
    female_death_prob[14]=0.047429027;
    female_death_prob[15]=0.075366165;
    female_death_prob[16]=0.111119515;
    female_death_prob[17]=0.173337897;
    female_death_prob[18]=0.28546191;

    age_group_index = 2;
    while (2+temp[1]/5>=age_group_index+1){
      age_group_index+=1;
    }

    if (age_group_index>18){
      age_group_index=18;
    }

    if (temp[3]==0){
      prob=female_death_prob[age_group_index];
    }
    else {
      prob=male_death_prob[age_group_index];
    }

    if (prob>rand){
      temp[32]=0;
    }

    return temp;
  }

  real smoke_rf(real[] update, real constant, real female,real age_diag,
  real rf_prev_year, real year, real ref_first, real rand){
    real temp[size(update)];
    real predictor;
    real P_smoke;
    real outcome;
    temp=update;

    predictor = constant + rf_prev_year*temp[8] + year*log(temp[7]) +
    ref_first*temp[39]+female*(temp[3]==1?1:0)+age_diag*temp[2];
    P_smoke=1/(1+exp(-predictor));

    if (P_smoke>rand){
      outcome=1;
    } else{
      outcome=0;
    }
  return outcome;
  }

  real micalb_rf(real[] update, real constant, real ro, real female,
  real age_diag, real smoker,real sbp, real hb1ac, real bmi, real hdl,
  real rand){
    real temp[size(update)];
    real predictor;
    real H1;
    real H2=H1;
    real prob;
    real outcome;
    temp=update;

    predictor=constant+age_diag*temp[2]+(sbp*temp[9])/10+female*temp[3]+
    hb1ac*temp[10]+smoker*temp[8]+bmi*temp[13]+10*hdl*temp[12];
    H1=exp(predictor)*temp[7]^(ro);
    H2=exp(predictor)*(temp[7]+1)^(ro);
    prob=1 - exp(H1 - H2);

    if (prob>rand){
      outcome=1;
    } else{
      outcome=0;
    }
    return outcome;
  }

  real pvd_rf(real[] update, real constant, real ro, real age_diag, real smoker,
  real sbp, real hb1ac, real bmi, real ldl, real rand){
    real temp[size(update)];
    real predictor;
    real H1;
    real H2=H1;
    real prob;
    real outcome;
    temp=update;

    predictor=constant+age_diag*temp[2]+(sbp*temp[9])/10+hb1ac*temp[10]+
    smoker*temp[8]+bmi*temp[13]+10*ldl*temp[11];
    H1=exp(predictor)*temp[7]^(ro);
    H2=exp(predictor)*(temp[7]+1)^(ro);
    prob=1 - exp(H1 - H2);

    if (prob>rand){
      outcome=1;
    } else{
      outcome=0;
    }
    return outcome;
  }

  real atfib_rf(real[] update,real intercept, real age_diag,real bmi,real rand){
    real predictor;
    real H1;
    real H2=H1;
    real prob;
    real temp[size(update)];
    real outcome;

    temp=update;
    predictor=intercept+age_diag*temp[2]+bmi*temp[13];
    H1=exp(predictor)*temp[7];
    H2=exp(predictor)*(temp[7]+1);
    prob=1 - exp(H1 - H2);
    if (prob>rand){
      outcome=1;
    } else{
      outcome=0;
    }
    return outcome;
  }

  real efgr60l_binary(real[] update, real constant, real ro, real female,
  real african, real asian, real age_diag, real sbp, real bmi, real hdl,
  real ldl, real rand){
    real temp[size(update)];
    real predictor;
    real H1;
    real H2=H1;
    real prob;
    real outcome;
    temp=update;

    predictor=constant+age_diag*temp[2]+(sbp*temp[9])/10+female*temp[3]+
    10*ldl*temp[11]+bmi*temp[13]+10*hdl*temp[12]+ asian*(temp[4]==1?1:0)+
    african*(temp[4]==2?1:0);
    H1=exp(predictor)*temp[7]^(ro);
    H2=exp(predictor)*(temp[7]+1)^(ro);
    prob=1 - exp(H1 - H2);

    if (prob>rand){
      outcome=1;
    } else{
      outcome=0;
    }
    return outcome;
  }

  real[] egfr60less_rf(real[] update, real constant, real ro, real female,
  real african, real asian, real egfr, real egfr_initial, real year){
    real temp[size(update)];
    real predictor;
    real ph1;
    real ph2;
    real PHI1;
    real PHI2;
    real outcome;
    temp=update;

    predictor=constant+female*temp[3]+african*(temp[4]==2?1:0)+
    asian*(temp[4]==1?1:0)+(egfr*temp[15])+ egfr_initial*temp[46]+
    year*log(temp[7]);

    PHI1=Phi((-predictor)/ro);
    PHI2=Phi((60-predictor)/ro);
    ph1=(1/sqrt(2*pi()))*exp((-1/2.0)*((-predictor)/ro)^2);
    ph2=(1/sqrt(2*pi()))*exp((-1/2.0)*((60-predictor)/ro)^2);

    outcome=predictor-ro*((ph1-ph2)/(PHI1-PHI2));
    temp[15]=outcome;
    return temp;
  }

  real[] egfr60more_rf(real[] update, real constant, real ro, real female,
  real african, real asian, real egfr, real egfr_initial, real year){
    real temp[size(update)];
    real predictor;
    real ph2;
    real PHI2;
    real outcome;
    temp=update;

    predictor=constant+female*temp[3]+african*(temp[4]==2?1:0)+
    asian*(temp[4]==1?1:0)+(egfr*temp[15])+ egfr_initial*temp[46]+
    year*log(temp[7]);

    PHI2=Phi((60-predictor)/ro);
    ph2=(1/sqrt(2*pi()))*exp((-1/2.0)*((60-predictor)/ro)^2);

    outcome=predictor+ro*((ph2)/(1-PHI2));
    temp[15]=outcome;
    return temp;
  }

  real[] continuous_rf(int position_rf, real[] update,real constant,real female,
  real african, real asian, real rf_prev_year, real age_diag, real ref_first) {
    real rf_current_year;
    real temp[size(update)];
    temp=update;

    rf_current_year=constant + rf_prev_year*temp[position_rf] +
    age_diag * log(temp[7]) + ref_first*temp[position_rf + 31] +
    asian*(temp[4]==1?1:0)+ african*(temp[4]==2?1:0) + female*(temp[3]==1?1:0);

    temp[position_rf] = rf_current_year;

    if (position_rf==13){
      temp[5] = rf_current_year*(temp[6])^2;
    }

    return temp;
  }

  real[] cholesterol_rf(int position_rf, real[] update,real constant,
  real female, real african, real asian, real rf_prev_year, real age_diag,
  real ref_first) {
    real rf_current_year;
    real temp[size(update)];
    temp=update;

    rf_current_year=constant + rf_prev_year*temp[position_rf] +
    age_diag * log(temp[7]) + ref_first*temp[position_rf + 31] +
    asian*(temp[4]==1?1:0)+ african*(temp[4]==2?1:0) + female*(temp[3]==1?1:0);

    temp[position_rf] = rf_current_year;

    return temp;
  }

  real ro_rng(real mu, real sigma) {
    real p = normal_cdf(0, mu, sigma);   // cdf for lb
    real u = uniform_rng(p, 1);
    real z = inv_Phi(u);
    real y = mu + sigma * z;
    return y;
  }

  real[] simCHF(real[] update, real intercept, real ro, real age_diab,
  real atfib, real bmi, real egfr60l, real ld, real mmalb, real pvd,
  real amp_hist, real ulcer, real rand){
    real predictor;
    real H1;
    real H2=H1;
    real prob;
    real temp[size(update)];

    temp=update;
    predictor=intercept+age_diab*temp[2]+bmi*temp[13]+10*ld*temp[11]+
    amp_hist*temp[25]+ulcer*temp[24]+atfib*temp[20]+
    (egfr60l*(temp[15]<60?temp[15]:0))/10+mmalb*temp[19]+pvd*temp[16];

    H1=exp(predictor)*temp[7]^(ro);
    H2=exp(predictor)*(temp[7]+1)^(ro);
    prob=1 - exp(H1 - H2);
    if (prob>rand){
      temp[29]=1;
      temp[33]=1;
    }
    return temp;
  }

  real[] simIHD(real[] update,real intercept,real ro,real age_diab,real female,
  real egfr,real hdl,real ldl,real pvd,real sbp,real amp_hist,real chf,
  real rand){
    real predictor;
    real H1;
    real H2=H1;
    real prob;
    real temp[size(update)];

    temp=update;
    predictor=intercept+age_diab*temp[2]+10*ldl*temp[11]+amp_hist*temp[25]+
    chf*temp[29]+(sbp*temp[9])/10+10*hdl*temp[12]+ female*temp[3]+
    (egfr*temp[15])/10+pvd*temp[16];
    H1=exp(predictor)*temp[7]^(ro);
    H2=exp(predictor)*(temp[7]+1)^(ro);
    prob=1 - exp(H1 - H2);
    if (prob>rand){
      temp[28]=1;
      temp[34]=1;
    }
    return temp;
  }

  real[] simMIm(real[] update,real intercept,real afro,real age_diab,real asian,
  real hb1ac,real hdl,real ldl,real mmalb,real pvd, real sbp,real smoker,
  real wbc,real amp,real chf,real ihd,real stroke,real rand){
    real predictor;
    real H1;
    real H2=H1;
    real prob;
    real temp[size(update)];

    temp=update;
    predictor=intercept+age_diab*temp[2]+10*ldl*temp[11]+amp*temp[25]+
    chf*temp[29]+hb1ac*temp[10]+(sbp*temp[9])/10+10*hdl*temp[12]+smoker*temp[8]+
    ihd*temp[28]+stroke*temp[27]+afro*(temp[4]==2?1:0)+asian*(temp[4]==1?1:0)+
    mmalb*temp[19]+pvd*temp[16]+wbc*temp[14];

    H1=exp(predictor)*temp[7];
    H2=exp(predictor)*(temp[7]+1);
    prob=1 - exp(H1 - H2);
    if (prob>rand){
      temp[21]=1;
      temp[26]=1;
      temp[35]=1;
    }
    return temp;
  }

  real[] simMIf(real[] update,real intercept,real ro,real afro,real age_diab,
  real egfr60l,real hb1ac,real ldl35,real mmalb,real pvd,real sbp,real smoker,
  real wbc,real chf,real ihd, real rand){
    real predictor;
    real H1;
    real H2=H1;
    real prob;
    real temp[size(update)];

    temp=update;
    predictor=intercept+age_diab*temp[2]+10*ldl35*(temp[11]>3.5?temp[11]:0)+
    chf*temp[29]+(sbp*temp[9])/10+hb1ac*temp[10]+smoker*temp[8]+ihd*temp[28]+
    afro*(temp[4]==2?1:0)+(egfr60l*(temp[15]<60?temp[15]:0))/10+mmalb*temp[19]+
    pvd*temp[16]+wbc*temp[14];

    H1=exp(predictor)*temp[7]^(ro);
    H2=exp(predictor)*(temp[7]+1)^(ro);
    prob=1 - exp(H1 - H2);
    if (prob>rand){
      temp[21]=1;
      temp[26]=1;
      temp[35]=1;
    }
    return temp;
  }

  real[] simMI2(real[] update,real intercept, real ldl,real mmalb,real rand){
    real predictor;
    real H1;
    real H2=H1;
    real prob;
    real temp[size(update)];

    temp=update;
    predictor=intercept+10*ldl*temp[11]+mmalb*temp[19];
    H1=exp(predictor)*temp[7];
    H2=exp(predictor)*(temp[7]+1);
    prob=1 - exp(H1 - H2);
    if (prob>rand){
      temp[21]=2;
      temp[26]=1;
      temp[35]=1;
    }
    return temp;
  }

  real[] simStroke(real[] update,real intercept,real ro,real age_diab,
  real female,real atfib,real egfr60l,real hb1ac,real ldl,real mmalb,real sbp,
  real smoker,real wbc,real amp,real ihd,real rand){
    real predictor;
    real H1;
    real H2=H1;
    real prob;
    real temp[size(update)];

    temp=update;
    predictor=intercept+age_diab*temp[2]+10*ldl*temp[11]+
    (sbp*temp[9])/10+female*temp[3]+hb1ac*temp[10]+smoker*temp[8]+ihd*temp[28]+
    atfib*temp[20]+amp*temp[25]+(egfr60l*(temp[15]<60?temp[15]:0))/10+
    mmalb*temp[19]+wbc*temp[14];

    H1=exp(predictor)*temp[7]^(ro);
    H2=exp(predictor)*(temp[7]+1)^(ro);
    prob=1 - exp(H1 - H2);
    if (prob>rand){
      temp[22]=1;
      temp[27]=1;
      temp[36]=1;
    }
    return temp;
  }

  real[] simStroke2(real[] update,real intercept,real ro,real age_diab,
  real mmalb,real smoker,real rand){
    real predictor;
    real H1;
    real H2=H1;
    real prob;
    real temp[size(update)];

    temp=update;
    predictor=intercept+age_diab*temp[2]+smoker*temp[8]+mmalb*temp[19];
    H1=exp(predictor)*temp[7]^(ro);
    H2=exp(predictor)*(temp[7]+1)^(ro);
    prob=1 - exp(H1 - H2);
    if (prob>rand){
      temp[22]=2;
      temp[27]=1;
      temp[36]=1;
    }
    return temp;
  }

  real[] simBlind(real[] update,real intercept, real age_diab,real hb1ac,
  real hr, real sbp, real wbc,real chf, real ihd, real rand){
    real predictor;
    real H1;
    real H2=H1;
    real prob;
    real temp[size(update)];

    temp=update;
    predictor=intercept+age_diab*temp[2]+hb1ac*temp[10]+(hr*temp[18])/10+
    wbc*temp[14]+(sbp*temp[9])/10+chf*temp[29]+ihd*temp[28];

    H1=exp(predictor)*temp[7];
    H2=exp(predictor)*(temp[7]+1);
    prob=1 - exp(H1 - H2);
    if (prob>rand){
      temp[30]=1;
    }
    return temp;
  }

  real[] simUlcer(real[] update,real intercept, real age_diab,real female,
  real bmi, real hb1ac,real pvd, real rand){
    real predictor;
    real H1;
    real prob;
    real temp[size(update)];

    temp=update;
    predictor=intercept+age_diab*temp[2]+hb1ac*temp[10]+pvd*temp[16]+
    bmi*temp[13]+female*temp[3];
    H1=exp(-predictor)/(1+exp(-predictor));
    prob=1 - H1;
    if (prob>rand){
      temp[24]=1;
    }
    return temp;
  }

  real[] simAmp_noUlcer(real[] update,real intercept,real ro, real age_diab,
  real female, real atfib,real hb1ac, real hdl, real hr, real mmalb, real pvd,
  real sbp, real wbc, real stroke, real rand){
    real predictor;
    real H1;
    real H2=H1;
    real prob;
    real temp[size(update)];

    temp=update;
    predictor=intercept+age_diab*temp[2]++(sbp*temp[9])/10+female*temp[3]+
    hb1ac*temp[10]+wbc*temp[14]+atfib*temp[20]+10*hdl*temp[12]+(hr*temp[18])/10+
    mmalb*temp[19]+pvd*temp[16]+stroke*temp[27];

    H1=exp(predictor)*temp[7]^(ro);
    H2=exp(predictor)*(temp[7]+1)^(ro);
    prob=1 - exp(H1 - H2);
    if (prob>rand){
      temp[23]=1;
      temp[25]=1;
      temp[37]=1;
    }
    return temp;
  }

  real[] simAmp_Ulcer(real[] update,real intercept, real age_diab,real pvd,
  real rand){
    real predictor;
    real H1;
    real H2=H1;
    real prob;
    real temp[size(update)];

    temp=update;
    predictor=intercept+age_diab*temp[2]+pvd*temp[16];
    H1=exp(predictor)*temp[7];
    H2=exp(predictor)*(temp[7]+1);
    prob=1 - exp(H1 - H2);
    if (prob>rand){
      temp[23]=1;
      temp[25]=1;
      temp[37]=1;
    }
    return temp;
  }

  real[] simAmp2(real[] update,real intercept, real hb1ac,real rand){
    real predictor;
    real H1;
    real H2=H1;
    real prob;
    real temp[size(update)];

    temp=update;
    predictor=intercept+hb1ac*temp[10];
    H1=exp(predictor)*temp[7];
    H2=exp(predictor)*(temp[7]+1);
    prob=1 - exp(H1 - H2);
    if (prob>rand){
      temp[23]=2;
      temp[25]=1;
      temp[37]=2;
    }
    return temp;
  }

  real[] simRenal(real[] update,real intercept, real afro,real age_diab,
  real female,real bmi, real egfr60l,real egfr60m, real haem, real ldl,
  real mmalb, real sbp,real wbc,real amp, real blind, real rand){
    real predictor;
    real H1;
    real H2=H1;
    real prob;
    real temp[size(update)];

    temp=update;
    predictor=intercept+age_diab*temp[2]+bmi*temp[13]+10*ldl*temp[11]+
    amp*temp[25]+blind*temp[30]+(egfr60l*(temp[15]<60?temp[15]:0))/10+
    (egfr60m*(temp[15]>=60?temp[15]:0))/10+female*temp[3]+afro*(temp[4]==2?1:0)+
    (sbp*temp[9])/10+haem*temp[17]+mmalb*temp[19]+wbc*temp[14];
    H1=exp(predictor)*temp[7];
    H2=exp(predictor)*(temp[7]+1);
    prob=1 - exp(H1 - H2);
    if (prob>rand){
      temp[31]=1;
      temp[38]=1;
    }
    return temp;
  }

  real[] simDeath_nohist(real[] update,real intercept, real phi,real female,
  real smoker, real rand){
    real predictor;
    real H1;
    real H2=H1;
    real prob;
    real temp[size(update)];

    temp=update;
    predictor=intercept+female*temp[3]+smoker*temp[8];
    H1=exp(predictor)*(exp(phi*(temp[1]))-1)/phi;
    H2=exp(predictor)*(exp(phi*(temp[1]+1))-1)/phi;
    prob=1 - exp(H1 - H2);
    if (prob>rand){
      temp[32]=0;
    }
    return temp;
  }

  real[] simDeath1st(real[] update,real intercept,real asian,real year,real age,
  real hr,real pvd,real smoker,real amp_event,real ihd_event,real mi_event,
  real renal_event,real stroke_event,real rand){
    real predictor;
    real H1;
    real prob;
    real temp[size(update)];

    temp=update;
    predictor=intercept+year*temp[7]+age*temp[1]+smoker*temp[8]+
    amp_event*(temp[37]==1?1:0)+ihd_event*temp[34]+mi_event*temp[35]+
    asian*(temp[4]==1?1:0)+pvd*temp[16]+renal_event*temp[38]+
    stroke_event*temp[36]+(hr*temp[18])/10;

    H1=exp(-predictor)/(1+exp(-predictor));
    prob=1 - H1;
    if (prob>rand){
      temp[32]=0;
    }
    return temp;
  }

  real[] simDeath_hist(real[] update,real intercept,real phi,real bm1,real bm3,
  real mmalb,real smoker,real wbc, real amp,real chf,real renal,real stroke,
  real rand){
    real predictor;
    real H1;
    real H2=H1;
    real prob;
    real temp[size(update)];

    temp=update;
    predictor=intercept+bm1*(temp[13]<18.5?temp[13]:0)+
    bm3*(temp[13]>25?temp[13]:0)+smoker*temp[8]+amp*temp[25]+chf*temp[29]+
    renal*temp[31]+stroke*temp[27]+mmalb*temp[19]+wbc*temp[14];

    H1=exp(predictor)*(exp(phi*(temp[1]))-1)/phi;
    H2=exp(predictor)*(exp(phi*(temp[1]+1))-1)/phi;
    prob=1 - exp(H1 - H2);
    if (prob>rand){
      temp[32]=0;
    }
    return temp;
  }

  real[] simDeath_sub(real[] update,real intercept,real atfib,real age,real hdl,
  real pvd,real wbc,real amp_event,real amp,real amp_event2,real ihd_event,
  real ihd,real mi_event,real mi,real renal,real stroke_event,real rand){
    real predictor;
    real H1;
    real prob;
    real temp[size(update)];

    temp=update;
    predictor=intercept+age*temp[1]+10*hdl*temp[12]+amp_event*(temp[37]==1?1:0)+
    amp*temp[25]+ihd_event*temp[34]+amp_event2*(temp[37]==2?1:0)+ihd*temp[28]+
    mi_event*temp[35]+mi*temp[26]+renal*temp[31]+stroke_event*temp[36]+
    atfib*temp[20]+pvd*temp[16]+wbc*temp[14];

    H1=exp(-predictor)/(1+exp(-predictor));
    prob=1 - H1;
    if (prob>rand){
      temp[32]=0;
    }
    return temp;
  }
}
data {
  //int<lower=0> N;
  int<lower=1> complicaciones;
  int<lower=1> time;
  int<lower=1> len_his;
  // real history[len_his];
  // real costs_input[11];
  real ag;       //current age of subject
  real age_dia;  //age at diagnostic
  real woma;      //if its a woman 1 for yes
  real eth;       //ethinicity 1 asian, 2 african, 0 white or other
  real smok;      //if smoker 1 for yes
  real sb;       //sbp
  real hba1;     //Hba1C
  real ld;       //LDL
  real hd;       //HDL
  real wbc;      //white blood cell count
  real haem;     //haemoglobine
  real hr;       //heart rate
  real weigh;    //weight
  real tall;     //height
  real atfib;     //if atrial fibrilation
  real egfr;      //egfr
  real micalb;    //albuminuria
  real pvd;       //if peripherial vascular disease
  real mii;      //mi history
  real strok;    //stroke history
  real ih;       //ihd history
  real ch;       //chf history
  real blin;     //blindness history
  real ampu;     //amount of amputation
  real rena;     //renal failure
  real ulce;     //ulcer hhistory
  real db;      //diastolic blood preassure
  real bg;       // blood glucose
  real tg;       // triglycerides
  real diabetes;// if has diabetes
  real bmpd;  //duplicates for pre diabetes functions, which we have no way
  real sbpd;  //of evolving through time
  real hdpd;
}

transformed data {
  real bm;
  real weipd; //weight for non diabetics
  real diab_years;
  real alive;
  real chf_even;
  real ihd_even;
  real mi_even;
  real stro_even;
  real amp_even;
  real renal_even;
  real mii_hist;
  real strok_hist;
  real amp_hist;

  alive = 1;
  chf_even = 0;
  ihd_even = 0;
  mi_even = 0;
  stro_even = 0;
  amp_even = 0;
  renal_even = 0;

  weipd= bmpd* tall^2;
  bm = weigh/(tall^2);
  diab_years = ag-age_dia;

  if (strok != 0){
    strok_hist = 1;
  }
  else {
    strok_hist = 0;
  }

  if (mii != 0){
    mii_hist = 1;
  }
  else {
    mii_hist = 0;
  }

  if (ampu != 0){
    amp_hist = 1;
  }
  else {
    amp_hist = 0;
  }
}

generated quantities {
  real mat_temp[time+2,len_his];
  real mat_temp_wr[time+2,len_his];
  int order[complicaciones]={1,2,3,4,5,6,7,8};
  simplex[complicaciones] theta=[0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125]';
  real history[len_his];
  real update[len_his];
  real output[len_his];
  real output_wr[len_his];
  real primera=0;
  history[1]=ag;                //current age of subject
  history[2]=age_dia;           //age diagnostic
  history[3]=woma;              //if its a woman 1 for yes
  history[4]=eth;               // eth 2 afro caribean, 1 asian indian, 0 hwait
  history[5]=weigh;
  history[6]=tall;
  history[7]=diab_years;        //time with diab
  history[8]=smok;               // if smoker 1 for yes
  history[9]=sb;                 // sbp
  history[10]=hba1;               //Hba1C
  history[11]=ld;                 //LDL
  history[12]=hd;                 //HDL
  history[13]=bm;               //bmi
  history[14]=wbc;               //white cell
  history[15]=egfr;              //efgr
  history[16]=pvd;               //pvd
  history[17]=haem;              //haemoglobine
  history[18]=hr;                //heart rate
  history[19]=micalb;
  history[20]=atfib;             //if atrial fibrilation
  history[21]=mii;               //mii amount
  history[22]=strok;             //stroke amount
  history[23]=ampu;              //amputation amount
  history[24]=ulce;              //ulcer outcome
  history[25]=amp_hist;          //hitories
  history[26]=mii_hist;
  history[27]=strok_hist;
  history[28]=ih;                //ihd outcome
  history[29]=ch;                // chf outcome
  history[30]=blin;              // blindeness
  history[31]=rena;              //renal failure
  history[32]=alive;             // if alive 1 if yes
  history[33]=chf_even;         //CHF event in current cycle
  history[34]=ihd_even;         //IHD " "
  history[35]=mi_even;          // MI event
  history[36]=stro_even;        // stroke event
  history[37]=amp_even;         // amputation even
  history[38]=renal_even;       //renal failure event
  history[39]=smok;               // if smoker 1 for yes
  history[40]=sb;                 // sbp initial
  history[41]=hba1;               //Hba1C initial
  history[42]=ld;                 //LDL initial
  history[43]=hd;                 //HDL initial
  history[44]=bm;               //bmi initial
  history[45]=wbc;               //white cell initial
  history[46]=egfr;              //efgr initial
  history[47]=pvd;               //pvd initial
  history[48]=haem;              //haemoglobine initial
  history[49]=hr;                //heart rate initial
  history[50]=micalb;            //albumia initial
  history[51]=atfib;             //if atrial fibrilation initial
  history[52]=diabetes;
  history[53]=db;               //diastolic blood preassure
  history[54]=bg;              // blood glucose
  history[55]=tg;              // triglycerides
  history[56]=bmpd;  //duplicates for pre diabetes functions
  history[57]=sbpd;
  history[58]=hdpd;
  //history[59]=weipd;

  for (m in 1:2){
  update=history;
  if (m==2){
    mat_temp[1]=update;
  }
  else if (m==1){
    mat_temp_wr[1]=update;
  }

  for (i in 1:time+1){
    if (update[32]==0) {
      break; // en algun mumento tengo que cambiar esta condicion
    }
    else{
    if (m==1 && i<4 && i>1){
      update = weightRedux_rng(update);
    }
    if (update[52]==0){
      real intercept;
      real age;
      real bmi;
      real sbp;
      real dbp;
      real bgl;
      real hdl;
      real trig;
      real female;
      real ethni;
      real rand;
      real rand2;

      intercept=-8.464;
      age=normal_rng(-0.014,0.004);
      bmi=normal_rng(0.053,0.005);
      sbp=normal_rng(0.006,0.002);
      dbp=normal_rng(0.003,0.004);
      bgl=normal_rng(0.062,0.002);
      hdl=normal_rng(-0.018,0.002);
      trig=normal_rng(0.001,0.00025);
      female=normal_rng(-0.084,0.066);
      ethni=normal_rng(-0.466,0.062);
      rand=uniform_rng(0,1);
      update=simDiabetes(update,intercept,age,bmi,sbp,dbp,bgl,hdl,trig,female,
      ethni,rand);

      rand2=uniform_rng(0,1);
      update=simDeath_not2d(update,rand2);
    }

    else {
    for (j in 1:complicaciones){
      int temp;
      int k;
      k=categorical_rng(theta);
      temp=order[j];
      order[j]=order[k];
      order[k]=temp;
    }
    for (s in 1:complicaciones){

      if (order[s]==1){
        if (update[29]!= 1){
          real intercept;
          real ro;
          real age_diab;
          real bmi;
          real ldl;
          real amp;
          real ulcer;
          real atfi;
          real egf60l;
          real mmalb;
          real pv;
          real rand;
          intercept=normal_rng(-12.332,0.859);
          ro=ro_rng(1.514,0.096);
          age_diab=normal_rng(0.068,0.008);
          bmi=normal_rng(0.072,0.008);
          ldl=normal_rng(0.012,0.005);
          amp=normal_rng(0.658,0.334);
          ulcer=normal_rng(0.654,0.291);
          atfi=normal_rng(1.562,0.245);
          egf60l=normal_rng(-0.22,0.065);
          mmalb=normal_rng(0.771,0.116);
          pv=normal_rng(0.479,0.136);
          rand=uniform_rng(0,1);
          update=simCHF(update,intercept,ro,age_diab,atfi,bmi,egf60l,ldl,mmalb,
          pv,amp,ulcer,rand);
        }
    }
    else if (order[s]==2){
      if (update[28]!=1){
          real intercept;
          real ro;
          real age_diab;
          real chf;
          real ldl;
          real amp;
          real sbp;
          real hdl;
          real female;
          real efgr;
          real pv;
          real rand;
          intercept=normal_rng(-6.709,0.503);
          ro=ro_rng(1.276,0.059);
          age_diab=normal_rng(0.016,0.005);
          ldl=normal_rng(0.023,0.003);
          amp=normal_rng(0.526,0.266);
          chf=normal_rng(0.824,0.168);
          sbp=normal_rng(0.058,0.019);
          hdl=normal_rng(-0.065,0.014);
          female=normal_rng(-0.532,0.085);
          efgr=normal_rng(-0.053,0.023);
          pv=normal_rng(0.486,0.181);
          rand=uniform_rng(0,1);
          update=simIHD(update,intercept,ro,age_diab,female,efgr,hdl,ldl,pv,sbp,
          amp,chf,rand);
          }
    }
    else if (order[s]==3){
        if (update[26]==0){
          if (update[3]==0){
            real intercept;
            real age_diab;
            real chf;
            real ldl;
            real amp;
            real hba1c;
            real sbp;
            real ihd;
            real hdl;
            real smoker;
            real stroke;
            real afro;
            real asian;
            real mmalb;
            real pv;
            real wb;
            real rand;
            intercept=normal_rng(-8.791,0.486);
            afro=normal_rng(-0.830,0.237);
            asian=normal_rng(0.279,0.126);
            hba1c=normal_rng(0.108,0.023);
            age_diab=normal_rng(0.045,0.006);
            ldl=normal_rng(0.023,0.004);
            amp=normal_rng(0.743,0.241);
            chf=normal_rng(0.814,0.195);
            sbp=normal_rng(0.046,0.022);
            hdl=normal_rng(-0.049,0.016);
            ihd=normal_rng(0.846,0.117);
            smoker=normal_rng(0.277,0.091);
            stroke=normal_rng(0.448,0.173);
            mmalb=normal_rng(0.023,0.094);
            pv=normal_rng(0.34,0.111);
            wb=normal_rng(0.026,0.013);
            rand=uniform_rng(0,1);
            update=simMIm(update,intercept,afro,age_diab,asian,hba1c,hdl,ldl,
            mmalb,pv,sbp,smoker,wb,amp,chf,ihd,stroke,rand);
          }
          else {
            real intercept;
            real age_diab;
            real chf;
            real ldl;
            real hba1c;
            real sbp;
            real ihd;
            real smoker;
            real ro;
            real afro;
            real egf60l;
            real mmalb;
            real pv;
            real wb;
            real ld35;
            real rand;
            intercept=normal_rng(-8.708,0.844);
            ro=ro_rng(1.376,0.097);
            hba1c=normal_rng(0.078,0.030);
            age_diab=normal_rng(0.041,0.008);
            ldl=normal_rng(0.035,0.007);
            chf=normal_rng(0.853,0.200);
            sbp=normal_rng(0.056,0.027);
            ihd=normal_rng(0.876,0.163);
            smoker=normal_rng(0.344,0.138);
            afro=normal_rng(-1.684,0.506);
            egf60l=normal_rng(-0.28,0.062);
            ld35=normal_rng(0.035,0.007);
            mmalb=normal_rng(0.277,0.129);
            pv=normal_rng(0.469,0.132);
            wb=normal_rng(0.07,0.023);
            rand=uniform_rng(0,1);
            update=simMIf(update,intercept,ro,afro,age_diab,egf60l,hba1c,
            ld35,mmalb,pv,sbp,smoker,wb,chf,ihd,rand);
          }
        }
       else if (update[26]==1){
         real intercept;
         real ldl;
         real mmalb;
         real rand;
         intercept=normal_rng(-4.179,0.262);
         ldl=normal_rng(0.021,0.007);
         mmalb=normal_rng(0.344,0.162);
         rand=uniform_rng(0,1);
         update=simMI2(update,intercept,ldl,mmalb,rand);
       }
    }
      else if (order[s]==4){
        if (update[27]==0){
          real intercept;
          real age_diab;
          real ldl;
          real hba1c;
          real sbp;
          real ihd;
          real smoker;
          real ro;
          real female;
          real amp;
          real mmalb;
          real egf60l;
          real wb;
          real atfi;
          real rand;
          intercept=normal_rng(-13.053,0.722);
          ro=ro_rng(1.466,0.081);
          hba1c=normal_rng(0.092,0.026);
          age_diab=normal_rng(0.066,0.007);
          female=normal_rng(-0.420,0.098);
          ldl=normal_rng(0.016,0.004);
          sbp=normal_rng(0.170,0.022);
          ihd=normal_rng(0.876,0.163);
          smoker=normal_rng(0.331,0.111);
          amp=normal_rng(1.090,0.240);
          mmalb=normal_rng(0.42,0.101);
          egf60l=normal_rng(-0.19,0.056);
          wb=normal_rng(0.04,0.012);
          atfi=normal_rng(1.476,0.201);
          rand=uniform_rng(0,1);
          update=simStroke(update,intercept,ro,age_diab,female,atfi,egf60l,
          hba1c,ldl,mmalb,sbp,smoker,wb,amp,ihd,rand);
        }
        else if (update[27]==1){
          real intercept;
          real age_diab;
          real smoker;
          real ro;
          real mmalb;
          real rand;
          intercept=normal_rng(-9.431,1.569);
          ro=ro_rng(1.956,0.291);
          age_diab=normal_rng(0.046,0.018);
          smoker=normal_rng(0.656,0.263);
          mmalb=normal_rng(0.537,0.228);
          rand=uniform_rng(0,1);
          update=simStroke2(update,intercept,ro,age_diab,mmalb,smoker,rand);
        }
      }
      else if (order[s]==5){
        if (update[30]!=1){
          real intercept;
          real age_diab;
          real hba1c;
          real sbp;
          real ihd;
          real chf;
          real heart;
          real wb;
          real rand;
          intercept=normal_rng(-11.607,0.759);
          hba1c=normal_rng(0.171,0.032);
          age_diab=normal_rng(0.047,0.009);
          sbp=normal_rng(0.068,0.032);
          ihd=normal_rng(0.610,0.208);
          chf=normal_rng(0.841,0.287);
          heart=normal_rng(0.08,0.039);
          wb=normal_rng(0.052,0.019);
          rand=uniform_rng(0,1);
          update=simBlind(update,intercept,age_diab,hba1c,heart,sbp,wb,chf,ihd,
          rand);
        }
      }
      else if (order[s]==6){
        real intercept;
        real age_diab;
        real female;
        real bmi;
        real hba1c;
        real rand;
        real pv;
        intercept=normal_rng(-4.179,0.262);
        age_diab=normal_rng(0.021,0.007);
        female=normal_rng(0.021,0.007);
        bmi=normal_rng(0.021,0.007);
        hba1c=normal_rng(0.021,0.007);
        pv=normal_rng(0.968,0.258);
        rand=uniform_rng(0,1);
        update=simUlcer(update,intercept,age_diab,female,bmi,hba1c,pv,rand);
      }
      else if (order[s]==7){
        if(update[25]==0){
          if (update[24]==0) {
            real intercept;
            real age_diab;
            real hdl;
            real hba1c;
            real sbp;
            real stroke;
            real ro;
            real female;
            real atfi;
            real heart;
            real mmalb;
            real pv;
            real wb;
            real rand;
            intercept=normal_rng(-14.844,1.205);
            ro=ro_rng(2.067,0.193);
            hba1c=normal_rng(0.248,0.042);
            age_diab=normal_rng(0.023,0.011);
            female=normal_rng(-0.445,0.189);
            hdl=normal_rng(-0.059,0.032);
            sbp=normal_rng(0.086,0.043);
            stroke=normal_rng(1.299,0.245);
            atfi=normal_rng(1.088,0.398);
            heart=normal_rng(0.098,0.05);
            mmalb=normal_rng(0.602,0.18);
            pv=normal_rng(1.01,0.189);
            wb=normal_rng(0.04,0.017);
            rand=uniform_rng(0,1);
            update=simAmp_noUlcer(update,intercept,ro,age_diab,female,atfi,
            hba1c,hdl,heart,mmalb,pv,sbp,wb,stroke,rand);
          }
          else {
            real intercept;
            real age_diab;
            real rand;
            real pv;
            intercept=normal_rng(-0.881,1.390);
            age_diab=normal_rng(-0.065,0.027);
            pv=normal_rng(1.769,0.449);
            rand=uniform_rng(0,1);
            update=simAmp_Ulcer(update,intercept,age_diab,pv,rand);
          }
        }
        else if (update[25]==1){
          real intercept;
          real hb1ac;
          real rand;
          intercept=normal_rng(-3.455,0.565);
          hb1ac=normal_rng(0.127,0.060);
          rand=uniform_rng(0,1);
          update=simAmp2(update,intercept,hb1ac,rand);
        }
      }
      else{
        if (update[31]!= 1){
          real intercept;
          real age_diab;
          real female;
          real bmi;
          real ldl;
          real sbp;
          real amp;
          real blind;
          real efgr;
          real afro;
          real efr60l;
          real efr60m;
          real haemo;
          real mmalb;
          real wb;
          real rand;
          intercept=normal_rng(-13.549,1.480);
          age_diab=normal_rng(-0.029,0.013);
          female=normal_rng(-0.869,0.224);
          bmi=normal_rng(-0.054,0.020);
          ldl=normal_rng(0.027,0.007);
          sbp=normal_rng(0.085,0.047);
          amp=normal_rng(1.108,0.337);
          blind=normal_rng(0.732,0.290);
          afro=normal_rng(0.686,0.324);
          rand=uniform_rng(0,1);
          efr60l=normal_rng(-1.031,0.085);
          efr60m=normal_rng(-0.487,0.136);
          haemo=normal_rng(-0.268,0.061);
          mmalb=normal_rng(1.373,0.236);
          wb=normal_rng(0.029,0.013);
          update=simRenal(update,intercept,afro,age_diab,female,bmi,efr60l,
          efr60m,haemo,ldl,mmalb,sbp,wb,amp,blind,rand);
        }
      }
    }
    if(sum(update[33:38])>=1){
      primera+=1;
    }
    if(sum(update[24:31])==0 && sum(update[33:38])==0){
      real intercept;
      real phi;
      real female;
      real smoker;
      real rand;
      intercept=normal_rng(-10.908,0.304);
      phi=ro_rng(0.098,0.004);
      female=normal_rng(-0.229,0.077);
      smoker=normal_rng(0.379,0.089);
      update=simDeath_nohist(update,intercept,phi,female,smoker,rand);
    }
    else if (primera==1){
      real intercept;
      real year;
      real age;
      real smoker;
      real asian;
      real heart;
      real pv;
      real amp_event;
      real ihd_event;
      real mi_event;
      real renal_event;
      real stroke_event;
      real rand;
      intercept=normal_rng(-6.916,0.591);
      asian=normal_rng(-0.54,0.205);
      year=normal_rng(0.042,0.010);
      age=normal_rng(0.058,0.007);
      smoker=normal_rng(0.444,0.117);
      heart=normal_rng(0.124,0.032);
      pv=normal_rng(0.367,0.13);
      amp_event=normal_rng(-0.734,0.321);
      ihd_event=normal_rng(0.423,0.168);
      mi_event=normal_rng(1.309,0.158);
      renal_event=normal_rng(0.584,0.305);
      stroke_event=normal_rng(0.547,0.176);
      rand=uniform_rng(0,1);
      update=simDeath1st(update,intercept,asian,year,age,heart,pv,smoker,
      amp_event,ihd_event,mi_event,renal_event,stroke_event,rand);
    }
    else if (sum(update[24:31])>=1){
      real intercept;
      real phi;
      real bm1;
      real bm3;
      real smoker;
      real amp;
      real chf;
      real renal;
      real stroke;
      real mmalb;
      real wb;
      real rand;
      intercept=normal_rng(-9.207,0.591);
      phi=ro_rng(0.073,0.007);
      bm1=normal_rng(1.083,0.511);
      bm3=normal_rng(-0.293,0.114);
      smoker=normal_rng(0.374,0.133);
      amp=normal_rng(0.539,0.198);
      chf=normal_rng(0.632,0.130);
      renal=normal_rng(1.150,0.197);
      stroke=normal_rng(0.473,0.122);
      mmalb=normal_rng(0.348,0.107);
      wb=normal_rng(0.048,0.011);
      rand=uniform_rng(0,1);
      update=simDeath_hist(update,intercept,phi,bm1,bm3,mmalb,smoker,wb,amp,
      chf,renal,stroke,rand);
    }
    else {
      real intercept;
      real age;
      real hdl;
      real atfi;
      real pv;
      real wb;
      real amp_event;
      real amp;
      real amp_event2;
      real ihd_event;
      real ihd;
      real mi_event;
      real mi;
      real renal;
      real stroke_event;
      real rand;
      intercept=normal_rng(-4.868,0.828);
      age=normal_rng(0.050,0.010);
      atfi=normal_rng(1.081,0.396);
      pv=normal_rng(0.352,0.178);
      wb=normal_rng(0.089,0.038);
      hdl=normal_rng(0.068,0.030);
      amp_event=normal_rng(-1.267,0.344);
      amp=normal_rng(0.753,0.300);
      amp_event2=normal_rng(-1.727,0.467);
      ihd_event=normal_rng(0.583,0.243);
      ihd=normal_rng(-0.507,0.191);
      mi_event=normal_rng(0.982,0.230);
      mi=normal_rng(0.440,0.186);
      renal=normal_rng(0.961,0.396);
      stroke_event=normal_rng(-0.619,0.246);
      rand=uniform_rng(0,1);
      update=simDeath_sub(update,intercept,atfi,age,hdl,pv,wb,amp_event,amp,
      amp_event2,ihd_event,ihd,mi_event,mi,renal,stroke_event,rand);
    }
    if (update[32]!=0){
      real mmalb_aux;
      real pvd_aux;
      real atfib_aux;
      real smoker_aux;
      real egfr60l_binary;

      real constant;
      real phi;
      real female;
      real african;
      real asian;
      real age_diag;
      real smoker;
      real smoker_first;
      real sbp;
      real hb1ac;
      real bmi;
      real hdl;
      real ldl;
      real efgr;
      real efgr_first;
      real rf_prev_value;
      real rf_first_value;
      real diabet_years;
      real rand;

      constant=normal_rng(-9.047,0.443);
      phi=ro_rng(1.138,0.049);
      female=normal_rng(-0.463,0.077);
      age_diag=normal_rng(0.012,0.004);
      smoker=normal_rng(0.329,0.076);
      sbp=normal_rng(0.186,0.018);
      hb1ac=normal_rng(0.165,0.028);
      bmi=normal_rng(0.028,0.006);
      hdl=normal_rng(-0.03,0.013);
      rand=uniform_rng(0,1);
      mmalb_aux=micalb_rf(update,constant,phi,female,age_diag,smoker,sbp,hb1ac,
      bmi,hdl,rand);

      constant=normal_rng(-11.784,0.425);
      phi=ro_rng(1.871,0.052);
      female=normal_rng(0.745,0.067);
      african=normal_rng(-0.974,0.169);
      asian=normal_rng(-0.302,0.142);
      age_diag=normal_rng(0.08,0.005);
      sbp=normal_rng(0.075,0.017);
      bmi=normal_rng(0.014,0.006);
      hdl=normal_rng(-0.028,0.011);
      ldl=normal_rng(0.008,0.003);
      rand=uniform_rng(0,1);
      egfr60l_binary=efgr60l_binary(update,constant,phi,female,african,asian,
      age_diag,sbp,bmi,hdl,ldl,rand);

      if(3%i==1 && i>1){

        constant=normal_rng(-12.271,0.058);
        phi=ro_rng(1.515,0.058);
        age_diag=normal_rng(0.057,0.006);
        smoker=normal_rng(0.865,0.083);
        sbp=normal_rng(0.098,0.022);
        hb1ac=normal_rng(0.095,0.022);
        bmi=normal_rng(0.023,0.007);
        ldl=normal_rng(0.025,0.003);
        rand=uniform_rng(0,1);
        pvd_aux=pvd_rf(update,constant,phi,age_diag,smoker,sbp,hb1ac,bmi,ldl,
        rand);

        constant=normal_rng(-13.313,1.148);
        age_diag=normal_rng(0.057,0.006);
        bmi=normal_rng(0.023,0.007);
        rand=uniform_rng(0,1);
        atfib_aux=atfib_rf(update,constant,age_diag,bmi,rand);

        constant=normal_rng(-9.047,0.443);
        female=normal_rng(-0.463,0.077);
        age_diag=normal_rng(0.012,0.004);
        smoker=normal_rng(0.329,0.076);
        diabet_years=normal_rng(-1.574,0.169);
        smoker_first=normal_rng(5.535,0.436);
        rand=uniform_rng(0,1);
        smoker_aux=smoke_rf(update,constant,female,age_diag,smoker,diabet_years,
        smoker_first,rand);

      }

      constant=normal_rng(1.419,0.041);
      female=normal_rng(0.054,0.012);
      african=normal_rng(0.066,0.026);
      asian=normal_rng(0.046,0.020);
      rf_prev_value=normal_rng(0.724,0.005);
      diabet_years=normal_rng(0.141,0.007);
      rf_first_value=normal_rng(0.081,0.007);
      update=continuous_rf(10,update,constant,female,african,asian,
      rf_prev_value,diabet_years,rf_first_value);

      constant=normal_rng(29.007,0.597);
      female=normal_rng(0.684,0.142);
      african=0;
      asian=normal_rng(-1.393,0.224);
      rf_prev_value=normal_rng(0.669,0.005);
      diabet_years=normal_rng(0.57,0.064);
      rf_first_value=normal_rng(0.118,0.005);
      update=continuous_rf(9,update,constant,female,african,asian,rf_prev_value,
      diabet_years,rf_first_value);

      constant=normal_rng(0.763,0.02);
      female=normal_rng(0.065,0.009);
      african=normal_rng(0.05,0.016);
      asian=normal_rng(0.074,0.014);
      rf_prev_value=normal_rng(0.578,0.007);
      diabet_years=normal_rng(-0.042,0.004);
      rf_first_value=normal_rng(0.21,0.007);
      update=cholesterol_rf(12,update,constant,female,african,asian,
      rf_prev_value,diabet_years,rf_first_value);

      constant=normal_rng(0.17,0.009);
      female=normal_rng(0.043,0.003);
      african=normal_rng(0.051,0.006);
      asian=0;
      rf_prev_value=normal_rng(0.603,0.015);
      diabet_years=0;
      rf_first_value=normal_rng(0.22,0.009);
      update=cholesterol_rf(11,update,constant,female,african,asian,
      rf_prev_value,diabet_years,rf_first_value);

      constant=normal_rng(0.83,0.039);
      female=normal_rng(0.045,0.011);
      african=normal_rng(-0.094,0.016);
      asian=normal_rng(-0.087,0.014);
      rf_prev_value=normal_rng(0.952,0.003);
      diabet_years=normal_rng(-0.165,0.006);
      rf_first_value=normal_rng(0.034,0.003);
      update=continuous_rf(13,update,constant,female,african,asian,
      rf_prev_value,diabet_years,rf_first_value);

      if(egfr60l_binary==1){

        constant=normal_rng(26.102,0.976);
        phi=9.452;
        female=normal_rng(-2.409,0.271);
        african=normal_rng(2.162,0.594);
        asian=normal_rng(1.229,0.506);
        diabet_years=normal_rng(-3.28,0.185);
        efgr=normal_rng(0.567,0.01);
        efgr_first=normal_rng(0.138,0.009);
        update=egfr60less_rf(update,constant,phi,female,african,asian,efgr,
        efgr_first,diabet_years);

      } else if(egfr60l_binary==0){

        constant=normal_rng(23.97,0.783);
        phi=12.575;
        female=normal_rng(-2.985,0.262);
        african=normal_rng(3.419,0.474);
        asian=normal_rng(2.404,0.421);
        diabet_years=normal_rng(-3.013,0.128);
        efgr=normal_rng(0.406,0.006);
        efgr_first=normal_rng(0.297,0.008);
        update=egfr60more_rf(update,constant,phi,female,african,asian,efgr,
        efgr_first,diabet_years);
      }

      if(3%i==1 && i>1){

        constant=normal_rng(31.231,1.468);
        female=normal_rng(1.006,0.316);
        african=0;
        asian=0;
        rf_prev_value=normal_rng(0.327,0.021);
        diabet_years=normal_rng(0.918,0.469);
        rf_first_value=normal_rng(0.272,0.021);
        update=continuous_rf(18,update,constant,female,african,asian,
        rf_prev_value,age_diag,rf_first_value);

        constant=normal_rng(1.446,0.242);
        female=normal_rng(0.087,0.042);
        african=normal_rng(-0.331,0.066);
        asian=0;
        rf_prev_value=normal_rng(0.46,0.116);
        diabet_years=normal_rng(0.167,0.05);
        rf_first_value=normal_rng(0.292,0.1);
        update=continuous_rf(14,update,constant,female,african,asian,
        rf_prev_value,age_diag,rf_first_value);

        constant=normal_rng(5.04,0.295);
        female=normal_rng(-0.349,0.036);
        african=normal_rng(-0.185,0.045);
        asian=0;
        rf_prev_value=0;
        diabet_years=normal_rng(-0.326,0.028);
        rf_first_value=normal_rng(0.692,0.02);
        update=continuous_rf(17,update,constant,female,african,asian,
        rf_prev_value,age_diag,rf_first_value);

        update[16]=pvd_aux;
        update[20]=atfib_aux;
        update[8]=smoker_aux;
      }
      update[19]=mmalb_aux;
    }
    update[33]=0;
    update[34]=0;
    update[35]=0;
    update[36]=0;
    update[37]=0;
    update[38]=0;
  }
  if (update[32]!=0){
    update[1]+=1;
    update[7]+=1;
  }
    }
  if (m==2){
    output = update;
    mat_temp[i+1]=update;
  }
  else if (m==1){
    output_wr = update;
    mat_temp_wr[i+1]=update;
  }
  }
  }
}


