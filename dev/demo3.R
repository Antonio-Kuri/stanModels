ag=25         #
age_dia=20    #
woma=1        #
eth=0         #
smok=0        #
sb=127        #
hba1=11       #
ld=143        #
hd=47.73      #
weigh=70      #
tall=1.60     #
wbc=8         #
egfr=70       #
pvd=0         #
haem=15       #
hr=70         #

diabetes=0    #
db=70         #
bg=90         #
tg=150        #

bmpd=24       #
sbpd=127      #
hdpd=48       #

time=30       #
complicaciones=8

micalb=0    #
atfib=0     #
mii=0       #
strok=0     #
ih=0        #
ch=0        #
blin=0
ampu=0
rena=0
ulce=0

len_his=58


stan_data=list(ag=ag,
               age_dia=age_dia,
               woma=woma,
               eth=eth,
               smok=smok,
               sb=sb,
               hba1=hba1,
               ld=ld,
               hd=hd,
               wbc=wbc,
               egfr=egfr,
               pvd=pvd,
               haem=haem,
               hr=hr,
               micalb=micalb,
               atfib=atfib,
               weigh=weigh,
               tall=tall,
               ulce=ulce,
               mii=mii,
               strok=strok,
               ih=ih,
               ch=ch,
               blin=blin,
               ampu=ampu,
               rena=rena,
               time=time,
               complicaciones=complicaciones,
               len_his=len_his,
               diabetes=diabetes,
               db=db,
               bg=bg,
               tg=tg,
               bmpd=bmpd,
               sbpd=sbpd,
               hdpd=hdpd)

library(rstan)
options(mc.cores = 4)
rstan_options(auto_write = TRUE)

m=stan_model(file = './inst/stan/diabetesnewmodel.stan')


fit <- sampling(m, data=stan_data,
                iter=10000, warmup=0, chains=1,
                algorithm="Fixed_param")

stan_fit <- extract(fit)
mat_resultados = stan_fit$mat_temp[ , ,c(1:24,28:31)]
mat_resultados_wr = stan_fit$mat_temp_wr[ , ,c(1:24,28:31)]
summary(stan_fit$output)
summary(stan_fit$output_wr)
