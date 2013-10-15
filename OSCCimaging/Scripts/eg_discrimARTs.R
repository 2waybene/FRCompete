
data(x_gideon)
## Assuming normal, estimate initial conditions from data
fit.default.gideon <- mix.mle(x_gideon$horn)
## Estimation of mixture of normals, explicitly specifying method and parameters
fit.gideon <- mix.mle(x_gideon$horn, method="normal",
                      mix.prob=0.5, dist1.par1=100, dist1.par2=10, dist2.par1=300, dist2.par2=10)
## Default printing and plotting methods
print(fit.gideon)
## Compare results
layout(1:2)
plot(fit.gideon)
plot(fit.default.gideon)

data(o_taurus)
## Scatter plot of horn versus body showing trait scaling
plot(o_taurus$horn, o_taurus$body)
## Plot histogram with observations
hist(o_taurus$horn, freq=FALSE)
## To include points on x-axis of histogram
points( o_taurus$horn, rep(0, nrow(o_taurus)))
## use the o_taurus dataset to estimate facing gamma
## Using an offset to shift data away from zero
horn.offset <- 8
taurus.fit <- mix.mle(input=o_taurus$horn+horn.offset, method="facing.gamma",
                      mix.prob=0.59, lower=2 + horn.offset - 7, upper=482 + horn.offset + 3,
                      dist1.par1=1.37, dist1.par2=52.7, dist2.par1=2.29, dist2.par2=43.8)
## Default printing function shows a subset of the available information
print(taurus.fit)
## Default plotting function shows histogram, raw observations, and the computed distributions,
## along with a legend including estimated values
plot(taurus.fit)
## To exclude legend
plot(taurus.fit, legend=FALSE)

norm.di <- rnorm(100, 1.3,0.2)
norm.di <- rnorm(7000, 1.3,0.001)
ecdf(norm.di)
hist(norm.di)

ab.norm.di <- rnorm(20,2.3, 0.0002)
hist(ab.norm.di)

cmd.di <- as.numeric(c (norm.di, ab.norm.di))
hist(cmd.di)

## Estimation of mixture of normals, explicitly specifying method and parameters
fit.cmd.di <- mix.mle(cmd.di, method="normal",
                      mix.prob=0.1, dist1.par1=1.3, dist1.par2=0.001, dist2.par1=2.3, dist2.par2=0.0002)


## Normal: draw samples
normalmix.draws <- mix.synthetic.normal(mix.prob=0.1,
                                        mu1=10, sd1=2, mu2=20, sd2=2)
## Estimated initial conditions may fail for mix.prob far from 0.5


normalmix.draws <- mix.synthetic.normal(mix.prob=0.1,
                                        mu1=1.3, sd1=0.001, mu2=2.3, sd2=0.0002)
normalmix.fit <- mix.mle(normalmix.draws)
plot(normalmix.fit)




## Facing gamma: draw samples
gammamix.params <- list(lower = 0.37, upper=4.72, mix.prob=0.55,
                        dist1.par1=1.50, dist1.par2=.4, dist2.par1=3.2, dist2.par2=.5)
## simulate synthetic data
gammamix.draws <- with(gammamix.params,
                       mix.synthetic.facing.gamma( lower=lower, upper=upper, mix.prob=mix.prob,
                                                   shape1=dist1.par1, scale1=dist1.par2,
                                                   shape2=dist2.par1, scale2=dist2.par2
                       )
)
## Fit and return
## Use parameters as initial values
gammamix.fit <- with(gammamix.params,
                     mix.mle(gammamix.draws, method="facing.gamma",
                             lower=lower, upper=upper, mix.prob=mix.prob,
                             dist1.par1=dist1.par1, dist1.par2=dist1.par2,
                             dist2.par1=dist2.par1, dist2.par2=dist2.par2
                     )
)
plot(gammamix.fit)


