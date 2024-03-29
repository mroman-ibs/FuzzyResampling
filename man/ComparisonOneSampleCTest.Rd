% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ComparisonOneSampleCTest.R
\name{ComparisonOneSampleCTest}
\alias{ComparisonOneSampleCTest}
\title{Comparison of the resampling approaches based on the power for the one-sample test for the mean.}
\usage{
ComparisonOneSampleCTest(
  generator,
  mu_0,
  shift = 0,
  sampleSize = 10,
  numberOfSamples = 10,
  initialSamples = 100,
  theta = 1/3,
  significance = 0.05,
  ...
)
}
\arguments{
\item{generator}{Name of the generator for sampling initial samples.
For the possible names check the values of \code{samplingGenerators} vector.}

\item{mu_0}{Triangular or trapezoidal fuzzy number which is used for the null hypothesis of the C-test.}

\item{shift}{Deterministic shift added to all initial samples.}

\item{sampleSize}{Size of the single initial sample.}

\item{numberOfSamples}{Number of the bootstrapped samples used to estimate the p-value.}

\item{initialSamples}{Number of the generated initial samples.
More than one value can be given in the form of matrix.}

\item{theta}{The weighting parameter for the mid/spread distance applied in the C-test.}

\item{significance}{The significance value used to accept/reject the hypothesis for the one-sample C-test.}

\item{...}{Parameters which are passed to \code{OneSampleCTest} or the respective \code{generator}}
}
\value{
This function returns a vector of percentage of rejections for the one-sample C-test for the mean.
}
\description{
\code{ComparisonOneSampleCTest} returns the percentage of rejections for the one-sample C-test when different resampling methods
are used.
}
\details{
The function generates a sequence of initial samples (their number is given in \code{initialSamples},
the size is determined by \code{sampleSize}) for fuzzy numbers of the type specified by \code{generator}.
Then some deterministic shift of the size \code{shift} is added to each fuzzy observation in these samples.
Next, function \code{OneSampleCTest} is executed to calculate the p-value for each combination of the initial sample and
resampling method. Then, by comparing the p-value with the assumed significance level
\code{significance} we make a decision whether to reject the null hypothesis for the one-sample C-test for the mean
(see Lubiano et al. (2016))  or not.
The output of this procedure is the percentage of rejections in the sequence of experiments.
}
\examples{

\dontrun{

# seed PRNG

set.seed(1234)

# compare the resampling methods for the synthetic data generated using GeneratorNU function

ComparisonOneSampleCTest(generator="GeneratorNU",mu_0 = c(-0.4,-0.1,0.1,0.4),
 sampleSize = 10,numberOfSamples = 100, initialSamples = 100,mu = 0, sigma = 1,a = 0.2,b = 0.6)}




}
\references{
Lubiano, M.A., Montenegro M., Sinova, B., de Saa, S.R., Gil, M.A. (2016)
Hypothesis testing for means in connection with fuzzy rating scale-based data: algorithms and applications
European Journal of Operational Research, 251, pp. 918-929
}
\seealso{
\code{\link{ComparisonSEMean}} for the comparison of resampling methods based on SE/MSE for the mean,
\code{\link{ComparePowerOneSampleCTest}} for the comparison of resampling methods based on power for the one-sample C-test
for the mean.

Other comparison of resampling methods: 
\code{\link{ComparePowerOneSampleCTest}()},
\code{\link{ComparisonSEMean}()}
}
\concept{comparison of resampling methods}
