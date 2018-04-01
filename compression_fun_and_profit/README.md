# Entropy based compression for time series

Apply simple arithmetic encoding to price time series.
A price time series is just a list of tuples (date, price). 
Or if the series is not sparse then it is a start date and a list of prices. 
Not all prices may have valid numerical values, some of them may be `NaN`.
To make things simple, compression will be lossy.


## Normalization

For each time series we do the following

* Discard if it has less than `Nd` datapoints
* Discover `max` and `min` prices
    * Discard if max > `M_scale` * min
* Normalize interval `[min,max]` to `[0,2^n-1]`
* Calculate deltas in the normal interval (`NaN` are transformed to 0)


## Stats calculation

Since there are many delta values the resulting probability distribution would need to much precision.
We limit to a distribution over a given alphabet : `{ 0 ... Max, Stop }`. 
To transalate any delta value to the alphabet : `delta = Max*q + r = Max*(Max/delta) + (Max%delta)`

For each normalized time series we compute the probability of delta values.

* Fix the alphabet `Max` size
* Transfor each delta to its `(q,r)` tuple
* Add to the alphabet histogram : `q` times `Max` bucket, 1 time `r` bucket

`Max` should be choosen in a way to have a probability distribution that can be represented in `[0, 2^n]`
and in which the `Max` histogram bucket does not over dominate the rest.


### Coherent subset of securities

We restrict the set of securities that are used to calculate the delta probability distribution.
Each subset groups securities that have something in common :

* Belonging to the same geographical market
* Belonging to the same industrial sector

The hope is that each security on those subsets is somehow correlated so the resulting probability distribution
fits well each individual time serie.


## Compression algorithms

* [Golomb coding][0] : easiest to implement
* [Arithmetic compression][1] : this will be a pain ...


## Benchmark

Compare with the size of gzip/bzip on :

* The raw data
  * Format : list of `SECURITY_ID|START_DATE|PRICES_AS_DOUBLES`
* The normalized data
  * Format : list of `SECURITY_ID|START_DATE|NORMAL_PRICES_AS_INT`
* The normalized deltas
  * Format : list of `SECURITY_ID|START_DATE|DELTA_PRICES_AS_INT`

### Is probability distribution overfitted ?

* Use probability distribution (on whole subset) to compress a random sampling of securities
* Use probability distribution (on whole subset) of closing prices to compress opening prices

### What is the best alphabet size ?

Compare compression ratios based on alphabet `Max`

* `Max` chosen so that `P(delta == Max) ~ 0.1`
* `Max` chosen so that `P(delta == Max) ~ 0.5`
* `Max` chosen so that `P(delta == Max) ~ 0.9`

### How much precision is lost ?

Denormalize back to prices and evaluate the distribution of RMS error across all securities.

### How does it compare to various theoretical limits ?

Compare with theoretical best compression : 

* if prices were ordered and flat prob distribution
* entropy based on histogram


## Other stuff 

* Is any type of frequency transformation useful to compress time series ?
  * Fast fourrier, wavelet transform, Karhunen–Loève ?
* Can we do better than a static probabilistic model ?
  * Train an ML model and use it to predict probability of next value based on last datapoints

[0]: http://preshing.com/20121026/1mb-sorting-explained/
[1]: https://gist.github.com/preshing/3952090
[2]: http://marknelson.us/2014/10/19/data-compression-with-arithmetic-coding/

