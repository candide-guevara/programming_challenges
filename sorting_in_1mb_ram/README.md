# Sort 1 million decimals in 1MB of memory

* Input : 1 million 8-digit decimals (uniformly randomly distributed)
* Output : sorted input in ascending order
* Constraint : use only 1MB of memory (no disk) besides the stack
  * Of course you cannot use the stack to store big arrays !

I originally heard this problem [here][0]

## My solution

* Model a decimal as a pair (integer, floating point index)
* Divide the decimal into buckets based on their value, each bucket contains only integers
* Use variable length encoding for the integers inside each bucket
  * Use zigzag encoding (cf why below)
* Sort the input on the fly, store the difference between each consecutive decimal
  * Substract a bias to each delta that correspond to the average distance between number in a bucket


[0]: http://preshing.com/20121025/heres-some-working-code-to-sort-one-million-8-digit-numbers-in-1mb-of-ram/
