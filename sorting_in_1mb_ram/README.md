# Sort 1 million decimals in 1MB of memory

* Input : 1 million 8-digit integers
* Output : sorted input in ascending order
* Constraint : use only 1MB of memory (no disk) besides the stack
  * Of course you cannot use the stack to store big arrays !

I originally heard this problem [here][0]

## Attempt 1 : encode deltas

Instead of keeping teh whole list in memory sorted, you do not lose any information if you store the difference between the current element and the previous one (sorted).
That way you store smaller integers that can be represented with fewer bytes.

### Encode using bit flags

The easiest solution, just consider that an integer is 8, 16, 24 bits long depending on the most significant bit.

* `0x00` - `0x7f` takes only one byte
* `0x80` - `0x3fff` takes 2 bytes
  * `0xabc` would be encoded `0x15bc`

### [Zigzag encoding][1]

At first I thought the deltas would be centered around the average difference (in this case `max_value / input_len`).
So I added average dif as a bias to each delta and used zigzag encoding to account for negative values.
It turned out the probability distribution was not as I expected. In fact the most probable values are the smaller ones.
(cannot prove why though ...).

### [Sqlite variable length][2]

Thanks to this method to can expand the range that can be represented by a single byte.
Also you can determine the length of the delta just by looking at the first byte.

* `0x00` - `0xfb` takes only one byte
* `0xfc` - `[0xfb + 0xff(0xfe-0xfb)] = 0x3fb` takes 2 bytes.
* if the first byte is `0xff` just look at the following 4 bytes an inter[ret as a standard `uint32_t`

### Optimizing run time with buckets

The vanilla algorithm is quadratic. Each time you insert a new element you must reconstruct the sequence by summing all deltas.
If you partition the integer space in buckets you pay a small price in stack memory but you divide the calculation by the number of buckets.

### Attempt 1 result : failure

It runs acceptably fast (around 2 secs) but it falls short by 32kb of the target.

## Attempt 2 : a radix tree

I given that the data is sparse (1 out of 100 integers is chosen) and unlikely to have repetitions, a radix tree like could work.
I do not expect it to achieve the memory target, but it should be faster.


[0]: http://preshing.com/20121025/heres-some-working-code-to-sort-one-million-8-digit-numbers-in-1mb-of-ram/
[1]: https://gist.github.com/mfuerstenau/ba870a29e16536fdbaba
[2]: https://sqlite.org/src4/doc/trunk/www/varint.wiki

