# Learning Rust exercises

## Spelling bee

[Game description](https://www.nytimes.com/2023/09/11/crosswords/getting-to-genius-part-1.html)

```
rm /tmp/perf.data /tmp/flamegraph.html
cargo build --release \
  && perf record --call-graph=dwarf -g -F 99 -o /tmp/perf.data \
    /tmp/release/spelling_bee --benchmark
# It is not possible to use the `-i` flags to make it read from somewhere else ...
(cd /tmp
  perf script report flamegraph --allow-download \
    && google-chrome-stable /tmp/flamegraph.html)
```

