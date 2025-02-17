# Learning Rust exercises

## Spelling bee

[Game description](https://www.nytimes.com/2023/09/11/crosswords/getting-to-genius-part-1.html)

Tips [for optimization](https://nnethercote.github.io/perf-book/title-page.html).

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

### Performance guided optimization workflow

```
# Tweak `config.toml` to collect profiles
cargo run --profile optimized -- --benchmark
llvm-profdata merge -o /tmp/pgo/merged.profdata /tmp/pgo
# Tweak `config.toml` to use merged profile
cargo run --profile optimized -- --benchmark
```

