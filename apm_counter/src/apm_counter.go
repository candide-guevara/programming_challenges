package main

import "context"
import "time"

func main() {
  conf := NewConfigImpl()
  prov := NewDevInputEventProvider(conf)
  ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()
  out_chan, err := prov.Listen(ctx)
  var counter uint = 0
  if err != nil {
    Infof("fiasco : %v", err)
    return
  }
  for range out_chan { counter += 1 }
  Infof("Done : %d", counter)
}

