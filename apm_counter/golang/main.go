package main

import "context"

func main() {
  var err error
  conf := NewConfigImpl()
  InitLogging(conf)
  Debugf("config=%v", conf)
  ctx, cancel := context.WithCancel(context.Background())
  CatchInterruptSignal(ctx, cancel)

  var in_ev <-chan SingleAction
  prov := NewDevInputEventProvider(conf)
  in_ev, err = prov.Listen(ctx)
  if err != nil { Fatalf("fiasco : %v", err) }

  var in_apm <-chan ApmBucket
  apms := NewApmProvider(conf)
  in_apm, err = apms.AggregateEvents(ctx, in_ev)
  if err != nil { Fatalf("fiasco : %v", err) }

  var recv_done <-chan bool
  recv := NewCsvApmReceiver(conf)
  recv_done, err = recv.Listen(ctx, in_apm)
  if err != nil { Fatalf("fiasco : %v", err) }

  select {
    case <-ctx.Done():
      err = WaitForClosureReflection(500, in_apm, in_ev, recv_done)
  }
  Infof("Done (err=%v)", err)
}

