package main

import "context"

import "apm_counter/provider"
import "apm_counter/receiver"
import "apm_counter/types"
import "apm_counter/util"

func main() {
  var err error
  conf := util.NewConfigImpl()
  util.InitLogging(conf)
  util.Debugf("config=%v", conf)
  ctx, cancel := context.WithCancel(context.Background())
  util.CatchInterruptSignal(ctx, cancel)

  var in_ev <-chan types.SingleAction
  prov := provider.NewDevInputEventProvider(conf)
  in_ev, err = prov.Listen(ctx)
  if err != nil { util.Fatalf("fiasco : %v", err) }

  var in_apm <-chan types.ApmBucket
  apms := provider.NewApmProvider(conf)
  in_apm, err = apms.AggregateEvents(ctx, in_ev)
  if err != nil { util.Fatalf("fiasco : %v", err) }

  var recv_done <-chan bool
  recv := receiver.NewCsvApmReceiver(conf)
  recv_done, err = recv.Listen(ctx, in_apm)
  if err != nil { util.Fatalf("fiasco : %v", err) }

  select {
    case <-ctx.Done():
      err = util.WaitForClosureReflection(500, in_apm, in_ev, recv_done)
  }
  util.Infof("Done (err=%v)", err)
}

