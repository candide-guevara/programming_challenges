package main

import "context"
import "reflect"
import "time"

func waitForClosureReflection(chs ... interface{}) {
  Infof("Context done, waiting for channels close")
  timeout := time.NewTimer(57 * time.Millisecond)
  defer timeout.Stop()

  ch_len := len(chs)
  done_count := 0
  cases := make([]reflect.SelectCase, ch_len + 1)
  for idx,ch := range chs {
    cases[idx].Dir = reflect.SelectRecv
    cases[idx].Chan = reflect.ValueOf(ch)
  }
  cases[ch_len].Dir = reflect.SelectRecv
  cases[ch_len].Chan = reflect.ValueOf(timeout.C)

  for done_count < ch_len {
    idx,_,ok := reflect.Select(cases)
    if idx == ch_len {
      Fatalf("Channels are still open after timeout")
    }
    if !ok {
      done_count += 1
      cases[idx].Chan = reflect.ValueOf(nil)
    }
  }
}

func main() {
  var err error
  conf := NewConfigImpl()
  InitLogging(conf)
  prov := NewDevInputEventProvider(conf)
  apms := NewApmProvider(conf)
  ctx, cancel := context.WithTimeout(context.Background(), 1000*time.Second)
	defer cancel()

  var in_ev <-chan SingleAction
  in_ev, err = prov.Listen(ctx)
  if err != nil { Fatalf("fiasco : %v", err) }
  var in_apm <-chan ApmBucket
  in_apm, err = apms.AggregateEvents(ctx, in_ev)
  if err != nil { Fatalf("fiasco : %v", err) }

  loop:for {
    select {
      case a,ok := <-in_apm:
        if ok { Infof("%+v", a) }
      case <-ctx.Done():
        waitForClosureReflection(in_apm, in_ev)
        break loop
    }
  }
  Infof("Done")
}

