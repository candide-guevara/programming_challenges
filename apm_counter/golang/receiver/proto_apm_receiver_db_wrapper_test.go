package receiver

import "testing"
import "time"

import "apm_counter/messages"
import "apm_counter/types"
import "apm_counter/util"

func createDummyRepo(t *testing.T, conf types.Config) messages.TimeserieRepo {
  entries := []*messages.TimeserieRepo_Entry {
    &messages.TimeserieRepo_Entry { StartSecs:1, EndSecs:2, Filepath:"fp1" },
    &messages.TimeserieRepo_Entry { StartSecs:3, EndSecs:4, Filepath:"fp2" },
  }
  repo := messages.TimeserieRepo { Entries:entries }
  err := util.WriteProtoIntoZipFile(conf.TimeserieRepo(), &repo)
  if err != nil { t.Fatalf("Could not create dummy repo: %v", err) }
  return repo
}

func compareRepoContents(t *testing.T, conf types.Config, ts_path string, ini_repo messages.TimeserieRepo) {
  new_repo := messages.TimeserieRepo{}
  err := util.ReadProtoFromZipFile(conf.TimeserieRepo(), &new_repo)
  if err != nil { t.Fatalf("Could not read dummy repo: %v", err) }

  if len(new_repo.Entries) != (len(ini_repo.Entries)+1) {
    t.Fatalf("Mistmatched entries:\n%v\n!= %v", &ini_repo, &new_repo)
  }
  last_entry := new_repo.Entries[len(new_repo.Entries)-1]
  bad_dates := (last_entry.StartSecs == 0) || (last_entry.EndSecs == 0)
  bad_filepath := last_entry.Filepath != ts_path
  if bad_dates || bad_filepath {
    t.Fatalf("Bad last entry:\n%v", &last_entry)
  }
}

func TestProtoApmRecieverDbWrapperListen(t *testing.T) {
  var err error
  ctx, cancel, conf := protoApmReceiverTestSetup()
  in_apm, expected_pb := fillApmsAndExpectedProto(t, conf)
  ini_repo := createDummyRepo(t, conf)

  var done_ch <-chan bool
  dumper := NewProtoApmReceiverDbWrapper(conf).(*protoApmReceiverDbWrapper)
  done_ch, err = dumper.Listen(ctx, in_apm)
  if err != nil { t.Fatalf("could not dump to proto: %v", err) }

  time.Sleep(10 * time.Millisecond)
  cancel()
  close(in_apm)
  select {
    case <-done_ch: break
    case <-time.After(10 * time.Millisecond):
      t.Fatalf("Receiver could not finish before the timeout")
  }

  filepath := dumper.Filepath()
  expected_pbs := []*messages.Timeserie{expected_pb}
  compareFileContentToProto(t, filepath, expected_pbs)
  compareRepoContents(t, conf, filepath, ini_repo)
}

