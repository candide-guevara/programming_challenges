/*
 * Created on: Nov 30, 2013
 *      Author: eguevara
 */

#include <unittest.h>
#include <ProgramMap.h>
#include <BusiestBeavers.h>
#include <ProgramToSave.h>

namespace TST {

  void testBusiestProgramQueue() {
    BigInteger queueSize = 10;
    BusiestBeavers queue;
    queue.maxSizeHint(queueSize);

    ProgramState prog;
    ExeStats stat1(1, 1, true, false);
    ExeStats stat2(1, 2, true, false);
    ExeStats stat3(1, 3, true, false);
    ExeStats stat4(1, 4, false, false);
    ExeStats stat5(1, 5, true, false);

    queue.pushCopy(prog, stat1);
    queue.pushCopy(prog, stat2);
    queue.pushCopy(prog, stat3);
    queue.pushCopy(prog, stat4);

    TST_ASSERT(queue.begin()->second.output == 1, "Bad priority sorting, expecting lower bound");
    TST_ASSERT(queue.busiest().second.output == 3, "Bad priority sorting, expecting higher bound");

    for (BigInteger i = 0; i < queueSize; ++i)
      queue.pushCopy(prog, stat2);
    TST_ASSERT(queue.size() == queueSize, "Did not add all elements " << queueSize);
    TST_ASSERT(queue.begin()->second.output == 2, "Bad priority sorting, expecting lower bound (full queue)" << queue);
    TST_ASSERT(queue.busiest().second.output == 3, "Bad priority sorting, expecting higher bound");

    queue.pushCopy(prog, stat5);
    TST_ASSERT(queue.busiest().second.output == 5, "Bad priority sorting, expecting higher bound");
  }

  void TestProgramMapInsertion() {
    ProgramMap map;
    ProgramState prog;
    ExeStats stat(1, 1, false, false);
    uint8_t testCode[20] = { I_ABS_TO_R0, I_PRINT, I_INC_R1, I_MOV_R0_TO_ADD, I_INC_R1, I_INC_R0, I_INC_R0, I_INC_R0, I_HALT };

    prog.loadCode(testCode);
    prog.resetRegisters();
    map.addCopy(prog);
    TST_ASSERT(map.size(), "Failed to add progam");
    TST_ASSERT(map.contains(prog), "Failed to detected duplicate");

    prog.setCode(0, I_ABS_TO_R1);
    map.addCopy(prog);
    map.addCopy(prog, stat);
    TST_ASSERT(map.size() == 2, "Failed to add progam (2)");
    TST_ASSERT(map.contains(prog), "Failed to detected duplicate (2)");

    prog.setIp(1);
    stat.output = 3;
    map.addCopy(prog, stat);
    TST_ASSERT(map.size() == 3, "Failed to add progam (3)");
    TST_ASSERT(map.contains(prog), "Failed to detected duplicate (3)");

    ExeStats* stat2 = map.get(prog);
    TST_ASSERT(stat2->output == 3, "Failed to retrieve stats from program");
  }

  void TestProgramToSaveCommonOperations() {
    BigInteger saveCountPerRun = 4;
    BigInteger saveDropRatio = 3;
    ProgramToSave container(saveCountPerRun, saveDropRatio);

    ProgramState prog;
    ExeStats stat(1, 1, true, false);

    for (BigInteger count = 0; count < saveDropRatio - 1; ++count) {
      container.maybeSaveForLater(prog, stat);
      TST_ASSERT(!container.size(), "The element should not be added to the container");
    }
    container.maybeSaveForLater(prog, stat);
    TST_ASSERT(container.size() == 1, "The element should be added to the container");

    container.absolutelySaveForLater(prog, stat);
    TST_ASSERT(container.size() == 2, "The element should absolutely be added to the container");

    stat.output = 2;
    for (BigInteger count = 0; count < saveCountPerRun - 1; ++count)
      container.absolutelySaveForLater(prog, stat);
    auto first = container.begin();
    TST_ASSERT(container.size() == saveCountPerRun, "Container size does not respect its limit " << saveCountPerRun);
    TST_ASSERT((*first).second.output != stat.output, "Circular insertion of elements is wrong");
    TST_ASSERT((*(++first)).second.output == stat.output, "Circular insertion of elements is wrong (2)");

    container.clear();
    TST_ASSERT(!container.size(), "Container was not empty");
    container.absolutelySaveForLater(prog, stat);
    first = container.begin();
    TST_ASSERT((*first).second.output == stat.output, "Circular insertion of elements is wrong (3)");
  }

  void TestProgramToSaveMergingWithMap() {
    BigInteger saveCountPerRun = 4;
    BigInteger saveDropRatio = 3;
    BigInteger initialStat = 1;
    ProgramToSave container(saveCountPerRun, saveDropRatio);
    ProgramState prog;
    ProgramMap map;
    ExeStats stat(initialStat, initialStat, false, false);
    uint8_t testCode[20] = { I_ABS_TO_R0, I_PRINT, I_INC_R1, I_MOV_R0_TO_ADD, I_INC_R1, I_INC_R0, I_INC_R0, I_INC_R0, I_HALT };

    prog.loadCode(testCode);
    container.absolutelySaveForLater(prog, stat);
    stat.output = stat.iterations = 3;
    stat.stops = true;

    container.saveIntoMap(map, stat);
    TST_ASSERT(map.size() == 1, "Failed to merge program states into map");
    const ExeStats& merged = map.begin()->second;
    TST_ASSERT(merged.stops, "Merged states were not updating according to final execution result");
    TST_ASSERT(merged.output == stat.output - initialStat, "Merged states were not updating according to final execution result (2)");
    TST_ASSERT(merged.iterations == stat.iterations - initialStat,
        "Merged states were not updating according to final execution result (3)");

    map.clear();
    container.clear();
    stat.stops = false;
    prog.setCode(0, I_ABS_TO_R1);
    container.absolutelySaveForLater(prog, stat);
    prog.setCode(0, I_DEC_R0);
    container.absolutelySaveForLater(prog, stat);
    stat.reachedMaxIt = true;

    container.saveIntoMap(map, stat);
    TST_ASSERT(map.size() == 2, "Failed to merge program states into map (2)");
    const ExeStats& merged2 = map.begin()->second;
    TST_ASSERT(!merged2.stops, "Merged states were not updating according to final execution result (2)");
    TST_ASSERT(merged2.reachedMaxIt, "Merged states were not updating according to final execution result (4)");
  }

}  // namespace TST

