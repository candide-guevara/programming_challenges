PROG_TO_TEST='self_reproducing_program.py'
ITERATIONS=10
COUNTER=0
OUT_PREFIX='output'

python $PROG_TO_TEST > $OUT_PREFIX$COUNTER.py
while [[ $COUNTER -lt $ITERATIONS ]]; do
  NEW_COUNTER=$(( $COUNTER + 1 ))
  python $OUT_PREFIX$COUNTER.py > $OUT_PREFIX$NEW_COUNTER.py 
  COUNTER=$NEW_COUNTER
done

DIFF_RESULT=`diff $PROG_TO_TEST $OUT_PREFIX$COUNTER.py`
[[ -z $DIFF_RESULT ]] && echo "Sweet :-)"

