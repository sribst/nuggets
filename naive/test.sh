MAX_ROWS=100
MAX_COLS=100
FRATIO=10

cd test/gen
./gen.sh $MAX_ROWS $MAX_COLS $FRATIO ../test.csv

cd ..

echo 'running ws'
../ws test.csv change.txt out.csv _log
