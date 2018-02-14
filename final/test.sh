MAX_ROWS=10000
MAX_COLS=10000
FRATIO=100
MAXUPDATE=50
FOCUSRATIO=10
cd test/gen
./gen.sh $MAX_ROWS $MAX_COLS $FRATIO "../test.csv" $MAXUPDATE $FOCUSRATIO "../user.txt"

cd ..

echo 'running ws'
../ws test.csv user.txt view0.csv changes.txt final.csv

# cleaning
rm *.byte
