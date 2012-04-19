import os

NUMEXPLICIT = 12
NUMTESTS = 27

print "Running tests for explicit leaks (these should all raise exceptions)"

for i in range(1, NUMEXPLICIT+1):
  if (i < 10):
    i = "0" + str(i)
  else:
    i = str(i)
    
  print "Running test" + i + ".not"

  os.system('cd build; echo "inputs 2 4 9" > ../test' + i + '.out; echo "2\n4\n9" | scala miniJS ../test\\ suite/test' + i + '.not >> ../test' + i + '.out 2>&1')

print "\n\nRunning tests for implicit leaks; outputs labeled \"true\" should raise exceptions, outputs labeled \"false\" should not."

for i in range(NUMEXPLICIT+1, NUMTESTS):
  i = str(i)
  print "Running test" + i + ".not"

  os.system('cd build; echo "inputs 2 4 9" > ../test' + i + 'true.out; echo "2\n4\n9" | scala miniJS ../test\\ suite/test' + i + '.not >> ../test' + i + 'true.out 2>&1')

  os.system('cd build; echo "inputs 0 0 0" > ../test' + i + 'false.out; echo "0\n0\n0" | scala miniJS ../test\\ suite/test' + i + '.not >> ../test' + i + 'false.out 2>&1')
