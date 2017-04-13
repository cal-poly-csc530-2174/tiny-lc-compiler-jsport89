#!/bin/bash

###########
## TESTS ##
###########

racket Assingment-1.rkt test-1 Tests/test-1.js
node Tests/test-1.js &> Tests/test-1.out
diff -q Tests/test-1.out Tests/test-1.expect && echo "test1 passed."

