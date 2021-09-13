#!/bin/bash
for i in $(ls -d */)
do
    cd ${i%%/}
    stack clean --full
    cd ..
done
