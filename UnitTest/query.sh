#!/bin/bash
source ../Utilities/bench_init.sh
python3 runquery.py --fields "software_name:'vasp'" --daterange 20200120-20200124 --result vasp.out
