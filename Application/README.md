# Compilation flag

on cascalake: -O3  -ansi-alias -ip -axCORE-AVX512,CORE-AVX2,AVX,SSE4.2
on amd egyc: -O3 -qopt-report=5 -march=core-avx2 -fma -ftz -fomit-frame-pointer -ipo -no-prec-div -fp-model fast=2


Missing system libs:
aclocal-1.15
