## This is the system features we will capture in each participated host

### 
```
 (System)Information from DMIDecode:
  bios-version
  system-manufacturer
  system-product-name
  system-version
  system-uuid
  processor-family
  processor-version
  processor-frequency

(Core) information from  lscpu (including hyperthreading info)
lscpu | grep -i -E  "^CPU\(s\):|core|socket"
CPU(s):                24
Thread(s) per core:    1 -> means hyperthreading doesn't turn on
Core(s) per socket:    12
Socket(s):             2

(Memory) Information from free command:
MemTotal (in 64, 128, 256 GB)

(Network IB info)
IB devices : ibstat --list_of_cas
IB rate: ibstat | grep rate:

(GPU info)
GPUs list: nvidia-smi --list-gpus
# GPUs

(OS info)
Kernel version: uname -r

```
