# User Interface to Get Provenance data from framework

There is a utility user can use to query the database to see the result of their runs. 
The script `runquery.py` under `Utilities` is used to query the database.  
User can specify the search condition by using `fields` and `daterange`:

## Arguments

### Fields
`fields` is a key:value string concatenated with `|` or '&', while '|' means logic or,
'&' means logic and. In the key:value pairs, value needs to be put in single quote, and wild card '%' can be 
used in the host_name and module fields. e.g. key1:'value1'&host_name:'%value2%'|key3:'value3'.

Keys user can specify are:
```
 exp_id
 mode
 software_name
 software_version
 scheduler
 username
 queue
 pbs_jobid
 nruns
 total_nproc
 nodes
 ppns
 ngpus
 script
 experiment_location
 md5
 command
 exit_status
 average_time_seconds
 maximum_time_seconds
 minimum_time_seconds
 standard_deviation
 variance
 exp_date
 exp_time
 host_name
 module
```

### Date Range
daterange sepcifies the date range in a format yyyymmdd-yyyymmdd

### Output
output is an optinal argument
  * when user didn't specify this argument, all fields (except script) in the database will be dumped out,
  * when user specify this argument, the default fileds [exp_id, host_name, module, software_name] will always be output
  * the output will be written into a file named result.csv, and it is a | separated file


## Example output
```
cd UnitTest
source ../Utilities/bench_init.sh
python3 runquery.py --fields "software_name:'leslie-spec'" --daterange 20191202-20191204 --output="exp_id,mode"
cat result.csv

0|170|host1,host2|intel/19.0.3/mvapich2/2.3.1,leslie-spec/21Feb19|leslie-spec|interactive
1|171|host3|intel/19.0.3/mvapich2/2.3.2,leslie-spec/21Feb19|leslie-spec|queue
2|172|host1,host3|intel/19.0.3/mvapich2/2.3.1,leslie-spec/21Feb19|leslie-spec|interactive
3|173|host4|intel/19.0.3/mvapich2/2.3.2,leslie-spec/21Feb19|leslie-spec|queue
4|178|host4|intel/19.0.3/mvapich2/2.3.2,leslie-spec/21Feb19|leslie-spec|queue
```


