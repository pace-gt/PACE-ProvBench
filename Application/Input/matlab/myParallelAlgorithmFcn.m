function [numWorkers,time] = myParallelAlgorithmFcn(ncores)


%Cherry March 2019, make sure to set the ulimit -u 8192
%complexities =  [2^18 2^20 2^21 2^22];
complexities =  [2^21];
%numWorkers = [1 2 4 16 28];
numWorkers= [ncores];
%time = zeros(numel(numWorkers),numel(complexities));

% To obtain obtain predictable sequences of composite numbers, fix the seed
% of the random number generator.
rng(0,'twister');
myCluster=parcluster('local');
myCluster.NumWorkers=ncores;
saveProfile(myCluster);
%parfor not creating pool automatically
ps=parallel.Settings;
ps.Pool.AutoCreate=false;
parpool('local',ncores);
tic;
for c = 1:numel(complexities)
    
    primeNumbers = primes(complexities(c));
    compositeNumbers = primeNumbers.*primeNumbers(randperm(numel(primeNumbers)));
    factors = zeros(numel(primeNumbers),2);
    
    for w = 1:numel(numWorkers)
        %parfor (idx = 1:numel(compositeNumbers), numWorkers(w))
        parfor (idx = 1:numel(compositeNumbers), ncores)
            factors(idx,:) = factor(compositeNumbers(idx));
        end
    end
end
time = toc;
fileID=fopen('time.output','w');
fprintf(fileID,'%8.6f seconds',time);
fclose(fileID);
poolobj=gcp('nocreate')
delete(poolobj)
