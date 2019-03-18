#!/bin/bash

SID=$1
cd $2


### quality trimming

time java -jar /opt/Trimmomatic.0.38/trimmomatic-0.38.jar PE -threads ${SLURM_CPUS_PER_TASK} -trimlog ./Trimmed/Trimmed_logs/${SID}"_trim.log" ./Clipped/${SID}"_R1.fastq" ./Clipped/${SID}"_R2.fastq" ./Trimmed/${SID}"_ptrim_R1.fastq" ./Trimmed/${SID}"_strim_R1.fastq" ./Trimmed/${SID}"_ptrim_R2.fastq" ./Trimmed/${SID}"_strim_R2.fastq" SLIDINGWINDOW:4:15 MINLEN:100


### read merging

pear --version

time pear -j ${SLURM_CPUS_PER_TASK} -v 10 -n $3 -m $4 -f ./Trimmed/${SID}"_ptrim_R1.fastq" -r ./Trimmed/${SID}"_ptrim_R2.fastq" -o ./Merged/${SID} > ./Merged/Merged_logs/${SID}"_merged.log"


### quality check

fastqc -version

cd ./FastQC/
time fastqc -o . ../Merged/${SID}".assembled.fastq"
unzip ${SID}".assembled_fastqc.zip"
cd ..


### extract fasta information

time reformat.sh in=./Merged/${SID}".assembled.fastq" out=./Swarm/${SID}"_good.fasta" fastawrap=1000 


### sample level dereplication
cd ./Swarm/
time grep -v "^>" ${SID}"_good.fasta" | \
grep -v [^ACGTacgt] | sort -d | uniq -c | \
while read abundance sequence ; do
    hash=$(printf "${sequence}" | sha1sum)
    hash=${hash:0:40}
    printf ">%s_%d_%s\n" "${hash}" "${abundance}" "${sequence}"
done | sort -t "_" -k2,2nr -k1.2,1d | \
sed -e 's/\_/\n/2' > ${SID}"_dereplicated.fasta"
cd ..

