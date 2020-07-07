# set working directory
WDIR="/home/chh/Documents/Projects/UniHB_MOeP_dada2_workshop/Library_052020"

# prepare files
cd $WDIR
mkdir Original
cat Library12_FDDP202322600-1a_HHMM7DRXX_L1_1.fq.gz Library12_FDDP202322600-1a_HJTTMDRXX_L2_1.fq.gz > Original/Library12_1.fastq.gz
cat Library12_FDDP202322600-1a_HHMM7DRXX_L1_2.fq.gz Library12_FDDP202322600-1a_HJTTMDRXX_L2_2.fq.gz > Original/Library12_2.fastq.gz
rm *.fq.gz
mkdir Logfiles
# CAUTION: It is not recommended to combine the output from several sequencer runs. Some argue, lanes should also be processed independently.
# If different sequencing batches of the same project are processed independently, they can be combined into one sample-by-asv table after merging (Before Chimera detection).
# This also applies to the separate processing of reads in the wrong orientation.


### Demultiplex using cutadapt version 2.1
mkdir Demux
sed '1d' Library12_mapping.txt | head -10 | while read line
do
  SID=$(echo "${line}" | cut -f1)
  BCD=$(echo "${line}" | cut -f2 | sed 's/^/\^/')
  OLP=$(expr ${#BCD} - 1)

  cutadapt -j 4 -O ${OLP} --no-indels -e 0 -g ${BCD} -G ${BCD} --discard-untrimmed -o Demux/${SID}"_R1.fastq.gz" -p Demux/${SID}"_R2.fastq.gz" Original/Library12_1.fastq.gz Original/Library12_2.fastq.gz > Logfiles/${SID}".demux.log" 2>&1

done

# further processing steps are easier and faster without gz
gunzip Demux/*.gz


### Primer clipping using cutadapt version 2.1
mkdir Clipped
sed '1d' Library12_mapping.txt | head -10 | while read line
do
  SID=$(echo "${line}" | cut -f1)
  FWD=$(echo "${line}" | cut -f3 | sed 's/^/\^/')
  REV=$(echo "${line}" | cut -f4 | sed 's/^/\^/')
  OFWD=$(expr ${#FWD} - 2)
  OREV=$(expr ${#REV} - 2)
  ERROR=0.16
  
  # process fwd-rev orientation
  cutadapt -j 1 --no-indels -e ${ERROR} -g "${FWD};o=${OFWD}" -G "${REV};o=${OREV}" -m 50 --discard-untrimmed -o Clipped/${SID}"_clip_fr_R1.fastq" -p Clipped/${SID}"_clip_fr_R2.fastq" Demux/${SID}"_R1.fastq" Demux/${SID}"_R2.fastq" > Logfiles/${SID}".clip_fr.log" 2>&1
  
  # process rev-fwd orientation
  cutadapt -j 1 --no-indels -e ${ERROR} -g "${REV};o=${OREV}" -G "${FWD};o=${OFWD}" -m 50 --discard-untrimmed -o Clipped/${SID}"_clip_rf_R1.fastq" -p Clipped/${SID}"_clip_rf_R2.fastq" Demux/${SID}"_R1.fastq" Demux/${SID}"_R2.fastq" > Logfiles/${SID}".clip_rf.log" 2>&1

  # re-orient reads
  # Change the read id in the headers "@MISEQ:41:000000000-A9A9U:1:1101:17488:1966 1:N:0:AGGCAGAAAGAGTAGA"
  awk '{if (NR%4==1){gsub("^1:","2:",$2); print $0}else{print $0}}' Clipped/${SID}"_clip_rf_R1.fastq" > Clipped/${SID}"_rf2fr_R2.fastq"
  awk '{if (NR%4==1){gsub("^2:","1:",$2); print $0}else{print $0}}' Clipped/${SID}"_clip_rf_R2.fastq" > Clipped/${SID}"_rf2fr_R1.fastq"

  # add all corrected seqs to the fwd-rev fastqs
  cat Clipped/${SID}"_clip_fr_R1.fastq" Clipped/${SID}"_rf2fr_R1.fastq" > ./Clipped/${SID}"_clip_R1.fastq"
  cat Clipped/${SID}"_clip_fr_R2.fastq" Clipped/${SID}"_rf2fr_R2.fastq" > ./Clipped/${SID}"_clip_R2.fastq"

done


### Count sequences for each step and switch to dada2 in R
ls -1v Demux/*_R1.fastq | xargs wc -l | grep -v "total" | awk '{print $1/4}' | paste <(ls -1v Demux/*_R1.fastq | xargs -n1 basename | sed 's/_R1\.fastq//') - > tmp1
ls -1v Clipped/*_clip_R1.fastq | xargs wc -l | grep -v "total" | awk '{print $1/4}' | paste tmp1 - > tmp2
echo -e 'SID\tDemux\tClipped' | cat - tmp2 > nSeqs_all.txt
rm tmp*

