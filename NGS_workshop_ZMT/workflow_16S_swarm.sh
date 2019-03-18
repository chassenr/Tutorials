# Sequence processing of amplicon data
# optimized for primer-clipped data that can be downloaded from e.g. ENA
# execute this script step by step to run the complete 16S analysis pipeline
# contents:
#   setting up working environment
#   per sample analysis steps (quality trimming, merging, quality check, dereplication)
#   swarming

# Check that you have permission to write to $TMPDIR
# if $TMPDIR has no value, this is also ok
# after starting a new session, always reload all modules
# if the module command is not found, run: source ~/.bashrc


### file and environment preparation

# set environment to our needs
# load required modules

module load pear/0.9.11 bbmap/35.02 fastqc/0.11.7 miniconda2 swarm/2.2.2 R/3.5.1 


# create directories for each analysis step

mkdir Logfiles
mkdir Trimmed
mkdir FastQC
mkdir Merged
mkdir Swarm
mkdir Silvangs

mkdir ./Trimmed/Trimmed_logs
mkdir ./Merged/Merged_logs


### per sample analysis
# the script master_sequencer.sh will execute the following programs for:
#   quality control (trimmomatic)
#   read merging (PEAR)
#   quality check (fastqc)
#   sequence dereplication (bash script available on https://github.com/torognes/swarm/wiki/Preparation)
# the script is run separately for each sample

# if not already done, generate file with sample names
# make sure that ordering of sample names in the ls command here matches the ordering used for counting the sequences later on

cd Clipped/
ls -1v *R1.fastq | sed 's/_R1\.fastq//' > ../sample_names.txt
cd ..

# set min and max expected fragment length for merging

MINLEN=380
MAXLEN=450

while read line
do
  srun --cpus-per-task=2 --mem-per-cpu=2000 -x ecomod01,ecomod02,ecomod03 master_sequencer.sh $line $PWD $MINLEN $MAXLEN 1>./Logfiles/master_sequencer.$line.out 2>./Logfiles/master_sequencer.$line.err &
done < sample_names.txt


# to run this step sequentially without a resource manager:

pear --version
fastqc -version
while read line
do
  SID=${line}
  # quality trimming
  time java -jar /opt/Trimmomatic.0.38/trimmomatic-0.38.jar PE -threads 2 -trimlog ./Trimmed/Trimmed_logs/${SID}"_trim.log" ./Clipped/${SID}"_R1.fastq" ./Clipped/${SID}"_R2.fastq" ./Trimmed/${SID}"_ptrim_R1.fastq" ./Trimmed/${SID}"_strim_R1.fastq" ./Trimmed/${SID}"_ptrim_R2.fastq" ./Trimmed/${SID}"_strim_R2.fastq" SLIDINGWINDOW:4:15 MINLEN:100 
  # merging
  time pear -j 2 -v 10 -n ${MINLEN} -m ${MAXLEN} -f ./Trimmed/${SID}"_ptrim_R1.fastq" -r ./Trimmed/${SID}"_ptrim_R2.fastq" -o ./Merged/${SID} > ./Merged/Merged_logs/${SID}"_merged.log"
  # quality check
  cd ./FastQC/
  time fastqc -o . ../Merged/${SID}".assembled.fastq"
  unzip ${SID}".assembled_fastqc.zip"
  cd ..
  # conversion to fasta
  time reformat.sh in=./Merged/${SID}".assembled.fastq" out=./Swarm/${SID}"_good.fasta" fastawrap=1000 
  # dereplication
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
done < sample_names.txt


# collect some quality flags from fastqc output
# combine flags of 'Per base sequence quality' module for all files

grep "Per base sequence quality" ./FastQC/*.assembled_fastqc/summary.txt > ./FastQC/QC_summary.txt


# range of read lengths

grep "Sequence length" ./FastQC/*.assembled_fastqc/fastqc_data.txt > ./FastQC/QC_read_length.txt


# combine flags of 'Sequence Length Distribution' module for all files including most abundant read lengths

while read line
do
  grep "Sequence Length Distribution" ./FastQC/${line}".assembled_fastqc"/summary.txt > tmp
  awk '/^>>Sequence Length Distribution/,/^>>END_MODULE/' ./FastQC/${line}".assembled_fastqc"/fastqc_data.txt | sed -e '1,2d' -e '$d' > ./FastQC/${line}".assembled_fastqc"/fastqc_SLD.txt
  sort -t$'\t' -k2nr ./FastQC/${line}".assembled_fastqc"/fastqc_SLD.txt | head -1 | paste tmp -
  rm tmp
done < sample_names.txt >> ./FastQC/QC_read_distribution.txt


# count number of sequences per analysis step
# only counting forward read as representative for PE

ls -1v ./Clipped/*_R1.fastq | xargs wc -l | grep -v "total" | awk '{print $1/4}' | paste sample_names.txt - > tmp1
ls -1v ./Trimmed/*_ptrim_R1.fastq | xargs wc -l | grep -v "total" | awk '{print $1/4}' | paste tmp1 - > tmp2
ls -1v ./Merged/*.assembled.fastq | xargs wc -l | grep -v "total" | awk '{print $1/4}' | paste tmp2 - > tmp3
echo -e 'SID\tClipped\tTrimmed\tMerged' | cat - tmp3 > nSeqs_all.txt
rm tmp*

# at this step it usually is a good idea to check the number of sequences lost at each step and to quickly scroll through the generated QC summary files


### swarming

# study-level dereplication
# code provided by https://github.com/torognes/swarm/wiki/Working-with-several-samples

cd Swarm
export LC_ALL=C
cat *_dereplicated.fasta | \
awk 'BEGIN {RS = ">" ; FS = "[_\n]"}
     {if (NR != 1) {abundances[$1] += $2 ; sequences[$1] = $3}}
     END {for (amplicon in sequences) {
         print ">" amplicon "_" abundances[amplicon] "_" sequences[amplicon]}}' | \
sort --temporary-directory=$(pwd) -t "_" -k2,2nr -k1.2,1d | \
sed -e 's/\_/\n/2' > all_samples.fasta


# swarm algorithm
# after swarm has finished, look at the swarm.log output to evaulate performance of algorithm
# focus on the number of heavy and light swarms compared to the number of amplicons/sequences contained in these swarms,
# and the number of successful grafts

python ../amplicon_contingency_table.py *_dereplicated.fasta > amplicons_table.csv

swarm --version

time swarm -b 3 -d 1 -f -t 6 -l swarm.log -o amplicons.swarms -s amplicons_stats.txt -w amplicons_seeds.fasta all_samples.fasta

STATS="amplicons_stats.txt"
SWARMS="amplicons.swarms"
AMPLICON_TABLE="amplicons_table.csv"
OTU_TABLE="OTU_contingency_table.csv"

# Header
echo -e "OTU\t$(head -n 1 "${AMPLICON_TABLE}")" > "${OTU_TABLE}"

# Compute "per sample abundance" for each OTU
awk -v SWARM="${SWARMS}" \
    -v TABLE="${AMPLICON_TABLE}" \
    'BEGIN {FS = " "
            while ((getline < SWARM) > 0) {
                swarms[$1] = $0
            }
            FS = "\t"
            while ((getline < TABLE) > 0) {
                table[$1] = $0
            }
           }

    {# Parse the stat file (OTUs sorted by decreasing abundance)
     seed = $3 "_" $4
     n = split(swarms[seed], OTU, "[ _]")
     for (i = 1; i < n; i = i + 2) {
         s = split(table[OTU[i]], abundances, "\t")
         for (j = 1; j < s; j++) {
             samples[j] += abundances[j+1]
         }
    }
    printf "%s\t%s", NR, $3
    for (j = 1; j < s; j++) {
        printf "\t%s", samples[j]
    }
  printf "\n"
  delete samples
  }' "${STATS}" >> "${OTU_TABLE}"


### Inspect output
# How many singletons are in the data set?
# What is the proportion of singletons (a) of all OTUs (b) of all sequences?
# Remove singletons from fasta file

# get accession number for heavy swarms
awk '$16>1' OTU_contingency_table.csv | cut -f2 | sed '1d' > heavy.accnos

# select these sequences
grep -A1 -F -f heavy.accnos amplicons_seeds.fasta | sed '/^--$/d' > amplicons_seeds_heavy.fasta


### blast for chloroplast sequences
# extract chlorplast sequences from dada2 output (in R)
module load ncbi-blast/2.7.1

# generate blast database with chloroplast sequence
# search nucleotide database on NCBI
# (chloroplast[All Fields] AND 16S[All Fields] AND ribosomal[All Fields] AND complete[All Fields]) AND ((protists[filter] OR plants[filter]) AND chloroplast[filter])
# send to complete record --> file --> Fasta

# format blast db
makeblastdb -in Chloroplast_ncbi20190315.fasta -dbtype nucl -out chloroplast_blastdb -logfile chloroplast_makeblastdb.log

# run blast
blastn -query bac_dada2_unique_chloro.fasta -task blastn -db chloroplast_blastdb -out dada2_chloro.blastout -evalue 1e-10 -outfmt "6 qseqid sseqid pident length mismatch gapopen qstart qend sstart send evalue bitscore qcovs qlen gaps" -num_threads 8
# filter blast output
awk -v threshold=93 '($3 + $13) / 2 >= threshold' dada2_chloro.blastout > dada2_chloro.blastout_filt.txt
sort -k1,1 -k12,12gr -k11,11g dada2_chloro.blastout_filt.txt | sort -u -k1,1 --merge > dada2_chloro.blastout_best.txt

# retrieve taxonomic path of best hit
# run for both chloroplasts and mitochondria
cut -f2 dada2_chloro.blastout_best.txt | sort | uniq > getPath.uid
# in R
require(rentrez)
require(XML)
uid <- read.table("getPath.uid", h = F)
colnames(uid) <- "uid"
uid$path <- c(NA)
uid <- uid[, c("path", "uid")]
for (i in 1:nrow(uid)) {
  tmp <- entrez_fetch("nucleotide", id = uid$uid[i], rettype = "full", retmode = "xml", parsed=TRUE)
  write(tmp, "efetch.tmp")
  system("grep \"<OrgName_lineage>\" efetch.tmp | head -1 | sed 's/^ *//' > path.tmp")
  uid[i, "path"] <- scan("path.tmp", what = "character", sep = "\t")
}
write.table(uid, "blastpPath.efetch", quote = F, sep = "\t", row.names = F, col.names = F)

# parse efetch output
sed -e 's/^ *//' -e 's/<OrgName_lineage>//' -e 's/<\/OrgName_lineage>//' blastpPath.efetch > chloro_path.txt
# were all taxonomic paths retrieved?
cut -f2 chloro_path.txt | diff - getPath.uid


