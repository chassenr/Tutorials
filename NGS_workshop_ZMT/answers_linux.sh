# Task 1: 	Open a terminal, navigate to a suitable location and create a directory for this workshop
# Task 2:	Move/Copy the example data set to this directory
# Task 3: 	Unzip the files
# Task 4: 	List the files, have a quick look at the contents, count the R1 and R2 files
# Task 5: 	Extract all sequences with “@MISEQ:41:000000000-A9A9U:1:1101“ in the header in CH_A_1_SB_clip_R1.fastq, and save them in a new fastq file
# Task 6: 	Shorten file names by removing “_SB_clip”, and generate a text file with sample names
# Task 7:	Count the number of sequences per sample
# Task 8: 	Move the sequence files into a new directory corresponding to the stage of analysis they are in
# Task 9:	Check what is in your .bashrc, modify it if necessary
# Task 10: 	Check what is in your $PATH, locate R and python 2 (using modules)

# Task 1+2
cd /models/chh/

# Task 3
tar -xzvf WorkshopData.tar.gz
cd WorkshopData
gunzip *.gz

# Task 4
ls -1 *R1.fastq | wc -l
ls -1 *R2.fastq | wc -l

# Task 5
grep -A3 -F "@MISEQ:41:000000000-A9A9U:1:1101" CH_A_1_SB_clip_R1.fastq | sed '/^--$/d' > test.fastq
rm test.fastq

# Task 6
ls -1v *R1.fastq | sed 's/_SB_clip_R1\.fastq//' > sample_names.txt

while read line
do
  mv ${line}"_SB_clip_R1.fastq" ${line}"_R1.fastq"
  mv ${line}"_SB_clip_R2.fastq" ${line}"_R2.fastq"
done < sample_names.txt

# Task 7
ls -1v *_R1.fastq | xargs wc -l | grep -v "total" | awk '{print $1/4}' | paste sample_names.txt -

# Task 8
mkdir Clipped
mv *.fastq ./Clipped/

# Task 9
cd ~
less .bashrc
# use vi or gedit for modification

# Task 10
echo $PATH
module load R/3.5.1 miniconda2
which R
which python
