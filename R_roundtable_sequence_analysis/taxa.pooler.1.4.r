#documentation start
#=============================================================================
# Copyright (C) 2011 Angelique Gobet & Alban Ramette
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#=============================================================================
# File contents
# part of the MultiCoLA package
# further documentation under clearingHouse/MultiCoLA Manual.1.4.pdf
#=============================================================================
#documentation end

cat("~~~~~~~~~~~Taxa Pooler~~~~~~~~~~~\n")
cat('Use the function as follows:\n')
cat('        storing_name<-taxa.pooler(M)\n')
cat('        M=read.table("input.txt",header=TRUE,row.names=1),\n') 
cat('        OTUs as rows\n')
cat('        samples followed by taxonomy as columns (e.g. sample1,sample2,...,phylum,class...)\n\n')
cat('--->type of output:\n') 
cat('	    list of new tables with the samples as rows & taxa\n')
cat('	    with sum of tags for each sample as columns\n')
cat('	    for each taxonomic level with complete annotation+whole dataset OTUs\n\n\n')

taxa.pooler<-function(M1){
sa=as.numeric(readline("\nNumber of samples? (e.g. 16)...\t"))
ta=as.numeric(readline("\nNumber of taxonomic levels? (e.g. phylum+class+order+family+genus=5)...\t"))
pa=readline("\nPresence/absence tables as output? (y/n)...\t")
OUTP=readline("\nOutput as text files? (y/n)...\t")

if(nrow(M1[-which(apply(M1,1,function(x)any(is.na(x)))),])==0){
  M<-M1
  } else {
  M<-M1[-which(apply(M1,1,function(x)any(is.na(x)))),]
  }
 
pool.1.level<-function(M,j){
	N<-matrix(NA,length(unique(M[,sa+j])),sa)
	row.names(N)<-sort(unique.default(M[,sa+j]))  #name of unique taxa
		colnames(N)<-colnames(M[,1:sa])	#name of samples
	for (i in 1:sa){
    N[,i]<-by(M[,i],factor(M[,sa+j]),sum)
  }	#end for i
	return(t(N))
}	#end pool.1.level()


taxa_res<-vector("list",ta+2)
names(taxa_res)<-c(colnames(M[,(sa+1):(sa+ta)]),"OTUs_completeDS","OTUs_wholeDS")

#loop to apply the function pool.1.level at all taxonomic levels
for (k in 1:ta){ 
	taxa_res[[k]]<-pool.1.level(M,k)
	}	#end for k

#table at the OTU level with only OTUs with a complete annotation
taxa_res[[ta+1]]<-t(M[,1:sa])

#table at the OTU level with all the OTUs
taxa_res[[ta+2]]<-t(M1[,1:sa])

  if(pa=="y"){
	 for(j in 1:(ta+2)){
   taxa_res[[j]][taxa_res[[j]]!=0]<-1
    }
  } #end if

  if(OUTP=="y"){
	 for(j in 1:(ta+2)){
    write.table(taxa_res[[j]],paste(names(taxa_res[j]),".matrix.txt",sep=""),quote=FALSE)
    }
  } #end if

return(taxa_res)            

}	#end taxa.pooler
