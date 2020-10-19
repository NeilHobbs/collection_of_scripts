###CHIC402, Week 3, Exercise 1


##1. Function to generate random gene sequence
generate_gene_seq = function(A_prob, C_prob, G_prob, T_prob, l){
  nucleotides = c("A", "C", "G", "T")
  gene_seq = sample(nucleotides, l, replace=TRUE,
                    prob=c(A_prob, C_prob, G_prob, T_prob))
  return(c(gene_seq))
}

sequence_short = generate_gene_seq(0.1,0.3,0.4,0.2,10)

sequence_long = generate_gene_seq(0.5, 0.1, 0.2, 0.2, 100)

##Unsure how to do the loop:
                      ##1. sample 1 position in sequence_long
                      ##2. sample 1 position in nucleotides
                      ##3. assign probability of mutation happening (25%)
                      ##4. make mutation happen 
                      ##5. repeat x100

for(i in 1:100){
  sequence_long[i+1] = sequence_long[i]%>%
              sample(1, prob=0.25, replace(sample(nucleotides, 1)))
}                        