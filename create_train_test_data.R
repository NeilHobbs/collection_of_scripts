create_train_test_data = function(dataset,
                                  proportion){
  
  sample.size = floor(proportion * nrow(dataset))
  train.ind = sample(seq_len(nrow(dataset)), size = sample.size)
  
  data.train = sequences.set.1.df[dataset, ] 
  data.test = sequences.set.1.df[-dataset, ]
  
  return(list(data.train,
              data.test))
  
}