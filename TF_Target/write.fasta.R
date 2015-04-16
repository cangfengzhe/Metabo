function (sequences, names, file.out, open = "w", nbchar = 60) 
{
  outfile <- file(description = file.out, open = open)
  
  write.oneseq <- function(sequence, name, nbchar) {
    writeLines(paste(">", name, sep = ""), outfile)
    l <- length(sequence)
    q <- floor(l/nbchar)
    r <- l - nbchar * q
    if (q > 0) {
      sapply(seq_len(q), function(x) writeLines(c2s(sequence[(nbchar * 
                                                                (x - 1) + 1):(nbchar * x)]), outfile))
    }
    if (r > 0) {
      writeLines(c2s(sequence[(nbchar * q + 1):l]), outfile)
    }
  }
  if (!is.list(sequences)) {
    write.oneseq(sequence = sequences, name = names, nbchar = nbchar)
  }
  else {
    n.seq <- length(sequences)
    sapply(seq_len(n.seq), function(x) write.oneseq(sequence = as.character(sequences[[x]]), 
                                                    name = names[x], nbchar = nbchar))
  }
  close(outfile)
}
<environment: namespace:seqinr>