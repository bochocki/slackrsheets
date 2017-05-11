A <- c("This is a message,
       written on multiple
       lines.")

B <- c("This is a message,
       written on multiple
       lines,\nwhich also includes a linebreak")

C <- c("This is a message,
       written on multiple
       lines,\n
       which also includes a linebreak")

# won't work for this case, who cares
D <- c("This is a message,
       written on multiple
       lines, \n
       which also includes a linebreak")


reflow <- function(x) {
  x <- paste(unlist(strsplit(gsub("[ ] ", "", x), "[\n] ")), collapse= " ")
  return(gsub("[\n] ", "\n", x))
}

reflow(A)
reflow(B)
reflow(C)
reflow(D)

message(reflow(A))
message(reflow(B))
message(reflow(C))
message(reflow(D))
