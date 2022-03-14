1:20

pi:10

15:1

seq(1, 20)
seq(0, 10, 0.5)

seq(5, 10, length=30)

my_seq <- seq(5, 10, length=30)

my_seq

length(my_seq)

#Another command

rep(0, times = 40)

# what if there is a vector (0, 1, 2) and I want to repeat it. How to do it?

print("-----------------------------------------------------------------------------")

rep(my_seq, times =10)

#or  

rep(c(0, 1, 2), each =10)

num_vect <-  c(0.5, 55, -10, 6)

num_vect

tf <- num_vect <1

tf

num_vect >=6

my_char <- c("My", "Name", "is")
my_char

my_line = paste(my_char, collapse = " ")

my_line

my_name = paste(c(my_line, "Vineeth"), collapse = " ")

my_name