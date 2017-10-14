14 Oct 2017 - planning meeting.

teamEM()

TESTING

EXTENSION

makehistogram()

testdifferentstarts()
-> plots + test all belong to same normal distribution

Maki:
  putting all blocks together, look for blocks that would make nice functions
  testing belong to (same?) Normal distribution
Ellen: 
  testing log-likelihood function
  how to test overall function, compare to standard version
Irma: 
  write assign k in different ways to initial posterior matrix
  testing exp-max function
  setup input sanitisation
  finish structure diagram
  

teamEM_test1()
- assign using mean
teamEM_test( assignk=mean/probability, and prob= P(xbelongs in k)=0.5 or 0.2.)

assignk_mean()
assignk_prob( prob=0.5)

set.seed()
run teamEM
set.seed(same)
run standard version teamEM
compare results are identical

mixtools::normalmixEM2comp()
