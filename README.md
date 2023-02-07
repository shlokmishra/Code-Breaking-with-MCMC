# Introduction

Cryptography has been an important aspect of communication for
centuries. Encryption techniques have evolved with time to provide more
secure communication. However, with the advancement in computing power,
it has become easier to break encryption. Markov Chain Monte Carlo
(MCMC) methods provide a powerful tool to break encryption by sampling
from a high-dimensional distribution and estimating the parameters of
the encryption algorithm.

# Problem Description

A substitution cipher is a simple method of encoding a message by
replacing each letter with a different letter or symbol. In this
project, we will be using MCMC to estimate the mapping from the encoded
message to the original message. Markov chains are defined on a state
space, where the chain is traveling from state to state. In the
framework of our problem, the states our Markov Chain is traveling
between are the $26!$ possible ciphers. We want to the Markov chain to
travel to the ciphers that are more “likely to be correct” and stay away
from the ciphers that are “unlikely to be correct”.

# Theoritical Background

The theory behind MCMC algorithms is based on Bayesian statistics and
can be represented mathematically as follows:

$$\[\pi(\theta|x) \propto L(x|\theta) \pi(\theta)\]$$

where $\(\pi(\theta|x)\)$ is the posterior distribution, $\(L(x|\theta)\)$
is the likelihood function and $\(\pi(\theta)\)$ is the prior
distribution.

# Algorithm

## Similarity Score Calculation

In order to compare any two ciphers in the given state space, we have
generated a mechanism to calculate the *English-Similarity* score of the
text obtained after decoding. This similarity is obtained by multiplying
the frequencies of the two character substrings of the given text.
However, the obtained were extremely small. Hence, for better numerical
precision, we work on the log scale. *War and Peace* has been used to
compile the bi-gram frequencies and enumerate a probability table.

## Metropolis Algorithm

Let $\(sim(cipher)\)$ be a function that returns a score from 0 and 1
indicating how similar the text that a cipher produces is to English.
With the $\(sim(cipher)\)$ function defined, the Metropolis algorithm
works like this. First, start with a randomly chosen cipher as the
initial state. Then repeat the following steps until the code is
cracked:

  - Choose a new (but closely related) cipher by swapping two letters in
    the current cipher at random. This is called the **proposal
    cipher**.

  - Compute the quantity
    $\(\frac{sim(proposal cipher)}{sim(current cipher)}\)$. If the
    proposal cipher produces text more similar to English than the
    current cipher, this ratio will always be greater than $1$. If the
    current cipher produces text more similar to English than the
    proposal cipher, this ratio will be between $0$ and \$1$.

  - If the ratio in the previous step is greater than $1$, set the
    current cipher to the proposed cipher. This is called **accepting
    the proposal**.

  - If the ratio is less than 1, accept the proposal with probability
    equal to $\(\frac{sim(proposal cipher)}{sim(current cipher)}\)$ and
    reject it (i.e., stay at the current cipher) with probability
    $\(1 - \frac{sim(proposal cipher)}{sim(current cipher)}\)$

In other words, if the proposal cipher produces text more similar to
English than the current cipher, we always accept it; and if the current
cipher produces text more similar to English than the proposal cipher,
we accept or reject it with probability given by the ratio of their
scores. The worse a proposal performs, the less likely it will be
accepted

## Modifications

There exist $\(26 \choose 2\)$ neighbours of any cipher. Instead of
proposing a cipher by randomly swapping two letters, we tend to make an
informed proposal by accepting a cipher with proportional to its
*English-similarity* score. I have also introduced a *correction factor*
in calculation of acceptance probability which ensures that the Markov
Chain converges at the correct point in space.

# Repository Structure

The repository contains the following files:

  - `code-breaking-using-metropolis.R` contains the main implentation of
    the algorithm which can be used by a user for any number of given
    iterations and any given piece of text

  - `metropFuncs.R` contains the implementation of a regular and an
    informed MCMC algorithm.

  - `cipherFuncs.R` contains all the user defined functions to deal with
    ciphers. For example: Generating a cipher, swapping two indicies,
    encoding and decoding texts using a given cipher.

  - `scoreFuncs.R` contains all the functions required to calculate the
    *English-similarity* score.

  - `data.Rdata` contains the probability table of all two character
    combinations generated from *War and Peace*.

  - Various `.cpp` files contain the implementation of various functions
    and sections of code in *C++* using the "Rcpp" package.

# Usage

To run the code, folloow the given steps:

1.  Clone the repository to your local machine.

2.  Navigate to the repository folder in your terminal.

3.  Simply run the `code-breaking-using-metropolis.R` script in R.

4.  The code can be modified by changing the user defined parameters
    $\(n\)$ (number of iterations) and $\(plainText\)$ (the text which will
    be encypted and the decrypted using the algorithm) to solve other
    code-breaking problems by using the MCMC algorithms in the
    `code-breaking-using-metropolis.R` file.

# Conclusion

This repository provides a powerful tool for breaking encryption using
MCMC methods. The results of this project demonstrate the effectiveness
of MCMC algorithms in cracking ciphers and analyzing encrypted text.
These algorithms have the potential to be used in real-world
applications for code breaking and encryption analysis. The use of Rcpp
for improved performance is also demonstrated. The code is
well-documented and easy to use, making it a great resource for anyone
interested in cryptography or code-breaking. I hope you find it useful\!
