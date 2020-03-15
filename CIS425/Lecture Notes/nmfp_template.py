##############################
###    Numerical Methods   ###
###          with          ###
### Functional Programming ###
###           in           ###
###         Python         ###
##############################

from math import log, sin

########################
### Infinite Streams ###
########################

# An abstract class for infinite streams, which represent an infinite sequence
# of elements
#
#     a0, a1, a2, a3, ...
class Stream:
    # The two primary observations (methods) of a Stream are `head`, which
    # returns the first element of the Stream, and `tail`, which returns the
    # remainder of the stream following the head.
    #
    #     head: Stream -> elem
    #     tail: Stream -> Stream
    #
    #    (a0, a1, a2, a3, a4, ...).head() = a0
    #    (a0, a1, a2, a3, a4, ...).tail() = a1, a2, a3, a4, ...
    #
    # To define a way of building a Stream, define a subclass of Stream with an
    # appropriate `head` and `tail` method following the above interface.

    # What follows are some helpful methods that make working with `Streams` in
    # Python a little easier. All these methods are defined in terms of the
    # above `head` and `tail` methods.
    
    # Iterate through the elements of the stream. This method enables the use of
    # Python's `for` loop syntax on streams, as in
    #
    #     for elem in stream: ...
    #
    # NOTE: Since a stream contains an infinite number of elements, iteration
    # will likewise go on forever, and must be stopped by some other means (like
    # a `return` in the middle of the loop).
    def __iter__(self):
        curr = self
        while True:
            yield curr.head()
            curr = curr.tail()

    # Return a (finite) list consisting of the first `n` elements of the stream.
    #
    #     (a0, a1, a2, ...).take(n) = [a0, a1, a2, ..., a_n-1]
    def take(self, n):
        taken = []
        for elem in self:
            if n <= 0:
                return taken
            taken += [elem]
            n -= 1

    # Drop the first `n` elements of a stream.  This corresponds to iterating
    # `tail` n times.
    #
    #     (a0, a1, a2, ...).drop(n) = a_n, a_n+1, a_n+2, ...
    def drop(self, n):
        curr = self
        for i in range(n):
            curr = curr.tail()
        return curr

    # Get the `i`th element of a stream, where the `0`th element is the head of
    # the stream, the `1`st element is the head of the tail, and so on.  This
    # informs Python on how to use the indexing syntax s[i] for streams.
    #
    #     (a0, a1, a2, ...)[i] = a_i
    def __getitem__(self, i):
        return self.drop(i).head()

    # A textual representation of a stream by showing the first 5 (by default)
    # elements. This is helpful for convincing Python to print out a useful bit
    # of information about a stream at the interactive prompt, instead of an
    # unhelpful memory address.
    def __repr__(self):
        strings = [ repr(elem) for elem in self.take(Stream.print_limit) ]
        return "\n".join(strings + ["..."])

    print_limit = 5

# Construct a stream by repeating a `step` function on some `state`, producing
#
#     Repeat(a, f) = a, f(a), f(f(a)), f(f(f(a))), ...
#
# If no `step` function is given, then by default the given `state` will just be
# repeated forever.
#
#     Repeat(a) = a, a, a, a, ...
class Repeat(Stream):
    def __init__(self, a, f=lambda x: x):
        self.state = a
        self.step = f

    def head(self):
        return self.state

    def tail(self):
        return Repeat(self.step(self.state), self.step)

# Two example repeat streams:
#
#     zeroes = 0, 0, 0, 0, 0, ...
#     nats = 1, 2, 3, 4, ...
zeroes = None # FILL IN
nats = None # FILL IN

# Transform a `stream` by applying a given `trans` function to each element.
#
#     Map((a0, a1, a2, a3, ...), f) = f(a0), f(a1), f(a2), f(a3), ...
class Map(Stream):
    def __init__(self, stream, f):
        self.stream = stream
        self.trans = f

    def head(self):
        pass # FILL IN

    def tail(self):
        pass # FILL IN

# An example of Map
#
#     squares = 0, 1, 4, 9, 16, ...
squares = Map(nats, lambda x: x*x)

# Combine two streams `left` and `right` pointwise with a binary function
# `combine`.
#
#     Zip((a0, a1, a2, a3, ...), (b0, b1, b2, b3, ...), f)
#     =
#     f(a0, b0), f(a1, b1), f(a2, b2), f(a3, b3), ...
#
# If no `combine` function is given, then by default the zipped stream will
# contain pairs of elements from the two streams, combined pointwise.
#
#     Zip((a0, a1, a2, ...), (b0, b1, b2, ...))
#     =
#     (a0, b0), (a1, b1), (a2, b2), (a3, b3), ...
class Zip(Stream):
    def __init__(self, l, r, f=lambda x, y: (x,y)):
        self.left = l
        self.right = r
        self.combine = f

    def head(self):
        pass # FILL IN

    def tail(self):
        pass # FILL IN


###################
### Square Root ###
###################

# Compute a more accurate square root of `n` given an approximation `a`.
def next_root(n, a):
    pass # FILL IN

# A sequence of increasingly accurate approximations of the square root of `n`,
# starting with the initial approximation `a0`.
def sqrts(a0, n):
    pass # FILL IN

# Group up consecutive elements of a stream.
#
#     by_twos((a0, a1, a2, a3, a4, ...))
#     =
#     (a0, a1), (a1, a2), (a2, a3), (a3, a4), ...
def by_twos(stream):
    pass # FILL IN

# Given that `stream` is a converging sequence of numbers,
#
#     stream = a0, a1, a2, a3, a4, ...
#
# find the first consecutive pair of elements `a` and `b` in `stream` whose
# difference `a-b` is within `eps`, and return the second one.
def within(eps, stream):
    pass # FILL IN

# Approximate the square root of `n` within an accuracy of `eps`, using `a0` as
# an initial approximation.
def sqrt(a0, eps, n):
    pass # FILL IN

# Given that `stream` is a converging sequence of numbers,
#
#     stream = a0, a1, a2, a3, a4, ...
#
# find the first consecutive pair of elements `a` and `b` in `stream` whose
# ratio `a/b` is within `eps` of 1, and return the second one.
def relative(eps, stream):
    pass # FILL IN

# Similar to `sqrt`, approximate the square root of `n` within an accuracy of
# `eps`, using a relative notion of difference instead.
def rsqrt(a0, eps, n):
    pass # FILL IN


#################################
### Numerical Differentiation ###
#################################

# A Stream starting with `h` where every element is half the previous one.
#
#     halves(h) = h, h/2, h/4, h/16, h/32, ...
def halves(h):
    pass # FILL IN

# For a function `f`, compute an approximation of the derivative f'(x) using `h`
# as a distance.
def easy_diff(h, x, f):
    pass # FILL IN

# For a function `f`, A sequence of increasingly accurate approximations of the
# derivative f'(x), beginning with the initial distance `h0`.
def differentiate(h0, x, f):
    pass # FILL IN

# Some example differentiation streams
const = differentiate(1.0, 10, lambda x: 5)
lin   = differentiate(1.0, 10, lambda x: 12*x)
quad  = differentiate(1.0, 10, lambda x: x**2)
expn  = differentiate(1.0, 10, lambda x: 2**x)
dexpn = differentiate(1.0,  5, lambda x: 2**(2**x))
trig  = differentiate(1.0, 10, lambda x: 2*sin(x))

# Given a stream of halving approximations, eliminate the error of order `n`,
# producing a faster-converging stream of approximations.
def elim_error(n, stream):
    pass # FILL IN

# Estimate the order of error in a stream of halving approximations.
#
# NOTE: It is possible to have a stream that converges so fast that the
# difference between consecutive approximations is (effectively) 0. In this
# case, the order cannot be estimated.
def order(stream):
    pass # FILL IN

# Improve the convergence rate of a stream of halving approximations by
# eliminating the error.
#
# NOTE: If the order could not be estimated, then no error can be eliminated, so
# return the same stream.
def improve(stream):
    pass # FILL IN

# Drastically improve the convergence rate of a stream of halving approximations
# by iterating the above `improve` function to eliminate more error with each
# successive approximation.
def super_improve(stream):
    return Map(Repeat(stream, improve), lambda s: s.tail().head())

# Approximate the derivative of `f` at `x` within an accuracy of `eps`,
# beginning at a distance of `h0`.
def diff(h0, eps, x, f):
    pass # FILL IN
