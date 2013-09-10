/*&

# Overview

At its heart FACTORIE is a toolkit for graphical models.  All its
specific pre-built models, all its data representation, inference and
learning methods are built on a small common set of primitives based
on graphical models.

## Introduction to Graphical Models
  
Graphical models are a formalism in which a graph denotes the
conditional dependence structure between random variables.  The
formalism is the marriage between probability theory and graph theory,
providing an elegant framework that combines uncertainty (or
probabilities) and logical structure (or independence constraints)
such that complex joint probability distributions over multiple
variables that would have otherwise been intractable to represent or
manipulate can instead be represented compactly and often manipulated
efficiently.  Since graphical models can comprehensibly express so
many different probabilistic models, they have become a lingua-frana
for statistics, machine learning, and data mining.

In graphical models variables are indicated by the nodes a graph
(drawn as circles), and dependencies among variables are indicated by
edges (drawn either as directed, with arrows, or undirected, without
arrows).

There are two main types of graphical models.

*Directed graphical models* (or *Bayesian networks*) represent a joint
distribution over random variables by a product of normalized
conditional probability distributions (one for each variable,
conditioned on all the other variables connected by an incoming
directed edge).  They are convenient generative models in which
variable values can be generated by an ordered iteration of the graph
nodes from "parent" variables to "child" variables.

*Undirected graphical models* (or *Markov random fields*) represent a
joint distribution over random variables by a product of unnormalized
non-negative values (one value for each clique in the graph).  They
are convenient models for data in which it is unintuitive to impose an
ordering on the variables' generative process.  They can represent
different patterns independence constraints than directed models can,
and vice versa---neither one is strictly more expressive than the other.

*Factor graphs* are a generalization of both directed and undirected
graphical models, capable of representing both.

When drawn, factor graphs depict variables as circular nodes in the
graph (just as in directed and undirected graphical models), however
rather than having edges that connect variables directly to each
other, edges instead connect variables to factors (which are drawn as
black squares).  In other words, variables are connected to other
variables only through factors.  The variables connected to a factor
are sometimes called the "neighbors" of the factor.

Factor graphs represent a joint distribution over random variables by
a product of (normalized or unnormalized) non-negative values---one
value for each factor in the graph.  The factors can be understood as
"compatibility functions" that take as input the values of the
variables to which they are connected by edges, and outputing a
"compatibility score" such that higher scores indicate the combination
of values is more likely, and lower scores indicate the combination of
values is less likely.  A score of 0 indicates the combination is impossible.

Directed graphical models can be represented with one factor for each
(child) variable, where the factor is also connected to the other
(parent) variables on whose values we must condition when generating
the child's value; the factor's scores are normalized probabilities.
Undirected graphical models can be represented with one factor per
clique, or some other arbitary subset of clique members; the factors
scores are unnormalized non-negative values, and thus in order to
obtain a normalized joint probability distribution over all the
variables in the model, we must normalize the product of factor scores
by the proper normalization value (called the "partition function").

In practice, most implementations use the log of the score values so
that they can be summed rather than multiplied, and a larger dynamic
range of scores can be represented in hardware.  Naturally these
log-scores can then vary from negative infinity (indicating an
impossible combination of values in the neighbors) to positive
infinity (indicating an obligatory combination of values in the
neighbors).  Since the use of log-scores is so common (and used in our
implementation) we will now simply use the term "score" to refer to
log-scores.


## Factor graphs in FACTORIE

The FACTORIE library defines Scala classes for representing the
elements of a factor graph.  Usually the names of our Scala classes
and methods are simply the standard names for the concept in English
machine learning or statistics vocabulary.

In this section we further describe factor graphs while introducing
FACTORIE class and method names indicated `in mono-space font`.  Class
(and trait) names are capitalized.  Method names begin with a
lowercase letter.

Here we provide a high-level introduction to FACTORIE's identifiers.
Detailed explanations with code examples are given later in the
following chapters.


### FACTORIE modular design philosophy

FACTORIE is explicitly designed to support independent,
non-intertwined definitions of

1. data representations with variables,
2. models (value preferences, distributions) with factors, 
3. inference methods, and
4. parameter estimation.

This separation provides FACTORIE users with great flexibilty to
mix-and-match different choices in each of these four dimensions.  For
example, the data representation for some task may be written just
once, while different models are explored for this task---there is no need
to re-write the data representation for each experiment with a
different model.  Similarly, inference methods may be selected
separately from the model definition.


### Representing data with variables and values

The data constituting any machine learning task are represented
with variables.  A variable can be understood as a container for a
value.  A variable may also have certain relations to other variables
(for example, by being part of a sequence of variables corresponding
to a sequence of part-of-speech tags).  FACTORIE has many classes for
representing different types of variables.  The root of this class
hierarchy is the abstract trait `Var`.

An `Assignment` stores a mapping from variables to values, and can be
treated as a function.  The value of a variable `v1` in assignment
`a2` can be obtained by `a2(v1)`.  

Note that an assignment stores a single value for a variable, not a
distribution over values.  (Of course FACTORIE has extensive
facilities for representing distributions over values, but these are
not intrinsic to a variable definition or assignment---rather they are
the purvue of inference, because different inference methods may
choose to use different distribution representations for the same
variable.)

There are multiple concrete sub-classes of `Assignment`.  A
`MutableAssignment` can be modified.  A `TargetAssignment` returns the
desired gold-standard (e.g. human-labeler-provided) value for a
variable when such a labeled value is available.  An `Assignment1` is
a compact efficient representation for the value assignment of a
single variable (similarly `Assignment2..4` also exist).  A
`HashMapAssignment` is an arbitrarily-sized mutable assignment with
storage based on a HashMap.

Because we want to efficiently support the common case in which many
variables have relatively static value or unique assignments (because
they are observed, or because they are being sampled in a single MCMC
inference process) FACTORIE also has the `GlobalAssignment` object.
There may exist multiple instances of the other assignment classes,
each with its own values; however, there is only one GlobalAssignment.
The GlobalAssignment values are not stored in the GlobalAssignment,
but rather in the corresponding variable instances themselves.

All variables have a `value` method that returns the variable's
currently assigned global value.  In other words
`GlobalAssignment(v1)` is equivalent to `v1.value`.  Henceforth when
we refer to "a variable's value" we mean "a variables
globally-assigned value".

All variables also have methods for testing the equality (`====`) or
inequality (`!===`) of their global values.  Thus `v1 === v2` will
return true if variables `v1` and `v2` have the same values, and is a
simple shorthand for `v1.value == v2.value`.

If some non-global assignment `a2` does not contain a value for `v1`
then looking up the value with `a2(v1)` may have different behavior in
different assignment subclasses.  For example, a `HashMapAssignment`
will throw an error, but an `Assignment1` (or `Assignment2`..`4`) will
simply return the variable's globally-assigned value.


### Many types of variables

FACTORIE has different subclasses of `Var` for holding values of
different types.  There are a large number of traits and classes for
variables.  The following naming convensions help make their
interpretation easier.  All abstract traits and classes end in `Var`,
while concrete classes end in `Variable`.  Almost all classes ending
in `Variable` have an abstract `Var` counterpart that does not
necessarily specify the mechanism by which the variable stores its
value in memory.  

Some variable are mutable (inheriting from the trait `MutableVar`).
Almost all classes ending in `Variable` are mutable.  Mutable
variables can be set with the `:=` method.  For example, if `v1` has
integer values, we can set the value of `v1` to 3 by `v1 := 3`.

The type of the value is named immediately before the `Var` or
`Variable` suffix (for example, `IntegerVariable`).  Further modifiers
may appear as prefixes.

The following is a selection of FACTORIE's most widely-used variable classes.

`IntegerVariable`
: has value with Scala type Int.
`DoubleVariable`
: has value with Scala type Double.
`TensorVariable`
: has value of type Tensor, which is defined in the FACTORIE linear algebra package.  This variable class makes no restritions on the dimensionality of the tensor, or the lengths of its dimensions.
`VectorVariable`
: has value of type Tensor1, which is a one-dimensional Tensor (also typically known as a "vector").  In addition each `VectorVariable` is associated with a `DiscreteDomain` (further described below) whose size matches the length of the variable's vector value.
`DiscreteVariable extends VectorVar`
: has a value among N possible values, each of type `DiscreteValue`, and each associated with an integer 0 through N-1.  This `DiscreteValue` inherits from `Tensor1` and can also be interpreted as a "one-hot" vector with value 1.0 in one position and 0.0 everywhere else.  Given a `DiscreteValue dv1` its integer value can be obtained by `dv1.intValue`.  The length of the vector (in other words, the value of N) can be obtained by `dv1.length`.
`CategoricalVariable[A] extends DiscreteVar`
: has value among N possible values, each of type `CategoricalValue[A]` (which inherits from `DiscreteValue`), each associated with an integer 0 through N-1, and also associated with a "category" (often of type String).  These variables are often used for representing class labels and words, when a mapping from String category names to integer indices is desirable for efficiency (such as indexing into an array of parameters).  Given a `CategoricalValue[String] cv1` its integer value can be obtained by `cv1.intValue` and its categorical (String) value can be obtained by `cv1.categoryValue`.  Its value may be set with an integer: `cv1 := 2` or set by category string: `cv1 := "sports"`.  (The mapping between Strings and integers is stored in a `CategoricalDomain`, which is described below.)
`BooleanVariable extends CategoricalVar[Boolean]`
: has value among two possible values, each of type `BooleanValue` (which inherits from CategoricalValue[Boolean]), one of which is associated with integer 0 and boolean value false, the other of which is associated with integer value 1 and boolean value true.  Given a `BooleanValue bv1` its integer value can be obtained by `bv1.intValue` and its boolean value can be obtained by `bv1.booleanValue`.
`MassesVariable extends TensorVar`
: has value `Masses`, which are Tensors constrained to contain non-negative values.  `Masses` are useful as the parameters of Dirichlet distributions.
`ProportionsVariable extends MassesVar`
: has value `Proportions`, which are `Masses` constrained to sum to 1.0.  `Proportions` are useful as the parameters of discrete or multinomial distributions.
`RealVariable extends VectorVar`
: has a single real scalar value, stored in an object of type `RealValue` (which inherits from Tensor1).  This variable is similar to `DoubleValue` in that it stores a scalar value, however since its value type inherits from Tensor1

All of the above variable classes have constructors in which the
initial value of the variable may be set.  For example, `new
IntegerVariable(3)` will create a new variable whose current value is 3.

Some of the above variable types have specializations for the case in
which human-labeled gold-standard target values are known.  These
specializations inherit from the `LabeledVar` trait which provides a
method `targetValue` that returns this gold-standard target value.
The target value is stored separately and may be different from the
variable's current global assignment value.  For example, if `i1` is a
`LabeledIntegerVariable` we can determine if the variable is currently
assigned to its gold-standard target value by evaluating the boolean
expression `i1.value == i1.targetValue`.  (The method `valueIsTarget`
is a convenient short-hand for this test.)  In almost all cases the
target value is initialized to be the value provided in the variable
constructor.

Common `LabeledVar` sub-classes include:

* LabeledDiscreteVariable
* LabeledCategoricalVariable[A]
* LabeledBooleanVariable
* LabeledStringVariable
* LabeledIntegerValue
* LabeledDoubleValue
* LabeledRealValue

All the above variable types are common in existing graphical models.
FACTORIE also has random variables for representing less traditional
values.  Although these may seem like peculiar value types for a
graphical model, they nontheless can be scored by a factor, and are
often useful in FACTORIE programs.

`StringVariable`
: has value with Scala type String.
`SeqVariable[A]`
: has value of type `Seq[A]`, that is, a sequence of objects of type `A`.
`ChainVariable[A] extends SeqVariable[A]`
: has value of type `Seq[A]`, but the elements `A` must inherit from `ChainLink` which have `next` and `prev` methods.
`SpanVariable[A] extends SeqVar[A]`
: has value of type `Seq[A]`, and in addition is a subsequence of a `ChainVar[A]`.
`SetVariable[A]`
: has value of type `Set[A]`, that is an unordered set of objects of type `A`.
`RefVariable[A]`
: has value of type A.  In other words it is a variable whose value is a pointer to a Scala object.
`EdgeVariable[A,B]`
: has value of type `Tuple[A,B]`, that is a pair of objects: a "source" of type `A` and a "destination" of type `B`.
`ArrowVariable[A,B] extends EdgeVar[A,B]
: like `EdgeVariable` has value of type `Tuple[A,B]`, but only the the "destination" is mutable, while the "source" is immutable.



### Variable domains

Some (but not all) FACTORIE variables have a corresponding domain
object that contains information about the set of values the variable
may take on.  Such variables inherit from `VarWithDomain` which
provides a `domain` method that returns the variable's domain.

For example, `DiscreteVariable` subclasses have a corresponding
`DiscreteDomain` whose main role is to provide its `size`---the number
of possible values the variable can take on, i.e. from 0 to N-1.

`CategoricalVariable[A]` subclasses have a corresponding
`CategoricalDomain[A]` (inheriting from `DiscreteDomain`) which
provides not only its `size` but also a one-to-one mapping between the
categories and the integers 0 to N-1.  For example, if `cd1` is a
`CategoricalDomain[String]` then `cd1.category(3)` returns the String
corresponding at index in the domain.  `cd1.index("apple")` returns
the integer index corresponding to the cateogry value `"apple"`.  If
`"apple"` is not yet in the domain, it will be added, assigned the
next integer value, and the size of the domain will be increased by
one---assuming that `cd1` has not previously been frozen.



### Expressing preferences with factors and models

A collection of variables and their values represents a possible state
of the world.  To express a degree of preference for one possible
world over another---or a probability distribution over possible
worlds---we need a mechanism for scoring the different worlds, such as
a collection of factors in a factor graph.

In FACTORIE, factors are represented by instances of the trait
`Factor`.  All `Factor`s have certain basic methods.  The
`numVariables` method returns the number of variables neighboring the
factor.  The list of neighboring variables themselves is returned by
the method `variables`.  Calling `currentScore` calculates the factor's
score for the neighboring variables' values in the current global
assignment.  You can obtain the score under alternative assignments by
calling `assignmentScore(a1)`, where `a1` is some `Assignment`.

Its `variables` method returns the list of variables
neighboring the factor.  Its `

### Searching for solutions with inference


### Estimating parameters 



 */
