/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */



package cc.factorie.optimize
import cc.factorie._
import cc.factorie.la._

class BackTrackLineOptimizer(val gradient:Tensor, val line:Tensor, val initialStepSize:Double = 1.0) extends GradientOptimizer with FastLogging {
  private var _isConverged = false
  def isConverged = _isConverged
  def stepSize = alam
  
  var gradientNormMax = 100.0
  var relTolx = 1e-7
  var absTolx = 1e-4
  var ALF = 1e-4
  val EPS = 3.0e-12
  val stpmax = 100.0
  var origWeights: Tensor = null //weights.copy

  var oldValue = Double.NaN
  var origValue = Double.NaN
  var slope = Double.NaN
  var alamin = Double.NaN
  var alam = initialStepSize
  var oldAlam = 0.0;
  var tmplam = 0.0;
  var alam2 = 0.0

  def reset(): Unit = {
    _isConverged = false
    origWeights = null
    oldValue = Double.NaN
    origValue = Double.NaN
    slope = Double.NaN
    alamin = Double.NaN
    alam = initialStepSize
    oldAlam = 0.0;
    tmplam = 0.0;
    alam2 = 0.0
  }
    def step(weights:Tensor, gradient:Tensor, value:Double, margin:Double): Unit = {
    logger.warn("BackTrackLineOptimizer step value="+value)
    // If first time in, do various initializations
    if (slope.isNaN) {
      origWeights = weights.copy
      // Set the slope

      val sum = line.twoNorm
      if (sum > initialStepSize) line *= (initialStepSize / sum) // If gradient is too steep, bring it down to gradientNormMax
      slope = gradient dot line     //todo, it's redundant, and expensive to do both gradient.twoNorm and gradient dot line when they're the same
      logger.warn("BackTrackLineOptimizer slope="+slope)
      if (slope <= 0.0) throw new Error("Slope=" + slope + " is negative or zero.")

      // Set alamin
      // Find maximum lambda; converge when (delta x) / x < REL_TOLX for all coordinates.
      // Largest step size that triggers this threshold is saved in alamin
      var test = 0.0;
      var temp = 0.0
      if (!weights.dimensionsMatch(line)) throw new Error("line and weights do not have same dimensionality.")
      // Do the check above because toArray will yield non-matching results if called on a WeightsTensor that has missing keys.
      val lineA = line.toArray
      val weightsA = weights.toArray
      var i = 0; val len = lineA.length
      while (i < len) {
        temp = math.abs(lineA(i)) / math.max(math.abs(weightsA(i)), 1.0)
        if (temp > test) test = temp
        i += 1
      }
      alamin = relTolx / test

      // Set oldValue and origValue
      oldValue = value
      origValue = value
      logger.warn("BackTrackLineOptimizer line factor="+(alam-oldAlam))
      if(!_isConverged) weights.+=(line, alam - oldAlam)

    }else{

      // Check for convergence by sufficient function increase (Wolf condition)
      if (value >= origValue + ALF * alam * slope) {
        if (value < origValue) throw new Error("value did not increase: original=" + origValue + " new=" + value)
        _isConverged = true
      } else if (value.isInfinity || oldValue.isInfinity) {
        // value is infinite; we have jumped into unstable territory.  Scale down jump
        tmplam =.2 * alam
        if (alam < alamin) {
          logger.warn("BackTrackLineOptimizer EXITING BACKTRACK: Jump too small. Exiting and using xold.");
          _isConverged = true // Exiting backtrack: jump to small; using previous parameters
        }
      }else if (alam == oldAlam){
          _isConverged = true
      } else {
        // backtrack
        if (alam == 1.0) tmplam = { -slope / (2.0 * (value - origValue - slope))} // first time through
        else {
          val rhs1 = value - origValue - alam * slope
          val rhs2 = oldValue - origValue - alam2 * slope
          assert((alam - alam2) != 0, "FAILURE: dividing by alam-alam2.  alam=" + alam)
          val a = (rhs1 / (alam * alam) - rhs2 / (alam2 * alam2)) / (alam - alam2)
          val b = (-alam2 * rhs1 / (alam * alam) + alam * rhs2 / (alam2 * alam2)) / (alam - alam2);
          if (a == 0.0) tmplam = -slope / (2.0 * b)
          else {
            val disc = b * b - 3.0 * a * slope
            if (disc < 0.0) tmplam =.5 * alam
            else if (b <= 0.0) tmplam = (-b + math.sqrt(disc)) / (3.0 * a)
            else tmplam = -slope / (b + math.sqrt(disc))
            if (tmplam > .5 * alam) tmplam =.5 * alam
          }
        }
      }
      alam2 = alam
      oldValue = value
      oldAlam = alam
      alam = math.max(tmplam, 0.1 * alam)
      if(alam == oldAlam) _isConverged = true

      if(!_isConverged){
        logger.warn("BackTrackLineOptimizer line factor="+(alam-oldAlam))
        weights.+=(line, alam - oldAlam)
      }

    }

    // Check for convergence
    if (alam < alamin || !origWeights.different(weights, absTolx)) {
      weights := origWeights
      logger.warn("EXITING BACKTRACK: Jump too small (alamin=" + alamin + "). Exiting and using xold.");
      _isConverged = true // Convergence on change in params
    }
  }

}
