package cc.factorie.tutorial

import java.io.{BufferedReader, FileReader}

import cc.factorie.Factorie.{CategoricalDomain, CategoricalVariable}
import cc.factorie.infer.{MaximizeByBP, MaximizeByBPLoopy, Summary}
import cc.factorie.la.{DenseTensor1, DenseTensor3, Tensor}
import cc.factorie.model.{Factor3, ItemizedModel, Model, Parameters, Template3, TemplateModel}
import cc.factorie.variable._

import scala.collection.mutable.ArrayBuffer
import org.json4s._
import org.json4s.jackson.JsonMethods._

import scala.collection.mutable
/**
  * Created by matteo on 09/08/16.
  */
object CodeDeobfuscationDemo {

  object LabelDomain extends CategoricalDomain[String]
  class Label(str:String) extends CategoricalVariable(str) { def domain = LabelDomain}
  object ArcTypeDomain extends CategoricalDomain[String]
  class ArcType(str:String) extends  CategoricalVariable(str) { def domain = ArcTypeDomain }
  // variable class
  class Variable(varId:Int) extends MutableIntegerVar {
    override def value: Int = varId

    /** Assign a new value to this variable */
    override def set(newValue: Int)(implicit d: DiffList): Unit = { }
  }

  // assignment class
  class Assignment(val label:Label, val infer:Boolean) {
    def l:Label = label
    def inf:Boolean = infer
  }

  // pairwise feature class
  class PairwiseFeature(val nodeA:Label, val nodeB:Label, val arcType:ArcType) extends Factor3(nodeA, nodeB, arcType) {
    def a:Label = nodeA
    def b:Label = nodeB
    def t:ArcType = arcType
    override def hashCode():Int = LabelDomain.index(nodeA.categoryValue) * 1037 + LabelDomain.index(nodeB.categoryValue) * 1037 + ArcTypeDomain.index(arcType.categoryValue)

    override def score(v1: CategoricalValue[String], v2: CategoricalValue[String], v3: CategoricalValue[String]): Double = {
      if (v1 == nodeA && v2 == nodeB && v3 == arcType) {
        return 1
      } else {
        return 0
      }
    }

    override def valuesScore(tensor:Tensor): Double = {
      return 0
    }

    override def statistics(v1: CategoricalValue[String], v2: CategoricalValue[String], v3: CategoricalValue[String]): PairwiseFeature.this.type = return this

    override type StatisticsType = this.type
  }



  class Program extends ItemizedModel with Parameters {

  }

  def main(args: Array[String]): Unit = {
    if (args.length != 2) throw new Error("Usage: CodeDeobfuscationDemo trainfile testfile")
    var arcs = new mutable.HashMap[PairwiseFeature, Int]()
    val fileReader = new BufferedReader(new FileReader(args(0)))
    var threads = new ArrayBuffer[Thread]()

//    for (i <- 1 to 8 ) {
//      val reader = new Thread(new Runnable {
//        def run() {
//          var line = fileReader.readLine()
//          //var numLine = 0
//          while (line != null) {
//            val trainingSample = parse (line)
//            val trainingAssignments = trainingSample \ "assign"
//            val a = parseAssignments (trainingAssignments)
//            val trainingQuery = trainingSample \ "query"
//            parseQuery (trainingQuery, a, arcs)
//            line = fileReader.readLine ()
//            //numLine += 1
//            //println(numLine)
//          }
//        }
//      })
//      threads.append(reader)
//      reader.start()
//    }
    var line = fileReader.readLine()
    //var numLine = 0
    while (line != null) {
      //val startParsingTime = System.currentTimeMillis()
      val trainingSample = parse (line)
      //println("Parsing finished in " + (System.currentTimeMillis() - startParsingTime) + " ms")
      //val startTime = System.currentTimeMillis()
      val trainingAssignments = trainingSample \ "assign"
      val a = parseAssignments (trainingAssignments)
      val trainingQuery = trainingSample \ "query"
      parseQuery (trainingQuery, a, arcs)
      //println("Adding query and assignment finished in " + (System.currentTimeMillis() - startTime) + " ms")
      line = fileReader.readLine()
      //numLine += 1
      //println(numLine)
    }

//    for (i <- 0 to 7) {
//      threads(i).join()
//    }
    println(LabelDomain.size)
    println(ArcTypeDomain.size)

    val featuresWeights = new DenseTensor1(arcs.size)
    val i:Int = 0
    for(arc <- arcs) {
      arcs.put(arc._1, i)
      featuresWeights(i) += 1
    }

    val testFileReader = new BufferedReader(new FileReader(args(0)))
    var testProgramLine = testFileReader.readLine()
    while (testProgramLine != null) {
      val testSample = parse(testProgramLine)
      val testAssignments = testSample \ "assign"
      val a = parseAssignments(testAssignments)
      var testArcs = new mutable.HashMap[PairwiseFeature, Int]()
      val testQuery = testSample \ "query"
      parseQuery(testQuery, a, testArcs)
      infer(a, testArcs, featuresWeights)
      testProgramLine = testFileReader.readLine()
    }
  }

  def infer(assignments:ArrayBuffer[Assignment], arcs:mutable.HashMap[PairwiseFeature, Int], weights:DenseTensor1): Unit = {
    var model = new Program()
    model.Weights(weights)
    var infVars = new ArrayBuffer[Label]()
    for (a <- assignments) {
      if (a.infer && !infVars.contains(a.label)) {
        infVars.append(a.label)
      }
    }

    for (arc <- arcs) {
      model += arc._1
    }
    println("Starting Inference")
    val inferenceStartTime = System.currentTimeMillis()
    MaximizeByBPLoopy.infer(infVars, model)
    println("Inference took " + (System.currentTimeMillis() - inferenceStartTime) + " ms")
  }
  def parseAssignments(assignments:JsonAST.JValue):ArrayBuffer[Assignment] = {
    val assignmentsArray = assignments.asInstanceOf[JArray]
    val assignmentsArrayValue = assignmentsArray.arr
    val a = ArrayBuffer.fill[Assignment](assignmentsArrayValue.length)(new Assignment(null, true))
    for (it <- assignmentsArrayValue) {
      val assignmentIdentifier = it \ "v"
      var assignmentInfer:Boolean = false
      var assignmentLabel:JValue = null
      if (it \ "inf" != JNothing) {
        assignmentInfer = true
        assignmentLabel = it \ "inf"
      } else {
        assignmentInfer = false
        assignmentLabel = it \ "giv"
      }
      val assignmentIdentifierJString = assignmentIdentifier.asInstanceOf[JInt]
      val assignmentLabelJString = assignmentLabel.asInstanceOf[JString]
      val varId = assignmentIdentifierJString.num.intValue()
      val label = assignmentLabelJString.s
      val labelVar = new Label(label)
      val assignment: Assignment = new Assignment(labelVar, assignmentInfer)
      try {
        a(varId) = assignment
      } catch  {
        case iobe:IndexOutOfBoundsException =>
      }
    }
    return a
  }

  def parseQuery(query:JsonAST.JValue, assignments:ArrayBuffer[Assignment], arcs:mutable.HashMap[PairwiseFeature, Int]) = {

    val queryJArray = query.asInstanceOf[JArray]
    val queryArray = queryJArray.arr
    for (it <- queryArray) {
      val varIdAJValue = it \ "a"
      val varIdBJValue = it \ "b"
      val arcTypeJValue = it \ "f2"
      if (varIdAJValue != JNothing && varIdBJValue != JNothing && arcTypeJValue != JNothing) {
        val varIdAJString = varIdAJValue.asInstanceOf[JInt]
        val varIdBJString = varIdBJValue.asInstanceOf[JInt]
        val arcTypeJString = arcTypeJValue.asInstanceOf[JString]
        val varIdA = varIdAJString.num.intValue()
        val varIdB = varIdBJString.num.intValue()
        val arcType = arcTypeJString.s
        val arcTypeVar = new ArcType(arcType)
        try{
          val arc: PairwiseFeature = new PairwiseFeature(assignments(varIdA).label, assignments(varIdB).label, arcTypeVar)
          if (!arcs.contains(arc)) {
            arcs.put(arc, 0)
          }
        } catch {
          case iobe:IndexOutOfBoundsException =>
        }
      }
    }

  }
}
