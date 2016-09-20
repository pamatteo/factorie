package cc.factorie.tutorial

import cc.factorie.Factorie.{CategoricalDomain, CategoricalVariable}
import cc.factorie.la.{DenseTensor1, DenseTensor3}
import cc.factorie.model.{Factor3, Parameters, Template3, TemplateModel}
import cc.factorie.variable.CategoricalValue
import org.json4s

import scala.collection.mutable.ArrayBuffer
import org.json4s._
import org.json4s.jackson.JsonMethods._

import scala.collection.mutable
import scala.io.Source

/**
  * Created by matteo on 09/08/16.
  */
object CodeDeobfuscationDemo {

  object LabelDomain extends CategoricalDomain[String]
  class Label(str:String) extends CategoricalVariable(str) { def domain = LabelDomain}
  object ArcTypeDomain extends CategoricalDomain[String]
  class ArcType(str:String) extends  CategoricalVariable(str) { def domain = ArcTypeDomain }

  // variable class
  class Assignment(val label:Label, val infer:Boolean) {
    def l:Label = label
    def inf:Boolean = infer
  }

  // arc class
  class Arc(val nodeA:Label, val nodeB:Label, val arcType:ArcType) {
    def a:Label = nodeA
    def b:Label = nodeB
    def t:ArcType = arcType
    override def hashCode():Int = LabelDomain.index(nodeA.categoryValue) * 1037 + LabelDomain.index(nodeB.categoryValue) * 1037 + ArcTypeDomain.index(arcType.categoryValue)
  }



  def main(args: Array[String]): Unit = {
    if (args.length != 2) throw new Error("Usage: CodeDeobfuscationDemo trainfile testfile")
    var arcs = new mutable.HashMap[Arc, Int]()
    for(line <-  Source.fromFile(args(0)).getLines()) {
      val trainingSample = parse(line)
      val trainingAssignments = trainingSample \ "assign"
      val a = parseAssignments(trainingAssignments)
      val trainingQuery = trainingSample \ "query"
      parseQuery(trainingQuery, a, arcs)

      // add the training sample to the model (i.e. add the pairwise features to the features set)
    }
    println(LabelDomain.size)
    println(ArcTypeDomain.size)

    val featuresWeights = new DenseTensor1(arcs.size)
    val i:Int = 0
    for(arc <- arcs) {
      arcs.put(arc._1, i)
      featuresWeights(i) += 1
    }
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
      val assignmentIdentifierJString = assignmentIdentifier.asInstanceOf[JString]
      val assignmentLabelJString = assignmentLabel.asInstanceOf[JString]
      val varId = assignmentIdentifierJString.s.toInt
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

  def parseQuery(query:JsonAST.JValue, assignments:ArrayBuffer[Assignment], arcs:mutable.HashMap[Arc, Int]) = {

    val queryJArray = query.asInstanceOf[JArray]
    val queryArray = queryJArray.arr
    for (it <- queryArray) {
      val varIdAJValue = it \ "a"
      val varIdBJValue = it \ "b"
      val arcTypeJValue = it \ "f2"
      if (varIdAJValue != JNothing && varIdBJValue != JNothing && arcTypeJValue != JNothing) {
        val varIdAJString = varIdAJValue.asInstanceOf[JString]
        val varIdBJString = varIdBJValue.asInstanceOf[JString]
        val arcTypeJString = arcTypeJValue.asInstanceOf[JString]
        val varIdA = varIdAJString.s.toInt
        val varIdB = varIdBJString.s.toInt
        val arcType = arcTypeJString.s
        val arcTypeVar = new ArcType(arcType)
        try{
          val arc: Arc = new Arc(assignments(varIdA).label, assignments(varIdB).label, arcTypeVar)
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
