package cc.factorie.app.nlp.coref

import cc.factorie.app.nlp.wordnet.WordNet
import scala.collection.mutable
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.pos.PennPosTag
import collection.mutable.ArrayBuffer
import cc.factorie.app.nlp.phrase.{NounPhraseEntityTypeLabeler,OntonotesPhraseEntityType}
import cc.factorie.app.nlp.phrase.ParseAndNerBasedPhraseFinder

/** Used for training with predicted mentions.
 * If the predicted mention is equal to or within some specified alignment width in options we add the true spans entity label if any
 * Otherwise we add the mention to the ground truth coref as a ground truth singleton.*/
object MentionAlignment {
  def makeLabeledData(documents:Seq[Document], outfile: String, useEntityTypes: Boolean, options: CorefOptions, map: DocumentAnnotatorMap): (Seq[Document]) = {

    //remove the gold POS annotation
    if(!options.useGoldBoundaries) documents.foreach( d => d.tokens.foreach(t => t.attr.remove[PennPosTag]))

    val shifts =  ArrayBuffer[Int]()
    shifts += 0
    for(i <- 1 to options.mentionAlignmentShiftWidth){
      shifts +=  i
      shifts += -1*i
    }

    //align gold mentions to detected mentions in order to get labels for detected mentions
    val alignmentInfo =  documents.par.map(d => alignMentions(d,WordNet,useEntityTypes, options, shifts))

    //do some analysis of the accuracy of this alignment
    val numCorrect = alignmentInfo.map(_.numcorrect).sum.toDouble
    val numGT = alignmentInfo.map(_.numGT).sum.toDouble
    val numDetected = alignmentInfo.map(_.numDetected).sum.toDouble
    println("precision = " + numCorrect/numDetected)
    println("recall = " + numCorrect/numGT)

    documents
  }

  def findMentions(doc: Document,options:CorefOptions,annotatorMap: DocumentAnnotatorMap = null) {
    if(options.useGoldBoundaries){
      doc.getTargetCoref.mentions.foreach(m => doc.coref.addMention(m.phrase).phrase.attr += m.phrase.attr[OntonotesPhraseEntityType])
    }else if(!options.useNERMentions){
      ParseAndNerBasedPhraseFinder.FILTER_APPOS = true
      val map = if(annotatorMap eq null) DocumentAnnotatorPipeline.defaultDocumentAnnotationMap else annotatorMap
      DocumentAnnotatorPipeline(map, prereqs=Nil, ParseAndNerBasedPhraseFinder.prereqAttrs.toSeq).process(doc)
      ParseAndNerBasedPhraseFinder.getPhrases(doc).foreach(doc.coref.addMention)
    }else {
      val defaultMap = if(annotatorMap eq null) DocumentAnnotatorPipeline.defaultDocumentAnnotationMap else annotatorMap
      val preReqs = ConllProperNounPhraseFinder.prereqAttrs ++ PronounFinder.prereqAttrs ++AcronymNounPhraseFinder.prereqAttrs
      DocumentAnnotatorPipeline.apply(map=defaultMap.toMap, prereqs=Nil, preReqs).process(doc)
      (ConllProperNounPhraseFinder(doc) ++ PronounFinder(doc) ++ AcronymNounPhraseFinder(doc)).foreach(doc.getCoref.addMention)
    }
  }

  case class PrecRecReport(numcorrect: Int,numGT: Int, numDetected: Int)

  //for each of the mentions in detectedMentions, this adds a reference to a ground truth entity
  //the alignment is based on an **exact match** between the mention boundaries
  def alignMentions(gtDoc: Document, wn: WordNet, useEntityTypes: Boolean, options: CorefOptions, shifts: Seq[Int],annotatorMap:DocumentAnnotatorMap = null): (PrecRecReport) = {
    val groundTruthMentions = gtDoc.targetCoref.entities.filter(!_.isSingleton).flatMap(e => e.children).toSeq
    //Set predicted mentions on the coref attribute of the document
    if(gtDoc.coref.mentions.isEmpty) findMentions(gtDoc,options)
    val detectedMentions = gtDoc.getCoref.mentions.toSeq

    val gtSpanHash = mutable.HashMap[(Int,Int),Mention]()
    gtSpanHash ++= groundTruthMentions.map(m => ((m.phrase.start, m.phrase.length), m))
    val gtHeadHash = mutable.HashMap[Int,Mention]()
    gtHeadHash ++= groundTruthMentions.map(m => (getHeadTokenInDoc(m),m))

    val gtAligned = mutable.HashMap[Mention,Boolean]()
    gtAligned ++= groundTruthMentions.map(m => (m,false))

    var exactMatches = 0
    var relevantExactMatches = 0
    var unAlignedEntityCount = 0
    val debug = false
    //here, we create a bunch of new entity objects, that differ from the entities that the ground truth mentions point to
    //however, we index them by the same uIDs that the ground mentions use
    val entityHash = groundTruthMentions.groupBy(m => m.entity).toMap
    val falsePositives1 = ArrayBuffer[Mention]()
    detectedMentions.foreach(m => {
      val alignment = checkContainment(gtSpanHash,gtHeadHash,m, options, shifts)
      if(alignment.isDefined){
          val gtMention = alignment.get
          val entity = gtMention.entity
          //Make the close alignment our new ground truth for training
          gtDoc.getTargetCoref.deleteMention(gtMention)
          gtDoc.getTargetCoref.addMention(m.phrase)
          if(entity != null){
            m._setEntity(entity)
            if(entityHash(gtMention.entity).length > 1) relevantExactMatches += 1
            exactMatches += 1
          }
          else
            NounPhraseEntityTypeLabeler.process(m.phrase)
          gtAligned(gtMention) = true
          if(debug) println("aligned: " + gtMention.string +":" + gtMention.phrase.start   + "  " + m.phrase.string + ":" + m.phrase.start)
      }else{
          if(debug) println("not aligned: "  +  m.string + ":" + m.phrase.start)
          //Add our mention which was unaligned to the target coref as a singleton for training
          gtDoc.getTargetCoref.addMention(m.phrase)
          m.phrase.attr += new OntonotesPhraseEntityType(m.phrase,"O")
          unAlignedEntityCount += 1
          falsePositives1 += m
      }
    })

    val relevantGTMentions = groundTruthMentions.count(m => entityHash(m.entity).length > 1)
    new PrecRecReport(relevantExactMatches,relevantGTMentions,detectedMentions.length)
  }

  def getHeadTokenInDoc(m: Mention): Int = m.phrase.start + m.phrase.headTokenOffset

  def checkContainment(startLengthHash: mutable.HashMap[(Int,Int),Mention], headHash: mutable.HashMap[Int,Mention] ,m: Mention, options: CorefOptions, shifts: Seq[Int]): Option[Mention] = {
    val start = m.phrase.start
    val length = m.phrase.length
    val headTokIdxInDoc = m.phrase.headTokenOffset + m.phrase.start
    val startIdx = start
    val endIdx = start + length

    for (startShift <- shifts; endShift <- shifts; if startIdx + startShift <= endIdx + endShift) {
      val newStart = startIdx + startShift
      val newEnd = endIdx + endShift
      val key = (newStart, newEnd - newStart)
      if(startLengthHash.contains(key))
        return Some(startLengthHash(key))
    }

    //next, back off to aligning it based on the head token
    if(headHash.contains(headTokIdxInDoc))
      return Some(headHash(headTokIdxInDoc))
    None
  }
}


