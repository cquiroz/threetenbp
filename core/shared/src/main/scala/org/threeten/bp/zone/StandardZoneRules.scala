/*
 * Copyright (c) 2007-present, Stephen Colebourne & Michael Nascimento Santos
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *  * Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 *  * Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 *  * Neither the name of JSR-310 nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package org.threeten.bp.zone

import java.io.DataInput
import java.io.DataOutput
import java.io.IOException
import java.io.Serializable
import java.util.Arrays
import java.util.Collections
import java.util.HashMap
import java.util.Map
import org.threeten.bp.Duration
import org.threeten.bp.Instant
import org.threeten.bp.LocalDate
import org.threeten.bp.LocalDateTime
import org.threeten.bp.Year
import org.threeten.bp.ZoneOffset

@SerialVersionUID(3044319355680032515L)
object StandardZoneRules {

  /** The last year to have its transitions cached. */
  private def LAST_CACHED_YEAR: Int = 2100

  /** Reads the state from the stream.
    *
    * @param in  the input stream, not null
    * @return the created object, not null
    * @throws IOException if an error occurs
    */
  @throws(classOf[IOException])
  @throws(classOf[ClassNotFoundException])
  private[zone] def readExternal(in: DataInput): StandardZoneRules = {
    val stdSize: Int          = in.readInt
    val stdTrans: Array[Long] = new Array[Long](stdSize)

    {
      var i: Int = 0
      while (i < stdSize) {
        stdTrans(i) = Ser.readEpochSec(in)
        i += 1
      }
    }
    val stdOffsets: Array[ZoneOffset] = new Array[ZoneOffset](stdSize + 1)

    {
      var i: Int = 0
      while (i < stdOffsets.length) {
        stdOffsets(i) = Ser.readOffset(in)
        i += 1
      }
    }
    val savSize: Int          = in.readInt
    val savTrans: Array[Long] = new Array[Long](savSize)

    {
      var i: Int = 0
      while (i < savSize) {
        savTrans(i) = Ser.readEpochSec(in)
        i += 1
      }
    }
    val savOffsets: Array[ZoneOffset] = new Array[ZoneOffset](savSize + 1)

    {
      var i: Int = 0
      while (i < savOffsets.length) {
        savOffsets(i) = Ser.readOffset(in)
        i += 1
      }
    }
    val ruleSize: Int                          = in.readByte.toInt
    val rules: Array[ZoneOffsetTransitionRule] = new Array[ZoneOffsetTransitionRule](ruleSize)

    {
      var i: Int = 0
      while (i < ruleSize) {
        rules(i) = ZoneOffsetTransitionRule.readExternal(in)
        i += 1
      }
    }
    new StandardZoneRules(stdTrans, stdOffsets, savTrans, savOffsets, rules)
  }

  /** Creates an instance.
    *
    * @param baseStandardOffset  the standard offset to use before legal rules were set, not null
    * @param baseWallOffset  the wall offset to use before legal rules were set, not null
    * @param standardOffsetTransitionList  the list of changes to the standard offset, not null
    * @param transitionList  the list of transitions, not null
    * @param lastRules  the recurring last rules, size 15 or less, not null
    */
  def apply(
    baseStandardOffset:           ZoneOffset,
    baseWallOffset:               ZoneOffset,
    standardOffsetTransitionList: java.util.List[ZoneOffsetTransition],
    transitionList:               java.util.List[ZoneOffsetTransition],
    lastRules:                    java.util.List[ZoneOffsetTransitionRule]
  ): StandardZoneRules = {
    val standardTransitions = new Array[Long](standardOffsetTransitionList.size)
    val standardOffsets     = new Array[ZoneOffset](standardOffsetTransitionList.size + 1)
    standardOffsets(0) = baseStandardOffset

    {
      var i: Int = 0
      while (i < standardOffsetTransitionList.size) {
        standardTransitions(i) = standardOffsetTransitionList.get(i).toEpochSecond
        standardOffsets(i + 1) = standardOffsetTransitionList.get(i).getOffsetAfter
        i += 1
      }
    }
    val localTransitionList: java.util.List[LocalDateTime]    = new java.util.ArrayList[LocalDateTime]
    val localTransitionOffsetList: java.util.List[ZoneOffset] = new java.util.ArrayList[ZoneOffset]
    localTransitionOffsetList.add(baseWallOffset)
    val transitions = transitionList.iterator
    while (transitions.hasNext) {
      val trans = transitions.next()
      if (trans.isGap) {
        localTransitionList.add(trans.getDateTimeBefore)
        localTransitionList.add(trans.getDateTimeAfter)
      } else {
        localTransitionList.add(trans.getDateTimeAfter)
        localTransitionList.add(trans.getDateTimeBefore)
      }
      localTransitionOffsetList.add(trans.getOffsetAfter)
    }
    val savingsLocalTransitions =
      localTransitionList.toArray(new Array[LocalDateTime](localTransitionList.size))
    val wallOffsets =
      localTransitionOffsetList.toArray(new Array[ZoneOffset](localTransitionOffsetList.size))
    val savingsInstantTransitions = new Array[Long](transitionList.size)

    {
      var i: Int = 0
      while (i < transitionList.size) {
        savingsInstantTransitions(i) = transitionList.get(i).getInstant.getEpochSecond
        i += 1
      }
    }
    if (lastRules.size > 15) {
      throw new IllegalArgumentException("Too many transition rules")
    }
    val resultLastRules = lastRules.toArray(new Array[ZoneOffsetTransitionRule](lastRules.size))

    new StandardZoneRules(standardTransitions,
                          standardOffsets,
                          savingsInstantTransitions,
                          wallOffsets,
                          resultLastRules,
                          savingsLocalTransitions)
  }
}

/** The rules describing how the zone offset varies through the year and historically.
  *
  * This class is used by the TZDB time-zone rules.
  *
  * <h3>Specification for implementors</h3>
  * This class is immutable and thread-safe.
  *
  * @constructor Utility constructor.
  *
  * @param standardTransitions  the standard transitions, not null
  * @param standardOffsets  the standard offsets, not null
  * @param savingsInstantTransitions  the standard transitions, not null
  * @param wallOffsets  the wall offsets, not null
  * @param lastRules  the recurring last rules, size 15 or less, not null
  * @param savingsLocalTransitions The transitions between local date-times, sorted.
  *                                This is a paired array, where the first entry is the start of the transition
  *                                and the second entry is the end of the transition.
  */
@SerialVersionUID(3044319355680032515L)
final class StandardZoneRules private (
  private val standardTransitions:       Array[Long],
  private val standardOffsets:           Array[ZoneOffset],
  private val savingsInstantTransitions: Array[Long],
  private val wallOffsets:               Array[ZoneOffset],
  private val lastRules:                 Array[ZoneOffsetTransitionRule],
  private val savingsLocalTransitions:   Array[LocalDateTime]
) extends ZoneRules
    with Serializable {

  /** The map of recent transitions. */
  private val lastRulesCache: Map[Integer, Array[ZoneOffsetTransition]] =
    new HashMap[Integer, Array[ZoneOffsetTransition]]()

  /** Creates an instance. */
  /* // Can't be implemented with Scala's constructor rules. Replaced with apply factory method.
  private[zone] def this(baseStandardOffset: ZoneOffset,
                         baseWallOffset: ZoneOffset,
                         standardOffsetTransitionList:
                         java.util.List[ZoneOffsetTransition],
                         transitionList: java.util.List[ZoneOffsetTransition],
                         lastRules: java.util.List[ZoneOffsetTransitionRule]) {
  }*/

  /** @constructor
    *
    * @param standardTransitions  the standard transitions, not null
    * @param standardOffsets  the standard offsets, not null
    * @param savingsInstantTransitions  the standard transitions, not null
    * @param wallOffsets  the wall offsets, not null
    * @param lastRules  the recurring last rules, size 15 or less, not null
    */
  private[zone] def this(
    standardTransitions:       Array[Long],
    standardOffsets:           Array[ZoneOffset],
    savingsInstantTransitions: Array[Long],
    wallOffsets:               Array[ZoneOffset],
    lastRules:                 Array[ZoneOffsetTransitionRule]
  ) {
    this(standardTransitions, standardOffsets, savingsInstantTransitions, wallOffsets, lastRules, {
      val localTransitionList: java.util.List[LocalDateTime] =
        new java.util.ArrayList[LocalDateTime]
      var i: Int = 0
      while (i < savingsInstantTransitions.length) {
        val before: ZoneOffset = wallOffsets(i)
        val after: ZoneOffset  = wallOffsets(i + 1)
        val trans: ZoneOffsetTransition =
          new ZoneOffsetTransition(savingsInstantTransitions(i), before, after)
        if (trans.isGap) {
          localTransitionList.add(trans.getDateTimeBefore)
          localTransitionList.add(trans.getDateTimeAfter)
        } else {
          localTransitionList.add(trans.getDateTimeAfter)
          localTransitionList.add(trans.getDateTimeBefore)
        }
        i += 1
      }
      localTransitionList.toArray(new Array[LocalDateTime](localTransitionList.size))
    })
  }

  /** Uses a serialization delegate.
    *
    * @return the replacing object, not null
    */
  private def writeReplace: AnyRef = new Ser(Ser.SZR, this)

  /** Writes the state to the stream.
    *
    * @param out  the output stream, not null
    * @throws IOException if an error occurs
    */
  @throws[IOException]
  private[zone] def writeExternal(out: DataOutput): Unit = {
    out.writeInt(standardTransitions.length)
    for (trans <- standardTransitions)
      Ser.writeEpochSec(trans, out)
    for (offset <- standardOffsets)
      Ser.writeOffset(offset, out)
    out.writeInt(savingsInstantTransitions.length)
    for (trans <- savingsInstantTransitions)
      Ser.writeEpochSec(trans, out)
    for (offset <- wallOffsets)
      Ser.writeOffset(offset, out)
    out.writeByte(lastRules.length)
    for (rule <- lastRules)
      rule.writeExternal(out)
  }

  def isFixedOffset: Boolean = savingsInstantTransitions.length == 0

  def getOffset(instant: Instant): ZoneOffset = {
    val epochSec: Long = instant.getEpochSecond
    if (lastRules.length > 0 && epochSec > savingsInstantTransitions(
          savingsInstantTransitions.length - 1
        )) {
      val year: Int                               = findYear(epochSec, wallOffsets(wallOffsets.length - 1))
      val transArray: Array[ZoneOffsetTransition] = findTransitionArray(year)
      var trans: ZoneOffsetTransition             = null

      {
        var i: Int = 0
        while (i < transArray.length) {
          trans = transArray(i)
          if (epochSec < trans.toEpochSecond) {
            return trans.getOffsetBefore
          }
          i += 1
        }
      }
      return trans.getOffsetAfter
    }
    var index: Int = Arrays.binarySearch(savingsInstantTransitions, epochSec)
    if (index < 0) {
      index = -index - 2
    }
    wallOffsets(index + 1)
  }

  def getOffset(localDateTime: LocalDateTime): ZoneOffset =
    getOffsetInfo(localDateTime) match {
      case transition: ZoneOffsetTransition => transition.getOffsetBefore
      case offset: ZoneOffset               => offset
    }

  def getValidOffsets(localDateTime: LocalDateTime): java.util.List[ZoneOffset] =
    getOffsetInfo(localDateTime) match {
      case transition: ZoneOffsetTransition => transition.getValidOffsets
      case offset: ZoneOffset               => Collections.singletonList(offset)
    }

  def getTransition(localDateTime: LocalDateTime): ZoneOffsetTransition =
    getOffsetInfo(localDateTime) match {
      case transition: ZoneOffsetTransition => transition
      case _                                => null
    }

  private def getOffsetInfo(dt: LocalDateTime): AnyRef = {
    if (lastRules.length > 0 && dt.isAfter(
          savingsLocalTransitions(savingsLocalTransitions.length - 1)
        )) {
      val transArray: Array[ZoneOffsetTransition] = findTransitionArray(dt.getYear)
      var info: AnyRef                            = null
      for (trans <- transArray) {
        info = findOffsetInfo(dt, trans)
        if (info.isInstanceOf[ZoneOffsetTransition] || (info == trans.getOffsetBefore)) {
          return info
        }
      }
      return info
    }
    var index: Int = Arrays.binarySearch(savingsLocalTransitions.asInstanceOf[Array[AnyRef]], dt)
    if (index == -1)
      return wallOffsets(0)
    if (index < 0)
      index = -index - 2
    else if (index < savingsLocalTransitions.length - 1 && (savingsLocalTransitions(index) == savingsLocalTransitions(
               index + 1
             )))
      index += 1
    if ((index & 1) == 0) {
      val dtBefore: LocalDateTime  = savingsLocalTransitions(index)
      val dtAfter: LocalDateTime   = savingsLocalTransitions(index + 1)
      val offsetBefore: ZoneOffset = wallOffsets(index / 2)
      val offsetAfter: ZoneOffset  = wallOffsets(index / 2 + 1)
      if (offsetAfter.getTotalSeconds > offsetBefore.getTotalSeconds)
        new ZoneOffsetTransition(dtBefore, offsetBefore, offsetAfter)
      else
        new ZoneOffsetTransition(dtAfter, offsetBefore, offsetAfter)
    } else {
      wallOffsets(index / 2 + 1)
    }
  }

  /** Finds the offset info for a local date-time and transition.
    *
    * @param dt  the date-time, not null
    * @param trans  the transition, not null
    * @return the offset info, not null
    */
  private def findOffsetInfo(dt: LocalDateTime, trans: ZoneOffsetTransition): AnyRef = {
    val localTransition: LocalDateTime = trans.getDateTimeBefore
    if (trans.isGap) {
      if (dt.isBefore(localTransition))
        trans.getOffsetBefore
      else if (dt.isBefore(trans.getDateTimeAfter))
        trans
      else
        trans.getOffsetAfter
    } else {
      if (!dt.isBefore(localTransition))
        trans.getOffsetAfter
      else if (dt.isBefore(trans.getDateTimeAfter))
        trans.getOffsetBefore
      else
        trans
    }
  }

  def isValidOffset(localDateTime: LocalDateTime, offset: ZoneOffset): Boolean =
    getValidOffsets(localDateTime).contains(offset)

  /** Finds the appropriate transition array for the given year.
    *
    * @param year  the year, not null
    * @return the transition array, not null
    */
  private def findTransitionArray(year: Int): Array[ZoneOffsetTransition] = {
    val yearObj: Integer                        = year
    var transArray: Array[ZoneOffsetTransition] = lastRulesCache.get(yearObj)
    if (transArray != null)
      return transArray
    val ruleArray: Array[ZoneOffsetTransitionRule] = lastRules
    transArray = new Array[ZoneOffsetTransition](ruleArray.length)

    var i: Int = 0
    while (i < ruleArray.length) {
      transArray(i) = ruleArray(i).createTransition(year)
      i += 1
    }

    if (year < StandardZoneRules.LAST_CACHED_YEAR)
      lastRulesCache.put(yearObj, transArray)
    transArray
  }

  def getStandardOffset(instant: Instant): ZoneOffset = {
    val epochSec: Long = instant.getEpochSecond
    var index: Int     = Arrays.binarySearch(standardTransitions, epochSec)
    if (index < 0)
      index = -index - 2
    standardOffsets(index + 1)
  }

  def getDaylightSavings(instant: Instant): Duration = {
    val standardOffset: ZoneOffset = getStandardOffset(instant)
    val actualOffset: ZoneOffset   = getOffset(instant)
    Duration.ofSeconds(actualOffset.getTotalSeconds.toLong - standardOffset.getTotalSeconds.toLong)
  }

  def isDaylightSavings(instant: Instant): Boolean =
    getStandardOffset(instant) != getOffset(instant)

  def nextTransition(instant: Instant): ZoneOffsetTransition = {
    if (savingsInstantTransitions.length == 0)
      return null
    val epochSec: Long = instant.getEpochSecond
    if (epochSec >= savingsInstantTransitions(savingsInstantTransitions.length - 1)) {
      if (lastRules.length == 0)
        return null
      val year: Int                               = findYear(epochSec, wallOffsets(wallOffsets.length - 1))
      var transArray: Array[ZoneOffsetTransition] = findTransitionArray(year)
      for (trans <- transArray) {
        if (epochSec < trans.toEpochSecond)
          return trans
      }
      if (year < Year.MAX_VALUE) {
        transArray = findTransitionArray(year + 1)
        return transArray(0)
      }
      return null
    }
    var index: Int = Arrays.binarySearch(savingsInstantTransitions, epochSec)
    if (index < 0)
      index = -index - 1
    else
      index += 1
    new ZoneOffsetTransition(savingsInstantTransitions(index),
                             wallOffsets(index),
                             wallOffsets(index + 1))
  }

  def previousTransition(instant: Instant): ZoneOffsetTransition = {
    if (savingsInstantTransitions.length == 0)
      return null
    var epochSec: Long = instant.getEpochSecond
    if (instant.getNano > 0 && epochSec < Long.MaxValue)
      epochSec += 1
    val lastHistoric: Long = savingsInstantTransitions(savingsInstantTransitions.length - 1)
    if (lastRules.length > 0 && epochSec > lastHistoric) {
      val lastHistoricOffset: ZoneOffset          = wallOffsets(wallOffsets.length - 1)
      var year: Int                               = findYear(epochSec, lastHistoricOffset)
      var transArray: Array[ZoneOffsetTransition] = findTransitionArray(year)

      {
        var i: Int = transArray.length - 1
        while (i >= 0) {
          if (epochSec > transArray(i).toEpochSecond) {
            return transArray(i)
          }
          i -= 1
        }
      }
      val lastHistoricYear: Int = findYear(lastHistoric, lastHistoricOffset)
      if ({ year -= 1; year } > lastHistoricYear) {
        transArray = findTransitionArray(year)
        return transArray(transArray.length - 1)
      }
    }
    var index: Int = Arrays.binarySearch(savingsInstantTransitions, epochSec)
    if (index < 0)
      index = -index - 1
    if (index <= 0)
      return null
    new ZoneOffsetTransition(savingsInstantTransitions(index - 1),
                             wallOffsets(index - 1),
                             wallOffsets(index))
  }

  private def findYear(epochSecond: Long, offset: ZoneOffset): Int = {
    val localSecond: Long   = epochSecond + offset.getTotalSeconds
    val localEpochDay: Long = Math.floorDiv(localSecond, 86400)
    LocalDate.ofEpochDay(localEpochDay).getYear
  }

  def getTransitions: java.util.List[ZoneOffsetTransition] = {
    val list: java.util.List[ZoneOffsetTransition] = new java.util.ArrayList[ZoneOffsetTransition]
    var i: Int                                     = 0
    while (i < savingsInstantTransitions.length) {
      list.add(
        new ZoneOffsetTransition(savingsInstantTransitions(i), wallOffsets(i), wallOffsets(i + 1))
      )
      i += 1
    }
    Collections.unmodifiableList(list)
  }

  def getTransitionRules: java.util.List[ZoneOffsetTransitionRule] =
    Collections.unmodifiableList(Arrays.asList(lastRules: _*))

  override def equals(obj: Any): Boolean =
    obj match {
      case other: StandardZoneRules =>
        (this eq other) || (Arrays.equals(standardTransitions, other.standardTransitions) && Arrays
          .equals(standardOffsets.asInstanceOf[Array[AnyRef]],
                  other.standardOffsets.asInstanceOf[Array[AnyRef]]) && Arrays.equals(
          savingsInstantTransitions,
          other.savingsInstantTransitions
        ) && Arrays.equals(wallOffsets.asInstanceOf[Array[AnyRef]],
                           other.wallOffsets.asInstanceOf[Array[AnyRef]]) && Arrays.equals(
          lastRules.asInstanceOf[Array[AnyRef]],
          other.lastRules.asInstanceOf[Array[AnyRef]]
        ))
      case _: ZoneRules.Fixed =>
        isFixedOffset && (getOffset(Instant.EPOCH) == obj
          .asInstanceOf[ZoneRules.Fixed]
          .getOffset(Instant.EPOCH))
      case _ => false
    }

  override def hashCode: Int =
    Arrays.hashCode(standardTransitions) ^ Arrays.hashCode(
      standardOffsets.asInstanceOf[Array[AnyRef]]
    ) ^ Arrays.hashCode(savingsInstantTransitions) ^ Arrays.hashCode(
      wallOffsets.asInstanceOf[Array[AnyRef]]
    ) ^ Arrays.hashCode(lastRules.asInstanceOf[Array[AnyRef]])

  /** Returns a string describing this object.
    *
    * @return a string for debugging, not null
    */
  override def toString: String =
    s"StandardZoneRules[currentStandardOffset=${standardOffsets(standardOffsets.length - 1)}]"
}
