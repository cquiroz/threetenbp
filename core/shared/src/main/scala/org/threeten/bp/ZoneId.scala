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
package org.threeten.bp

import java.io.DataOutput
import java.io.IOException
import java.io.Serializable
import java.util.{ Locale, Objects, TimeZone }
import org.threeten.bp.format.DateTimeFormatterBuilder
import org.threeten.bp.format.TextStyle
import org.threeten.bp.temporal.TemporalAccessor
import org.threeten.bp.temporal.TemporalField
import org.threeten.bp.temporal.TemporalQueries
import org.threeten.bp.temporal.TemporalQuery
import org.threeten.bp.temporal.UnsupportedTemporalTypeException
import org.threeten.bp.zone.ZoneRules
import org.threeten.bp.zone.ZoneRulesException
import org.threeten.bp.zone.ZoneRulesProvider
import scala.collection.JavaConverters._

@SerialVersionUID(8352817235686L)
object ZoneId {

  /** A map of zone overrides to enable the short time-zone names to be used.
    *
    * Use of short zone IDs has been deprecated in {@code java.util.TimeZone}.
    * This map allows the IDs to continue to be used via the
    * {@link #of(String, Map)} factory method.
    *
    * This map contains a mapping of the IDs that is in line with TZDB 2005r and
    * later, where 'EST', 'MST' and 'HST' map to IDs which do not include daylight
    * savings.
    *
    * This maps as follows:
    *<ul>
    * <li>EST - -05:00</li>
    * <li>HST - -10:00</li>
    * <li>MST - -07:00</li>
    * <li>ACT - Australia/Darwin</li>
    * <li>AET - Australia/Sydney</li>
    * <li>AGT - America/Argentina/Buenos_Aires</li>
    * <li>ART - Africa/Cairo</li>
    * <li>AST - America/Anchorage</li>
    * <li>BET - America/Sao_Paulo</li>
    * <li>BST - Asia/Dhaka</li>
    * <li>CAT - Africa/Harare</li>
    * <li>CNT - America/St_Johns</li>
    * <li>CST - America/Chicago</li>
    * <li>CTT - Asia/Shanghai</li>
    * <li>EAT - Africa/Addis_Ababa</li>
    * <li>ECT - Europe/Paris</li>
    * <li>IET - America/Indiana/Indianapolis</li>
    * <li>IST - Asia/Kolkata</li>
    * <li>JST - Asia/Tokyo</li>
    * <li>MIT - Pacific/Apia</li>
    * <li>NET - Asia/Yerevan</li>
    * <li>NST - Pacific/Auckland</li>
    * <li>PLT - Asia/Karachi</li>
    * <li>PNT - America/Phoenix</li>
    * <li>PRT - America/Puerto_Rico</li>
    * <li>PST - America/Los_Angeles</li>
    * <li>SST - Pacific/Guadalcanal</li>
    * <li>VST - Asia/Ho_Chi_Minh</li>
    * </ul><p>
    * The map is unmodifiable.
    */
  lazy val SHORT_IDS: java.util.Map[String, String] = {
    val base = Map[String, String](
      "ACT" -> "Australia/Darwin",
      "AET" -> "Australia/Sydney",
      "AGT" -> "America/Argentina/Buenos_Aires",
      "ART" -> "Africa/Cairo",
      "AST" -> "America/Anchorage",
      "BET" -> "America/Sao_Paulo",
      "BST" -> "Asia/Dhaka",
      "CAT" -> "Africa/Harare",
      "CNT" -> "America/St_Johns",
      "CST" -> "America/Chicago",
      "CTT" -> "Asia/Shanghai",
      "EAT" -> "Africa/Addis_Ababa",
      "ECT" -> "Europe/Paris",
      "IET" -> "America/Indiana/Indianapolis",
      "IST" -> "Asia/Kolkata",
      "JST" -> "Asia/Tokyo",
      "MIT" -> "Pacific/Apia",
      "NET" -> "Asia/Yerevan",
      "NST" -> "Pacific/Auckland",
      "PLT" -> "Asia/Karachi",
      "PNT" -> "America/Phoenix",
      "PRT" -> "America/Puerto_Rico",
      "PST" -> "America/Los_Angeles",
      "SST" -> "Pacific/Guadalcanal",
      "VST" -> "Asia/Ho_Chi_Minh",
      "EST" -> "-05:00",
      "MST" -> "-07:00",
      "HST" -> "-10:00"
    )
    base.asJava
  }

  /** Gets the system default time-zone.
    *
    * This queries {@link TimeZone#getDefault()} to find the default time-zone
    * and converts it to a {@code ZoneId}. If the system default time-zone is changed,
    * then the result of this method will also change.
    *
    * @return the zone ID, not null
    * @throws DateTimeException if the converted zone ID has an invalid format
    * @throws ZoneRulesException if the converted zone region ID cannot be found
    */
  def systemDefault: ZoneId = ZoneId.of(TimeZone.getDefault.getID, SHORT_IDS)

  /** Gets the set of available zone IDs.
    *
    * This set includes the string form of all available region-based IDs.
    * Offset-based zone IDs are not included in the returned set.
    * The ID can be passed to {@link #of(String)} to create a {@code ZoneId}.
    *
    * The set of zone IDs can increase over time, although in a typical application
    * the set of IDs is fixed. Each call to this method is thread-safe.
    *
    * @return a modifiable copy of the set of zone IDs, not null
    */
  def getAvailableZoneIds: java.util.Set[String] =
    new java.util.HashSet(ZoneRulesProvider.getAvailableZoneIds)

  /** Obtains an instance of {@code ZoneId} using its ID using a map
    * of aliases to supplement the standard zone IDs.
    *
    * Many users of time-zones use short abbreviations, such as PST for
    * 'Pacific Standard Time' and PDT for 'Pacific Daylight Time'.
    * These abbreviations are not unique, and so cannot be used as IDs.
    * This method allows a map of string to time-zone to be setup and reused
    * within an application.
    *
    * @param zoneId  the time-zone ID, not null
    * @param aliasMap  a map of alias zone IDs (typically abbreviations) to real zone IDs, not null
    * @return the zone ID, not null
    * @throws DateTimeException if the zone ID has an invalid format
    * @throws ZoneRulesException if the zone ID is a region ID that cannot be found
    */
  def of(zoneId: String, aliasMap: java.util.Map[String, String]): ZoneId = {
    Objects.requireNonNull(zoneId, "zoneId")
    Objects.requireNonNull(aliasMap, "aliasMap")
    var id: String = aliasMap.get(zoneId)
    id = if (id != null) id else zoneId
    of(id)
  }

  /** Obtains an instance of {@code ZoneId} from an ID ensuring that the
    * ID is valid and available for use.
    *
    * This method parses the ID producing a {@code ZoneId} or {@code ZoneOffset}.
    * A {@code ZoneOffset} is returned if the ID is 'Z', or starts with '+' or '-'.
    * The result will always be a valid ID for which {@link ZoneRules} can be obtained.
    *
    * Parsing matches the zone ID step by step as follows.
    * <ul>
    * <li>If the zone ID equals 'Z', the result is {@code ZoneOffset.UTC}.
    * <li>If the zone ID consists of a single letter, the zone ID is invalid
    * and {@code DateTimeException} is thrown.
    * <li>If the zone ID starts with '+' or '-', the ID is parsed as a
    * {@code ZoneOffset} using {@link ZoneOffset#of(String)}.
    * <li>If the zone ID equals 'GMT', 'UTC' or 'UT' then the result is a {@code ZoneId}
    * with the same ID and rules equivalent to {@code ZoneOffset.UTC}.
    * <li>If the zone ID starts with 'UTC+', 'UTC-', 'GMT+', 'GMT-', 'UT+' or 'UT-'
    * then the ID is a prefixed offset-based ID. The ID is split in two, with
    * a two or three letter prefix and a suffix starting with the sign.
    * The suffix is parsed as a {@link ZoneOffset#of(String) ZoneOffset}.
    * The result will be a {@code ZoneId} with the specified UTC/GMT/UT prefix
    * and the normalized offset ID as per {@link ZoneOffset#getId()}.
    * The rules of the returned {@code ZoneId} will be equivalent to the
    * parsed {@code ZoneOffset}.
    * <li>All other IDs are parsed as region-based zone IDs. Region IDs must
    * match the regular expression <code>[A-Za-z][A-Za-z0-9~/._+-]+</code>
    * otherwise a {@code DateTimeException} is thrown. If the zone ID is not
    * in the configured set of IDs, {@code ZoneRulesException} is thrown.
    * The detailed format of the region ID depends on the group supplying the data.
    * The default set of data is supplied by the IANA Time Zone Database (TZDB).
    * This has region IDs of the form '{area}/{city}', such as 'Europe/Paris' or 'America/New_York'.
    * This is compatible with most IDs from {@link java.util.TimeZone}.
    * </ul>
    *
    * @param zoneId  the time-zone ID, not null
    * @return the zone ID, not null
    * @throws DateTimeException if the zone ID has an invalid format
    * @throws ZoneRulesException if the zone ID is a region ID that cannot be found
    */
  def of(zoneId: String): ZoneId = {
    Objects.requireNonNull(zoneId, "zoneId")
    if (zoneId == "Z") {
      ZoneOffset.UTC
    } else if (zoneId.length == 1) {
      throw new DateTimeException(s"Invalid ID for ZoneOffset, invalid format: $zoneId")
    } else if (zoneId.startsWith("+") || zoneId.startsWith("-")) {
      ZoneOffset.of(zoneId)
    } else if ((zoneId == "UTC") || (zoneId == "GMT") || (zoneId == "UT")) {
      new ZoneRegion(zoneId, ZoneOffset.UTC.getRules)
    } else if (zoneId.startsWith("UTC+") || zoneId.startsWith("GMT+") || zoneId.startsWith("UTC-") || zoneId
                 .startsWith("GMT-")) {
      val offset: ZoneOffset = ZoneOffset.of(zoneId.substring(3))
      if (offset.getTotalSeconds == 0) {
        new ZoneRegion(zoneId.substring(0, 3), offset.getRules)
      } else {
        new ZoneRegion(zoneId.substring(0, 3) + offset.getId, offset.getRules)
      }
    } else if (zoneId.startsWith("UT+") || zoneId.startsWith("UT-")) {
      val offset: ZoneOffset = ZoneOffset.of(zoneId.substring(2))
      if (offset.getTotalSeconds == 0) {
        new ZoneRegion("UT", offset.getRules)
      } else {
        new ZoneRegion(s"UT${offset.getId}", offset.getRules)
      }
    } else {
      ZoneRegion.ofId(zoneId, true)
    }
  }

  /** Obtains an instance of {@code ZoneId} wrapping an offset.
    *
    * If the prefix is "GMT", "UTC", or "UT" a {@code ZoneId}
    * with the prefix and the non-zero offset is returned.
    * If the prefix is empty {@code ""} the {@code ZoneOffset} is returned.
    *
    * @param prefix  the time-zone ID, not null
    * @param offset  the offset, not null
    * @return the zone ID, not null
    * @throws IllegalArgumentException if the prefix is not one of
    *                                  "GMT", "UTC", or "UT", or ""
    */
  def ofOffset(prefix: String, offset: ZoneOffset): ZoneId = {
    Objects.requireNonNull(prefix, "prefix")
    Objects.requireNonNull(offset, "offset")
    if (prefix.length == 0)
      return offset
    if ((prefix == "GMT") || (prefix == "UTC") || (prefix == "UT")) {
      if (offset.getTotalSeconds == 0)
        return new ZoneRegion(prefix, offset.getRules)
      return new ZoneRegion(prefix + offset.getId, offset.getRules)
    }
    throw new IllegalArgumentException(s"Invalid prefix, must be GMT, UTC or UT: $prefix")
  }

  /** Obtains an instance of {@code ZoneId} from a temporal object.
    *
    * A {@code TemporalAccessor} represents some form of date and time information.
    * This factory converts the arbitrary temporal object to an instance of {@code ZoneId}.
    *
    * The conversion will try to obtain the zone in a way that favours region-based
    * zones over offset-based zones using {@link TemporalQueries#zone()}.
    *
    * This method matches the signature of the functional interface {@link TemporalQuery}
    * allowing it to be used in queries via method reference, {@code ZoneId::from}.
    *
    * @param temporal  the temporal object to convert, not null
    * @return the zone ID, not null
    * @throws DateTimeException if unable to convert to a { @code ZoneId}
    */
  def from(temporal: TemporalAccessor): ZoneId = {
    val obj: ZoneId = temporal.query(TemporalQueries.zone)
    if (obj == null)
      throw new DateTimeException(
        s"Unable to obtain ZoneId from TemporalAccessor: $temporal, type ${temporal.getClass.getName}"
      )
    else obj
  }
}

/** A time-zone ID, such as {@code Europe/Paris}.
  *
  * A {@code ZoneId} is used to identify the rules used to convert between
  * an {@link Instant} and a {@link LocalDateTime}.
  * There are two distinct types of ID:
  * <ul>
  * <li>Fixed offsets - a fully resolved offset from UTC/Greenwich, that uses
  * the same offset for all local date-times
  * <li>Geographical regions - an area where a specific set of rules for finding
  * the offset from UTC/Greenwich apply
  * </ul>
  * Most fixed offsets are represented by {@link ZoneOffset}.
  * Calling {@link #normalized()} on any {@code ZoneId} will ensure that a
  * fixed offset ID will be represented as a {@code ZoneOffset}.
  *
  * The actual rules, describing when and how the offset changes, are defined by {@link ZoneRules}.
  * This class is simply an ID used to obtain the underlying rules.
  * This approach is taken because rules are defined by governments and change
  * frequently, whereas the ID is stable.
  *
  * The distinction has other effects. Serializing the {@code ZoneId} will only send
  * the ID, whereas serializing the rules sends the entire data set.
  * Similarly, a comparison of two IDs only examines the ID, whereas
  * a comparison of two rules examines the entire data set.
  *
  * <h3>Time-zone IDs</h3>
  * The ID is unique within the system.
  * There are three types of ID.
  *
  * The simplest type of ID is that from {@code ZoneOffset}.
  * This consists of 'Z' and IDs starting with '+' or '-'.
  *
  * The next type of ID are offset-style IDs with some form of prefix,
  * such as 'GMT+2' or 'UTC+01:00'.
  * The recognised prefixes are 'UTC', 'GMT' and 'UT'.
  * The offset is the suffix and will be normalized during creation.
  * These IDs can be normalized to a {@code ZoneOffset} using {@code normalized()}.
  *
  * The third type of ID are region-based IDs. A region-based ID must be of
  * two or more characters, and not start with 'UTC', 'GMT', 'UT' '+' or '-'.
  * Region-based IDs are defined by configuration, see {@link ZoneRulesProvider}.
  * The configuration focuses on providing the lookup from the ID to the
  * underlying {@code ZoneRules}.
  *
  * Time-zone rules are defined by governments and change frequently.
  * There are a number of organizations, known here as groups, that monitor
  * time-zone changes and collate them.
  * The default group is the IANA Time Zone Database (TZDB).
  * Other organizations include IATA (the airline industry body) and Microsoft.
  *
  * Each group defines its own format for the region ID it provides.
  * The TZDB group defines IDs such as 'Europe/London' or 'America/New_York'.
  * TZDB IDs take precedence over other groups.
  *
  * It is strongly recommended that the group name is included in all IDs supplied by
  * groups other than TZDB to avoid conflicts. For example, IATA airline time-zone
  * region IDs are typically the same as the three letter airport code.
  * However, the airport of Utrecht has the code 'UTC', which is obviously a conflict.
  * The recommended format for region IDs from groups other than TZDB is 'group~region'.
  * Thus if IATA data were defined, Utrecht airport would be 'IATA~UTC'.
  *
  * <h3>Serialization</h3>
  * This class can be serialized and stores the string zone ID in the external form.
  * The {@code ZoneOffset} subclass uses a dedicated format that only stores the
  * offset from UTC/Greenwich.
  *
  * A {@code ZoneId} can be deserialized in a Java Runtime where the ID is unknown.
  * For example, if a server-side Java Runtime has been updated with a new zone ID, but
  * the client-side Java Runtime has not been updated. In this case, the {@code ZoneId}
  * object will exist, and can be queried using {@code getId}, {@code equals},
  * {@code hashCode}, {@code toString}, {@code getDisplayName} and {@code normalized}.
  * However, any call to {@code getRules} will fail with {@code ZoneRulesException}.
  * This approach is designed to allow a {@link ZonedDateTime} to be loaded and
  * queried, but not modified, on a Java Runtime with incomplete time-zone information.
  *
  * <h3>Specification for implementors</h3>
  * This abstract class has two implementations, both of which are immutable and thread-safe.
  * One implementation models region-based IDs, the other is {@code ZoneOffset} modelling
  * offset-based IDs. This difference is visible in serialization.
  *
  * @constructor Constructor only accessible within the package.
  */
@SerialVersionUID(8352817235686L)
abstract class ZoneId private[bp] () extends Serializable {
  if ((getClass ne classOf[ZoneOffset]) && (getClass ne classOf[ZoneRegion]))
    throw new AssertionError("Invalid subclass")

  /** Gets the unique time-zone ID.
    *
    * This ID uniquely defines this object.
    * The format of an offset based ID is defined by {@link ZoneOffset#getId()}.
    *
    * @return the time-zone unique ID, not null
    */
  def getId: String

  /** Gets the time-zone rules for this ID allowing calculations to be performed.
    *
    * The rules provide the functionality associated with a time-zone,
    * such as finding the offset for a given instant or local date-time.
    *
    * A time-zone can be invalid if it is deserialized in a Java Runtime which
    * does not have the same rules loaded as the Java Runtime that stored it.
    * In this case, calling this method will throw a {@code ZoneRulesException}.
    *
    * The rules are supplied by {@link ZoneRulesProvider}. An advanced provider may
    * support dynamic updates to the rules without restarting the Java Runtime.
    * If so, then the result of this method may change over time.
    * Each individual call will be still remain thread-safe.
    *
    * {@link ZoneOffset} will always return a set of rules where the offset never changes.
    *
    * @return the rules, not null
    * @throws ZoneRulesException if no rules are available for this ID
    */
  def getRules: ZoneRules

  /** Gets the textual representation of the zone, such as 'British Time' or
    * '+02:00'.
    *
    * This returns the textual name used to identify the time-zone ID,
    * suitable for presentation to the user.
    * The parameters control the style of the returned text and the locale.
    *
    * If no textual mapping is found then the {@link #getId() full ID} is returned.
    *
    * @param style  the length of the text required, not null
    * @param locale  the locale to use, not null
    * @return the text value of the zone, not null
    */
  def getDisplayName(style: TextStyle, locale: Locale): String =
    new DateTimeFormatterBuilder()
      .appendZoneText(style)
      .toFormatter(locale)
      .format(new TemporalAccessor() {
        def isSupported(field: TemporalField): Boolean = false
        def getLong(field:     TemporalField): Long =
          throw UnsupportedTemporalTypeException.field(field)
        override def query[R](query: TemporalQuery[R]): R =
          if (query eq TemporalQueries.zoneId)
            ZoneId.this.asInstanceOf[R]
          else
            super.query(query)
      })

  /** Normalizes the time-zone ID, returning a {@code ZoneOffset} where possible.
    *
    * The returns a normalized {@code ZoneId} that can be used in place of this ID.
    * The result will have {@code ZoneRules} equivalent to those returned by this object,
    * however the ID returned by {@code getId()} may be different.
    *
    * The normalization checks if the rules of this {@code ZoneId} have a fixed offset.
    * If they do, then the {@code ZoneOffset} equal to that offset is returned.
    * Otherwise {@code this} is returned.
    *
    * @return the time-zone unique ID, not null
    */
  def normalized: ZoneId = {
    try {
      val rules: ZoneRules = getRules
      if (rules.isFixedOffset)
        return rules.getOffset(Instant.EPOCH)
    } catch {
      case _: ZoneRulesException =>
    }
    this
  }

  /** Checks if this time-zone ID is equal to another time-zone ID.
    *
    * The comparison is based on the ID.
    *
    * @param obj  the object to check, null returns false
    * @return true if this is equal to the other time-zone ID
    */
  override def equals(obj: Any): Boolean =
    obj match {
      case other: ZoneId => (this eq other) || (getId == other.getId)
      case _             => false
    }

  /** A hash code for this time-zone ID.
    *
    * @return a suitable hash code
    */
  override def hashCode: Int = getId.hashCode

  /** Outputs this zone as a {@code String}, using the ID.
    *
    * @return a string representation of this time-zone ID, not null
    */
  override def toString: String = getId

  @throws[IOException]
  private[bp] def write(out: DataOutput): Unit
}
