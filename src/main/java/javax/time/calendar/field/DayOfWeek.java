/*
 * Copyright (c) 2007-2010, Stephen Colebourne & Michael Nascimento Santos
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
package javax.time.calendar.field;

import java.util.Locale;

import javax.time.calendar.DateTimeFieldRule;
import javax.time.calendar.ISOChronology;
import javax.time.calendar.IllegalCalendarFieldValueException;
import javax.time.calendar.format.DateTimeFormatterBuilder.TextStyle;

/**
 * A day-of-week, such as 'Tuesday'.
 * <p>
 * <code>DayOfWeek</code> is an enum representing the 7 days of the week -
 * Monday, Tuesday, Wednesday, Thursday, Friday, Saturday and Sunday.
 * <p>
 * The calendrical framework requires date-time fields to have an <code>int</code> value.
 * The <code>int</code> value follows the ISO-8601 standard, from 1 (Monday) to 7 (Sunday).
 * It is recommended that applications use the enum rather than the <code>int</code> value
 * to ensure code clarity.
 * <p>
 * <b>Do not use ordinal() to obtain the numeric representation of <code>DayOfWeek</code>.
 * Use getValue() instead.</b>
 * <p>
 * This enum represents a common concept that is found in many calendar systems.
 * As such, this enum may be used by any calendar system that has the day-of-week
 * concept with a seven day week where the names are equivalent to those defined.
 * Note that the implementation of {@link DateTimeFieldRule} for day-of-week may
 * vary by calendar system.
 * <p>
 * DayOfWeek is an immutable and thread-safe enum.
 *
 * @author Michael Nascimento Santos
 * @author Stephen Colebourne
 */
public enum DayOfWeek {

    /**
     * The singleton instance for the day-of-week of Monday.
     */
    MONDAY,
    /**
     * The singleton instance for the day-of-week of Tuesday.
     */
    TUESDAY,
    /**
     * The singleton instance for the day-of-week of Wednesday.
     */
    WEDNESDAY,
    /**
     * The singleton instance for the day-of-week of Thursday.
     */
    THURSDAY,
    /**
     * The singleton instance for the day-of-week of Friday.
     */
    FRIDAY,
    /**
     * The singleton instance for the day-of-week of Saturday.
     */
    SATURDAY,
    /**
     * The singleton instance for the day-of-week of Sunday.
     */
    SUNDAY;

    //-----------------------------------------------------------------------
    /**
     * Obtains an instance of <code>DayOfWeek</code> from an <code>int</code> value.
     * <p>
     * <code>DayOfWeek</code> is an enum representing the 7 days of the week.
     * This factory allows the enum to be obtained from the <code>int</code> value.
     * The <code>int</code> value follows the ISO-8601 standard, from 1 (Monday) to 7 (Sunday).
     * <p>
     * An exception is thrown if the value is invalid. The exception uses the
     * {@link ISOChronology} day-of-week rule to indicate the failed rule.
     *
     * @param dayOfWeek  the day-of-week to represent, from 1 (Monday) to 7 (Sunday)
     * @return the DayOfWeek singleton, never null
     * @throws IllegalCalendarFieldValueException if the day-of-week is invalid
     */
    public static DayOfWeek of(int dayOfWeek) {
        switch (dayOfWeek) {
            case 1:
                return MONDAY;
            case 2:
                return TUESDAY;
            case 3:
                return WEDNESDAY;
            case 4:
                return THURSDAY;
            case 5:
                return FRIDAY;
            case 6:
                return SATURDAY;
            case 7:
                return SUNDAY;
            default:
                throw new IllegalCalendarFieldValueException(ISOChronology.dayOfWeekRule(), dayOfWeek, 1, 7);
        }
    }

    //-----------------------------------------------------------------------
    /**
     * Returns the <code>DayOfWeek</code> instance that corresponds to the first
     * day-of-week for a given <code>locale</code>.
     * <p>
     * If there is no information for a locale, <code>MONDAY</code> is returned.
     *
     * @param locale the locale to use, not null
     * @return the DayOfWeek singleton, never null
     */
    public static DayOfWeek firstDayOfWeekFor(Locale locale) {
        // TODO: Move/reinvent method
        if (locale == null) {
            throw new NullPointerException("Locale must not be null");
        }
        if (locale.equals(Locale.US) || (locale.getLanguage().equals("pt") &&
              locale.getCountry().equals("BR"))) {
            return SUNDAY;
        }

        return MONDAY;
    }

    //-----------------------------------------------------------------------
    /**
     * Gets the day-of-week <code>int</code> value.
     * <p>
     * The values are numbered following the ISO-8601 standard,
     * from 1 (Monday) to 7 (Sunday).
     *
     * @return the day-of-week, from 1 (Monday) to 7 (Sunday)
     */
    public int getValue() {
        return ordinal() + 1;
    }

    /**
     * Gets the short textual representation of this day-of-week, such as 'Mon' or 'Fri'.
     * <p>
     * This method is notionally specific to {@link ISOChronology} as it uses
     * the day-of-week rule to obtain the text. However, it is expected that
     * the text will be equivalent for all day-of-week rules, thus this aspect
     * of the implementation should be irrelevant to applications.
     * <p>
     * If there is no textual mapping for the locale, then the value is
     * returned as per {@link Integer#toString()}.
     *
     * @param locale  the locale to use, not null
     * @return the short text value of the day-of-week, never null
     */
    public String getShortText(Locale locale) {
        return ISOChronology.dayOfWeekRule().getText(getValue(), locale, TextStyle.SHORT);
    }

    /**
     * Gets the short textual representation of this day-of-week, such as 'Monday' or 'Friday'.
     * <p>
     * This method is notionally specific to {@link ISOChronology} as it uses
     * the day-of-week rule to obtain the text. However, it is expected that
     * the text will be equivalent for all day-of-week rules, thus this aspect
     * of the implementation should be irrelevant to applications.
     * <p>
     * If there is no textual mapping for the locale, then the value is
     * returned as per {@link Integer#toString()}.
     *
     * @param locale  the locale to use, not null
     * @return the long text value of the day-of-week, never null
     */
    public String getText(Locale locale) {
        return ISOChronology.dayOfWeekRule().getText(getValue(), locale, TextStyle.FULL);
    }

    //-----------------------------------------------------------------------
    /**
     * Is this instance representing Monday.
     *
     * @return true if this instance represents Monday
     */
    public boolean isMonday() {
        return (this == MONDAY);
    }

    /**
     * Is this instance representing Tuesday.
     *
     * @return true if this instance represents Tuesday
     */
    public boolean isTuesday() {
        return (this == TUESDAY);
    }

    /**
     * Is this instance representing Wednesday.
     *
     * @return true if this instance represents Wednesday
     */
    public boolean isWednesday() {
        return (this == WEDNESDAY);
    }

    /**
     * Is this instance representing Thursday.
     *
     * @return true if this instance represents Thursday
     */
    public boolean isThursday() {
        return (this == THURSDAY);
    }

    /**
     * Is this instance representing Friday.
     *
     * @return true if this instance represents Friday
     */
    public boolean isFriday() {
        return (this == FRIDAY);
    }

    /**
     * Is this instance representing Saturday.
     *
     * @return true if this instance represents Saturday
     */
    public boolean isSaturday() {
        return (this == SATURDAY);
    }

    /**
     * Is this instance representing Sunday.
     *
     * @return true if this instance represents Sunday
     */
    public boolean isSunday() {
        return (this == SUNDAY);
    }

    //-----------------------------------------------------------------------
    /**
     * Gets the next day-of-week.
     * <p>
     * This calculates based on the time-line, thus it rolls around the end of
     * the week. The next day after Sunday is Monday.
     *
     * @return the next day-of-week, never null
     */
    public DayOfWeek next() {
        return values()[(ordinal() + 1) % 7];
    }

    /**
     * Gets the previous day-of-week.
     * <p>
     * This calculates based on the time-line, thus it rolls around the end of
     * the week. The previous day before Monday is Sunday.
     *
     * @return the previous day-of-week, never null
     */
    public DayOfWeek previous() {
        return values()[(ordinal() + 7 - 1) % 7];
    }

    /**
     * Rolls the day-of-week, adding the specified number of days.
     * <p>
     * This calculates based on the time-line, thus it rolls around the end of
     * the week from Sunday to Monday. The days to roll by may be negative.
     * <p>
     * This instance is immutable and unaffected by this method call.
     *
     * @param days  the days to roll by, positive or negative
     * @return the resulting day-of-week, never null
     */
    public DayOfWeek roll(int days) {
        return values()[(ordinal() + (days % 7 + 7)) % 7];
    }

}
