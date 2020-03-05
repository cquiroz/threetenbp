package org.threeten.bp.chrono

import java.io.File
import java.io.FileInputStream
import java.io.BufferedReader
import java.io.IOException
import java.io.InputStream
import java.io.InputStreamReader
import java.util.zip.ZipEntry
import java.util.zip.ZipFile
import java.util.StringTokenizer
import java.text.ParseException

object HijrahDateConfigurator {

  /** File separator. */
  private def FILE_SEP: Char = File.separatorChar

  /** Path separator. */
  private def PATH_SEP: String = File.pathSeparator

  /** Default config file name. */
  private def DEFAULT_CONFIG_FILENAME: String = "hijrah_deviation.cfg"

  /** Default path to the config file. */
  private def DEFAULT_CONFIG_PATH: String = s"org${FILE_SEP}threeten${FILE_SEP}bp${FILE_SEP}chrono"

  /** Read hijrah_deviation.cfg file. The config file contains the deviation data with
    * following format.
    *
    * StartYear/StartMonth(0-based)-EndYear/EndMonth(0-based):Deviation day (1,
    * 2, -1, or -2)
    *
    * Line separator or ";" is used for the separator of each deviation data.
    *
    * Here is the example.
    *
    * 1429/0-1429/1:1
    * 1429/2-1429/7:1;1429/6-1429/11:1
    * 1429/11-9999/11:1
    *
    * @throws IOException for zip/jar file handling exception.
    * @throws ParseException if the format of the configuration file is wrong.
    */
  @throws[IOException]
  @throws[ParseException]
  private[chrono] def readDeviationConfig(): Unit = {
    val is: InputStream = getConfigFileInputStream
    if (is != null) {
      var br: BufferedReader = null
      try {
        br = new BufferedReader(new InputStreamReader(is))
        var line: String = ""
        var num: Int     = 0
        while ({ line = br.readLine; line } != null) {
          num += 1
          line = line.trim
          parseLine(line, num)
        }
      } finally {
        if (br != null)
          br.close()
      }
    }
  }

  /** Parse each deviation element.
    *
    * @param line  a line to parse
    * @param num  line number
    * @throws ParseException if line has incorrect format.
    */
  @throws[ParseException]
  private def parseLine(line: String, num: Int): Unit = {
    val st: StringTokenizer = new StringTokenizer(line, ";")
    while (st.hasMoreTokens) {
      val deviationElement: String = st.nextToken
      val offsetIndex: Int         = deviationElement.indexOf(':')
      if (offsetIndex != -1) {
        val offsetString: String =
          deviationElement.substring(offsetIndex + 1, deviationElement.length)
        var offset: Int = 0
        try offset = offsetString.toInt
        catch {
          case _: NumberFormatException =>
            throw new ParseException(s"Offset is not properly set at line $num.", num)
        }
        val separatorIndex: Int = deviationElement.indexOf('-')
        if (separatorIndex != -1) {
          val startDateStg: String       = deviationElement.substring(0, separatorIndex)
          val endDateStg: String         = deviationElement.substring(separatorIndex + 1, offsetIndex)
          val startDateYearSepIndex: Int = startDateStg.indexOf('/')
          val endDateYearSepIndex: Int   = endDateStg.indexOf('/')
          var startYear: Int             = -1
          var endYear: Int               = -1
          var startMonth: Int            = -1
          var endMonth: Int              = -1
          if (startDateYearSepIndex != -1) {
            val startYearStg: String = startDateStg.substring(0, startDateYearSepIndex)
            val startMonthStg: String =
              startDateStg.substring(startDateYearSepIndex + 1, startDateStg.length)
            try startYear = startYearStg.toInt
            catch {
              case _: NumberFormatException =>
                throw new ParseException(s"Start year is not properly set at line $num.", num)
            }
            try startMonth = startMonthStg.toInt
            catch {
              case _: NumberFormatException =>
                throw new ParseException(s"Start month is not properly set at line $num.", num)
            }
          } else
            throw new ParseException(s"Start year/month has incorrect format at line $num.", num)
          if (endDateYearSepIndex != -1) {
            val endYearStg: String = endDateStg.substring(0, endDateYearSepIndex)
            val endMonthStg: String =
              endDateStg.substring(endDateYearSepIndex + 1, endDateStg.length)
            try endYear = endYearStg.toInt
            catch {
              case _: NumberFormatException =>
                throw new ParseException(s"End year is not properly set at line $num.", num)
            }
            try {
              endMonth = endMonthStg.toInt
            } catch {
              case _: NumberFormatException =>
                throw new ParseException(s"End month is not properly set at line $num.", num)
            }
          } else
            throw new ParseException(s"End year/month has incorrect format at line $num.", num)
          if (startYear != -1 && startMonth != -1 && endYear != -1 && endMonth != -1)
            HijrahDate.addDeviationAsHijrah(startYear, startMonth, endYear, endMonth, offset)
          else
            throw new ParseException(s"Unknown error at line $num.", num)
        } else
          throw new ParseException(s"Start and end year/month has incorrect format at line $num.",
                                   num)
      } else
        throw new ParseException(s"Offset has incorrect format at line $num.", num)
    }
  }

  /** Return InputStream for deviation configuration file.
    * The default location of the deviation file is:
    * <pre>
    * $CLASSPATH/org/threeten/bp/chrono
    * </pre>
    * And the default file name is:
    * <pre>
    * hijrah_deviation.cfg
    * </pre>
    * The default location and file name can be overriden by setting
    * following two Java's system property.
    * <pre>
    * Location: org.threeten.bp.i18n.HijrahDate.deviationConfigDir
    * File name: org.threeten.bp.i18n.HijrahDate.deviationConfigFile
    * </pre>
    * Regarding the file format, see readDeviationConfig() method for details.
    *
    * @return InputStream for file reading exception.
    * @throws IOException for zip/jar file handling exception.
    */
  @throws[IOException]
  private def getConfigFileInputStream: InputStream = {
    var fileName: String = System.getProperty("org.threeten.bp.i18n.HijrahDate.deviationConfigFile")
    if (fileName == null)
      fileName = DEFAULT_CONFIG_FILENAME
    var dir: String = System.getProperty("org.threeten.bp.i18n.HijrahDate.deviationConfigDir")
    if (dir != null) {
      if (!(dir.length == 0 && dir.endsWith(System.getProperty("file.separator"))))
        dir = dir + System.getProperty("file.separator")
      val file: File = new File(dir + FILE_SEP + fileName)
      if (file.exists) {
        try new FileInputStream(file)
        catch {
          case ioe: IOException =>
            throw ioe
        }
      } else null
    } else {
      val classPath: String   = System.getProperty("java.class.path")
      val st: StringTokenizer = new StringTokenizer(classPath, PATH_SEP)
      while (st.hasMoreTokens) {
        val path: String = st.nextToken
        val file: File   = new File(path)
        if (file.exists) {
          if (file.isDirectory) {
            val f: File = new File(path + FILE_SEP + DEFAULT_CONFIG_PATH, fileName)
            if (f.exists) {
              try return new FileInputStream(
                path + FILE_SEP + DEFAULT_CONFIG_PATH + FILE_SEP + fileName
              )
              catch {
                case ioe: IOException => throw ioe
              }
            }
          } else {
            var zip: ZipFile = null
            try zip = new ZipFile(file)
            catch {
              case _: IOException => zip = null
            }
            if (zip != null) {
              var targetFile: String = DEFAULT_CONFIG_PATH + FILE_SEP + fileName
              var entry: ZipEntry    = zip.getEntry(targetFile)
              if (entry == null) {
                if (FILE_SEP == '/')
                  targetFile = targetFile.replace('/', '\\')
                else if (FILE_SEP == '\\')
                  targetFile = targetFile.replace('\\', '/')
                entry = zip.getEntry(targetFile)
              }
              if (entry != null) {
                try return zip.getInputStream(entry)
                catch {
                  case ioe: IOException => throw ioe
                }
              }
            }
          }
        }
      }
      null
    }
  }
}
