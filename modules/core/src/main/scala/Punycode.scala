/*
 * Copyright 2020 Anton Sviridov
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package punycode

object Punycode {

  def encode(input: String): String = {
    bootstringEncode(input, PunycodeBootstringConfig, noTrace)
  }

  def decode(input: String): String = {
    bootstringDecode(input, PunycodeBootstringConfig, noTrace)
  }

  private[punycode] def decodeTraced(input: String): String = {
    bootstringDecode(input, PunycodeBootstringConfig, System.err.println(_))
  }

  private[punycode] def encodeTraced(input: String): String = {
    bootstringEncode(
      input,
      PunycodeBootstringConfig,
      trace = System.err.println(_)
    )
  }

  private final val noTrace: String => Unit = _ => ()

  private def bootstringDecode(
      input: String,
      config: BootstringConfig,
      trace: String => Unit
  ): String = {
    val result = Array.newBuilder[Char]
    var n = config.initialN
    var bias = config.initialBias

    val codepoints = input.codePoints().toArray
    val positionOfLastDelimiter = codepoints.lastIndexOf('-')
    var b = positionOfLastDelimiter.max(0)
    val caseFlags = codepoints.map(_.toChar.isUpper)
    trace(s"b = $b")

    val insertionsBuilder = Array.newBuilder[(Int, Char)]

    var out = 0
    for (j <- 0 until b) {
      result += codepoints(j).toChar
      out += 1
    }

    var in = if (b > 0) b + 1 else 0
    var i = 0
    while (in < codepoints.length) {
      trace(s"in = ${codepoints(in).toChar}, out = $out, bias = $bias")

      var oldi = i
      var w = 1
      var k = config.base
      var stop = false
      while (!stop) {
        if (in > codepoints.length) sys.error("TODO: bad input error")
        val digit = decodeDigit(codepoints(in), config.base)
        if (digit > config.base) sys.error("TODO: bad input error")
        // TODO: handle overflow?
        i += digit * w
        val t =
          if (k <= bias + config.tmin) config.tmin
          else if (k >= bias + config.tmax) config.tmax
          else k - bias

        trace(
          s"  in = ${codepoints(in).toChar}, k = $k, w = $w, digit = ${digit.toChar}, i = $i, t = $t"
        )
        in += 1
        if (digit < t) stop = true
        else {
          // TODO handle overflow
          w *= config.base - t
          k += config.base
        }
      }
      bias = adapt(config, i - oldi, out + 1, oldi == 0)
      // handle overflow
      n += i / (out + 1)
      i = i % (out + 1)

      trace(
        s"  adding ${n.toChar} ($n, 0x${n.formatted("%04x").toUpperCase}) at $i"
      )
      insertionsBuilder += i -> n.toChar
      i += 1
      out += 1

      // TODO: handle big output
    }

    val insertionsList = insertionsBuilder.result()
    trace(s"insertions = $insertionsList")
    var decoded = result.result().toList
    insertionsList.foreach { case (i, n) =>
      trace(
        s"  ${n.toChar} ($n, 0x${n.toInt.formatted("%04d").toUpperCase}) at $i, decoded = $decoded"
      )
      if (decoded.length == i) {
        decoded = decoded :+ n.toChar
      } else if (i == 0) {
        decoded = n.toChar +: decoded
      } else if (i > 0 && i < decoded.length) {
        decoded = (decoded.take(i) :+ n.toChar) ++ decoded.drop(i)
      }
    }

    trace(s"out=$out")

    new String(decoded.toArray)
  }

  def decodeDigit(cp: Int, base: Int) = {
    if (cp - 48 < 10) cp - 22
    else {
      if (cp - 65 < 26) cp - 65
      else {
        if (cp - 97 < 26) cp - 97 else base
      }
    }
  }

  private def bootstringEncode(
      input: String,
      config: BootstringConfig,
      trace: String => Unit
  ): String = {
    val result = Array.newBuilder[Char]
    val chars = input.toCharArray()

    // First, copy the basic code points
    var numBasic = 0
    chars.foreach { char =>
      if (char.toInt < 128) {
        result += char
        numBasic += 1
      }
    }

    if (numBasic > 0) {
      result += '-'
    }

    def flagged(bcp: Int) = (bcp - 65) < 26

    def encodeDigit(d: Int, flag: Boolean): Char = {
      val f = if (flag) 1 else 0
      (d + 22 + 75 * (if (d < 26) 1 else 0) - (f << 5)).toChar
    }

    var n = config.initialN
    var delta = 0
    var bias = config.initialBias
    var h = numBasic
    var b = numBasic

    val codepoints = input.codePoints().toArray

    val caseFlags = codepoints.map(_.toChar.isUpper)

    val sortedNonBasicCodepoints = codepoints.filter(_ >= 128).sorted.distinct
    var sortedIdx = 0

    val println = (s: String) => ()

    trace("Sorted codepoints: " + sortedNonBasicCodepoints.mkString(", "))
    trace("Input: " + codepoints.mkString(", "))
    trace("case flags: " + caseFlags.mkString(", "))

    while (h < codepoints.length) { // MAIN ENCODING LOOP
      val m = sortedNonBasicCodepoints(sortedIdx)
      delta = delta + (m - n) * (h + 1)
      trace(
        s"m = $m, h=$h, n = $n, delta = $delta, encoding ${sortedNonBasicCodepoints(sortedIdx).toChar}"
      )
      n = m
      var j = 0
      while (j < codepoints.length) { // PER CHARACTER ENCODING LOOP
        val c = codepoints(j)
        if (c < n || c < config.initialN) {
          delta += 1 // TODO: check overflow?
        }
        if (c == n) {
          var q = delta
          var k = config.base
          var t = 0
          var stop = false
          while (!stop) { // VARIABLE LENGTH INTEGER ENCODING LOOP
            trace(s"  q=$q, k=$k, bias=$bias, j=$j")
            t = {
              if (k <= bias + config.tmin) config.tmin
              else {
                if (k >= bias + config.tmax) config.tmax else k - bias
              }
            }
            if (q < t) {
              stop = true
            } else {
              val newChar = encodeDigit(t + (q - t) % (config.base - t), false)
              trace(s"  + $newChar")
              result += newChar
              q = (q - t) / (config.base - t)
              k += config.base
            }
          } // END VARIABLE LENGTH INTEGER ENCODING LOOP
          val newChar = encodeDigit(q, caseFlags(j))
          result += newChar
          trace(s"  ++ $newChar")
          bias = adapt(config, delta, h + 1, h == b)
          delta = 0
          h += 1
        }

        j += 1
      } // END PER CHARACTER ENCODING LOOP

      sortedIdx += 1
      delta += 1
      n += 1
    } // END MAIN ENCODING LOOP

    new String(result.result())
  }

  private def adapt(
      config: BootstringConfig,
      deltaInit: Int,
      numPoints: Int,
      firstTime: Boolean
  ): Int = {
    var delta = if (firstTime) deltaInit / config.damp else deltaInit / 2
    delta += delta / numPoints
    var k = 0
    while (delta > ((config.base - config.tmin) * config.tmax) / 2) {
      delta = delta / (config.base - config.tmin)
      k = k + config.base
    }
    k + ((config.base - config.tmin + 1) * delta) / (delta + config.skew)
  }

  private case class BootstringConfig(
      base: Int,
      tmin: Int,
      tmax: Int,
      skew: Int,
      damp: Int,
      initialBias: Int,
      initialN: Int
  )

  private def verify(bc: BootstringConfig): Boolean = {
    bc.base > 1 && bc.base <= 36 && bc.tmin <= bc.tmax && bc.skew >= 2 && bc.damp >= 2 && bc.initialBias >= 0 && bc.initialN >= 0
  }

  private final val PunycodeBootstringConfig =
    BootstringConfig(
      base = 36,
      tmin = 1,
      tmax = 26,
      skew = 38,
      damp = 700,
      initialBias = 72,
      initialN = 0x80
    )
}
