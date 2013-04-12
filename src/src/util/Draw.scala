/**
 * Copyright 2012 David Stark
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package src.util

import java.awt.Color
import java.awt.Graphics
import java.awt.Transparency
import java.awt.image.BufferedImage

object Draw {

  private var tints : Map[String, BufferedImage] = Map()
  private val font : BufferedImage = MediaProvider.readImage("font4", Transparency.BITMASK)
  private val F_WIDTH : Int = 8
  private val F_DISP_WIDTH : Int = 7
  private val F_HEIGHT : Int = 13
  private val F_BASE : Int = ' '.toInt
  private val F_CEIL : Int = '~'.toInt + 1
  private val F_ERR : Int = '?'.toInt - F_BASE
  private val IMG_OFFSET : Int = 0

  private def findNextSpace(cs : List[Char], n : Int) : Option[Int] =
    cs match {
      case Nil                        => None
      case c :: css if c.isWhitespace => Some(n)
      case c :: css                   => findNextSpace(css, n + 1)
    }

  private def getColor(tintN : String) : Color =
    if (tintN.matches("[a-fA-F0-9]{8}"))
      new Color(
        Integer.parseInt(tintN.substring(0, 2), 16),
        Integer.parseInt(tintN.substring(2, 4), 16),
        Integer.parseInt(tintN.substring(4, 6), 16),
        Integer.parseInt(tintN.substring(6, 8), 16))
    else if (tintN.matches("[a-fA-F0-9]{6}"))
      new Color(
        Integer.parseInt(tintN.substring(0, 2), 16),
        Integer.parseInt(tintN.substring(2, 4), 16),
        Integer.parseInt(tintN.substring(4, 6), 16))
    else
      try {
        Color.getColor(tintN)
      } catch {
        case e : Exception => {
          e.printStackTrace
          null
        }
      }

  def text(g : Graphics, t : String, x : Int, y : Int) : Unit =
    innerText(g, x, y, font, Integer.MAX_VALUE / F_DISP_WIDTH, Integer.MAX_VALUE / F_HEIGHT, 0, 0, t.toList, None)

  def text(g : Graphics, t : String, x : Int, y : Int, maxWidth : Int, maxHeight : Int) : Unit =
    innerText(g, x, y, font, maxWidth / F_DISP_WIDTH, maxHeight / F_HEIGHT, 0, 0, t.toList, None)

  private def innerText(g : Graphics, x : Int, y : Int,
                        f : BufferedImage, cols : Int, rows : Int, c0 : Int, r0 : Int, cs : List[Char], bgC : Option[Color]) : Unit = {

    // Look ahead
    val nextSpace : Option[Int] = findNextSpace(cs, 0)
    val (c, r) = if (nextSpace.isDefined && nextSpace.get + c0 >= cols) (0, r0 + 1) else (c0, r0)

    def drawChar(ch : Char) : Unit = {
      val vall : Int = if (ch.toInt < F_CEIL) ch.toInt - F_BASE else F_ERR
      if (bgC.isDefined) {
        g.setColor(bgC.get)
        g.fillRect(x + c * F_DISP_WIDTH - 1, y + r * F_HEIGHT - 2, F_DISP_WIDTH, F_HEIGHT)
      }
      g.drawImage(
        f,
        x + c * F_DISP_WIDTH, y + r * F_HEIGHT,
        x + (c + 1) * F_DISP_WIDTH, y + r * F_HEIGHT + F_HEIGHT,
        vall * F_WIDTH + IMG_OFFSET, 0,
        vall * F_WIDTH + F_DISP_WIDTH + IMG_OFFSET, F_HEIGHT,
        null)
    }

    if (r < rows) {

      cs match {
        case Nil => ()

        case '\\' :: ch :: css => {
          drawChar(ch)
          innerText(g, x, y, f, cols, rows, c + 1, r, css, bgC)
        }

        case '\n' :: css =>
          innerText(g, x, y, f, cols, rows, 0, r + 1, css, bgC)

        case '{' :: css => {
          val name : List[Char] = css.takeWhile(_ != '}')
          val nameS : String = name.mkString
          var sym : BufferedImage = null
          if (nameS.startsWith("[") && nameS.contains("]")) {
            val tintN : String = nameS.substring(1, nameS.indexOf("]"))
            var tintC : Color = null
            if (!tintN.isEmpty) {
              tintC = getColor(tintN)
              if (tintC != null) {
                sym = MediaProvider.getImage(nameS.substring(nameS.indexOf("]") + 1))
                sym = MediaProvider.tint(sym, tintC)
              }
            }
          }

          if (sym == null) {
            sym = MediaProvider.getImage(nameS)
          }
          val overhang : Int = (F_HEIGHT - sym.getHeight) / 2
          g.drawImage(sym, x + (c) * F_DISP_WIDTH, y + r * F_HEIGHT + overhang, null)
          val newC : Int = c + (sym.getWidth / F_DISP_WIDTH) + (if (sym.getWidth % F_DISP_WIDTH == 0) 0 else 1)
          innerText(g, x, y, f, cols, rows, newC, r, css.dropWhile(_ != '}').tail, bgC)
        }

        case '[' :: css => {
          val name : List[Char] = css.takeWhile(_ != ']')
          var tintN : String = name.mkString
          val bg : Boolean = tintN.startsWith("bg=")
          if (bg) {
            tintN = tintN.substring(3)
          }
          var tintC : Color = null
          if (bg || !tints.contains(tintN)) {
            if (!tintN.isEmpty) {
              tintC = getColor(tintN)
            }
            if (bg) {

            } else if (tintC == null) {
              tints = tints + (tintN -> font)
            } else {
              tints = tints + (tintN -> MediaProvider.tint(font, tintC))
            }
          }
          innerText(g, x, y, if (bg) f else tints(tintN), cols, rows, c, r, css.dropWhile(_ != ']').tail, if (bg) Some(tintC) else bgC)
        }

        case ch :: css => {
          drawChar(ch)
          innerText(g, x, y, f, cols, rows, c + 1, r, css, bgC)
        }
      }
    }
  }

}
