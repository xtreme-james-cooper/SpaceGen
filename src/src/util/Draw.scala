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

  def text(g : Graphics, t : String, x : Int, y : Int) : Unit =
    text(g, t, x, y, Integer.MAX_VALUE, Integer.MAX_VALUE, true)

  def text(g : Graphics, t : String, x : Int, y : Int, maxWidth : Int, maxHeight : Int) : Unit =
    text(g, t, x, y, maxWidth, maxHeight, true)

  private def findNextSpace(cs : List[Char], n : Int) : Option[Int] =
    if (n < cs.length)
      if (cs(n).isWhitespace)
        Some(n)
      else
        findNextSpace(cs, n + 1)
    else
      None

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

  private def drawText(cs : List[Char], n : Int, bgC : Color, g : Graphics, x : Int, y : Int, c : Int, r : Int, f : BufferedImage) : Unit = {
    val vall : Int = if (cs(n).toInt < F_CEIL) cs(n).toInt - F_BASE else F_ERR
    if (bgC != null) {
      g.setColor(bgC)
      g.fillRect(x + c * F_DISP_WIDTH - (if (n == 0) -2 else 1), y + r * F_HEIGHT - 2, F_DISP_WIDTH, F_HEIGHT)
    }
    g.drawImage(
      f,
      x + c * F_DISP_WIDTH, y + r * F_HEIGHT,
      x + (c + 1) * F_DISP_WIDTH, y + r * F_HEIGHT + F_HEIGHT,
      vall * F_WIDTH + IMG_OFFSET, 0,
      vall * F_WIDTH + F_DISP_WIDTH + IMG_OFFSET, F_HEIGHT,
      null)
  }

  private def text(g : Graphics, text : String, x : Int, y : Int, maxWidth : Int, maxHeight : Int, allowCommands : Boolean) : Unit =
    innerText(g, x, y, allowCommands, font, maxWidth / F_DISP_WIDTH, maxHeight / F_HEIGHT, 0, 0, 0, text.toList, null)

  private def innerText(g : Graphics, x : Int, y : Int, allowCommands : Boolean,
                        f : BufferedImage, cols : Int, rows : Int, c0 : Int, r0 : Int, n : Int, cs : List[Char], bgC : Color) : Unit =
    if (n < cs.length) {
      // Look ahead
      val nextSpace : Option[Int] = findNextSpace(cs, n)
      val (c, r) = if (nextSpace.isDefined && nextSpace.get - n + c0 >= cols) (0, r0 + 1) else (c0, r0)
      if (r < rows) {

        if (cs(n) == '\\' && allowCommands) {
          drawText(cs, n + 1, bgC, g, x, y, c, r, f)
          innerText(g, x, y, allowCommands, f, cols, rows, c + 1, r, n + 2, cs, bgC)

        } else if (cs(n) == '\n') {
          innerText(g, x, y, allowCommands, f, cols, rows, 0, r + 1, n + 1, cs, bgC)

        } else if (cs(n) == '{' && allowCommands) {
          var n2 : Int = n + 1
          while (cs(n2) != '}') {
            n2 = n2 + 1
          }
          val name : List[Char] = cs.slice(n + 1, n2)
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
          innerText(g, x, y, allowCommands, f, cols, rows, newC, r, n2 + 1, cs, bgC)

        } else if (cs(n) == '[' && allowCommands) {
          var n2 : Int = n + 1
          while (cs(n2) != ']') {
            n2 = n2 + 1
          }
          val name : List[Char] = cs.slice(n + 1, n2)
          var tintN : String = name.mkString
          var bg : Boolean = false
          if (tintN.startsWith("bg=")) {
            bg = true
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
          innerText(g, x, y, allowCommands, if (bg) f else tints(tintN), cols, rows, c, r, n2 + 1, cs, if (bg) tintC else bgC)

        } else {
          drawText(cs, n, bgC, g, x, y, c, r, f)
          innerText(g, x, y, allowCommands, f, cols, rows, c + 1, r, n + 1, cs, bgC)
        }
      }
    }

}
