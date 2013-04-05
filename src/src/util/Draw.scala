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

  var tints : Map[String, BufferedImage] = Map()
  val font : BufferedImage = MediaProvider.readImage("font4", Transparency.BITMASK)
  val F_WIDTH : Int = 8
  val F_DISP_WIDTH : Int = 7
  val F_HEIGHT : Int = 13
  val F_BASE : Int = ' '.toInt
  val F_CEIL : Int = '~'.toInt + 1
  val F_ERR : Int = '?'.toInt - F_BASE
  val IMG_OFFSET : Int = 0

  def button(g : Graphics, t : String, x : Int, y : Int, width : Int) : Unit = {
    g.setColor(Color.DARK_GRAY)
    g.fill3DRect(x, y, width, 20, true)
    text(g, t, x + 10, y + 4)
  }

  def text(g : Graphics, t : String, x : Int, y : Int) : Unit =
    text(g, t, x, y, Integer.MAX_VALUE, Integer.MAX_VALUE)

  def text(g : Graphics, t : String, x : Int, y : Int, maxWidth : Int, maxHeight : Int) : Unit =
    text(g, t, x, y, maxWidth, maxHeight, true)

  def esc(text : String) : String =
    text.replace("\\", "\\\\").replace("{", "\\{").replace("}", "\\}").replace("[", "\\[").replace("]", "\\]")

  def findNextSpace(cs : List[Char], n : Int) : Int = {
    var nextSpace : Int = n
    while (nextSpace < cs.length) {
      if (cs(nextSpace) == ' ' || cs(nextSpace) == '\n') {
        return nextSpace
      }
      nextSpace = nextSpace + 1
    }
    nextSpace
  }

  def text(g : Graphics, text : String, x : Int, y : Int, maxWidth : Int, maxHeight : Int, allowCommands : Boolean) : Unit = {
    var f : BufferedImage = font
    val cols : Int = maxWidth / F_DISP_WIDTH
    val rows : Int = maxHeight / F_HEIGHT
    var c : Int = 0
    var r : Int = 0
    var n : Int = 0
    val cs : List[Char] = text.toList
    var bgC : Color = null
    while (n < cs.length) {
      // Look ahead
      val nextSpace : Int = findNextSpace(cs, n)
      if (nextSpace - n + c >= cols && nextSpace != cs.length) {
        c = 0
        r = r + 1
      }
      if (r >= rows) {
        return

      } else if (cs(n) == '\\' && allowCommands) {
        n = n + 1
        val vall : Int = if (cs(n).toInt < F_CEIL) cs(n).toInt - F_BASE else F_ERR
        if (bgC != null) {
          g.setColor(bgC)
          g.fillRect(
            x + c * F_DISP_WIDTH - (if (n == 0) -2 else 1), y + r * F_HEIGHT - 2,
            F_DISP_WIDTH, F_HEIGHT)
        }
        g.drawImage(
          f,
          x + c * F_DISP_WIDTH, y + r * F_HEIGHT,
          x + (c + 1) * F_DISP_WIDTH, y + r * F_HEIGHT + F_HEIGHT,
          vall * F_WIDTH + IMG_OFFSET, 0,
          vall * F_WIDTH + F_DISP_WIDTH + IMG_OFFSET, F_HEIGHT,
          null)
        c = c + 1
        n = n + 1

      } else if (cs(n) == '\n') {
        c = 0
        r = r + 1
        n = n + 1

      } else if (cs(n) == '{' && allowCommands) {
        var n2 : Int = n + 1
        while (cs(n2) != '}') { n2 = n2 + 1 }
        val name : List[Char] = cs.slice(n + 1, n2)
        val nameS : String = name.mkString
        var sym : BufferedImage = null
        if (nameS.startsWith("[") && nameS.contains("]")) {
          val tintN : String = nameS.substring(1, nameS.indexOf("]"))
          var tintC : Color = null
          if (!tintN.isEmpty) {
            if (tintN.matches("[a-fA-F0-9]{6}")) {
              try {
                tintC = new Color(
                  Integer.parseInt(tintN.substring(0, 2), 16),
                  Integer.parseInt(tintN.substring(2, 4), 16),
                  Integer.parseInt(tintN.substring(4, 6), 16))
              } catch {
                case e:Exception => e.printStackTrace
              }
            }
            if (tintN.matches("[a-fA-F0-9]{8}")) {
              try {
                tintC = new Color(
                  Integer.parseInt(tintN.substring(0, 2), 16),
                  Integer.parseInt(tintN.substring(2, 4), 16),
                  Integer.parseInt(tintN.substring(4, 6), 16),
                  Integer.parseInt(tintN.substring(6, 8), 16))
              } catch {
                case e:Exception => e.printStackTrace
              }
            }
            if (tintC == null) {
              try {
                tintC = Color.getColor(tintN)
              } catch {
                case e:Exception => e.printStackTrace
              }
            }
            if (tintC != null) {
              sym = MediaProvider.getImage(nameS.substring(nameS.indexOf("]") + 1))
              sym = MediaProvider.tint(sym, tintC)
            }
          }
        }

        if (sym == null) { sym = MediaProvider.getImage(nameS) }
        val overhang : Int = (F_HEIGHT - sym.getHeight) / 2
        g.drawImage(sym, x + (c) * F_DISP_WIDTH, y + r * F_HEIGHT + overhang, null)
        n = n2 + 1
        c = c + (sym.getWidth / F_DISP_WIDTH) + (if (sym.getWidth % F_DISP_WIDTH == 0) 0 else 1)

      } else if (cs(n) == '[' && allowCommands) {
        var n2 : Int = n + 1
        while (cs(n2) != ']') { n2 = n2 + 1 }
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
            if (tintN.matches("[a-fA-F0-9]{6}")) {
              try {
                tintC = new Color(
                  Integer.parseInt(tintN.substring(0, 2), 16),
                  Integer.parseInt(tintN.substring(2, 4), 16),
                  Integer.parseInt(tintN.substring(4, 6), 16))
              } catch {
                case e:Exception => e.printStackTrace
              }
            }
            if (tintN.matches("[a-fA-F0-9]{8}")) {
              try {
                tintC = new Color(
                  Integer.parseInt(tintN.substring(0, 2), 16),
                  Integer.parseInt(tintN.substring(2, 4), 16),
                  Integer.parseInt(tintN.substring(4, 6), 16),
                  Integer.parseInt(tintN.substring(6, 8), 16))
              } catch {
                case e:Exception => e.printStackTrace
              }
            }
            if (tintC == null) {
              try {
                tintC = Color.getColor(tintN)
              } catch {
                case e:Exception => e.printStackTrace
              }
            }
          }
          if (bg) {
            bgC = tintC
          } else if (tintC == null) {
            tints = tints + (tintN -> font)
          } else {
            tints = tints + (tintN -> MediaProvider.tint(font, tintC))
          }
        }
        if (!bg) { f = tints(tintN) }
        n = n2 + 1

      } else {
        val vall : Int = if (cs(n).toInt < F_CEIL) cs(n).toInt - F_BASE else F_ERR
        if (bgC != null) {
          g.setColor(bgC)
          g.fillRect(
            x + c * F_DISP_WIDTH - (if (n == 0) -2 else 1), y + r * F_HEIGHT - 2,
            F_DISP_WIDTH, F_HEIGHT)
        }
        g.drawImage(
          f,
          x + c * F_DISP_WIDTH, y + r * F_HEIGHT,
          x + (c + 1) * F_DISP_WIDTH, y + r * F_HEIGHT + F_HEIGHT,
          vall * F_WIDTH + IMG_OFFSET, 0,
          vall * F_WIDTH + F_DISP_WIDTH + IMG_OFFSET, F_HEIGHT,
          null)
        c = c + 1
        n = n + 1

      }
    }
  }

}
