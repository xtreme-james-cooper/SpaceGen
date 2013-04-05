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

package src.models

import src.util.Names
import src.SentientType
import src.SpaceGen
import java.awt.image.BufferedImage
import src.util.MediaProvider

object Plague {

  val varieties : List[String] = List("Rot", "Death", "Plague", "Fever", "Wasting", "Pox")

}

class Plague(
  val name : String,
  val lethality : Int,
  val mutationRate : Int,
  val transmissivity : Int,
  val curability : Int,
  val color : String,
  var affects : List[SentientType]) {

  def this(color : String, sg : SpaceGen) = this(
    color + " " + sg.pick(Plague.varieties),
    sg.d(9),
    sg.d(3),
    sg.d(3),
    sg.d(3),
    color,
    Nil)

  def this(sg : SpaceGen) = this(sg.pick(Names.COLORS), sg)

  def this(plague : Plague) = this(
    plague.name,
    plague.lethality,
    plague.mutationRate,
    plague.transmissivity,
    plague.curability,
    plague.color,
    plague.affects)

  def desc : String = name + ", which affects " +
    (for (i <- 0 until affects.length) yield (if (i > 0 && i == affects.length - 1) " and " else if (i > 0) ", " else "") + affects(i).getName).mkString

  def getSprite : BufferedImage = MediaProvider.border(MediaProvider.tint(MediaProvider.getImage("misc/plague"), MediaProvider.TINTS(color)))
    
}
