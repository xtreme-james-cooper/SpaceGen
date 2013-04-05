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

import java.awt.image.BufferedImage
import src.util.MediaProvider

sealed abstract class StructureType(val name : String) {
  def getSprite : BufferedImage
}

sealed abstract class Standard(name : String, imageName : String) extends StructureType(name) {
  def getSprite : BufferedImage = MediaProvider.border(MediaProvider.getImage(imageName))
}

case object MILITARY_BASE extends Standard("military base", "structures/military_base")
case object MINING_BASE extends Standard("mining base", "structures/mining_base")
case object SCIENCE_LAB extends Standard("science lab", "structures/science_lab")
case object CITY extends Standard("city of spires", "structures/city_of_spires")
case object VAULT extends Standard("vast underground vault", "structures/vast_underground_vault")
case object PALACE extends Standard("grand palace", "structures/grand_palace")
case object MUSEUM extends Standard("vast museum", "structures/vast_museum")
case object ARCOLOGY extends Standard("complex of arcologies", "structures/complex_of_arcologies")
case object ORBITAL_ELEVATOR extends Standard("orbital elevator", "structures/orbital_elevator")
case object SKULL_PILE extends Standard("skull pile", "structures/skull_pile")

object Standard {

  val COLONY_ONLY : List[StructureType] = List(CITY, VAULT, PALACE, MUSEUM, ARCOLOGY, ORBITAL_ELEVATOR)

}

class SpecialStructureType(name : String) extends StructureType(name) {

  //TODO clean up
  def getSprite : BufferedImage = {
    for {
      p <- Prefix.values
      if p.specialStruct.isDefined && this == p.specialStruct.get
    } return MediaProvider.border(MediaProvider.getImage("structures/" + p.name.toLowerCase.replace(" ", "_")))

    for {
      b <- Base.values
      if this == b.specialStructure
    } {
      return MediaProvider.border(MediaProvider.getImage("structures/" + b.name.toLowerCase.replace(" ", "_")))
    }

    MediaProvider.border(MediaProvider.getImage("structures/building"))
  }

}

