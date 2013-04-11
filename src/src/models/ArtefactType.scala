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

import src.Civ
import src.SpaceGen
import src.Agent
import java.awt.image.BufferedImage
import src.util.MediaProvider
import java.awt.Graphics2D
import java.awt.Transparency
import java.awt.image.WritableRaster
import java.awt.Color
import scala.util.Random

sealed abstract class ArtefactType(val getName : String, val imageName : String) {
  def getSprite(contained : Option[Container]) : BufferedImage = MediaProvider.getImage(imageName)
}

case object TIME_ICE extends ArtefactType("Block of Time Ice", "artefacts/time_ice") {
  override def getSprite(contained : Option[Container]) : BufferedImage = {
    val img : BufferedImage = MediaProvider.scale(super.getSprite(contained), 32)
    val g : Graphics2D = img.createGraphics
    contained match {
      case Some(c) => g.drawImage(c.getSprite, 2, 2, 28, 28, 2, 2, 28, 28, null)
      case None    => ()
    }
    g.drawImage(MediaProvider.getImage("artefacts/time_ice_overlay", Transparency.TRANSLUCENT), 0, 0, null)
    img
  }
}
case object WRECK extends ArtefactType("Spaceship Wreck", "artefacts/wreck")
case object PIRATE_HOARD extends ArtefactType("Pirate Hoard", "artefacts/hoard")
case object PIRATE_TOMB extends ArtefactType("Pirate Tomb", "artefacts/tomb")
case object ADVENTURER_TOMB extends ArtefactType("Tomb", "artefacts/tomb")

sealed abstract class Device(name : String, imageName : String) extends ArtefactType(name, imageName) {
  def create(actor : Civ, sg : SpaceGen) = name
}

case object TELEPORT_GATE extends Device("Teleport Gate", "artefacts/teleport_gate")
case object PLANET_DESTROYER extends Device("Planet Destroyer", "artefacts/planet_destroyer")
case object MIND_CONTROL_DEVICE extends Device("Mind Control Device", "artefacts/mind_control_device")
case object MIND_READER extends Device("Mind Reader", "artefacts/mind_reader")
case object MASTER_COMPUTER extends Device("Master Computer", "artefacts/master_computer")
case object YOUTH_SERUM extends Device("Youth Serum", "artefacts/youth_serum")
case object STASIS_CAPSULE extends Device("Stasis Capsule", "artefacts/stasis_capsule")
case object TIME_MACHINE extends Device("Time Machine", "artefacts/time_machine")
case object LIVING_WEAPON extends Device("Living Weapon", "artefacts/living_weapon")
case object MIND_ARCHIVE extends Device("Mind Archive", "artefacts/mind_archive")
case object UNIVERSAL_NUTRIENT extends Device("Universal Nutrient", "artefacts/universal_nutrient")
case object VIRTUAL_REALITY_MATRIX extends Device("Virtual Reality Matrix", "artefacts/virtual_reality_matrix")
case object UNIVERSAL_ANTIDOTE extends Device("Universal Antidote", "artefacts/universal_antidote")
case object ARTIFICIAL_PLAGUE extends Device("Artificial Plague", "artefacts/artificial_plague")
case object KILLER_MEME extends Device("Killer Meme", "artefacts/killer_meme")
case object UNIVERSAL_COMPUTER_VIRUS extends Device("Universal Computer Virus", "artefacts/universal_computer_virus")

object Device {

  val values : List[Device] = List(
    TELEPORT_GATE, PLANET_DESTROYER, MIND_CONTROL_DEVICE, MIND_READER, MASTER_COMPUTER, YOUTH_SERUM, STASIS_CAPSULE,
    TIME_MACHINE, LIVING_WEAPON, MIND_ARCHIVE, UNIVERSAL_NUTRIENT, VIRTUAL_REALITY_MATRIX, UNIVERSAL_ANTIDOTE,
    ARTIFICIAL_PLAGUE, KILLER_MEME, UNIVERSAL_COMPUTER_VIRUS)

}

sealed abstract class Art(name : String, longName : String, imageName : String) extends ArtefactType(name, imageName) {

  def create(actor : Civ, sg : SpaceGen) : Artefact = {
    val art : Artefact = new Artefact(sg.year, Some(actor), this, "")

    val agent : Option[Agent] = sg.pickMaybe(sg.agents)
    val sent = sg.pick(actor.fullMembers)
    val arts : List[Artefact] = for {
      p <- actor.colonies
      a <- p.artefacts
    } yield a
    val artefact : Option[Artefact] = sg.pickMaybe(arts)

    val roll : Int = sg.d(7)
    art.contained = Some(roll match {
      case 0 | 1 if agent.isDefined    => ContainedAgent(agent.get)
      case 4 | 5 if artefact.isDefined => ContainedArtefact(artefact.get)
      case _                           => ContainedST(sent)
    })
    art.desc = longName + (roll match {
      case 0 | 1 if agent.isDefined    => " " + agent.get.name
      case 0 | 1 | 2                   => actor.govt.maleLeader + actor.name
      case 3                           => actor.govt.femaleLeader + actor.name
      case 4 | 5 if artefact.isDefined => " " + artefact.get
      case 4 | 5 | 6                   => " " + sent.name
    })
    art
  }

}

object Art {

  val values : List[Art] = List(STATUE, PAINTING, HOLOGRAM, FILM, HYMN)

}

case object STATUE extends Art("statue", "statue of", "artefacts/statue") {
  override def getSprite(contained : Option[Container]) : BufferedImage = contained match {
    case Some(c) => {
      val img : BufferedImage = c.getSprite
      val r : Random = new Random
      val stat : BufferedImage = MediaProvider.createImage(32, 32, Transparency.BITMASK)
      val src : BufferedImage = new BufferedImage(32, 32, BufferedImage.TYPE_INT_ARGB)
      src.createGraphics.drawImage(img, 0, 0, null)
      val ar : WritableRaster = src.getAlphaRaster
      for {
        y <- 0 until 32
        x <- 0 until 32
        if ar.getSample(x, y, 0) != 0
      } {
        var c : Color = new Color(src.getRGB(x, y))
        var intensity : Int = (c.getRed + c.getGreen + c.getBlue + r.nextInt(100) - 50) / 3
        if (intensity < 0) intensity = 0
        if (intensity > 255) intensity = 255
        c = new Color(intensity, intensity, intensity, c.getAlpha)
        stat.setRGB(x, y, c.getRGB)
      }
      stat
    }
    case None => super.getSprite(contained)
  }

}
case object PAINTING extends Art("painting", "painting of", "artefacts/painting") {
  override def getSprite(contained : Option[Container]) : BufferedImage = contained match {
    case Some(c) => {
      val img : BufferedImage = MediaProvider.scale(super.getSprite(contained), 32)
      val g : Graphics2D = img.createGraphics
      val port : BufferedImage = c.getSprite
      g.drawImage(port, 3, 5, 29, 27, 3, 0, 29, 22, null)
      img
    }
    case None => super.getSprite(contained)
  }

}
case object HOLOGRAM extends Art("hologram", "hologram of", "artefacts/hologram") {
  override def getSprite(contained : Option[Container]) : BufferedImage = contained match {
    case Some(c) => {
      val img : BufferedImage = c.getSprite
      val hol : BufferedImage = MediaProvider.createImage(32, 32, Transparency.BITMASK)
      val src : BufferedImage = new BufferedImage(32, 32, BufferedImage.TYPE_INT_ARGB)
      src.createGraphics.drawImage(img, 0, 0, null)
      val ar : WritableRaster = src.getAlphaRaster
      for {
        y <- 0 until 32
        x <- 0 until 32
        if ar.getSample(x, y, 0) != 0
      } {
        var c : Color = new Color(src.getRGB(x, y))
        if (y % 2 == 1) {
          c = new Color(c.getRed / 2, c.getGreen / 2, c.getBlue / 2, c.getAlpha)
        }
        hol.setRGB(x, y, c.getRGB)
      }
      MediaProvider.tint(hol, new Color(100, 220, 180, 200))
    }
    case None => super.getSprite(contained)
  }
}
case object FILM extends Art("film", "film about", "artefacts/film") {
  override def getSprite(contained : Option[Container]) : BufferedImage = contained match {
    case Some(c) => {
      val img : BufferedImage = MediaProvider.scale(super.getSprite(contained), 32)
      val g : Graphics2D = img.createGraphics
      val port : BufferedImage = c.getSprite
      c match {
        case ContainedST(_) => {
          g.drawImage(port, 8, 2, 24, 14, 8, 2, 24, 14, null)
          g.drawImage(port, 8, 16, 24, 28, 8, 2, 24, 14, null)
        }
        case _ => {
          g.drawImage(port, 10, 2, 22, 14, 0, 0, 32, 32, null)
          g.drawImage(port, 10, 16, 22, 28, 0, 0, 32, 32, null)
        }
      }
      img
    }
    case None => super.getSprite(contained)
  }
}
case object HYMN extends Art("hymn", "hymn about", "artefacts/hymn")

