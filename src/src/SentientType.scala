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

package src

import src.util.Names
import src.models.StructureType
import src.models.PARASITES
import src.models.Base
import src.models.Postfix
import src.models.HUMANOIDS
import src.models.Prefix
import src.models.FLYING
import src.models.SIX_LEGGED
import src.models.TWO_HEADED
import src.models.SCALY
import src.models.FEATHERED
import src.models.FOUR_ARMED
import src.models.AMORPHOUS
import src.models.EYES
import src.models.TELEPATHIC
import src.models.TAILS
import src.models.SLIM
import src.models.S_5
import src.models.S_3
import src.models.IMMORTAL
import src.models.ROBOTS
import java.awt.image.BufferedImage
import src.util.MediaProvider
import java.awt.Transparency
import java.awt.Graphics2D
import java.awt.RenderingHints
import java.awt.Color
import src.models.TINY
import src.models.GIANT

object SentientType {

  val PARASITE : SentientType = new SentientType
  PARASITE.base = PARASITES
  PARASITE.name = PARASITE.mkName
  PARASITE.specialStructures = PARASITE.specialStructures ++ List(PARASITES.specialStructure)
  PARASITE.personality = "insidious"
  PARASITE.goal = "strive to dominate all sentient life"

  def invent(sg : SpaceGen, creator : Civ, p : Planet, specialOrigin : Option[String]) : SentientType = {
    var st : SentientType = null
    do {
      st = invent2(sg, creator, p, specialOrigin)
    } while (sg.historicalSentientNames.contains(st.name))
    sg.historicalSentientNames = sg.historicalSentientNames + st.name
    st
  }

  def invent2(sg : SpaceGen, creator : Civ, p : Planet, newSpecialOrigin : Option[String]) : SentientType = {
    val st : SentientType = new SentientType
    val bss : List[Base] = (for {
      b <- Base.values
      if b.evolvable
    } yield b) //TODO rip out?
    st.creators = creator
    st.evolvedLoc = p
    st.birth = sg.year
    st.base = sg.pick(bss)
    st.specialStructures = st.specialStructures ++ List(st.base.specialStructure)
    if (sg.coin && st.base != HUMANOIDS) st.color = sg.pick(Names.COLORS)

    if (st.color == null || sg.p(3)) {
      st.prefixes = st.prefixes ++ List(sg.pick(Prefix.values))
      if (st.prefixes.head.specialStruct.isDefined) {
        st.specialStructures = st.specialStructures ++ List(st.prefixes.head.specialStruct.get)
      }
    }
    if (sg.p(5)) st.postfix = sg.pick(Postfix.values)

    st.personality = sg.pick(PERSONALITY)
    st.goal = sg.pick(GOAL)
    st.name = st.mkName
    st.specialOrigin = newSpecialOrigin
    st
  }

  def genRobots(sg : SpaceGen, creator : Civ, p : Planet, specialOrigin : Option[String]) : SentientType = {
    var st : SentientType = null
    do {
      st = genRobots2(sg, creator, p, specialOrigin)
    } while (sg.historicalSentientNames.contains(st.name))
    sg.historicalSentientNames = sg.historicalSentientNames + st.name
    st
  }

  def genRobots2(sg : SpaceGen, creator : Civ, p : Planet, newSpecialOrigin : Option[String]) : SentientType = {
    val st : SentientType = new SentientType
    st.creators = creator
    st.evolvedLoc = p
    st.birth = sg.year
    st.base = ROBOTS
    st.specialStructures = st.specialStructures ++ List(st.base.specialStructure)
    if (sg.coin && st.base != HUMANOIDS) {
      st.color = sg.pick(Names.COLORS)
    }
    st.prefixes = st.prefixes ++ List(sg.pick(Prefix.values))
    if (st.prefixes.head.specialStruct.isDefined) {
      st.specialStructures = st.specialStructures ++ List(st.prefixes.head.specialStruct.get)
    }
    if (sg.p(5)) {
      st.postfix = sg.pick(Postfix.values)
    }
    st.personality = sg.pick(PERSONALITY)
    st.goal = sg.pick(GOAL)
    st.name = st.mkName
    st.specialOrigin = newSpecialOrigin
    st
  }

  val PERSONALITY : List[String] = List(
    "cautious",
    "violent",
    "cowardly",
    "peaceful",
    "humorless",
    "hyperactive",
    "gregarious",
    "reclusive",
    "meditative",
    "fond of practical jokes",
    "modest",
    "thrill-seeking")

  val GOAL : List[String] = List(
    "deeply religious",
    "value personal integrity above all else",
    "have a complex system of social castes",
    "strive to uphold their ideals of personal honour",
    "deeply rationalist",
    "unwilling to compromise",
    "always willing to listen",
    "have a deep need to conform",
    "believe in the survival of the fittest",
    "believe in the inherent superiority of their species",
    "believe that each individual must serve the community",
    "have a complex system of social rules")

}

class SentientType {

  var birth : Int = 0
  var evolvedLoc : Planet = null
  var creators : Civ = null
  var base : Base = null
  var specialStructures : List[StructureType] = Nil //Real
  var prefixes : List[Prefix] = Nil //Real
  var color : String = null
  var postfix : Postfix = null
  var cyborg : Boolean = false
  var personality : String = null
  var goal : String = null
  var name : String = null
  var specialOrigin : Option[String] = None

  override def equals(o2 : Any) : Boolean = o2.isInstanceOf[SentientType] && o2.asInstanceOf[SentientType].name == name

  override def hashCode : Int = name.hashCode

  def getName : String = name

  def mutate(sg : SpaceGen, specialOrigin : Option[String]) : SentientType = {
    var st : SentientType = null
    do {
      st = mutate2(sg, specialOrigin)
    } while (sg.historicalSentientNames.contains(st.name))
    st
  }

  def mutate2(sg : SpaceGen, newSpecialOrigin : Option[String]) : SentientType = {
    val st2 : SentientType = new SentientType
    st2.base = base
    st2.prefixes = st2.prefixes ++ prefixes
    st2.color = color
    st2.postfix = postfix
    st2.cyborg = cyborg
    st2.personality = personality
    st2.evolvedLoc = evolvedLoc
    st2.creators = creators
    st2.goal = goal
    st2.birth = birth

    while (st2.mkName == name) {
      sg.d(3) match {
        case 0 => if (st2.base != HUMANOIDS) {
          st2.color = sg.pick(Names.COLORS)
          if (sg.p(3)) st2.personality = sg.pick(SentientType.PERSONALITY)
          if (sg.p(3)) st2.goal = sg.pick(SentientType.GOAL)
        }
        case 1 => {
          st2.prefixes = List(sg.pick(Prefix.values))
          if (sg.p(3)) st2.personality = sg.pick(SentientType.PERSONALITY)
          if (sg.p(3)) st2.goal = sg.pick(SentientType.GOAL)
        }
        case 2 => {
          st2.postfix = sg.pick(Postfix.values)
          if (sg.p(3)) st2.personality = sg.pick(SentientType.PERSONALITY)
          if (sg.p(3)) st2.goal = sg.pick(SentientType.GOAL)
        }
      }
    }

    st2.specialStructures = st2.specialStructures ++ List(st2.base.specialStructure)
    st2.name = st2.mkName
    st2.specialOrigin = Some(newSpecialOrigin.getOrElse("They developed from " + name + " in " + sg.year + "."))
    st2
  }

  def mkName : String = {
    var sb : String = ""
    for (pf <- prefixes) {
      sb = sb + pf.name + " "
    }
    if (color != null) sb = sb + color + " "
    if (cyborg) sb = sb + "Cyborg "
    sb = sb + base.name
    if (postfix != null) sb = sb + " " + postfix.name
    sb
  }

  def getDesc : String = {
    var sb : String = base.desc
    // Skin
    sb = sb + (
      if (color != null)
        " They have " + color.toLowerCase + (
        if (prefixes.contains(FEATHERED)) " feathers."
        else if (prefixes.contains(SCALY)) " scales."
        else " skin.")
      else
        "")
    // LIMBS!
    if (prefixes.contains(SIX_LEGGED)) sb = sb + " They have six legs."
    if (prefixes.contains(FOUR_ARMED)) sb = sb + " They have four arms."
    if (prefixes.contains(TWO_HEADED)) sb = sb + " They have two heads that tend to constantly bicker with one another."

    sb = sb + " They have " + (if (postfix == S_3) "trilateral" else if (postfix == S_5) "five-fold" else "bilateral") + " symmetry"

    sb = sb + (
      if (prefixes.contains(SLIM))
        " and a slim shape."
      else if (prefixes.contains(AMORPHOUS))
        " and are able to greatly alter their shape."
      else
        ".")

    if (postfix == EYES) sb = sb + " Their giant eyes take up most of their head."
    if (postfix == TAILS) sb = sb + " They use their long tails for balance."
    if (prefixes.contains(FLYING)) sb = sb + " They can fly."
    if (prefixes.contains(TELEPATHIC)) sb = sb + " They can read each others' minds."
    if (prefixes.contains(IMMORTAL)) sb = sb + " They have no fixed lifespan and only die of disease or accidents."

    sb = sb + " They are " + personality + " and " + goal + "."

    sb = sb + (specialOrigin match {
      case Some(so) => " " + so
      case None =>
        if (evolvedLoc != null)
          if (base == ROBOTS)
            " They were created by the " + creators.name + " on " + evolvedLoc.name + " as servants in " + birth + "."
          else if (creators != null)
            " They were uplifted by the " + creators.name + " on " + evolvedLoc.name + " in " + birth + "."
          else
            " They first evolved on " + evolvedLoc.name + " in " + birth + "."
        else
          ""
    })

    sb
  }

  def getSprite(cage : Boolean, spear : Boolean) : BufferedImage = getSprite(false, null, cage, spear)

  def getSprite(eyepatch : Boolean, specialColor : String, cage : Boolean, spear : Boolean) : BufferedImage = {
    if (base == PARASITES)
      MediaProvider.border(MediaProvider.getImage("sentients/parasites"))
    else {
      var img : BufferedImage = MediaProvider.createImage(32, 32, Transparency.BITMASK)
      var g : Graphics2D = img.createGraphics

      if (prefixes.contains(FLYING)) g.drawImage(MediaProvider.getImage("sentients/wings"), 0, 0, null)
      if (postfix == TAILS) g.drawImage(MediaProvider.getImage("sentients/tail"), 0, 0, null)
      if (prefixes.contains(TELEPATHIC)) g.drawImage(MediaProvider.getImage("sentients/telepathic"), 0, 0, null)

      val bodyType : String =
        if (prefixes.contains(AMORPHOUS)) "sentients/amorphous_body" else if (prefixes.contains(SLIM)) "sentients/slim_body" else "sentients/body"
      g.drawImage(MediaProvider.getImage(bodyType), 0, 0, null)

      val legType : String = if (prefixes.contains(SIX_LEGGED)) "sentients/6_legs" else "sentients/legs"
      g.drawImage(MediaProvider.getImage(legType), 0, 0, null)

      val armType : String = if (prefixes.contains(FOUR_ARMED)) "sentients/4_arms" else "sentients/arms"
      g.drawImage(MediaProvider.getImage(armType), 0, 0, null)

      if (prefixes.contains(SCALY)) g.drawImage(MediaProvider.getImage("sentients/scales"), 0, 0, null)
      else if (prefixes.contains(FEATHERED)) g.drawImage(MediaProvider.getImage("sentients/feathers"), 0, 0, null)

      if (prefixes.contains(TWO_HEADED)) {
        g.drawImage(MediaProvider.getImage("sentients/" + base.name.toLowerCase), -6, 0, null)
        g.drawImage(MediaProvider.getImage("sentients/" + base.name.toLowerCase), 6, 0, null)
      } else
        g.drawImage(MediaProvider.getImage("sentients/" + base.name.toLowerCase), 0, 0, null)

      if (postfix == EYES) g.drawImage(MediaProvider.getImage("sentients/giant_eyes"), 0, 0, null)

      img = MediaProvider.tint(img, new Color(255, 255, 255, 31))
      if (specialColor != null)
        img = MediaProvider.tint(img, MediaProvider.TINTS(specialColor))
      else if (color != null)
        img = MediaProvider.tint(img, MediaProvider.TINTS(color))

      if (eyepatch) {
        g = img.createGraphics
        g.drawImage(MediaProvider.getImage("agents/eyepatch"), 0, 0, null)
      }

      if (postfix == S_3 || postfix == S_5) {
        val img2 : BufferedImage = MediaProvider.createImage(32, 32, Transparency.BITMASK)
        val g2 : Graphics2D = img2.createGraphics
        g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR)
        val symm : Int = if (postfix == S_3) 3 else 5
        for (j <- 0 until symm) {
          g2.translate(16, 16)
          g2.rotate(scala.math.Pi * 2 / symm)
          g2.translate(-16, -16)
          g2.drawImage(img, 0, 0, null)
        }
        img = img2
      }

      if (spear) {
        g = img.createGraphics
        g.drawImage(MediaProvider.getImage("sentients/spear"), 0, 0, null)
      }

      if (cage) {
        g = img.createGraphics
        g.drawImage(MediaProvider.getImage("sentients/cage"), 0, 0, null)
      }

      img = if (prefixes.contains(GIANT))
        MediaProvider.scale(img, 40)
      else if (prefixes.contains(TINY))
        MediaProvider.scale(img, 24)
      else
        img

      MediaProvider.border(img)
    }
  }

}

  		