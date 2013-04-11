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

  val PARASITE : SentientType =
    new SentientType(0, null /* TODO null origin? */ , None, PARASITES, None,
      None, "insidious", "strive to dominate all sentient life", None, Set(PARASITES.specialStructure), Nil)

  def invent(sg : SpaceGen, creator : Option[Civ], p : Planet, specialOrigin : Option[String]) : SentientType = {
    var st : SentientType = null
    do {
      st = invent2(sg, creator, p, specialOrigin)
    } while (sg.historicalSentientNames.contains(st.name))
    sg.historicalSentientNames = sg.historicalSentientNames + st.name
    st
  }

  def invent2(sg : SpaceGen, creator : Option[Civ], p : Planet, newSpecialOrigin : Option[String]) : SentientType = {
    val base : Base = sg.pick(Base.values.filter(_ evolvable))
    val color : Option[String] = if (sg.coin && base != HUMANOIDS) Some(sg.pick(Names.COLORS)) else None
    val postfix : Option[Postfix] = if (sg.p(5)) Some(sg.pick(Postfix.values)) else None
    val st : SentientType =
      new SentientType(sg.year, p, creator, base, color, postfix, sg.pick(PERSONALITY), sg.pick(GOAL), newSpecialOrigin, Set(base.specialStructure), Nil)

    if (st.color.isEmpty || sg.p(3)) {
      st.prefixes = List(sg.pick(Prefix.values))
      if (st.prefixes.head.specialStruct.isDefined) {
        st.specialStructures__ = st.specialStructures__ ++ List(st.prefixes.head.specialStruct.get)
      }
    }

    st
  }

  def genRobots(sg : SpaceGen, creator : Option[Civ], p : Planet, specialOrigin : Option[String]) : SentientType = {
    var st : SentientType = null
    do {
      st = genRobots2(sg, creator, p, specialOrigin)
    } while (sg.historicalSentientNames.contains(st.name))
    sg.historicalSentientNames = sg.historicalSentientNames + st.name
    st
  }

  def genRobots2(sg : SpaceGen, creator : Option[Civ], p : Planet, newSpecialOrigin : Option[String]) : SentientType = {
    val color : Option[String] = if (sg.coin) Some(sg.pick(Names.COLORS)) else None
    val postfix : Option[Postfix] = if (sg.p(5)) Some(sg.pick(Postfix.values)) else None
    val st : SentientType =
      new SentientType(sg.year, p, creator, ROBOTS, color, postfix,
        sg.pick(PERSONALITY), sg.pick(GOAL), newSpecialOrigin, Set(ROBOTS.specialStructure), List(sg.pick(Prefix.values)))
    if (st.prefixes.head.specialStruct.isDefined) {
      st.specialStructures__ = st.specialStructures__ + st.prefixes.head.specialStruct.get
    }

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

class SentientType(
  birth : Int,
  evolvedLoc : Planet,
  creators : Option[Civ],
  val base : Base,
  var color : Option[String],
  var postfix : Option[Postfix],
  var personality : String,
  var goal : String,
  specialOrigin : Option[String],
  var specialStructures__ : Set[StructureType],
  var prefixes : List[Prefix]) {

  override def equals(o2 : Any) : Boolean = o2.isInstanceOf[SentientType] && o2.asInstanceOf[SentientType].name == name

  override def hashCode : Int = name.hashCode

  def mutate(sg : SpaceGen, specialOrigin : Option[String]) : SentientType = {
    var st : SentientType = null
    do {
      st = mutate2(sg, specialOrigin)
    } while (sg.historicalSentientNames.contains(st.name))
    st
  }

  def mutate2(sg : SpaceGen, newSpecialOrigin : Option[String]) : SentientType = {
    val so : Option[String] = Some(newSpecialOrigin.getOrElse("They developed from " + name + " in " + sg.year + "."))
    val st2 : SentientType =
      new SentientType(birth, evolvedLoc, creators, base, color, postfix, personality, goal, so, Set(base.specialStructure), prefixes)

    while (st2.name == name) {
      sg.d(3) match {
        case 0 if st2.base != HUMANOIDS => st2.color = Some(sg.pick(Names.COLORS))
        case 0                          => ()
        case 1                          => st2.prefixes = List(sg.pick(Prefix.values))
        case 2                          => st2.postfix = Some(sg.pick(Postfix.values))
      }
      if (sg.p(3)) st2.personality = sg.pick(SentientType.PERSONALITY)
      if (sg.p(3)) st2.goal = sg.pick(SentientType.GOAL)
    }

    st2
  }

  def name : String = {
    var sb : String = ""
    for (pf <- prefixes) {
      sb = sb + pf.name + " "
    }
    sb + (color match {
      case Some(col) => col + " "
      case None      => ""
    }) + base.name + (postfix match {
      case Some(post) => " " + post.name
      case None       => ""
    })
  }

  def getDesc : String = {
    var sb : String = base.desc
    // Skin
    sb = sb + (color match {
      case Some(col) => " They have " + col.toLowerCase + (
        if (prefixes.contains(FEATHERED)) " feathers."
        else if (prefixes.contains(SCALY)) " scales."
        else " skin.")
      case None => ""
    })
    // LIMBS!
    if (prefixes.contains(SIX_LEGGED)) sb = sb + " They have six legs."
    if (prefixes.contains(FOUR_ARMED)) sb = sb + " They have four arms."
    if (prefixes.contains(TWO_HEADED)) sb = sb + " They have two heads that tend to constantly bicker with one another."

    sb = sb + " They have " + (if (postfix == Some(S_3)) "trilateral" else if (postfix == Some(S_5)) "five-fold" else "bilateral") + " symmetry"

    sb = sb + (
      if (prefixes.contains(SLIM))
        " and a slim shape."
      else if (prefixes.contains(AMORPHOUS))
        " and are able to greatly alter their shape."
      else
        ".")

    if (postfix == Some(EYES)) sb = sb + " Their giant eyes take up most of their head."
    if (postfix == Some(TAILS)) sb = sb + " They use their long tails for balance."
    if (prefixes.contains(FLYING)) sb = sb + " They can fly."
    if (prefixes.contains(TELEPATHIC)) sb = sb + " They can read each others' minds."
    if (prefixes.contains(IMMORTAL)) sb = sb + " They have no fixed lifespan and only die of disease or accidents."

    sb = sb + " They are " + personality + " and " + goal + "."

    sb = sb + (specialOrigin match {
      case Some(so) => " " + so
      case None =>
        if (evolvedLoc != null)
          creators match {
            case Some(cre) if base == ROBOTS => " They were created by the " + cre.name + " on " + evolvedLoc.name + " as servants in " + birth + "."
            case Some(cre)                   => " They were uplifted by the " + cre.name + " on " + evolvedLoc.name + " in " + birth + "."
            case None                        => " They first evolved on " + evolvedLoc.name + " in " + birth + "."
          }
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
      if (postfix == Some(TAILS)) g.drawImage(MediaProvider.getImage("sentients/tail"), 0, 0, null)
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

      if (postfix == Some(EYES)) g.drawImage(MediaProvider.getImage("sentients/giant_eyes"), 0, 0, null)

      img = MediaProvider.tint(img, new Color(255, 255, 255, 31))
      if (specialColor != null)
        img = MediaProvider.tint(img, MediaProvider.TINTS(specialColor))
      else
        color match {
          case Some(col) => img = MediaProvider.tint(img, MediaProvider.TINTS(col))
          case None      => ()
        }

      if (eyepatch) {
        g = img.createGraphics
        g.drawImage(MediaProvider.getImage("agents/eyepatch"), 0, 0, null)
      }

      if (postfix == Some(S_3) || postfix == Some(S_5)) {
        val img2 : BufferedImage = MediaProvider.createImage(32, 32, Transparency.BITMASK)
        val g2 : Graphics2D = img2.createGraphics
        g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR)
        val symm : Int = if (postfix == Some(S_3)) 3 else 5
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

  		