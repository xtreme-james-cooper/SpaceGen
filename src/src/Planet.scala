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

import scala.util.Random
import src.util.Stage
import src.runner.Main
import src.util.Sprite
import src.util.PlanetSprite
import src.util.CivSprite
import src.models.Structure
import src.models.StructureType
import src.models.SCIENCE_LAB
import src.models.MINING_BASE
import src.models.MILITARY_BASE
import src.models.Stratum
import src.models.SpecialLifeform
import src.models.Ruin
import src.models.Remnant
import src.models.PlanetSpecial
import src.models.Plague
import src.models.LostArtefact
import src.models.Fossil
import src.models.Cataclysm
import src.models.Artefact
import java.awt.image.BufferedImage
import src.util.MediaProvider
import java.awt.Graphics2D
import java.awt.Color
import src.models.DisappearanceCause
import src.models.DisappearanceCause
import src.models.DisappearanceCause
import src.models.ByCataclysm
import src.models.ForReason
import src.models.Transcended

object Planet {

  val P_NAMES : List[String] = List(
    "Taranis", "Krantor", "Mycon", "Urbon", "Metatron", "Autorog", "Pastinakos", "Orra",
    "Hylon", "Wotan", "Erebus", "Regor", "Sativex", "Vim", "Freia", "Tabernak", "Helmettepol",
    "Lumen", "Atria", "Bal", "Orgus", "Hylus", "Jurvox", "Kalamis", "Ziggurat", "Xarlan",
    "Chroma", "Nid", "Mera")

  def getName(p : Int) : String =
    if (p % 2 == 0)
      P_NAMES(p % P_NAMES.length) + List(" I", " II", " III", " IV", " V", " VI")((p / 77 + 3) % 6)
    else
      "" + ('A' + (p + 5) % 7).toChar +
        List('u', 'e', 'y', 'o', 'i')((p + 2) % 5) +
        ('k' + (p / 3) % 4).toChar +
        List('u', 'e', 'i', 'o', 'a')((p / 2 + 1) % 5) +
        ('p' + (p / 2) % 9).toChar +
        List(" I", " II", " III", " IV", " V", " VI")((p / 4 + 3) % 6)

  def getLocation(r : Random, sg : SpaceGen) : (Int, Int) = {
    while (true) {
      val x : Int = r.nextInt(7)
      val y : Int = r.nextInt(7)
      if (sg.planets.forall(p => p.x != x || p.y != y))
        return (x, y)
    }
    (-1, -1)
  }

}

class Planet(r : Random, sg : SpaceGen) {

  val name : String = Planet.getName(Math.abs(r.nextInt))

  private var pollution : Int = 0
  var habitable : Boolean = false
  val evoNeeded : Int = 15000 + (if (r.nextInt(3) == 0) 0 else 1000000)
  var evoPoints : Int = -evoNeeded
  var specials : List[PlanetSpecial] = Nil
  var lifeforms : List[SpecialLifeform] = Nil
  var inhabitants : List[Population] = Nil
  var artefacts : List[Artefact] = Nil
  var owner : Option[Civ] = None
  var structures : List[Structure] = Nil
  var plagues : List[Plague] = Nil

  var strata : List[Stratum] = Nil
  def strataRemove(s : Stratum) : Unit = strata = strata.filter(_ != s)

  val (x, y) : (Int, Int) = Planet.getLocation(r, sg)

  val sprite : PlanetSprite = new PlanetSprite(this)

  def getPollution : Int = pollution

  def setPollution(p : Int) : Unit = {
    pollution = p
    Main.animate(Stage.change(sprite, getSprite))
  }

  def addPlague(p : Plague) : Unit = {
    plagues = plagues ++ List(p)
    val ps : Sprite = new Sprite(p.getSprite, (plagues.size - 1) * 36, 36 * 4)
    sprite.plagueSprites = sprite.plagueSprites + (p -> ps)
    Main.animate(Stage.tracking(sprite, Stage.delay(10)))
    Main.animate(Stage.add(ps, Some(sprite)))
  }

  def removePlague(p : Plague) : Unit = {
    val sIndex : Int = plagues.indexOf(p)
    val ss : Sprite = sprite.plagueSprites.get(p).get //TODO get
    Main.animate(Stage.tracking(sprite, Stage.delay(10)))
    Main.animate(Stage.remove(ss))
    for (i <- sIndex + 1 until plagues.size) {
      Main.add(Stage.move(sprite.plagueSprites.get(plagues(i)).get, (i - 1) * 36, 36 * 2)) //TODO get
    }
    Main.animate
    plagues = plagues.filter(_ != p)
    sprite.plagueSprites = sprite.plagueSprites - p
  }

  def clearPlagues : Unit = {
    for (p <- plagues) {
      Main.add(Stage.remove(sprite.plagueSprites.get(p).get)) //TODO get
    }
    Main.animate
    plagues = Nil
    sprite.plagueSprites = Map()
  }

  def addArtefact(a : Artefact) : Unit = {
    artefacts = artefacts ++ List(a)
    val as : Sprite = new Sprite(a.getSprite, (artefacts.size - 1) * 36, 36)
    sprite.artefactSprites = sprite.artefactSprites + (a -> as)
    Main.animate(Stage.tracking(sprite, Stage.delay(10)))
    Main.animate(Stage.add(as, Some(sprite)))
  }

  def moveArtefact(a : Artefact, dst : Planet) : Unit = {
    val as : Sprite = sprite.artefactSprites.get(a).get //TODO get
    Main.animate(Stage.emancipate(as))
    Main.animate(Stage.tracking(as, Stage.move(as, dst.sprite.x + dst.artefacts.size * 36, dst.sprite.y + 36)))
    Main.animate(Stage.subordinate(as, dst.sprite))
    dst.sprite.artefactSprites = dst.sprite.artefactSprites + (a -> as) //TODO get
    dst.artefacts = dst.artefacts ++ List(a)
    val aIndex : Int = artefacts.indexOf(a)
    for (i <- aIndex + 1 until artefacts.size) {
      Main.add(Stage.move(sprite.artefactSprites.get(artefacts(i)).get, (i - 1) * 36, 36)) //TODO get
    }
    Main.animate
    artefacts = artefacts.filter(_ != a)
    sprite.artefactSprites = sprite.artefactSprites - a
  }

  def removeArtefact(a : Artefact) : Unit = {
    val aIndex : Int = artefacts.indexOf(a)
    val as : Sprite = sprite.artefactSprites.get(a).get //TODO get
    Main.animate(Stage.tracking(sprite, Stage.delay(10)))
    Main.animate(Stage.remove(as))
    for (i <- aIndex + 1 until artefacts.size) {
      Main.add(Stage.move(sprite.artefactSprites.get(artefacts(i)).get, (i - 1) * 36, 36)) //TODO get
    }
    Main.animate
    artefacts = artefacts.filter(_ != a)
    sprite.artefactSprites = sprite.artefactSprites - a
  }

  def clearArtefacts : Unit = {
    for (a <- artefacts) {
      Main.add(Stage.remove(sprite.artefactSprites.get(a).get)) //TODO get
    }
    Main.animate
    artefacts = Nil
    sprite.artefactSprites = Map()
  }

  def addStructure(s : Structure) : Unit = {
    structures = structures ++ List(s)
    val ss : Sprite = new Sprite(s.typ.getSprite, (structures.size - 1) * 36, 36 * 2)
    sprite.structureSprites = sprite.structureSprites + (s -> ss)
    Main.animate(Stage.tracking(sprite, Stage.delay(10)))
    Main.animate(Stage.add(ss, Some(sprite)))
  }

  def removeStructure(s : Structure) : Unit = {
    val sIndex : Int = structures.indexOf(s)
    val ss : Sprite = sprite.structureSprites.get(s).get //TODO get
    Main.animate(Stage.tracking(sprite, Stage.delay(10)))
    Main.animate(Stage.remove(ss))
    for (i <- sIndex + 1 until structures.size) {
      Main.add(Stage.move(sprite.structureSprites.get(structures(i)).get, (i - 1) * 36, 36 * 2)) //TODO get
    }
    Main.animate
    structures = structures.filter(_ != s)
    sprite.structureSprites = sprite.structureSprites - s
  }

  def clearStructures : Unit = {
    for (s <- structures) {
      Main.add(Stage.remove(sprite.structureSprites.get(s).get)) //TODO get
    }
    Main.animate
    structures = Nil
    sprite.structureSprites = Map()
  }

  def addLifeform(slf : SpecialLifeform) : Unit = {
    lifeforms = lifeforms ++ List(slf)
    val slfs : Sprite = new Sprite(slf.getSprite, (lifeforms.size - 1) * 36, 36 * 3)
    sprite.lifeformSprites = sprite.lifeformSprites + (slf -> slfs)
    Main.animate(Stage.tracking(sprite, Stage.delay(10)))
    Main.animate(Stage.add(slfs, Some(sprite)))
  }

  def removeLifeform(slf : SpecialLifeform) : Unit = {
    val lfIndex : Int = lifeforms.indexOf(slf)
    val slfs : Sprite = sprite.lifeformSprites.get(slf).get //TODO get
    Main.animate(Stage.tracking(sprite, Stage.delay(10)))
    Main.animate(Stage.remove(slfs))
    for (i <- lfIndex + 1 until lifeforms.size) {
      Main.add(Stage.move(sprite.lifeformSprites.get(lifeforms(i)).get, (i - 1) * 36, 36 * 3)) //TODO get
    }
    Main.animate
    lifeforms = lifeforms.filter(_ != slf)
    sprite.lifeformSprites = sprite.lifeformSprites - slf
  }

  def clearLifeforms : Unit = {
    for (slf <- lifeforms) {
      Main.add(Stage.remove(sprite.lifeformSprites.get(slf).get)) //TODO get
    }
    Main.animate
    lifeforms = Nil
    sprite.lifeformSprites = Map()
  }

  def setOwner(newOwner : Option[Civ]) : Unit = {
    owner match {
      case Some(o) => {
        o.sprites = o.sprites.filterNot(_ == sprite.ownerSprite)
        Main.animate(Stage.tracking(sprite, Stage.remove(sprite.ownerSprite)))
        sprite.ownerSprite = null
      }
      case None => ()
    }
    owner = newOwner
    newOwner match {
      case Some(o) => {
        sprite.ownerSprite = new CivSprite(o, false)
        o.sprites = o.sprites :+ sprite.ownerSprite
        sprite.ownerSprite.init
        Main.animate(Stage.tracking(sprite, Stage.add(sprite.ownerSprite, Some(sprite))))
      }
      case None => ()
    }
    Main.animate(Stage.delay(10))
  }

  def dePop(pop : Population, time : Int, cause : DisappearanceCause) : Unit = {
    strata = strata ++ List(new Remnant(pop, time, cause))
    pop.eliminate
    for (p <- plagues) {
      if ((inhabitants.map(_ typ).toSet & p.affects.toSet).isEmpty)
        removePlague(p)
    }
  }

  def darkAge(time : Int) : Unit = {
    for (s <- structures) {
      strata = strata ++ List(new Ruin(s, time, ForReason("during the collapse of the " + owner.get.name))) //TODO get
    }
    clearStructures
    for (a <- artefacts) {
      strata = strata ++ List(new LostArtefact("lost", time, a))
    }
    clearArtefacts

    setOwner(None)
  }

  def transcend(time : Int) : Unit =
    owner match {
      case Some(o) => {
        for (p <- inhabitants) {
          strata = strata ++ List(new Remnant(p, time, Transcended))
          p.eliminate
        }
        for (s <- structures) {
          strata = strata ++ List(new Ruin(s, time, ForReason("after the transcendence of the " + o.name))) //TDOD move to real transcended
        }
        clearStructures
        for (a <- artefacts) {
          strata = strata ++ List(new LostArtefact("lost and buried when the " + o.name + " transcended", time, a))
        }
        clearArtefacts
        clearPlagues
        setOwner(None)
      }
      case None => ()
    }

  def deCiv(time : Int, cause : DisappearanceCause) : Unit = {
    if (owner.isDefined) {
      setOwner(None)
    }
    for (p <- inhabitants) {
      dePop(p, time, cause)
    }
    for (s <- structures) {
      strata = strata ++ List(new Ruin(s, time, cause))
    }
    clearStructures
    for (a <- artefacts) {
      strata = strata ++ List(new LostArtefact("buried", time, a))
    }
    clearArtefacts
  }

  def deLive(time : Int, cause : DisappearanceCause) : Unit = {
    Main.animate(Stage.tracking(sprite, Stage.delay(10)))
    deCiv(time, cause)
    evoPoints = 0
    val cat : Option[Cataclysm] = cause match {
      case ByCataclysm(c) => Some(c)
      case _              => None
    }
    for (slf <- lifeforms) {
      strata = strata ++ List(new Fossil(slf, time, cat))
    }
    clearPlagues
    clearLifeforms
    habitable = false
    Main.animate(Stage.tracking(sprite, Stage.change(sprite, getSprite)))
  }

  def has(st : StructureType) : Boolean = structures.exists(s => s.typ == st)

  def isOutpost : Boolean = has(MILITARY_BASE) || has(SCIENCE_LAB) || has(MINING_BASE)

  def population : Int = inhabitants.foldRight(0)((p, n) => n + p.size)

  def fullDesc(sg : SpaceGen) : String = {
    var sb : String = name.toUpperCase + "\nA " + (if (habitable) "life-bearing " else "barren ") + "planet"
    sb = sb + (owner match {
      case Some(o) => " of the " + o.name
      case None    => ""
    }) + ".\n"
    for {
      ag <- sg.agents
      if ag.location == this
    } {
      sb = sb + ag.typ.describe(ag, sg) + "\n"
    }
    if (getPollution > 0) {
      sb = sb + "It is " + (getPollution match {
        case 1 => "a little"
        case 2 => "slightly"
        case 3 => "somewhat"
        case 4 => "heavily"
        case 5 => "very heavily"
        case _ => "incredibly"
      }) + " polluted.\n"
    }
    if (inhabitants.size > 0) {
      sb = sb + "It is populated by:\n"
      for (p <- inhabitants) {
        sb = sb + p + "\n"
      }
    }
    if (!plagues.isEmpty) {
      sb = sb + "Plagues:\n"
    }
    for (pl <- plagues) {
      sb = sb + pl.desc + "\n"
    }
    for (s <- structures) {
      sb = sb + "A " + s + "\n"
    }
    for (art <- artefacts) {
      sb = sb + "A " + art + "\n"
    }
    for (ps <- specials) {
      sb = sb + ps.explanation + "\n"
    }
    if (!lifeforms.isEmpty) {
      sb = sb + "Lifeforms of note:\n"
    }
    for (ps <- lifeforms) {
      sb = sb + ps.name + ": " + ps.desc + "\n"
    }
    if (!strata.isEmpty) {
      sb = sb + "Strata:\n"
      for (i <- strata.size - 1 to 0 by -1) {
        sb = sb + strata(i) + "\n"
      }
    }

    sb
  }

  def getSprite : BufferedImage = {
    val img : BufferedImage =
      if (specials.isEmpty)
        MediaProvider.getImage("planets/planet")
      else
        MediaProvider.getImage("planets/" + specials.head.name.toLowerCase)

    val img2 : BufferedImage =
      if (habitable)
        MediaProvider.tint(img, new Color(0, 255, 0, 63))
      else
        MediaProvider.scale(img, 32) // just copies really

    if (getPollution > 0) {
      val pollImg : BufferedImage = MediaProvider.tint(img, new Color(111, 88, 63, 220))
      val g : Graphics2D = img2.createGraphics
      val amt : Int = Math.max(0, 32 - getPollution * 5)
      g.drawImage(pollImg, 0, amt, 32, 32, 0, amt, 32, 32, null)
    }

    MediaProvider.scale(img2, 160)
  }

}
