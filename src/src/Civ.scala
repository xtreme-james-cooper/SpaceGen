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

import src.util.Stage
import src.runner.Main
import src.util.Names
import src.util.CivSprite
import src.models.Government
import src.models.Outcome
import src.models.PEACE
import src.models.WAR
import src.models.Artefact
import src.models.ArtefactType
import src.models.Device
import src.models.TELEPORT_GATE
import src.models.Base
import java.awt.image.BufferedImage
import src.util.MediaProvider
import java.awt.Graphics2D
import java.awt.Transparency
import src.util.Draw

class Civ(var birthYear : Int, var fullMembers : List[SentientType], home : Planet, var govt : Government, var resources : Int, sg : SpaceGen) {

  var relations : Map[Civ, Outcome] = Map()

  var number : Int = 0

  var sprites : List[CivSprite] = Nil

  var science : Int = 0
  var military : Int = 0
  var weapLevel : Int = 0
  var techLevel : Int = 0
  var name : String = null //Not real
  var nextBreakthrough : Int = 6
  var decrepitude : Int = 0

  setResources(resources)
  updateName
  home.setOwner(Some(this))
  setTechLevel(1)

  sg.civs = sg.civs + this
  
  def colonies : List[Planet] = sg.planets.filter(p => p.owner == Some(this))

  def setResources(newRess : Int) : Unit = {
    val newRes : Int = Math.max(0, newRess)
    val oldRes : Int = resources
    resources = newRes
    sprites.map(_ changeRes (oldRes, newRes))
    Main.animate
  }

  def setScience(sci : Int) : Unit = {
    val newSci : Int = Math.max(0, sci)
    val oldSci : Int = science
    science = newSci
    sprites.map(_ changeScience (oldSci, newSci))
    Main.animate
  }

  def setMilitary(mil : Int) : Unit = {
    val newMil : Int = Math.max(0, mil)
    val oldMil : Int = military
    military = newMil
    sprites.map(_ changeFleet (oldMil, newMil))
    Main.animate
  }

  def setTechLevel(newTechh : Int) : Unit = {
    techLevel = Math.max(0, newTechh)
    sprites.map(_ changeTech (techLevel))
    Main.animate
  }

  def setWeapLevel(wp : Int) : Unit = {
    weapLevel = Math.max(0, wp)
    sprites.map(_ changeMilTech (weapLevel))
    Main.animate
  }

  def leastPopulousFullColony : Planet = {
    var c : Planet = null
    var pop : Int = 0
    for {
      p <- fullColonies
      if c == null || p.population < pop
    } {
      c = p
      pop = p.population
    }
    c
  }

  def closestColony(p : Planet) : Planet = {
    var c : Planet = null
    var closestDist : Int = 0
    for (col <- fullColonies) {
      val dist = (p.x - col.x) * (p.x - col.x) + (p.y - col.y) * (p.y - col.y)
      if (dist < closestDist || c == null) {
        c = col
        closestDist = dist
      }
    }
    if (c == null)
      colonies.head
    else
      c
  }

  def reachables(sg : SpaceGen) : List[Planet] = {
    var range : Int = 3 + techLevel * techLevel
    if (has(TELEPORT_GATE)) range = 10000
    for {
      p <- sg.planets
      if (closestApproach(p) <= range)
    } yield p
  }

  private def closestApproach(p : Planet) : Int = {
    var closestR : Int = 100000
    for (c <- colonies) {
      val dist : Int = (p.x - c.x) * (p.x - c.x) + (p.y - c.y) * (p.y - c.y)
      closestR = Math.min(dist, closestR)
    }
    closestR
  }

  def has(at : ArtefactType) : Boolean = colonies.exists(c => c.artefacts.exists(a => a.typ == at))

  def use(at : ArtefactType) : Artefact = {
    for {
      c <- colonies
      a <- c.artefacts
      if (a.typ == at)
    } {
      c.artefacts = c.artefacts.filter(_ != a)
      return a
    }
    new Artefact(13, Some(this), at, "mysterious " + at.getName + "")
  }

  def relation(c : Civ) : Outcome = {
    if (!relations.contains(c)) {
      relations = relations + (c -> PEACE)
    }
    relations(c)
  }

  def population : Int = colonies.map(_ population).foldRight(0)(_ + _)

  def fullColonies : List[Planet] = colonies.filter(col => col.population > 0)

  def largestColony : Option[Planet] = {
    var sz : Int = -1
    var largest : Option[Planet] = None
    for {
      col <- colonies
      if col.population > sz
    } {
      largest = Some(col)
      sz = col.population
    }
    largest
  }

  def genName(nth : Int) : String = {
    var n : String = ""
    if (nth > 1) n = Names.nth(nth) + " "
    n = n + govt.title + " of "
    if (fullMembers.length == 1)
      n + fullMembers.head.getName
    else {
      for (i <- 0 until fullMembers.length) {
        if (i > 0) {
          n = n + (if (i == fullMembers.length - 1) " and " else ", ")
        }
        n = n + fullMembers(i).name
      }
      n
    }
  }

  def updateName : Unit = {
    number = 0
    while (true) {
      number = number + 1
      val n : String = genName(number)
      if (!sg.historicalCivNames.contains(n)) {
        name = n
        sg.historicalCivNames = sg.historicalCivNames + name
        return
      }
    }
  }

  def fullDesc(sg : SpaceGen) : String = {
    var sb : String = "THE " + name.toUpperCase + ":\n"
    val age : Int = sg.year - birthYear
    sb = sb + (
      if (age < 3) "A recently emerged"
      else if (age < 8) "A young"
      else if (age < 16) "A well-established"
      else "An ancient")
    sb = sb + (
      if (decrepitude < 20) ""
      else if (decrepitude < 40) ", corrupt"
      else ", crumbling")

    sb = sb + (
      if (resources < 2) ", dirt poor"
      else if (resources < 4) ", impoverished"
      else if (resources < 16) ""
      else if (resources < 25) ", wealthy"
      else ", fantastically wealthy")

    sb = sb + (
      if (techLevel < 2) ", primitive"
      else if (techLevel < 4) ""
      else if (techLevel < 7) ", advanced"
      else ", highly advanced")

    sb = sb + " " + govt.title + " of "
    sb = sb + (if (colonies.size == 1) "a single planet, " + colonies.head.name else colonies.size + " planets")
    sb = sb + ", with " + population + " billion inhabitants.\n"
    sb = sb + "Major populations:\n"
    var pops : Map[SentientType, Int] = Map()
    for {
      c <- colonies
      pop <- c.inhabitants
    } {
      pops = pops + (pop.typ -> (pops.getOrElse(pop.typ, 0) + pop.size))
    }
    for {
      (k, v) <- pops
      if fullMembers.contains(k)
    } {
      sb = sb + v + " billion " + k.getName + ".\n"
    }
    for {
      (k, v) <- pops
      if !fullMembers.contains(k)
    } {
      sb = sb + v + " billion enslaved " + k.getName + ".\n"
    }
    for {
      c <- colonies
      a <- c.artefacts
      if a.typ.isInstanceOf[Device]
    } {
      sb = sb + "It controls a " + a.typ.asInstanceOf[Device].getName + ".\n"
    }
    for {
      other <- sg.civs
      if other != this
    } {
      sb = sb + "It is at " + (if (relation(other) == WAR) "war" else "peace") + " with the " + other.name + ".\n"
    }

    sb
  }

  def has(base : Base) : Boolean = fullMembers.exists(st => st.base == base)

  def setGovt(gov : Government) : Unit = {
    govt = gov
    updateName
    Main.animate(Stage.tracking(largestColony.get.sprite, Stage.delay(10))) //TODO get
    for (s <- sprites) Main.add(Stage.change(s, getSprite))
    Main.animate
  }

  def getSprite : BufferedImage = {
    val crest : BufferedImage = MediaProvider.createImage(32, 32, Transparency.BITMASK)
    if (fullMembers.isEmpty)
      crest
    else {
      val g : Graphics2D = crest.createGraphics
      val sliceSz : Int = 32 / fullMembers.size
      for (i <- 0 until fullMembers.size) {
        var slice : BufferedImage = MediaProvider.getImage(govt.imageName)
        if (number > 1) {
          slice = MediaProvider.scale(slice, 32)
          val sg : Graphics2D = slice.createGraphics
          Draw.text(sg, "[333333]" + number, 8, 6)
        }
        if (fullMembers(i).color != null) {
          slice = MediaProvider.tint(slice, MediaProvider.TINTS(fullMembers(i).color))
        }
        g.drawImage(slice, i * sliceSz, 0, i * sliceSz + sliceSz, 32, i * sliceSz, 0, i * sliceSz + sliceSz, 32, null)
      }
      if (fullMembers.forall(m => m.color == null))
        MediaProvider.border(MediaProvider.tint(crest, MediaProvider.TINTS.values.toList(Math.abs(name.hashCode) % MediaProvider.TINTS.size)))
      else
        MediaProvider.border(crest)
    }
  }

}
