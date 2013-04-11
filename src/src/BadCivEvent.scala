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
import src.runner.Main
import src.util.Utils
import src.models.Plague
import src.models.LostArtefact
import src.models.THEOCRACY
import src.models.REPUBLIC
import src.models.Government
import src.models.DICTATORSHIP
import src.models.WAR
import src.models.Artefact
import src.models.PLANET_DESTROYER
import src.models.MIND_CONTROL_DEVICE
import src.models.UNIVERSAL_NUTRIENT
import src.models.VIRTUAL_REALITY_MATRIX
import src.models.CATOIDS
import src.models.ROBOTS
import src.models.ForReason

sealed abstract class BadCivEvent
case object REVOLT extends BadCivEvent
case object THEFT extends BadCivEvent
case object PUTSCH extends BadCivEvent
case object RELIGIOUS_REVIVAL extends BadCivEvent
case object MARKET_CRASH extends BadCivEvent
case object DARK_AGE extends BadCivEvent
case object MASS_HYSTERIA extends BadCivEvent
case object ACCIDENT extends BadCivEvent
case object CIVIL_WAR extends BadCivEvent
case object SPAWN_PLAGUE extends BadCivEvent
case object STARVATION extends BadCivEvent
case object SPAWN_PIRATE extends BadCivEvent
case object SPAWN_ROGUE_AI extends BadCivEvent

object BadCivEvent {

  val values : List[BadCivEvent] = List(
    REVOLT, THEFT, PUTSCH, RELIGIOUS_REVIVAL, MARKET_CRASH, DARK_AGE, MASS_HYSTERIA, ACCIDENT,
    CIVIL_WAR, SPAWN_PLAGUE, STARVATION, SPAWN_PIRATE, SPAWN_ROGUE_AI)

  def invoke(actor : Civ, sg : SpaceGen) : Unit = invoke(sg.pick(values), actor, sg)
  def invoke(event : BadCivEvent, actor : Civ, sg : SpaceGen) : Unit = event match {

    case REVOLT => {
      if (actor.fullColonies.size >= 2) {
        for {
          col <- actor.fullColonies
          val rebels : List[Population] = col.inhabitants.filterNot(pop => actor.fullMembers.contains(pop.typ))
          val nRebels : Int = rebels.foldRight(0)((pop, n) => n + pop.size)
          if nRebels > col.population / 2
        } {
          if (actor.has(MIND_CONTROL_DEVICE)) {
            sg.l("A slave revolt on " + col.name + " is quickly suppressed using the mind control device of the " + actor.name + ".")
          } else if (actor.has(VIRTUAL_REALITY_MATRIX)) {
            sg.l("A slave revolt on " + col.name + " fizzles out when the virtual reality matrix of the " + actor.name + " is adjusted.")
          } else if (actor.has(PLANET_DESTROYER)) {
            sg.l("A slave revolt on " + col.name + " falters from fear of the planet destroyer wielded by the " + actor.name + ".")
          } else if (col.has(CATOIDS.specialStructure)) {
            sg.l("A slave revolt on " + col.name + " falters from fear of torture pits of the " + actor.name + ".")
          } else {
            val resTaken : Int = actor.resources / actor.colonies.size
            val milTaken : Int = actor.military / actor.colonies.size
            val newCiv : Civ = new Civ(sg.year, List(rebels.head.typ), col, REPUBLIC, resTaken, sg)
            actor.setResources(actor.resources - resTaken)
            actor.setMilitary(actor.military - milTaken)
            newCiv.setMilitary(milTaken)
            newCiv.relations = newCiv.relations + (actor -> WAR)
            col.setOwner(Some(newCiv))
            actor.relations = actor.relations + (newCiv -> WAR)
            for {
              pop <- col.inhabitants
              if !rebels.contains(pop)
            } {
              col.dePop(pop, sg.year, ForReason("during a slave revolt"))
            }
            newCiv.updatePopImages
            Main.animate
            sg.l("Slaves on " + col.name + " revolt, killing their oppressors and declaring the Free " + newCiv.name + ".")
          }
          Main.confirm
          return
        }
      }
    }

    case THEFT => {
      val cands : List[Artefact] = actor.colonies.flatMap(p => p.artefacts)
      sg.pickMaybe(cands) match {
        case Some(a) => {
          var p : Planet = null
          for (p2 <- actor.colonies) if (p2.artefacts.contains(a)) p = p2
          val newP : Planet = sg.pick(sg.planets)
          p.removeArtefact(a)
          newP.strata = newP.strata ++ List(new LostArtefact("hidden", sg.year, a))
          sg.l("The " + a + " on " + p.name + " has been stolen and hidden on " + newP.name + ".")
          Main.confirm
        }
        case None => ()
      }
    }

    case PUTSCH if actor.govt != DICTATORSHIP => {
      val rulers : SentientType = sg.pick(actor.fullMembers)
      val oldName : String = actor.name
      actor.fullMembers = List(rulers)
      actor.setGovt(DICTATORSHIP)
      actor.updatePopImages
      Main.animate
      sg.l("A military putsch turns the " + oldName + " into the " + actor.name + ".")
      Main.confirm
    }

    case RELIGIOUS_REVIVAL => {
      val oldName : String = actor.name
      actor.setGovt(THEOCRACY)
      actor.updatePopImages
      Main.animate
      sg.l("Religious fanatics sieze power in the " + oldName + " and declare the " + actor.name + ".")
      Main.confirm
    }

    case MARKET_CRASH => {
      actor.setResources(actor.resources / 5)
      sg.l("A market crash impoverishes the " + actor.name + ".")
      Main.confirm
    }

    case DARK_AGE => {
      var ret : String = "The " + actor.name + " enters a dark age."
      actor.setTechLevel(actor.techLevel - 1)
      if (actor.techLevel == 0) {
        if (actor.fullMembers.size > 1) {
          ret = ret + " With the knowledge of faster-than-light travel lost, each planet in the empire has to fend for itself."
        }
        actor.colonies.foreach(_ darkAge (sg.year))
        actor.updatePopImages
        Main.animate
      }
      sg.l(ret)
      Main.confirm
    }

    case MASS_HYSTERIA => {
      for (c <- actor.fullColonies) {
        val pop : Int = c.population
        c.inhabitants.head.setSize(1)
        while (c.inhabitants.size > 1) {
          c.dePop(c.inhabitants.tail.head, sg.year, ForReason("from mass hysteria"))
        }
      }
      sg.l("Mass hysteria breaks out in the " + actor.name + ", killing billions.")
      Main.confirm
    }

    case ACCIDENT => {
      val p : Planet = sg.pick(actor.fullColonies)
      p.setPollution(p.getPollution + 5)
      sg.l("An industrial accident on " + p.name + " causes deadly levels of pollution.")
      Main.confirm
    }

    case CIVIL_WAR => {
      val bigPlanetsRaw : List[Planet] = actor.colonies.filter(c => c.population > 2)
      if (bigPlanetsRaw.size > 1) {
        val bigPlanets : List[Planet] = Utils.shuffle(bigPlanetsRaw, sg.r)
        val newCiv : Civ = new Civ(sg.year, Nil, bigPlanets.head, sg.pick(Government.values), actor.resources / 2, sg)
        newCiv.setMilitary(actor.military / 2)
        newCiv.setTechLevel(actor.techLevel)
        newCiv.setWeapLevel(actor.weapLevel)
        actor.setMilitary(actor.military - newCiv.military)
        actor.setResources(actor.resources - newCiv.resources)
        for (i <- 1 until bigPlanets.size / 2) {
          bigPlanets(i).setOwner(Some(newCiv))
          for {
            pop <- bigPlanets(i).inhabitants
            if !newCiv.fullMembers.contains(pop.typ)
          } {
            newCiv.fullMembers = newCiv.fullMembers :+ pop.typ
          }
        }
        for (pop <- bigPlanets.head.inhabitants) {
          if (!newCiv.fullMembers.contains(pop.typ)) {
            newCiv.fullMembers = newCiv.fullMembers :+ pop.typ
          }
        }
        for (c <- actor.colonies) {
          if (!bigPlanets.contains(c) && sg.coin) {
            c.setOwner(Some(newCiv))
          }
        }
        newCiv.setGovt(newCiv.govt)
        newCiv.relations = newCiv.relations + (actor -> WAR)
        actor.relations = actor.relations + (newCiv -> WAR)
        newCiv.updatePopImages
        Main.animate
        sg.l("The " + newCiv.name + " secedes from the " + actor.name + ", leading to a civil war!")
        Main.confirm
      } else
        ()
    }

    case SPAWN_PLAGUE if !actor.fullColonies.isEmpty => {
      val p : Planet = sg.pick(actor.fullColonies)
      val plague : Plague = new Plague(sg)
      val is : List[Population] = p.inhabitants.filter(pop => pop.typ.base != ROBOTS)
      sg.pickMaybe(is) match {
        case Some(pop) => {
          plague.affects = plague.affects :+ pop.typ
          p.addPlague(plague)
          sg.l("The deadly " + plague.desc + ", arises on " + p.name + ".")
          Main.confirm
        }
        case None => ()
      }
    }

    case STARVATION if !actor.has(UNIVERSAL_NUTRIENT) => {
      val p : Planet = sg.pick(actor.fullColonies)
      var deaths : Int = 0
      for {
        pop <- p.inhabitants
        if pop.typ.base != ROBOTS
      } {
        val d : Int = pop.size - pop.size / 2
        if (d >= pop.size) {
          p.dePop(pop, sg.year, ForReason("due to starvation"))
          deaths = deaths + pop.size
        } else {
          pop.setSize(pop.size - d)
          deaths = deaths + d
        }
      }
      if (deaths != 0) {
        sg.l("A famine breaks out on " + p.name + ", killing " + deaths + " billion" + (
          if (p.population == 0) {
            p.deCiv(sg.year, ForReason("due to starvation"))
            ", wiping out all sentient life."
          } else
            "."))
        Main.confirm
      } else
        ()
    }

    case SPAWN_PIRATE => {
      val st : SentientType = sg.pick(actor.fullMembers)
      val color : String = sg.pick(Names.COLORS)
      val name : String = color + st.base.pSuffix
      val p : Planet = sg.pick(actor.colonies)
      val ag : Agent = new Agent(PIRATE(color, st), sg.year, name, sg)
      ag.fleet = 2 + sg.d(6)
      ag.resources = sg.d(6)
      ag.setLocation(p)
      sg.l("The pirate " + name + " establishes " + (if (sg.coin) "himself" else "herself") + " on " + p.name + ".")
      Main.confirm
    }

    case SPAWN_ROGUE_AI => {
      val p : Planet = sg.pick(actor.colonies)
      val pref : String = sg.pick(List("Experiment ", "System ", "Mind ", "Simulation "))
      val ag : Agent = new Agent(ROGUE_AI, sg.year, pref + sg.r.nextInt(500), sg)
      ag.setLocation(p)
      sg.l("The " + actor.name + " accidentally create the rogue AI " + ag.name + " on " + p.name + ".")
      Main.confirm
    }
    
    case _ => ()

  }

}


